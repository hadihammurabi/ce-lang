open Cmdliner
open Lsp.Types
open Linol_lwt

let log msg = Printf.eprintf "[ce-lsp] %s\n%!" msg

let check_syntax src =
  let lexbuf = Lexing.from_string src in
  try
    let _ast = Ce_parser.Parser.prog Ce_lexer.Lexer.tokenize lexbuf in
    None
  with
  | Ce_parser.Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum - 1 in
      let col = pos.pos_cnum - pos.pos_bol in
      Some (line, col, "Syntax Error: unexpected token")
  | Ce_lexer.Lexer.Lexer_error (msg, pos) ->
      let line = pos.pos_lnum - 1 in
      let col = pos.pos_cnum - pos.pos_bol in
      Some (line, col, "Lexer Error: " ^ msg)
  | e -> Some (0, 0, "Unknown Error: " ^ Printexc.to_string e)

let publish_diags notify_back uri src =
  let diags =
    match check_syntax src with
    | None -> []
    | Some (line, col, msg) ->
        let start_pos = Position.create ~line ~character:col in
        let end_pos = Position.create ~line ~character:(col + 1) in
        let range = Range.create ~start:start_pos ~end_:end_pos in
        let diagnostic = Diagnostic.create ~range ~message:(`String msg) () in
        [ diagnostic ]
  in
  notify_back#send_diagnostic diags

let get_word_at_pos content line col =
  let lines = String.split_on_char '\n' content in
  try
    let target_line = List.nth lines line in
    let is_ident c =
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let start_idx = ref col in
    while !start_idx > 0 && is_ident target_line.[!start_idx - 1] do
      decr start_idx
    done;
    let end_idx = ref col in
    let len = String.length target_line in
    while !end_idx < len && is_ident target_line.[!end_idx] do
      incr end_idx
    done;
    if !start_idx < !end_idx then
      String.sub target_line !start_idx (!end_idx - !start_idx)
    else ""
  with _ -> ""

let get_hover_docs word =
  match word with
  | "println" ->
      Some
        "```ce\n\
         fn println(args... any) void\n\
         ```\n\
         Prints values to stdout, followed by a newline."
  | "print" ->
      Some "```ce\nfn print(args... any) void\n```\nPrints values to stdout."
  | "printf" ->
      Some
        "```ce\n\
         fn printf(fmt string, args... any) void\n\
         ```\n\
         Formats and prints values to stdout."
  | "malloc" ->
      Some
        "```ce\n\
         fn malloc<T>(count uint) *T\n\
         ```\n\
         Allocates space on the heap for `count` elements of type `T`."
  | "realloc" ->
      Some
        "```ce\n\
         fn realloc<T>(ptr *T, new_size uint) *T\n\
         ```\n\
         Reallocates memory to a new size."
  | "free" ->
      Some
        "```ce\n\
         fn free<T>(ptr *T) void\n\
         ```\n\
         Frees memory previously allocated by `malloc`."
  | "typeof" ->
      Some
        "```ce\n\
         fn typeof(val any) string\n\
         ```\n\
         Returns the name of the type of the given value."
  | "int" | "string" | "bool" | "float" | "char" | "void" | "uint" | "u8" | "i8"
  | "i32" | "i64" | "f32" | "f64" ->
      Some ("**" ^ word ^ "**\n\nBuilt-in primitive type.")
  | "fn" -> Some "**fn**\n\nDeclares a new function."
  | "let" -> Some "**let**\n\nDeclares a variable."
  | "mut" -> Some "**mut**\n\nMarks a variable or struct field as mutable."
  | "struct" -> Some "**struct**\n\nDeclares a custom data structure."
  | "trait" -> Some "**trait**\n\nDeclares an interface/trait."
  | "impl" -> Some "**impl**\n\nImplements methods for a struct or trait."
  | _ -> None

class ce_lsp_server =
  object (self)
    inherit Jsonrpc2.server as super
    val documents : (string, string) Hashtbl.t = Hashtbl.create 10
    method spawn_query_handler f = Lwt.async f

    method! config_completion =
      Some
        (Linol_lsp.Lsp.Types.CompletionOptions.create ~resolveProvider:false ())

    method! config_hover = Some (`Bool true)

    method on_notif_doc_did_open ~notify_back d ~content =
      log "on_notif_doc_did_open: checking syntax...";
      Hashtbl.replace documents (DocumentUri.to_string d.uri) content;
      publish_diags notify_back d.uri content

    method on_notif_doc_did_close ~notify_back:_ uri =
      Hashtbl.remove documents (DocumentUri.to_string uri.uri);
      Lwt.return_unit

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_ ~new_content
        =
      log "on_notif_doc_did_change: file changed, running parser...";
      Hashtbl.replace documents (DocumentUri.to_string d.uri) new_content;
      publish_diags notify_back d.uri new_content

    method! on_req_completion ~notify_back:_ ~id:_ ~uri:_ ~pos:_ ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      log "Autocomplete requested!";

      let create_keyword label =
        CompletionItem.create ~label ~kind:CompletionItemKind.Keyword ()
      in

      let create_type label =
        CompletionItem.create ~label ~kind:CompletionItemKind.TypeParameter ()
      in

      let keywords =
        List.map create_keyword
          [
            "let";
            "mut";
            "if";
            "else";
            "for";
            "break";
            "return";
            "import";
            "type";
            "impl";
            "raise";
            "catch";
            "struct";
            "trait";
            "true";
            "false";
            "nil";
          ]
      in

      let types =
        List.map create_type
          [
            "int";
            "string";
            "bool";
            "float";
            "char";
            "void";
            "i8";
            "i16";
            "i32";
            "i64";
            "i128";
            "uint";
            "u8";
            "u16";
            "u32";
            "u64";
            "u128";
            "f32";
            "f64";
          ]
      in

      let fn_snippet =
        CompletionItem.create ~label:"fn" ~kind:CompletionItemKind.Snippet
          ~insertText:"fn ${1:name}(${2}) ${3:void} {\n  $0\n}"
          ~insertTextFormat:InsertTextFormat.Snippet
          ~detail:"Define a new function" ()
      in
      let items = fn_snippet :: (keywords @ types) in
      Lwt.return_some (`List items)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        _doc_state =
      log "Hover requested!";
      let uri_str = DocumentUri.to_string uri in
      match Hashtbl.find_opt documents uri_str with
      | None -> Lwt.return_none
      | Some content -> (
          let line = pos.line in
          let col = pos.character in
          let word = get_word_at_pos content line col in
          log @@ Printf.sprintf "search: %d %d %s" line col word;

          match get_hover_docs word with
          | None -> Lwt.return_none
          | Some markdown_text ->
              let contents =
                `MarkupContent
                  (MarkupContent.create ~kind:MarkupKind.Markdown
                     ~value:markdown_text)
              in
              let hover_response = Hover.create ~contents () in
              Lwt.return_some hover_response)
  end

let execute () =
  let server = new ce_lsp_server in
  let run_server () =
    let looper = Jsonrpc2.create_stdio ~env:() server in
    Jsonrpc2.run looper
  in
  Lwt_main.run (run_server ())

let command =
  let doc = "Compile inserted ce-lang code file then execute that" in
  let info = Cmd.info "lsp" ~doc in
  Cmd.v info Term.(const execute $ const ())
