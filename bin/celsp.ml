open Cmdliner
open Lsp.Types
open Linol_lwt

let log msg = Printf.eprintf "[ce-lsp] %s\n%!" msg
let documents : (string, string) Hashtbl.t = Hashtbl.create 10
let signatures : (string * string, string) Hashtbl.t = Hashtbl.create 50

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
  | "fn" -> Some "**fn**\n\nDeclares a new function."
  | "let" -> Some "**let**\n\nDeclares a variable."
  | "mut" -> Some "**mut**\n\nMarks a variable or struct field as mutable."
  | "struct" -> Some "**struct**\n\nDeclares a custom data structure."
  | "trait" -> Some "**trait**\n\nDeclares an interface/trait."
  | "impl" -> Some "**impl**\n\nImplements methods for a struct or trait."
  | "int" | "string" | "bool" | "float" | "char" | "void" | "uint" | "u8" | "i8"
  | "i32" | "i64" | "f32" | "f64" ->
      Some ("```ce\n" ^ word ^ "\n```\n\nBuilt-in primitive type.")
  | _ -> None

let get_dynamic_completions src =
  let lexbuf = Lexing.from_string src in
  let symbols = Hashtbl.create 50 in

  let rec loop () =
    try
      match Ce_lexer.Lexer.tokenize lexbuf with
      | Ce_parser.Parser.EOF -> ()
      | Ce_parser.Parser.LET ->
          (match Ce_lexer.Lexer.tokenize lexbuf with
          | Ce_parser.Parser.MUT -> (
              match Ce_lexer.Lexer.tokenize lexbuf with
              | Ce_parser.Parser.IDENT name ->
                  Hashtbl.replace symbols name CompletionItemKind.Variable
              | _ -> ())
          | Ce_parser.Parser.IDENT name ->
              Hashtbl.replace symbols name CompletionItemKind.Variable
          | _ -> ());
          loop ()
      | Ce_parser.Parser.FN ->
          (match Ce_lexer.Lexer.tokenize lexbuf with
          | Ce_parser.Parser.IDENT name ->
              Hashtbl.replace symbols name CompletionItemKind.Function
          | _ -> ());
          loop ()
      | Ce_parser.Parser.STRUCT ->
          (match Ce_lexer.Lexer.tokenize lexbuf with
          | Ce_parser.Parser.IDENT name ->
              Hashtbl.replace symbols name CompletionItemKind.Struct
          | _ -> ());
          loop ()
      | Ce_parser.Parser.TRAIT ->
          (match Ce_lexer.Lexer.tokenize lexbuf with
          | Ce_parser.Parser.IDENT name ->
              Hashtbl.replace symbols name CompletionItemKind.Interface
          | _ -> ());
          loop ()
      | _ -> loop ()
    with _ -> ()
  in

  loop ();

  Hashtbl.fold
    (fun label kind acc -> CompletionItem.create ~label ~kind () :: acc)
    symbols []

let definitions : (string * string, Linol_lsp.Lsp.Types.Location.t) Hashtbl.t =
  Hashtbl.create 50

let get_word_at_pos content line col =
  let lines = String.split_on_char '\n' content in
  if line < 0 || line >= List.length lines then ""
  else
    let target_line = List.nth lines line in
    if col < 0 || col > String.length target_line then ""
    else
      let is_ident_char c =
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false
      in
      let rec find_start i =
        if i > 0 && is_ident_char target_line.[i - 1] then find_start (i - 1)
        else i
      in
      let rec find_end i =
        if i < String.length target_line && is_ident_char target_line.[i] then
          find_end (i + 1)
        else i
      in
      let start_idx = find_start col in
      let end_idx = find_end col in
      if start_idx < end_idx then
        String.sub target_line start_idx (end_idx - start_idx)
      else ""

let index_document uri src =
  let lexbuf = Lexing.from_string src in
  let uri_str = DocumentUri.to_string uri in

  let rec loop () =
    try
      let token = Ce_lexer.Lexer.tokenize lexbuf in
      match token with
      | Ce_parser.Parser.EOF -> ()
      | Ce_parser.Parser.LET | Ce_parser.Parser.FN | Ce_parser.Parser.STRUCT
      | Ce_parser.Parser.TRAIT ->
          let next_tok = Ce_lexer.Lexer.tokenize lexbuf in
          let target_tok =
            if token = Ce_parser.Parser.LET && next_tok = Ce_parser.Parser.MUT
            then Ce_lexer.Lexer.tokenize lexbuf
            else next_tok
          in

          (match target_tok with
          | Ce_parser.Parser.IDENT name ->
              let pos = lexbuf.lex_curr_p in
              let line = pos.pos_lnum - 1 in
              let col = pos.pos_cnum - pos.pos_bol in
              let start_pos =
                Position.create ~line ~character:(col - String.length name)
              in
              let end_pos = Position.create ~line ~character:col in
              let range = Range.create ~start:start_pos ~end_:end_pos in
              let loc = Location.create ~uri ~range in

              Hashtbl.replace definitions (uri_str, name) loc;

              let lines = String.split_on_char '\n' src in
              if line >= 0 && line < List.length lines then
                let sig_text = String.trim (List.nth lines line) in
                Hashtbl.replace signatures (uri_str, name) sig_text
          | _ -> ());
          loop ()
      | _ -> loop ()
    with _ -> ()
  in
  loop ()

let get_document_symbols src =
  let lexbuf = Lexing.from_string src in
  let symbols = ref [] in

  let rec loop () =
    try
      let token = Ce_lexer.Lexer.tokenize lexbuf in
      match token with
      | Ce_parser.Parser.EOF -> ()
      | Ce_parser.Parser.LET | Ce_parser.Parser.FN | Ce_parser.Parser.STRUCT
      | Ce_parser.Parser.TRAIT ->
          let kind =
            match token with
            | Ce_parser.Parser.LET -> SymbolKind.Variable
            | Ce_parser.Parser.FN -> SymbolKind.Function
            | Ce_parser.Parser.STRUCT -> SymbolKind.Struct
            | Ce_parser.Parser.TRAIT -> SymbolKind.Interface
            | _ -> SymbolKind.Variable
          in

          let next_tok = Ce_lexer.Lexer.tokenize lexbuf in
          let target_tok =
            if token = Ce_parser.Parser.LET && next_tok = Ce_parser.Parser.MUT
            then Ce_lexer.Lexer.tokenize lexbuf
            else next_tok
          in

          (match target_tok with
          | Ce_parser.Parser.IDENT name ->
              let pos = lexbuf.lex_curr_p in
              let line = pos.pos_lnum - 1 in
              let col = pos.pos_cnum - pos.pos_bol in
              let start_pos =
                Position.create ~line ~character:(col - String.length name)
              in
              let end_pos = Position.create ~line ~character:col in
              let range = Range.create ~start:start_pos ~end_:end_pos in

              let symbol =
                DocumentSymbol.create ~name ~kind ~range ~selectionRange:range
                  ()
              in
              symbols := symbol :: !symbols
          | _ -> ());
          loop ()
      | _ -> loop ()
    with _ -> ()
  in
  loop ();
  List.rev !symbols

let get_inlay_hints src =
  let lexbuf = Lexing.from_string src in
  let hints : Linol_lsp.Types.InlayHint.t list ref = ref [] in

  let rec loop () =
    try
      let token = Ce_lexer.Lexer.tokenize lexbuf in
      match token with
      | Ce_parser.Parser.EOF -> ()
      | Ce_parser.Parser.LET ->
          let next_tok = Ce_lexer.Lexer.tokenize lexbuf in
          let is_mut, name_tok =
            if next_tok = Ce_parser.Parser.MUT then
              (true, Ce_lexer.Lexer.tokenize lexbuf)
            else (false, next_tok)
          in

          (match name_tok with
          | Ce_parser.Parser.IDENT name ->
              let after_name = Ce_lexer.Lexer.tokenize lexbuf in
              if after_name = Ce_parser.Parser.EQUALS then begin
                let pos = lexbuf.lex_curr_p in
                let line = pos.pos_lnum - 1 in
                let col = pos.pos_cnum - pos.pos_bol - 1 in
                let val_tok = Ce_lexer.Lexer.tokenize lexbuf in
                let inferred_type =
                  match val_tok with
                  | Ce_parser.Parser.INT _ -> "int"
                  | Ce_parser.Parser.FLOAT _ -> "float"
                  | Ce_parser.Parser.STRING _ -> "string"
                  | Ce_parser.Parser.TRUE | Ce_parser.Parser.FALSE -> "bool"
                  | Ce_parser.Parser.CHAR _ -> "char"
                  | _ -> "any"
                in

                let hint_pos = Position.create ~line ~character:col in
                let hint =
                  InlayHint.create ~position:hint_pos
                    ~label:(`String (": " ^ inferred_type))
                    ~kind:InlayHintKind.Type ~paddingLeft:false
                    ~paddingRight:true ()
                in
                hints := hint :: !hints
              end
          | _ -> ());
          loop ()
      | _ -> loop ()
    with _ -> ()
  in
  loop ();
  !hints

let get_code_lenses uri src =
  let lexbuf = Lexing.from_string src in
  let lenses = ref [] in
  let uri_str = DocumentUri.to_string uri in

  let rec loop () =
    try
      let token = Ce_lexer.Lexer.tokenize lexbuf in
      match token with
      | Ce_parser.Parser.EOF -> ()
      | Ce_parser.Parser.FN ->
          (match Ce_lexer.Lexer.tokenize lexbuf with
          | Ce_parser.Parser.IDENT "main" ->
              let pos = lexbuf.lex_curr_p in
              let line = pos.pos_lnum - 1 in
              let range =
                Range.create
                  ~start:(Position.create ~line ~character:0)
                  ~end_:(Position.create ~line ~character:7)
              in

              let command =
                Command.create ~title:"▶ Run Program" ~command:"ce.run"
                  ~arguments:[ `String uri_str ]
                  ()
              in
              let lens = CodeLens.create ~range ~command () in
              lenses := lens :: !lenses
          | _ -> ());
          loop ()
      | _ -> loop ()
    with _ -> ()
  in
  loop ();
  !lenses

class ce_lsp_server =
  object (self)
    inherit Jsonrpc2.server as super
    method! config_hover = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method! config_symbol = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)

    method! config_code_lens_options =
      Some (CodeLensOptions.create ~resolveProvider:false ())

    method! config_completion =
      Some
        (Linol_lsp.Lsp.Types.CompletionOptions.create ~resolveProvider:false ())

    method spawn_query_handler f = Lwt.async f

    method on_notif_doc_did_open ~notify_back d ~content =
      log "on_notif_doc_did_open: checking syntax...";
      Hashtbl.replace documents (DocumentUri.to_string d.uri) content;
      index_document d.uri content;
      publish_diags notify_back d.uri content

    method on_notif_doc_did_close ~notify_back:_ uri =
      Hashtbl.remove documents (DocumentUri.to_string uri.uri);
      Lwt.return_unit

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_ ~new_content
        =
      log "on_notif_doc_did_change: file changed, running parser...";
      Hashtbl.replace documents (DocumentUri.to_string d.uri) new_content;
      index_document d.uri new_content;
      publish_diags notify_back d.uri new_content

    method! on_req_execute_command ~notify_back ~id:_ ~workDoneToken:_ _command
        _args =
      Lwt.return `Null

    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos:_ ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ _doc_state =
      log "Autocomplete requested!";
      let uri_str = DocumentUri.to_string uri in
      let content =
        match Hashtbl.find_opt documents uri_str with Some c -> c | None -> ""
      in

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
      let dynamic_items = get_dynamic_completions content in
      let items = fn_snippet :: (keywords @ types @ dynamic_items) in
      Lwt.return_some (`List items)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        _doc_state =
      let uri_str = DocumentUri.to_string uri in
      match Hashtbl.find_opt documents uri_str with
      | None -> Lwt.return_none
      | Some content -> (
          let word = get_word_at_pos content pos.line pos.character in
          if word = "" then Lwt.return_none
          else
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

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ _doc_state =
      log "Go to definition requested!";
      let uri_str = DocumentUri.to_string uri in

      match Hashtbl.find_opt documents uri_str with
      | None -> Lwt.return_none
      | Some content -> (
          let word = get_word_at_pos content pos.line pos.character in
          log ("Looking for definition of: " ^ word);

          if word = "" then Lwt.return_none
          else
            match Hashtbl.find_opt definitions (uri_str, word) with
            | Some loc -> Lwt.return_some (`Location [ loc ])
            | None -> Lwt.return_none)

    method on_req_symbol ~notify_back:_ ~id:_ ~uri ~workDoneToken:_
        ~partialResultToken:_ _doc_state =
      log "Document Symbols (Outline) requested!";
      let uri_str = DocumentUri.to_string uri in

      match Hashtbl.find_opt documents uri_str with
      | None -> Lwt.return_none
      | Some content ->
          let symbols = get_document_symbols content in
          Lwt.return_some (`DocumentSymbol symbols)

    method! on_req_inlay_hint ~notify_back:_ ~id:_ ~uri ~range:_ () =
      log "Inlay hints requested!";
      let uri_str = DocumentUri.to_string uri in

      match Hashtbl.find_opt documents uri_str with
      | None -> Lwt.return_none
      | Some content ->
          let hints = get_inlay_hints content in
          Lwt.return_some hints

    method! on_req_code_lens ~notify_back:_ ~id:_ ~uri ~workDoneToken:_
        ~partialResultToken:_ state =
      let uri_str = DocumentUri.to_string uri in
      match Hashtbl.find_opt documents uri_str with
      | None -> Lwt.return []
      | Some content -> Lwt.return (get_code_lenses uri content)
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
