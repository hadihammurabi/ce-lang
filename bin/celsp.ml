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

class ce_lsp_server =
  object (self)
    inherit Jsonrpc2.server as super
    method spawn_query_handler f = Lwt.async f

    method! config_completion =
      Some
        (Linol_lsp.Lsp.Types.CompletionOptions.create ~resolveProvider:false ())

    method on_notif_doc_did_open ~notify_back d ~content =
      log "on_notif_doc_did_open: checking syntax...";
      publish_diags notify_back d.uri content

    method on_notif_doc_did_close ~notify_back:_ _uri = Lwt.return_unit

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_ ~new_content
        =
      log "on_notif_doc_did_change: file changed, running parser...";
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
