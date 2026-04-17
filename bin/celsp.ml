open Cmdliner
open Lsp.Types
open Linol_lwt

let log msg =
  Printf.eprintf "[ce-lsp] %s\n%!" msg

class ce_lsp_server =
  object (self)
    inherit Jsonrpc2.server
    method spawn_query_handler f = Lwt.async f
    method on_notif_doc_did_open ~notify_back:_ _d ~content:_ = 
      log "A .ce file was opened!";
      Lwt.return_unit
    method on_notif_doc_did_close ~notify_back:_ _uri = Lwt.return_unit

    method on_notif_doc_did_change ~notify_back _d _changes ~old_content:_
        ~new_content =
      Lwt.return_unit
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
