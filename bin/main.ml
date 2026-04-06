open Cmdliner

let cmd =
  let doc = "Ce-lang - A Good Programming Language" in
  let info = Cmd.info "ce" ~version:"0.1.0" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [Run.command; Build.command; Debug.command]

let () = exit (Cmd.eval cmd)
