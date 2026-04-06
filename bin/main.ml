open Cmdliner

let main_do args = Printf.printf "\n"

let main = 
  let info = Cmd.info "main" in
  Cmd.v info Term.(const main_do $ const())

let cmd =
  let doc = "Ce-lang - A Good Programming Language" in
  let info = Cmd.info "ce" ~version:"0.1.0" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [Build.command; Run.command]

let () = exit (Cmd.eval cmd)
