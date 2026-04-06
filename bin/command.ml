open Cmdliner

let file_arg =
  let doc = "The ce-lang file to process." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"file" ~doc)


