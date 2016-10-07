
open Ocamlbuild_plugin
let () =
  dispatch @@ function
  | Before_options ->
    Options.use_ocamlfind := true
  | After_rules ->
    rule "ocaml internal module source"
      ~prod:"%Internal.ml"
      ~dep:"%.ml"
      ~doc:"Creates a copy of an Ocaml module without an mli interface, for internal testing."
      begin fun env _build ->
        let arg = env "%.ml" and out = env "%Internal.ml" in
        Cmd(S[A"cp" ; P arg ; Px out])
      end
  | _ -> ()
