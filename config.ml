(* Copyright (C) 2017, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

(** Configuration for the "mirage" tool. *)

open Mirage

type framebuffer_ty = Framebuffer
let framebuffer = Type Framebuffer

let config_framebuffer =
  impl @@ object inherit Mirage.base_configurable
    method module_name = "Framebuffer_placeholder_goes_here"
    method name = "my framebuffer, hello!"
    method ty = framebuffer
    method! packages : package list value =
    (Key.match_ Key.(value target) @@ begin function
      | `Xen -> [package ~min:"0.4" "mirage-qubes";
                 package "mirage-framebuffer-qubes"]
      | `Unix | `MacOSX ->
         [package "mirage-unix"; package "mirage-framebuffer-tsdl"]
      | `Qubes | `Hvt | `Virtio -> []
      end)
    |> Mirage.Key.map (List.cons (package "mirage-framebuffer"))
    method! deps = []
    method! connect mirage_info _modname _args =
      Key.eval (Info.context mirage_info) @@
      Key.match_ Key.(value target) @@ begin function
        | `Unix | `MacOSX ->
            {| Lwt.return (
                 let fb =
                   let module X = Framebuffer.Make(Framebuffer_tsdl) in
                   X.init ()
                 in
                 Lwt.return ((), fb))
            |}
        | `Xen ->
            {| Lwt.return (
                 Qubes.RExec.connect ~domid:0 () >>= fun qrexec ->
                 Qubes.GUI.connect ~domid:0 () >>= fun gui ->

                 let fb =
                   let module X = Framebuffer.Make(Framebuffer_qubes) in
                   X.init gui
                 in
                 Lwt.async (Qubes.GUI.listen gui) ;
                 Lwt.async (fun () ->
                   OS.Lifecycle.await_shutdown_request ()
                   >>= fun (`Poweroff | `Reboot) ->
                   Qubes.RExec.disconnect qrexec
                 );

                 Lwt.return ((12345, qrexec, gui),fb) )
            |}
        | `Virtio | `Hvt ->
          failwith "Mirage_Framebuffer is not implemented for Virtio | Hvt"
        | `Qubes ->
          failwith "Mirage_framebuffer must be used with -t xen for Qubes"
      end
  end

let card_secret : string option Mirage.Key.key =
  let doc = Key.Arg.info ~doc:"The 20-byte SHA1 secret provisioned on the card"
      ~docv:"SECRET" ["card_secret"] in
  Key.(create "card_secret" Arg.(required ~stage:`Configure string doc))

let suitestring : string option Mirage.Key.key =
  let doc = Key.Arg.info ~doc:"suitestring" ["suite"] in
  Key.(create "suitestring" Arg.(required ~stage:`Configure string doc))

let main =
  let keys = List.map Key.abstract [ card_secret ; suitestring ] in
  foreign
    ~keys
    ~deps:[abstract config_framebuffer ; abstract nocrypto ]
    ~packages:[
      package "nocrypto" ;
      package "cstruct";
      package "cs";
      package ~min:"1.0.3" "rfc6287";
      package "mirage-logs";
    ] "Unikernel.Main" (time @-> job)

let () =
  register "ocra" [ main $ default_time ] ~argv:no_argv
