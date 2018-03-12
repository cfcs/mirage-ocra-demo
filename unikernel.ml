(* Copyright (C) 2016, Thomas Leonard
   See the README file for details. *)

open Lwt.Infix

let src = Logs.Src.create "ocra" ~doc:"OCRA RFC6287 auth unikernel"
module Log = (val Logs.src_log src : Logs.LOG)

module Main(Time : Mirage_time_lwt.S)
=
struct

  module Ocra(FB : Framebuffer.S) =
  struct

  let input_buffer = Buffer.create 0

  type input_state =
    | Menu
    | Enter_response of int * string (* counter window , challenge *)

  let state = ref Menu

  let counter = ref 0_L (* emulate persistent storage *)

  let rec input_loop suite fb =
    let open Framebuffer__S in
    FB.recv_event fb >>= function
    | Window_close -> Lwt.return_unit
    | Keypress {pressed = true; keysym; mods; _} ->
      let open Framebuffer__Keycodes in
      begin match keysym, mods with
        | None , _ -> Lwt.return_unit (* Not a printable character *)
        | Some (`Return), _ -> (* "Enter" key pressed, handle input: *)
          let line = Buffer.contents input_buffer in
          Buffer.clear input_buffer ;
          let display text =
            FB.output_tty fb (FB.term_size fb) (text ^ "\n") >>= fun () ->
            FB.redraw fb
          in
          let pin = `String "1234" in
          (* Enter advanced 1990-style menu system*)
          begin match !state with
            | Menu when line = "s" || line = "S" ->
              let challenge = Rfc6287.challenge suite in
              counter := 0_L ;
              state := Enter_response (100_000, challenge);
              display @@ "Sync (PIN: 1234), challenge: " ^ challenge
            | Menu when line = "c" || line = "C" ->
              let challenge = Rfc6287.challenge suite in
              state := Enter_response (1, challenge);
              display ("Challenge (PIN 1234): " ^ challenge )
            | Menu ->
              display "Enter 's' for SYNC or 'c' for CHALLENGE"
            | Enter_response (counter_window, challenge) ->
              state := Menu ;
              display "" >>= fun () -> (* print newline *)
              begin match Rfc6287.verify ~c:!counter ~p:pin
                            ~q:challenge ~cw:counter_window
                            ~key:(Key_gen.card_secret () |> Cs.of_cstruct
                                  |> Cs.of_hex |> R.get_ok |> Cs.to_cstruct)
                            (* line is the card's output: *)
                            ~a:(Cstruct.of_string line) suite with
              | Ok (true, Some next_counter) ->
                counter := Int64.(sub next_counter one);
                display ( if counter_window = 1
                          then "You're authenticated, next counter: "
                               ^ Int64.to_string next_counter
                          else "Next counter: " ^ Int64.to_string next_counter )
              | Ok (true, None) ->
                display "SOMETHING WORKED (not using counter)"
              | Ok (false, _) ->
                display ( if counter_window = 1
                          then "Incorrect, didn't check out"
                          else "Failed to sync after 100k tries, giving up.")
              | Error _ -> display "The RFC6287 lib crashed"
              end >|= fun () -> counter := Int64.succ !counter
          end
        | Some ks , kmods ->
          (* Add input to line buffer: *)
          ignore @@ ( US_keyboard.to_unicode kmods ks
                      |> List.map (fun c ->
                          Buffer.add_char input_buffer @@ Uchar.to_char c) ) ;
          ( match !state with
            | Menu -> Lwt.return_unit
            | Enter_response _ -> FB.output_tty fb (FB.term_size fb)
                                    "*" >>= fun () -> FB.redraw fb )
      end >>= fun () -> input_loop suite fb
    | _event -> input_loop suite fb

  let start () =
    Log.info (fun f -> f "Starting OCRA unikernel");

    let suite = Key_gen.suitestring () in

    FB.window ~width:550 ~height:600 >>= fun fb ->
    FB.output_tty fb (FB.term_size fb) @@ "Welcome to OCRA!\n" >>= fun () ->
    FB.output_tty fb (FB.term_size fb)
      ("Card suite: " ^ suite ^"\n") >>= fun () ->
    begin match Key_gen.card_secret () |> Cstruct.of_hex with
      | exception _ ->
        FB.output_tty fb (FB.term_size fb)
          "Error: Card secret must be a valid hex string \
           (*WITHOUT* 0x prefix)\n" >>= fun () ->
        FB.redraw fb >|= fun () -> `card_secret_error
      | _ -> Lwt.return `card_secret_ok
    end >>= fun secret_status ->
    (* Check that the suite is good: *)
    begin match secret_status, Rfc6287.t_of_string suite with
      | `card_secret_ok, Ok suite -> input_loop suite fb
      | _, Error err ->
        FB.output_tty fb (FB.term_size fb) @@ "Error: " ^
          (match err with
            | Rfc6287.Invalid_suite_string -> "Invalid_suite_string"
            | Rfc6287.DataInput s -> "DataInput: " ^ s
            | Rfc6287.Window s -> "Window: " ^ s
          ) ^ "\n" >>= fun () ->
        FB.redraw fb >>= fun () ->
        Time.sleep_ns 10_000_000_000_L
      | `card_secret_error, _ -> Time.sleep_ns 10_000_000_000_L
      end
end

  let start _time
      (fb_init: unit -> ('a * (module Framebuffer.S) Lwt.t) Lwt.t) () =
  fb_init () >>= fun (_platform_specific, fb_promise) ->
  fb_promise >>= fun fb_module ->
  let module FB : Framebuffer.S = (val (fb_module) : Framebuffer.S) in
  let module App = Ocra(FB) in
  App.start ()

end
