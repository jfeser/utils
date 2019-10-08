open! Core

let src = Logs.Src.create ~doc:"Logs for command runs." "util_cmd"
module Log = (val Logs.src_log src : Logs.LOG)

(** Run a command, logging it if it fails. *)
let command_exn ?quiet:_ = function
  | [] -> Error.of_string "Empty command" |> Error.raise
  | cmd ->
      let cmd_str = String.concat cmd ~sep:" " in
      Log.info (fun m -> m "%s" cmd_str) ;
      let err = Unix.system cmd_str in
      Or_error.(
        tag_arg
          (Unix.Exit_or_signal.or_error err)
          "Running command failed." cmd_str [%sexp_of: string]
        |> ok_exn)

(** Run a command and return its output on stdout, logging it if it fails. *)
let command_out_exn ?quiet:_ = function
  | [] -> Error.of_string "Empty command" |> Error.raise
  | cmd ->
      let open Unix in
      let cmd_str = String.concat cmd ~sep:" " in
      Log.info (fun m -> m "%s" cmd_str) ;
      let ch = open_process_in cmd_str in
      let out = In_channel.input_all ch in
      Or_error.(
        tag_arg
          (Exit_or_signal.or_error (close_process_in ch))
          "Running command failed." cmd_str [%sexp_of: string]
        |> ok_exn) ;
      out
