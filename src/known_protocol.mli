open! Core

type t =
  | Krb
  | Krb_test_mode
  | Rpc
[@@deriving compare, enumerate, sexp, globalize]

val magic_number : t -> int
val by_magic_number : t Int.Map.t

(** Turns a short word such as ["RPC"] into a magic number by packing its bytes into an
    int. Every magic number in this library is made this way, so the word shows up as
    readable ASCII in a hex dump of the wire. *)
val gen_magic_number : string -> int

(** Every magic number this library has ever used, including those of protocols we have
    since retired. Retired numbers are kept so that nothing reuses them. *)
val all_magic_numbers_including_retired : int list
