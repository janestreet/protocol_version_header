@@ portable

[@@@zero_alloc all]

open! Core

(** A header is a list of ints. Each is either a version number or a magic number; magic
    numbers are all greater than [max_supported_version]. *)

val max_supported_version : int
val is_version_number : int -> bool
val is_magic_number : int -> bool
