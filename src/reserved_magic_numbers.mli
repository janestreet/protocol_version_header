open! Core

(** Every magic number this library reserves for its own use: those of all known
    protocols, those of protocols since retired, and the range marker. [create_exn]
    rejects [additional_magic_numbers] that collide with any of these, and tests check
    that no two of them collide with each other. A new marker added to the header format
    belongs in this list. *)
val all : int list
