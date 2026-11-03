[@@@zero_alloc all]

open! Core

(** The part of a header that carries version ranges: everything after the first
    [magic_number] and before the next magic number (or the end of the header), read as
    inclusive [(lo, hi)] pairs. See the wire format in [protocol_version_header.mli]. *)

(** The range marker: the magic number that introduces the range section. *)
val magic_number : int

(** The well-formed range section of a protocol version header: one with an even number of
    elements, each pair having [lo <= hi], and each pair lying strictly above the one
    before it ([hi < next lo]). Maintains a reference to the original header wire array
    that it was parsed from. *)
type t

module Located : sig
  type nonrec t =
    | No_marker
    | Malformed
    | Well_formed of t
end

(** Finds the range section of a header. *)
val located_of_wire : int iarray -> Located.t

(** The index at which the range marker appeared in the header that this [t] was parsed
    from. *)
val marker_index : t -> int

val num_ranges : t -> int

(** The ranges, in increasing order, numbered [0] to [num_ranges - 1]. *)
val nth_range : t -> int -> int * int

(** For error messages. *)
val ranges : t -> (int * int) list [@@zero_alloc ignore]
