[@@@zero_alloc all]

open! Core

(** The set of versions a header advertises: the version numbers it lists individually,
    plus, if its range section is being honored, every version covered by one of its
    ranges. *)
type t

(** Every version number on the wire, where range endpoints are treated as normal,
    individual version numbers instead of ranges.

    This is how a header is read when negotiating with a peer that predates version
    ranges. *)
val of_wire_treating_range_endpoints_as_individual_versions : int iarray -> t

(** The version numbers a header lists before its range marker, plus the ranges of its
    range section. [section] must be the section that [located_of_wire] found in [wire]. *)
val of_range_section : int iarray -> Range_section.t -> t

(** The highest version advertised by both, or [Null] if there is none. *)
val max_common_version : t -> t -> int or_null
