open! Core

type t =
  { versions : int iarray
  (** The individually listed versions, sorted in ascending order. *)
  ; ranges : Range_section.t or_null
  }

(* The version numbers among the first [before_index] elements of [wire], sorted. *)
let[@zero_alloc] sorted_versions (wire : int iarray) ~before_index =
  let versions =
    (Iarray.Local.filteri
    [@zero_alloc assume]
    (* We have to [assume] here because the zero_alloc checker can't tell that the closure
       doesn't alloc *))
      wire
      ~f:(fun index element ->
        index < before_index && Header_element.is_version_number element)
  in
  (Iarray.Local.sort_immediates
  [@zero_alloc assume]
  (* We have to [assume] here because the zero_alloc checker can't tell that the
     [~compare] closure doesn't alloc *))
    versions
    ~compare:Int.compare
;;

let[@zero_alloc] of_wire_treating_range_endpoints_as_individual_versions wire =
  { versions = sorted_versions wire ~before_index:(Iarray.length wire); ranges = Null }
;;

let[@zero_alloc] of_range_section wire section =
  let versions =
    sorted_versions wire ~before_index:(Range_section.marker_index section)
  in
  { versions; ranges = This section }
;;

let[@zero_alloc] num_ranges t =
  match t.ranges with
  | Null -> 0
  | This section -> Range_section.num_ranges section
;;

(* [max_common_version] walks the two headers' intervals downwards from the highest, where
   an individually listed version [v] counts as the interval [(v, v)]. Each header's
   intervals are visited in decreasing order of upper bound, which is possible without
   merging them up front because both sources are sorted: the versions by us, and the
   ranges by the well-formedness check.

   The records below are unboxed so that the walk doesn't allocate. *)

(** How far we are through a given [Protocol_version_header]'s specified intervals. *)
module Cursor = struct
  type t =
    { versions_left : int
    ; ranges_left : int
    }

  let[@zero_alloc] is_exhausted { versions_left; ranges_left } =
    versions_left + ranges_left = 0
  ;;
end

let[@zero_alloc] starting_cursor t : Cursor.t =
  { versions_left = Iarray.length t.versions; ranges_left = num_ranges t }
;;

module Source = struct
  type t =
    | Individual_versions
    | Ranges
end

(** An interval, together with where it came from, so that the caller knows which count to
    decrement to consume it. *)
module Top_interval = struct
  type t =
    { lo : int
    ; hi : int
    ; source : Source.t
    }
end

(* The remaining interval with the highest upper bound. Requires that something remains. *)
let[@zero_alloc] top_interval t cursor : Top_interval.t =
  let { Cursor.versions_left; ranges_left } = cursor in
  match t.ranges with
  | This section when ranges_left > 0 ->
    let lo, hi = Range_section.nth_range section (ranges_left - 1) in
    if versions_left > 0 && t.versions.:(versions_left - 1) >= hi
    then (
      let version = t.versions.:(versions_left - 1) in
      { lo = version; hi = version; source = Individual_versions })
    else { lo; hi; source = Ranges }
  | Null | This _ ->
    let version = t.versions.:(versions_left - 1) in
    { lo = version; hi = version; source = Individual_versions }
;;

let[@zero_alloc] consume (cursor : Cursor.t) (top : Top_interval.t) =
  match top.source with
  | Individual_versions -> { cursor with versions_left = cursor.versions_left - 1 }
  | Ranges -> { cursor with ranges_left = cursor.ranges_left - 1 }
;;

let rec walk us peer ~us_cursor ~peer_cursor =
  if Cursor.is_exhausted us_cursor || Cursor.is_exhausted peer_cursor
  then Null
  else (
    let us_top = top_interval us us_cursor in
    let peer_top = top_interval peer peer_cursor in
    let potential_highest_common_version = Int.min us_top.hi peer_top.hi in
    if Int.max us_top.lo peer_top.lo <= potential_highest_common_version
    then This potential_highest_common_version
    else if us_top.hi > peer_top.hi
    then walk us peer ~us_cursor:(consume us_cursor us_top) ~peer_cursor
    else walk us peer ~us_cursor ~peer_cursor:(consume peer_cursor peer_top))
;;

let[@zero_alloc] max_common_version us peer =
  walk us peer ~us_cursor:(starting_cursor us) ~peer_cursor:(starting_cursor peer)
;;
