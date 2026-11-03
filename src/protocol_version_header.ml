open! Core
module Known_protocol = Known_protocol
module Reserved_magic_numbers = Reserved_magic_numbers

let max_supported_version = Header_element.max_supported_version

module Bounded_list_in_case_someone_sends_garbage_on_the_wire =
List_with_max_len.Make (struct
    let max_len = 100
    let context = Info.Portable.of_string "Protocol_version_header"
  end)

include struct
  open Stable_witness.Export

  type t = int Bounded_list_in_case_someone_sends_garbage_on_the_wire.t
  [@@deriving bin_io ~localize ~portable, globalize, sexp, stable_witness]

  let[@zero_alloc opt] bin_read_t__local buf ~pos_ref = exclave_
    (* We can assume this will not allocate, as [bin_read_list__local] will not allocate
       as long as its argument doesn't, and [bin_read_int] doesn't allocate *)
    (Bounded_list_in_case_someone_sends_garbage_on_the_wire.bin_read_t__local
    [@zero_alloc assume])
      bin_read_int
      buf
      ~pos_ref
  ;;
end

(* The version numbers listed individually in a header, or just those among its first
   [before_index] elements. *)
let explicit_versions ?before_index wire =
  let elements = Iarray.to_list wire in
  let elements =
    match before_index with
    | None -> elements
    | Some before_index -> List.take elements before_index
  in
  List.filter elements ~f:Header_element.is_version_number
;;

let reserved_magic_numbers =
  Portable_lazy.from_fun (fun () -> Int.Set.of_list Reserved_magic_numbers.all)
;;

let merge_supported_version_ranges ranges =
  List.sort ranges ~compare:[%compare: int * _]
  |> List.fold ~init:[] ~f:(fun merged (lo, hi) ->
    match merged with
    | (merged_lo, merged_hi) :: rest when lo <= merged_hi + 1 ->
      (merged_lo, Int.max merged_hi hi) :: rest
    | _ -> (lo, hi) :: merged)
  |> List.rev
;;

let create_exn
  ?(additional_magic_numbers = [])
  ?(supported_version_ranges = [])
  ~protocol
  ~supported_versions
  ()
  =
  let protocol_magic_number = Known_protocol.magic_number protocol in
  if List.exists supported_versions ~f:Header_element.is_magic_number
  then
    raise_s
      [%message
        "Unable to advertise versions larger than max supported version"
          (max_supported_version : int)
          (supported_versions : int list)];
  if List.exists supported_version_ranges ~f:(fun (lo, hi) ->
       Header_element.is_magic_number lo || Header_element.is_magic_number hi)
  then
    raise_s
      [%message
        "Unable to advertise version ranges larger than max supported version"
          (max_supported_version : int)
          (supported_version_ranges : (int * int) list)];
  if List.exists supported_version_ranges ~f:(fun (lo, hi) -> lo > hi)
  then
    raise_s
      [%message
        "[supported_version_ranges] should only contain ranges whose lower bound is at \
         most its upper bound"
          (supported_version_ranges : (int * int) list)];
  if List.exists additional_magic_numbers ~f:Header_element.is_version_number
  then
    raise_s
      [%message
        "[additional_magic_numbers] shouldn't be within [max_supported_version] range"
          (max_supported_version : int)
          (additional_magic_numbers : int list)];
  if List.exists
       additional_magic_numbers
       ~f:(Set.mem (Portable_lazy.force reserved_magic_numbers))
  then
    raise_s
      [%message
        "[additional_magic_numbers] shouldn't be overlapping with magic numbers reserved \
         by [Protocol_version_header]"
          (additional_magic_numbers : int list)
          ~reserved_magic_numbers:(Portable_lazy.force reserved_magic_numbers : Int.Set.t)];
  let supported_versions = List.dedup_and_sort supported_versions ~compare:Int.compare in
  let range_section =
    Range_section.magic_number
    :: (merge_supported_version_ranges supported_version_ranges
        |> List.concat_map ~f:(fun (lo, hi) -> [ lo; hi ]))
  in
  (protocol_magic_number :: additional_magic_numbers) @ supported_versions @ range_section
  |> Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn
;;

module Known_protocols_in_header = struct
  type t =
    | Zero
    | One of Known_protocol.t
    | Several
end

(* A header that repeats a protocol's magic number is as ambiguous as one that names two
   different protocols, so both classify as [Several]. *)
let[@zero_alloc opt] known_protocols_in_header (wire : int iarray)
  : Known_protocols_in_header.t
  = exclave_
  Iarray.Local.fold
    wire
    ~init:Known_protocols_in_header.Zero
    ~f:(fun found magic_number -> exclave_
      match Map.find_or_null Known_protocol.by_magic_number magic_number with
      | Null -> found
      | This protocol ->
        (match found with
         | Zero -> One protocol
         | One (_ : Known_protocol.t) | Several -> Several))
;;

(* The error builders below are allowed to allocate: they are only called once negotiation
   has already failed. *)
module Errors = struct
  let multiple_protocols (wire : int iarray) =
    let protocols =
      Iarray.to_list wire |> List.filter_map ~f:(Map.find Known_protocol.by_magic_number)
    in
    let versions = explicit_versions wire in
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Could not determine protocol: Multiple \
         magic numbers corresponding to known protocols observed in header"
          (protocols : Known_protocol.t list)
          (versions : int list)]
  ;;

  let could_not_determine_our_protocol (us_wire : int iarray) =
    let us_versions = explicit_versions us_wire in
    error_s
      [%message
        "[Protocol_version_header.negotiate]: Could not determine our own protocol"
          (us_versions : int list)]
  ;;

  let could_not_determine_peers_protocol ~us_protocol =
    let peer_protocol = `Unknown in
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Could not determine peer's protocol"
          (us_protocol : Known_protocol.t)
          (peer_protocol : [ `Unknown ])]
  ;;

  let different_protocols ~us_protocol ~peer_protocol =
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
          (us_protocol : Known_protocol.t)
          (peer_protocol : Known_protocol.t)]
  ;;

  let no_compatible_versions ~us_wire ~peer_wire ~protocol =
    let us_versions = explicit_versions us_wire
    and peer_versions = explicit_versions peer_wire in
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
          (us_versions : int list)
          (peer_versions : int list)
          (protocol : Known_protocol.t)]
  ;;

  let no_compatible_versions_or_ranges
    ~us_wire
    ~(us : Range_section.t @ local)
    ~peer_wire
    ~(peer : Range_section.t @ local)
    ~protocol
    =
    let us_versions =
      explicit_versions us_wire ~before_index:(Range_section.marker_index us)
    in
    let us_ranges = Range_section.ranges us in
    let peer_versions =
      explicit_versions peer_wire ~before_index:(Range_section.marker_index peer)
    in
    let peer_ranges = Range_section.ranges peer in
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
          (us_versions : int list)
          (us_ranges : (int * int) list)
          (peer_versions : int list)
          (peer_ranges : (int * int) list)
          (protocol : Known_protocol.t)]
  ;;

  let malformed_range_section ~which_header ~(wire : int iarray) =
    let header = Iarray.to_list wire in
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Malformed version range section. This \
         indicates a bug in the construction of this header rather than a genuine \
         version mismatch."
          (which_header : [ `Us | `Peer ])
          (header : int list)]
  ;;
end

let negotiate_versions_in_range_mode
  ~us_wire
  ~(us : Range_section.t @ local)
  ~peer_wire
  ~(peer : Range_section.t @ local)
  ~protocol
  =
  match
    Advertised_versions.max_common_version
      (Advertised_versions.of_range_section us_wire us)
      (Advertised_versions.of_range_section peer_wire peer)
  with
  | This version -> Ok version
  | Null ->
    Errors.no_compatible_versions_or_ranges ~us_wire ~us ~peer_wire ~peer ~protocol
;;

let negotiate_versions_ignoring_ranges ~us_wire ~peer_wire ~protocol =
  match
    Advertised_versions.max_common_version
      (Advertised_versions.of_wire_treating_range_endpoints_as_individual_versions
         us_wire)
      (Advertised_versions.of_wire_treating_range_endpoints_as_individual_versions
         peer_wire)
  with
  | This version -> Ok version
  | Null -> Errors.no_compatible_versions ~us_wire ~peer_wire ~protocol
;;

let negotiate_versions ~us_wire ~peer_wire ~protocol =
  match
    Range_section.located_of_wire us_wire, Range_section.located_of_wire peer_wire
  with
  | No_marker, (No_marker | Malformed | Well_formed _)
  | (Malformed | Well_formed _), No_marker ->
    (* At least one header has no marker, so that side doesn't understand version ranges
       (most likely it was built before they existed). It reads range endpoints as
       ordinary supported versions, and both sides must reach the same answer, so we do
       the same and ignore the range sections entirely. *)
    negotiate_versions_ignoring_ranges ~us_wire ~peer_wire ~protocol
  | Malformed, (Malformed | Well_formed _) ->
    Errors.malformed_range_section ~which_header:`Us ~wire:us_wire
  | Well_formed _, Malformed ->
    Errors.malformed_range_section ~which_header:`Peer ~wire:peer_wire
  | Well_formed us, Well_formed peer ->
    negotiate_versions_in_range_mode ~us_wire ~us ~peer_wire ~peer ~protocol [@nontail]
;;

let negotiate ~allow_legacy_peer ~(us : t) ~(peer : t) =
  let us_wire = Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray us in
  let peer_wire = Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray peer in
  match known_protocols_in_header us_wire, known_protocols_in_header peer_wire with
  | Several, _ -> Errors.multiple_protocols us_wire
  | _, Several -> Errors.multiple_protocols peer_wire
  | Zero, _ -> Errors.could_not_determine_our_protocol us_wire
  | One us_protocol, Zero ->
    (* we assume peer is speaking our protocol if [allow_legacy_peer] *)
    if allow_legacy_peer
    then negotiate_versions ~us_wire ~peer_wire ~protocol:us_protocol
    else Errors.could_not_determine_peers_protocol ~us_protocol
  | One us_protocol, One peer_protocol ->
    if ([%compare.equal: Known_protocol.t] [@mode local]) us_protocol peer_protocol
    then negotiate_versions ~us_wire ~peer_wire ~protocol:us_protocol
    else Errors.different_protocols ~us_protocol ~peer_protocol
;;

let matches_magic_prefix (t : t) ~protocol =
  let magic_number = Known_protocol.magic_number protocol in
  Iarray.mem
    ~equal:Int.equal
    (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray t)
    magic_number
;;

let contains_magic_prefix ~protocol =
  Bin_prot.Type_class.cnv_reader (matches_magic_prefix ~protocol) bin_t.reader
;;

let any_magic_prefix =
  let f t =
    List.find Known_protocol.all ~f:(fun protocol -> matches_magic_prefix ~protocol t)
  in
  Bin_prot.Type_class.cnv_reader f bin_t.reader
;;

let magic_number_bin_size = 5

module Magic_prefix_bin_repr = struct
  type t = int [@@deriving bin_shape, bin_write]

  (*=The bin prot representation of a protocol version header is the standard
     representation for an int list:

     | nat0 indicating size | element 0 | element 1 | ...

     [create_exn] will always put the known protocol magic number as "element 0".

     The bin size of "nat0 indicating size" and "element 0" is fixed (there are expect
     tests to make sure we never change them). *)
  let bin_size =
    bin_size_t Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
    + magic_number_bin_size
  ;;

  let bin_read_t buf ~pos_ref =
    let (_list_length : Bin_prot.Nat0.t) = Bin_prot.Read.bin_read_nat0 buf ~pos_ref in
    Bin_prot.Read.bin_read_int buf ~pos_ref
  ;;

  let bin_reader_t =
    { Bin_prot.Type_class.read = bin_read_t; vtag_read = Int.__bin_read_t__ }
  ;;
end

let any_magic_prefix_from_six_bytes =
  Bin_prot.Type_class.cnv_reader
    (fun magic_number -> (Map.find Known_protocol.by_magic_number) magic_number)
    Magic_prefix_bin_repr.bin_reader_t
;;

let any_magic_prefix_from_six_bytes_bin_size = Magic_prefix_bin_repr.bin_size

module Pair = struct
  type nonrec t =
    { us : t
    ; peer : t
    }
end

module Expert = struct
  let raw_version_list = Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_list
  let none = Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn []

  let is_none (t : t) =
    Iarray.is_empty (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray t)
  ;;
end

module For_test = struct
  module Make_list_with_max_len = List_with_max_len.Make

  let magic_number_bin_size = magic_number_bin_size
  let max_supported_version = max_supported_version
  let range_magic_number = Range_section.magic_number

  let create_unvalidated =
    Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn
  ;;
end

let%test_unit "bin sizes are not changed by accident" =
  (* Ensure the bin_size of Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
     is always 1. This means that regardless of how long the list of versions in the
     protocol header is, the representation will be the same number of bytes. The
     representation of an int jumps to 3 bytes at the value 128. *)
  let bounded_list_bin_size =
    Int.bin_size_t Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
  in
  assert (bounded_list_bin_size = 1);
  assert (6 = magic_number_bin_size + bounded_list_bin_size);
  assert (
    any_magic_prefix_from_six_bytes_bin_size
    = magic_number_bin_size + bounded_list_bin_size)
;;
