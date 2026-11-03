open! Core

let magic_number = Known_protocol.gen_magic_number "VRG" (* "Version RanGe" *)
let%test_unit "range magic number" = assert (magic_number = 4_674_134)

type t =
  { wire : int iarray @@ global
  ; marker_index : int
  (** The index of the range marker. The section starts just after it. *)
  ; stop_exclusive : int (** One past the last element of the section. *)
  }

module Located = struct
  type nonrec t =
    | No_marker
    | Malformed
    | Well_formed of t
end

let rec find_marker (wire : int iarray) ~index : int or_null =
  if index >= Iarray.length wire
  then Null
  else if wire.:(index) = magic_number
  then This index
  else find_marker wire ~index:(index + 1)
;;

(* The index of the first magic number at or after [index], or the length of the header if
   there is none. Anything from there on belongs to a later section, which this version of
   the library doesn't know about and so ignores. *)
let rec find_stop (wire : int iarray) ~index =
  if index >= Iarray.length wire || Header_element.is_magic_number wire.:(index)
  then index
  else find_stop wire ~index:(index + 1)
;;

(* [is_well_formed_from wire ~index ~stop_exclusive] checks that the interval
    [[index, stop_exclusive)] of [wire] only contains tuples [lo1; hi1; lo2; hi2; ... ;
    loN; hiN]
    where every [loN <= hiN] and [hiN < loN+1] *)
let rec is_well_formed_from (wire : int iarray) ~index ~stop_exclusive =
  let lo = index in
  let hi = index + 1 in
  let next_lo = index + 2 in
  if lo >= stop_exclusive
  then true
  else if hi >= stop_exclusive
  then
    (* We don't have proper [loN; hiN] tuples (i.e., this interval ended with [... ; loN]) *)
    false
  else
    wire.:(lo) <= wire.:(hi)
    && (next_lo >= stop_exclusive || wire.:(hi) < wire.:(next_lo))
    && is_well_formed_from wire ~index:next_lo ~stop_exclusive
;;

let located_of_wire wire : Located.t @ local = exclave_
  match find_marker wire ~index:0 with
  | Null -> No_marker
  | This marker_index ->
    let start = marker_index + 1 in
    let stop_exclusive = find_stop wire ~index:start in
    if is_well_formed_from wire ~index:start ~stop_exclusive
    then Well_formed { wire; marker_index; stop_exclusive }
    else Malformed
;;

let marker_index (t @ local) = t.marker_index
let num_ranges (t @ local) = (t.stop_exclusive - t.marker_index - 1) / 2

let nth_range (t @ local) i =
  let lo_index = t.marker_index + 1 + (2 * i) in
  #(t.wire.:(lo_index), t.wire.:(lo_index + 1))
;;

let ranges (t @ local) =
  List.init (num_ranges t) ~f:(fun i ->
    let #(lo, hi) = nth_range t i in
    lo, hi)
  [@nontail]
;;

let%expect_test "individually listed versions and ranges" =
  let show wire =
    match located_of_wire (Iarray.of_list wire) with
    | No_marker -> print_s [%message "No_marker"]
    | Malformed -> print_s [%message "Malformed"]
    | Well_formed t ->
      let ranges = ranges t in
      print_s [%message (ranges : (int * int) list)]
  in
  let krb = Known_protocol.magic_number Krb in
  let unknown_magic = Header_element.max_supported_version + 1 in
  show [ krb; 1; 2 ];
  [%expect {| No_marker |}];
  show [ krb; magic_number; 1 ];
  [%expect {| Malformed |}];
  show [ krb; magic_number; 5; 1 ];
  [%expect {| Malformed |}];
  (* Ranges must be in increasing order and must not overlap or touch. *)
  show [ krb; magic_number; 5; 9; 1; 3 ];
  [%expect {| Malformed |}];
  show [ krb; magic_number; 1; 5; 3; 9 ];
  [%expect {| Malformed |}];
  show [ krb; magic_number; 1; 3; 3; 5 ];
  [%expect {| Malformed |}];
  show [ krb; magic_number; 1; 2; 3; 4 ];
  [%expect {| (ranges ((1 2) (3 4))) |}];
  show [ krb; magic_number ];
  [%expect {| (ranges ()) |}];
  show [ krb; 1; 2; magic_number; 3; 4 ];
  [%expect {| (ranges ((3 4))) |}];
  (* Several ranges come out in wire order. *)
  show [ krb; magic_number; 3; 4; 6; 9; 20; 20 ];
  [%expect {| (ranges ((3 4) (6 9) (20 20))) |}];
  (* Magic numbers before the marker are left out, and a later section is ignored. *)
  show [ krb; 1; unknown_magic; 2; magic_number; 3; 4; 6; 9; unknown_magic; 7; 8 ];
  [%expect {| (ranges ((3 4) (6 9))) |}]
;;
