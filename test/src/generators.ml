open! Core
open Protocol_version_header

let reserved_magic_numbers = Int.Set.of_list Reserved_magic_numbers.all

let header_generator ~min_versions ~max_versions =
  let open Quickcheck.Generator.Let_syntax in
  let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
  let%bind num_supported_versions = Int.gen_incl min_versions max_versions in
  let%bind supported_versions =
    List.gen_with_length
      num_supported_versions
      (Int.gen_incl 0 For_test.max_supported_version)
  in
  let%bind num_additional_magic_numbers =
    Int.gen_incl 0 (99 - num_supported_versions - 1)
  in
  let%bind additional_magic_numbers =
    List.gen_with_length
      num_additional_magic_numbers
      (Quickcheck.Generator.filter
         (Int.gen_uniform_incl (For_test.max_supported_version + 1) Int.max_value)
         ~f:(fun candidate -> not (Set.mem reserved_magic_numbers candidate)))
  in
  return (protocol, supported_versions, additional_magic_numbers)
;;

let conditionally_permute header =
  let open Quickcheck.Generator.Let_syntax in
  match%bind Quickcheck.Generator.bool with
  | false -> return header
  | true ->
    let%map wire = List.gen_permutations (Expert.raw_version_list header) in
    For_test.create_unvalidated wire
;;

(* Versions are drawn from a small range so that two independently generated headers are
   likely to have some in common. *)
let highest_generated_version = 20
let version = Int.gen_incl 0 highest_generated_version

let range =
  let open Quickcheck.Generator.Let_syntax in
  let%bind lo = version in
  let%map hi = Int.gen_incl lo highest_generated_version in
  lo, hi
;;

let short_list generator ~max_length =
  let open Quickcheck.Generator.Let_syntax in
  let%bind length = Int.gen_incl 0 max_length in
  List.gen_with_length length generator
;;

let list_of_arbitrary_unknown_magic_numbers =
  let list =
    List.range
      ~start:`exclusive
      For_test.max_supported_version
      (For_test.max_supported_version + 4)
  in
  assert (is_none (List.find list ~f:(Set.mem reserved_magic_numbers)));
  list
;;

let unknown_magic_number =
  Quickcheck.Generator.of_list list_of_arbitrary_unknown_magic_numbers
;;

let wire_element =
  Quickcheck.Generator.union
    [ version
    ; Int.gen_incl (-3) (-1)
    ; Quickcheck.Generator.of_list (Set.to_list reserved_magic_numbers)
    ; unknown_magic_number
    ]
;;

let advertised_versions =
  let open Quickcheck.Generator.Let_syntax in
  let%bind supported_versions = short_list version ~max_length:5 in
  let%map supported_version_ranges = short_list range ~max_length:3 in
  supported_versions, supported_version_ranges
;;

let header_without_ranges =
  let open Quickcheck.Generator.Let_syntax in
  let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
  let%map supported_versions = short_list version ~max_length:5 in
  create_exn () ~protocol ~supported_versions
;;

let negotiation_header =
  let open Quickcheck.Generator.Let_syntax in
  let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
  let%bind supported_versions = short_list version ~max_length:5 in
  let%bind supported_version_ranges = short_list range ~max_length:3 in
  let%bind additional_magic_numbers = short_list unknown_magic_number ~max_length:3 in
  let created () =
    create_exn
      ()
      ~protocol
      ~supported_versions
      ~supported_version_ranges
      ~additional_magic_numbers
  in
  match%bind
    Quickcheck.Generator.of_list [ `Created; `Predates_ranges; `Permuted; `Garbage ]
  with
  | `Created -> return (created ())
  | `Predates_ranges ->
    return
      (For_test.create_unvalidated
         ((Known_protocol.magic_number protocol :: additional_magic_numbers)
          @ supported_versions))
  | `Permuted ->
    let%map wire = List.gen_permutations (Expert.raw_version_list (created ())) in
    For_test.create_unvalidated wire
  | `Garbage ->
    let%map wire = short_list wire_element ~max_length:8 in
    For_test.create_unvalidated wire
;;
