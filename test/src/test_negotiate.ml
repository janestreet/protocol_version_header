open! Core
open Expect_test_helpers_core
open Protocol_version_header

module Legacy = struct
  type t = int list [@@deriving bin_io]
end

let test ~us:(p_us, v_us) ~peer:(p_peer, v_peer) =
  let us = create_exn () ~protocol:p_us ~supported_versions:v_us in
  let peer = create_exn () ~protocol:p_peer ~supported_versions:v_peer in
  let result = negotiate ~allow_legacy_peer:false ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)]
;;

let test_legacy ~allow_legacy_peer ~us:(p_us, v_us) ~peer =
  let us = create_exn () ~protocol:p_us ~supported_versions:v_us in
  let peer =
    let str = Binable.to_string (module Legacy) peer in
    Binable.of_string (module Protocol_version_header) str
  in
  let result = negotiate ~allow_legacy_peer ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)]
;;

let%expect_test "We have a bad header" =
  let peer = create_exn ~protocol:Krb ~supported_versions:[ 1 ] () in
  let us =
    let str = Binable.to_string (module Legacy) [ 1 ] in
    Binable.of_string (module Protocol_version_header) str
  in
  let result = negotiate ~allow_legacy_peer:false ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Could not determine our own protocol"
      (us_versions (1))))
    |}];
  let result = negotiate ~allow_legacy_peer:true ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Could not determine our own protocol"
      (us_versions (1))))
    |}]
;;

let%expect_test _ =
  test ~us:(Krb, [ 1 ]) ~peer:(Krb, [ 1 ]);
  let () = [%expect {| (Ok 1) |}] in
  test ~us:(Krb, [ 1 ]) ~peer:(Krb, [ 2 ]);
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
      (us_versions (1))
      (us_ranges ())
      (peer_versions (2))
      (peer_ranges ())
      (protocol Krb)))
    |}];
  test ~us:(Krb, [ 1 ]) ~peer:(Rpc, [ 1 ]);
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
      (us_protocol   Krb)
      (peer_protocol Rpc)))
    |}];
  test_legacy ~allow_legacy_peer:false ~us:(Krb, [ 1; 2 ]) ~peer:[ 1 ];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Could not determine peer's protocol"
      (us_protocol   Krb)
      (peer_protocol Unknown)))
    |}];
  test_legacy ~allow_legacy_peer:true ~us:(Krb, [ 1; 2 ]) ~peer:[ 1 ];
  [%expect {| (Ok 1) |}];
  test_legacy ~allow_legacy_peer:true ~us:(Krb, [ 2 ]) ~peer:[ 1 ];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
      (us_versions   (2))
      (peer_versions (1))
      (protocol Krb)))
    |}]
;;

(** A simple reference implementation of [negotiate] that conforms to the documented
    behavior in the mli. The quickcheck tests below check that the two agree.

    It returns [None] whenever negotiation fails, whatever the reason: the tests only care
    that both implementations fail, not that they fail with the same error. *)
module Model = struct
  let is_version element = element <= For_test.max_supported_version

  let protocol_magic_numbers wire =
    List.filter wire ~f:(Map.mem Known_protocol.by_magic_number)
  ;;

  (** The elements before the first range marker, and those after it up to the next magic
      number; or [None] if the header has no marker. *)
  let split_at_range_marker wire =
    match
      List.split_while wire ~f:(fun element -> element <> For_test.range_magic_number)
    with
    | _, [] -> None
    | before, _ :: after -> Some (before, List.take_while after ~f:is_version)
  ;;

  (** The ranges of a section, or [None] if it is malformed: an odd number of elements, a
      range with [lo > hi], or ranges that aren't in strictly increasing order. *)
  let ranges section =
    let%bind.Option ranges =
      List.chunks_of section ~length:2
      |> List.map ~f:(function
        | [ lo; hi ] when lo <= hi -> Some (lo, hi)
        | _ -> None)
      |> Option.all
    in
    let rec sorted_and_disjoint = function
      | [] | [ _ ] -> true
      | (_, hi) :: ((next_lo, _) :: _ as rest) -> hi < next_lo && sorted_and_disjoint rest
    in
    Option.some_if (sorted_and_disjoint ranges) ranges
  ;;

  let intervals versions ~ranges =
    List.filter_map versions ~f:(fun element ->
      if is_version element then Some (element, element) else None)
    @ ranges
  ;;

  let max_common_version us_intervals peer_intervals =
    List.cartesian_product us_intervals peer_intervals
    |> List.filter_map ~f:(fun ((us_lo, us_hi), (peer_lo, peer_hi)) ->
      let highest = Int.min us_hi peer_hi in
      if Int.max us_lo peer_lo <= highest then Some highest else None)
    |> List.max_elt ~compare:Int.compare
  ;;

  let negotiate ~allow_legacy_peer ~us ~peer =
    let us_wire = Expert.raw_version_list us in
    let peer_wire = Expert.raw_version_list peer in
    let peer_speaks_our_protocol =
      match protocol_magic_numbers us_wire, protocol_magic_numbers peer_wire with
      | [ _ ], [] -> allow_legacy_peer
      | [ us_protocol_magic_number ], [ peer_protocol_magic_number ] ->
        us_protocol_magic_number = peer_protocol_magic_number
      | _ -> false
    in
    if not peer_speaks_our_protocol
    then None
    else (
      match split_at_range_marker us_wire, split_at_range_marker peer_wire with
      | Some (us_versions, us_section), Some (peer_versions, peer_section) ->
        (match ranges us_section, ranges peer_section with
         | Some us_ranges, Some peer_ranges ->
           max_common_version
             (intervals us_versions ~ranges:us_ranges)
             (intervals peer_versions ~ranges:peer_ranges)
         | None, _ | _, None -> None)
      | None, _ | _, None ->
        max_common_version (intervals us_wire ~ranges:[]) (intervals peer_wire ~ranges:[]))
  ;;
end

let negotiated_versions_agree first second =
  [%equal: int option] (Result.ok first) (Result.ok second)
;;

let matches_expectation (result : int Or_error.t) expected =
  [%equal: int option] (Result.ok result) expected
;;

let check_against_model ~allow_legacy_peer ~us ~peer =
  let result = negotiate ~allow_legacy_peer ~us ~peer in
  let expected = Model.negotiate ~allow_legacy_peer ~us ~peer in
  if not (matches_expectation result expected)
  then
    raise_s
      [%message
        "negotiate disagrees with the model"
          (us : t)
          (peer : t)
          (allow_legacy_peer : bool)
          (result : int Or_error.t)
          (expected : int option)]
;;

let%test_unit "negotiate agrees with the model" =
  Quickcheck.test
    (Quickcheck.Generator.both
       Generators.negotiation_header
       Generators.negotiation_header)
    ~f:(fun (us, peer) ->
      List.iter [ false; true ] ~f:(fun allow_legacy_peer ->
        check_against_model ~allow_legacy_peer ~us ~peer))
;;

(* [Model] works from the wire representation, so if [create_exn] encoded a header
   wrongly, [Model] and [negotiate] would agree on the wrong answer. This test instead
   computes the expected answer from the versions and ranges that were passed to
   [create_exn], so it catches encoding bugs as well. *)
let%test_unit "negotiate returns the max version advertised by both callers" =
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
    let%bind us = Generators.advertised_versions in
    let%map peer = Generators.advertised_versions in
    protocol, us, peer
  in
  Quickcheck.test
    generator
    ~f:(fun (protocol, (us_versions, us_ranges), (peer_versions, peer_ranges)) ->
      let header supported_versions supported_version_ranges =
        create_exn () ~protocol ~supported_versions ~supported_version_ranges
      in
      let advertised versions ranges =
        versions
        @ List.concat_map ranges ~f:(fun (lo, hi) -> List.range lo hi ~stop:`inclusive)
        |> Int.Set.of_list
      in
      let expected =
        Set.inter
          (advertised us_versions us_ranges)
          (advertised peer_versions peer_ranges)
        |> Set.max_elt
      in
      let result =
        negotiate
          ~allow_legacy_peer:false
          ~us:(header us_versions us_ranges)
          ~peer:(header peer_versions peer_ranges)
      in
      if not (matches_expectation result expected)
      then
        raise_s
          [%message
            "negotiate disagrees with the versions the callers advertised"
              (us_versions : int list)
              (us_ranges : (int * int) list)
              (peer_versions : int list)
              (peer_ranges : (int * int) list)
              (result : int Or_error.t)
              (expected : int option)])
;;

let%test_unit "negotiate agrees with the model on headers with many versions" =
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
    let%bind _us_protocol, us_versions, us_additional =
      Generators.header_generator ~min_versions:1 ~max_versions:30
    in
    let%bind _peer_protocol, peer_versions, peer_additional =
      Generators.header_generator ~min_versions:1 ~max_versions:30
    in
    (* We want to make sure to permute the header lists sometimes so that we're not only
       testing headers in the shape [create_exn] builds. *)
    let%bind us =
      Generators.conditionally_permute
        (create_exn
           ()
           ~protocol
           ~supported_versions:us_versions
           ~additional_magic_numbers:us_additional)
    in
    let%bind peer =
      Generators.conditionally_permute
        (create_exn
           ()
           ~protocol
           ~supported_versions:peer_versions
           ~additional_magic_numbers:peer_additional)
    in
    return (us, peer)
  in
  Quickcheck.test generator ~f:(fun (us, peer) ->
    check_against_model ~allow_legacy_peer:false ~us ~peer)
;;

let%test_unit "negotiate is symmetric" =
  Quickcheck.test
    (Quickcheck.Generator.both
       Generators.negotiation_header
       Generators.negotiation_header)
    ~f:(fun (us, peer) ->
      let one_way = negotiate ~allow_legacy_peer:false ~us ~peer in
      let other_way = negotiate ~allow_legacy_peer:false ~us:peer ~peer:us in
      if not (negotiated_versions_agree one_way other_way)
      then
        raise_s
          [%message
            "negotiate is not symmetric"
              (us : t)
              (peer : t)
              (one_way : int Or_error.t)
              (other_way : int Or_error.t)])
;;

let has_range_marker t =
  List.mem (Expert.raw_version_list t) For_test.range_magic_number ~equal:Int.equal
;;

let without_range_marker t =
  Expert.raw_version_list t
  |> List.filter ~f:(fun element -> element <> For_test.range_magic_number)
  |> For_test.create_unvalidated
;;

let%test_unit "the marker is invisible unless both headers carry it" =
  Quickcheck.test
    (Quickcheck.Generator.both
       Generators.negotiation_header
       Generators.negotiation_header)
    ~f:(fun (us, peer) ->
      if not (has_range_marker us && has_range_marker peer)
      then
        List.iter [ false; true ] ~f:(fun allow_legacy_peer ->
          let with_marker = negotiate ~allow_legacy_peer ~us ~peer in
          let without_marker =
            negotiate
              ~allow_legacy_peer
              ~us:(without_range_marker us)
              ~peer:(without_range_marker peer)
          in
          if not
               (Sexp.equal
                  [%sexp (with_marker : int Or_error.t)]
                  [%sexp (without_marker : int Or_error.t)])
          then
            raise_s
              [%message
                "the range marker changed how negotiation went"
                  (us : t)
                  (peer : t)
                  (allow_legacy_peer : bool)
                  (with_marker : int Or_error.t)
                  (without_marker : int Or_error.t)]))
;;

let%test_unit "an empty range section negotiates like no marker at all" =
  Quickcheck.test
    (Quickcheck.Generator.both
       Generators.header_without_ranges
       Generators.header_without_ranges)
    ~f:(fun (us, peer) ->
      List.iter [ false; true ] ~f:(fun allow_legacy_peer ->
        let with_marker = negotiate ~allow_legacy_peer ~us ~peer in
        let without_marker =
          negotiate
            ~allow_legacy_peer
            ~us:(without_range_marker us)
            ~peer:(without_range_marker peer)
        in
        if not (negotiated_versions_agree with_marker without_marker)
        then
          raise_s
            [%message
              "an empty range section changed the negotiated version"
                (us : t)
                (peer : t)
                (with_marker : int Or_error.t)
                (without_marker : int Or_error.t)]))
;;
