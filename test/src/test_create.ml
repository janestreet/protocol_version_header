open! Core
open! Expect_test_helpers_core
open! Protocol_version_header

let%expect_test "test create results and also document the scenario where [create_exn] \
                 would raise"
  =
  let test ~supported_versions =
    create_exn () ~protocol:Rpc ~supported_versions |> [%sexp_of: t] |> print_s
  in
  test ~supported_versions:[];
  [%expect {| (4411474 4674134) |}];
  (* many supported versions. *)
  test ~supported_versions:(List.init 98 ~f:Fn.id);
  let (_output_too_long : string) = [%expect.output] in
  (* too many supported versions *)
  require_does_raise (fun () ->
    create_exn () ~protocol:Krb ~supported_versions:(List.init 99 ~f:Fn.id));
  [%expect
    {|
    ("List is too large"
      (context Protocol_version_header)
      (len     101)
      (max_len 100))
    |}];
  (* Supported version at the max supported version limit *)
  test ~supported_versions:[ For_test.max_supported_version ];
  [%expect {| (4411474 1000000 4674134) |}];
  (* Supported version over the max supported version limit *)
  require_does_raise (fun () ->
    create_exn () ~protocol:Krb ~supported_versions:[ For_test.max_supported_version + 1 ]);
  [%expect
    {|
    ("Unable to advertise versions larger than max supported version"
     (max_supported_version 1000000)
     (supported_versions (1000001)))
    |}];
  (* [additional_magic_numbers] smaller than or equal to max supported version *)
  require_does_raise (fun () ->
    create_exn () ~protocol:Krb ~additional_magic_numbers:[ 1 ] ~supported_versions:[]);
  [%expect
    {|
    ("[additional_magic_numbers] shouldn't be within [max_supported_version] range"
     (max_supported_version 1000000)
     (additional_magic_numbers (1)))
    |}];
  require_does_raise (fun () ->
    create_exn
      ()
      ~protocol:Krb
      ~additional_magic_numbers:[ For_test.max_supported_version ]
      ~supported_versions:[]);
  [%expect
    {|
    ("[additional_magic_numbers] shouldn't be within [max_supported_version] range"
     (max_supported_version 1000000)
     (additional_magic_numbers (1000000)))
    |}];
  (* [additional_magic_numbers] clashing with known protocols *)
  require_does_raise (fun () ->
    create_exn
      ()
      ~protocol:Krb
      ~supported_versions:[]
      ~additional_magic_numbers:[ Known_protocol.magic_number Rpc ]);
  [%expect
    {|
    ("[additional_magic_numbers] shouldn't be overlapping with magic numbers reserved by [Protocol_version_header]"
     (additional_magic_numbers (4411474))
     (reserved_magic_numbers (4346443 4411474 4674134 5521995 843207243)))
    |}];
  (* [additional_magic_numbers] clashing with the range marker *)
  require_does_raise (fun () ->
    create_exn
      ()
      ~protocol:Krb
      ~supported_versions:[]
      ~additional_magic_numbers:[ For_test.range_magic_number ]);
  [%expect
    {|
    ("[additional_magic_numbers] shouldn't be overlapping with magic numbers reserved by [Protocol_version_header]"
     (additional_magic_numbers (4674134))
     (reserved_magic_numbers (4346443 4411474 4674134 5521995 843207243)))
    |}]
;;

let%expect_test "supported version ranges" =
  let test ?(supported_versions = []) supported_version_ranges =
    create_exn () ~protocol:Rpc ~supported_versions ~supported_version_ranges
    |> [%sexp_of: t]
    |> print_s
  in
  (* The marker is sent even when there are no ranges. *)
  test [];
  [%expect {| (4411474 4674134) |}];
  test [ 5, 9 ];
  [%expect {| (4411474 4674134 5 9) |}];
  (* Ranges may overlap the individually listed versions. *)
  test ~supported_versions:[ 1; 7 ] [ 5, 9 ];
  [%expect {| (4411474 1 7 4674134 5 9) |}];
  (* Ranges are sorted, and overlapping or adjacent ranges are merged. *)
  test [ 10, 12; 1, 2 ];
  [%expect {| (4411474 4674134 1 2 10 12) |}];
  test [ 4, 9; 2, 5 ];
  [%expect {| (4411474 4674134 2 9) |}];
  test [ 6, 7; 2, 5 ];
  [%expect {| (4411474 4674134 2 7) |}];
  test [ 2, 9; 4, 5 ];
  [%expect {| (4411474 4674134 2 9) |}];
  test [ 1, 1; 3, 3; 2, 2 ];
  [%expect {| (4411474 4674134 1 3) |}];
  (* Range with a lower bound above its upper bound *)
  require_does_raise (fun () ->
    create_exn () ~protocol:Krb ~supported_versions:[] ~supported_version_ranges:[ 5, 1 ]);
  [%expect
    {|
    ("[supported_version_ranges] should only contain ranges whose lower bound is at most its upper bound"
     (supported_version_ranges ((5 1))))
    |}];
  (* Range endpoint over the max supported version limit *)
  require_does_raise (fun () ->
    create_exn
      ()
      ~protocol:Krb
      ~supported_versions:[]
      ~supported_version_ranges:[ 1, For_test.max_supported_version + 1 ]);
  [%expect
    {|
    ("Unable to advertise version ranges larger than max supported version"
     (max_supported_version 1000000)
     (supported_version_ranges ((1 1000001))))
    |}];
  (* The 100 element limit counts the marker and both endpoints of every range: 49
     disjoint ranges fit (1 magic number + 1 marker + 98 endpoints), 50 don't. *)
  let disjoint_ranges count = List.init count ~f:(fun i -> 3 * i, (3 * i) + 1) in
  test (disjoint_ranges 49);
  let (_output_too_long : string) = [%expect.output] in
  require_does_raise (fun () ->
    create_exn
      ()
      ~protocol:Krb
      ~supported_versions:[]
      ~supported_version_ranges:(disjoint_ranges 50));
  [%expect
    {|
    ("List is too large"
      (context Protocol_version_header)
      (len     102)
      (max_len 100))
    |}]
;;
