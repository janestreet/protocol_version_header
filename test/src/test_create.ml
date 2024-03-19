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
  [%expect {| (4411474) |}];
  (* many supported versions *)
  test ~supported_versions:(List.init 99 ~f:Fn.id);
  let (_output_too_long : string) = [%expect.output] in
  (* too many supported versions *)
  require_does_raise [%here] (fun () ->
    create_exn () ~protocol:Krb ~supported_versions:(List.init 100 ~f:Fn.id));
  [%expect
    {|
    ("List is too large"
      (context Protocol_version_header)
      (len     101)
      (max_len 100))
    |}];
  (* Supported version at the max supported version limit *)
  test ~supported_versions:[ For_test.max_supported_version ];
  [%expect {| (4411474 1000000) |}];
  (* Supported version over the max supported version limit *)
  require_does_raise [%here] (fun () ->
    create_exn () ~protocol:Krb ~supported_versions:[ For_test.max_supported_version + 1 ]);
  [%expect
    {|
    ("Unable to advertise versions larger than max supported version"
     (max_supported_version 1000000)
     (supported_versions (1000001)))
    |}];
  (* [additional_magic_numbers] smaller than or equal to max supported version *)
  require_does_raise [%here] (fun () ->
    create_exn () ~protocol:Krb ~additional_magic_numbers:[ 1 ] ~supported_versions:[]);
  [%expect
    {|
    ("[additional_magic_numbers] shouldn't be within [max_supported_version] range"
     (max_supported_version 1000000)
     (additional_magic_numbers (1)))
    |}];
  require_does_raise [%here] (fun () ->
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
  require_does_raise [%here] (fun () ->
    create_exn
      ()
      ~protocol:Krb
      ~supported_versions:[]
      ~additional_magic_numbers:[ Known_protocol.magic_number Rpc ]);
  [%expect
    {|
    ("[additional_magic_numbers] shouldn't be overlapping with potential [protocol_magic_number]s"
     (additional_magic_numbers (4411474))
     (known_protocol_magic_numbers (4411474 5521995 843207243)))
    |}]
;;
