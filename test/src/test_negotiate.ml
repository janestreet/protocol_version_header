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
      (us_versions   (1))
      (peer_versions (2))
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
