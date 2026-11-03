open! Core
open Protocol_version_header

(* A reference table of [negotiate]'s behavior on representative headers, including
   garbage a peer could send on the wire. The intent of these tests is to catch any
   regressions introduced by refactors that were meant to be behavior-preserving. *)

let krb = Known_protocol.magic_number Krb
let krb_test_mode = Known_protocol.magic_number Krb_test_mode
let rpc = Known_protocol.magic_number Rpc

(* An unknown magic number, e.g. the [additional_magic_numbers] of a [create_exn] caller
   with newer knowledge than us. *)
let unknown_magic = For_test.max_supported_version + 1
let range_marker = For_test.range_magic_number

let case ~us ~peer =
  List.iter [ false; true ] ~f:(fun allow_legacy_peer ->
    let result =
      negotiate
        ~allow_legacy_peer
        ~us:(For_test.create_unvalidated us)
        ~peer:(For_test.create_unvalidated peer)
    in
    print_s
      [%message
        ""
          (us : int list)
          (peer : int list)
          (allow_legacy_peer : bool)
          ~_:(result : int Or_error.t)])
;;

let%expect_test "same protocol" =
  case ~us:[ krb; 1 ] ~peer:[ krb; 1 ];
  [%expect
    {|
    ((us (843207243 1)) (peer (843207243 1)) (allow_legacy_peer false) (Ok 1))
    ((us (843207243 1)) (peer (843207243 1)) (allow_legacy_peer true) (Ok 1))
    |}];
  case ~us:[ krb; 1; 2; 3 ] ~peer:[ krb; 2; 3; 4 ];
  [%expect
    {|
    ((us (843207243 1 2 3)) (peer (843207243 2 3 4)) (allow_legacy_peer false)
     (Ok 3))
    ((us (843207243 1 2 3)) (peer (843207243 2 3 4)) (allow_legacy_peer true)
     (Ok 3))
    |}];
  case ~us:[ krb; 1 ] ~peer:[ krb; 2 ];
  [%expect
    {|
    ((us (843207243 1)) (peer (843207243 2)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (peer_versions (2)) (protocol Krb))))
    ((us (843207243 1)) (peer (843207243 2)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (peer_versions (2)) (protocol Krb))))
    |}];
  case
    ~us:[ rpc; For_test.max_supported_version ]
    ~peer:[ rpc; For_test.max_supported_version ];
  [%expect
    {|
    ((us (4411474 1000000)) (peer (4411474 1000000)) (allow_legacy_peer false)
     (Ok 1000000))
    ((us (4411474 1000000)) (peer (4411474 1000000)) (allow_legacy_peer true)
     (Ok 1000000))
    |}]
;;

let%expect_test "different protocols" =
  case ~us:[ krb; 1 ] ~peer:[ rpc; 1 ];
  [%expect
    {|
    ((us (843207243 1)) (peer (4411474 1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
       (us_protocol Krb) (peer_protocol Rpc))))
    ((us (843207243 1)) (peer (4411474 1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
       (us_protocol Krb) (peer_protocol Rpc))))
    |}];
  case ~us:[ krb_test_mode; 5 ] ~peer:[ krb; 5 ];
  [%expect
    {|
    ((us (5521995 5)) (peer (843207243 5)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
       (us_protocol Krb_test_mode) (peer_protocol Krb))))
    ((us (5521995 5)) (peer (843207243 5)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
       (us_protocol Krb_test_mode) (peer_protocol Krb))))
    |}]
;;

let%expect_test "legacy peers (no magic number)" =
  case ~us:[ krb; 1; 2 ] ~peer:[ 1 ];
  [%expect
    {|
    ((us (843207243 1 2)) (peer (1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine peer's protocol"
       (us_protocol Krb) (peer_protocol Unknown))))
    ((us (843207243 1 2)) (peer (1)) (allow_legacy_peer true) (Ok 1))
    |}];
  case ~us:[ krb; 2 ] ~peer:[ 1 ];
  [%expect
    {|
    ((us (843207243 2)) (peer (1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine peer's protocol"
       (us_protocol Krb) (peer_protocol Unknown))))
    ((us (843207243 2)) (peer (1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (2)) (peer_versions (1)) (protocol Krb))))
    |}];
  case ~us:[ 1 ] ~peer:[ krb; 1 ];
  [%expect
    {|
    ((us (1)) (peer (843207243 1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions (1)))))
    ((us (1)) (peer (843207243 1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions (1)))))
    |}];
  case ~us:[ 1; 2 ] ~peer:[ 2; 3 ];
  [%expect
    {|
    ((us (1 2)) (peer (2 3)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions (1 2)))))
    ((us (1 2)) (peer (2 3)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions (1 2)))))
    |}]
;;

let%expect_test "empty headers" =
  case ~us:[] ~peer:[ krb; 1 ];
  [%expect
    {|
    ((us ()) (peer (843207243 1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions ()))))
    ((us ()) (peer (843207243 1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions ()))))
    |}];
  case ~us:[ krb; 1 ] ~peer:[];
  [%expect
    {|
    ((us (843207243 1)) (peer ()) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine peer's protocol"
       (us_protocol Krb) (peer_protocol Unknown))))
    ((us (843207243 1)) (peer ()) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (peer_versions ()) (protocol Krb))))
    |}];
  case ~us:[] ~peer:[];
  [%expect
    {|
    ((us ()) (peer ()) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions ()))))
    ((us ()) (peer ()) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine our own protocol"
       (us_versions ()))))
    |}]
;;

let%expect_test "multiple known protocols in one header" =
  case ~us:[ krb; 1 ] ~peer:[ krb; rpc; 1 ];
  [%expect
    {|
    ((us (843207243 1)) (peer (843207243 4411474 1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Rpc)) (versions (1)))))
    ((us (843207243 1)) (peer (843207243 4411474 1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Rpc)) (versions (1)))))
    |}];
  case ~us:[ krb; rpc; 1 ] ~peer:[ 1 ];
  [%expect
    {|
    ((us (843207243 4411474 1)) (peer (1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Rpc)) (versions (1)))))
    ((us (843207243 4411474 1)) (peer (1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Rpc)) (versions (1)))))
    |}];
  case ~us:[ krb; rpc; 1 ] ~peer:[ krb_test_mode; rpc; 1 ];
  [%expect
    {|
    ((us (843207243 4411474 1)) (peer (5521995 4411474 1))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Rpc)) (versions (1)))))
    ((us (843207243 4411474 1)) (peer (5521995 4411474 1))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Rpc)) (versions (1)))))
    |}];
  (* A repeated magic number counts as multiple. *)
  case ~us:[ krb; krb; 1 ] ~peer:[ krb; 1 ];
  [%expect
    {|
    ((us (843207243 843207243 1)) (peer (843207243 1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Krb)) (versions (1)))))
    ((us (843207243 843207243 1)) (peer (843207243 1)) (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine protocol: Multiple magic numbers corresponding to known protocols observed in header"
       (protocols (Krb Krb)) (versions (1)))))
    |}]
;;

let%expect_test "unknown magic numbers are ignored for negotiation" =
  case ~us:[ krb; unknown_magic; 1; 2 ] ~peer:[ krb; unknown_magic + 1; 2 ];
  [%expect
    {|
    ((us (843207243 1000001 1 2)) (peer (843207243 1000002 2))
     (allow_legacy_peer false) (Ok 2))
    ((us (843207243 1000001 1 2)) (peer (843207243 1000002 2))
     (allow_legacy_peer true) (Ok 2))
    |}];
  (* Unknown magic numbers alone don't make a peer non-legacy. *)
  case ~us:[ krb; 1 ] ~peer:[ unknown_magic; 1 ];
  [%expect
    {|
    ((us (843207243 1)) (peer (1000001 1)) (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Could not determine peer's protocol"
       (us_protocol Krb) (peer_protocol Unknown))))
    ((us (843207243 1)) (peer (1000001 1)) (allow_legacy_peer true) (Ok 1))
    |}]
;;

let%expect_test "headers that [create_exn] would never build" =
  (* Versions out of order, magic number not at the head. *)
  case ~us:[ krb; 3; 1; 2 ] ~peer:[ 2; 3; krb; 1 ];
  [%expect
    {|
    ((us (843207243 3 1 2)) (peer (2 3 843207243 1)) (allow_legacy_peer false)
     (Ok 3))
    ((us (843207243 3 1 2)) (peer (2 3 843207243 1)) (allow_legacy_peer true)
     (Ok 3))
    |}];
  (* Duplicate versions. *)
  case ~us:[ krb; 2; 2; 1 ] ~peer:[ krb; 2 ];
  [%expect
    {|
    ((us (843207243 2 2 1)) (peer (843207243 2)) (allow_legacy_peer false)
     (Ok 2))
    ((us (843207243 2 2 1)) (peer (843207243 2)) (allow_legacy_peer true) (Ok 2))
    |}];
  (* Negative "versions". *)
  case ~us:[ krb; -1; 1 ] ~peer:[ krb; -1 ];
  [%expect
    {|
    ((us (843207243 -1 1)) (peer (843207243 -1)) (allow_legacy_peer false)
     (Ok -1))
    ((us (843207243 -1 1)) (peer (843207243 -1)) (allow_legacy_peer true)
     (Ok -1))
    |}]
;;

let%expect_test "both peers understand version ranges" =
  (* The only versions in common lie inside the ranges. *)
  case ~us:[ krb; range_marker; 5; 9 ] ~peer:[ krb; range_marker; 7; 12 ];
  [%expect
    {|
    ((us (843207243 4674134 5 9)) (peer (843207243 4674134 7 12))
     (allow_legacy_peer false) (Ok 9))
    ((us (843207243 4674134 5 9)) (peer (843207243 4674134 7 12))
     (allow_legacy_peer true) (Ok 9))
    |}];
  (* The only version in common is a range endpoint. *)
  case ~us:[ krb; range_marker; 5; 9 ] ~peer:[ krb; range_marker; 9; 12 ];
  [%expect
    {|
    ((us (843207243 4674134 5 9)) (peer (843207243 4674134 9 12))
     (allow_legacy_peer false) (Ok 9))
    ((us (843207243 4674134 5 9)) (peer (843207243 4674134 9 12))
     (allow_legacy_peer true) (Ok 9))
    |}];
  (* A range and an individually listed version. *)
  case ~us:[ krb; range_marker; 5; 9 ] ~peer:[ krb; 7; range_marker ];
  [%expect
    {|
    ((us (843207243 4674134 5 9)) (peer (843207243 7 4674134))
     (allow_legacy_peer false) (Ok 7))
    ((us (843207243 4674134 5 9)) (peer (843207243 7 4674134))
     (allow_legacy_peer true) (Ok 7))
    |}];
  case ~us:[ krb; range_marker; 5; 9 ] ~peer:[ krb; 9; range_marker ];
  [%expect
    {|
    ((us (843207243 4674134 5 9)) (peer (843207243 9 4674134))
     (allow_legacy_peer false) (Ok 9))
    ((us (843207243 4674134 5 9)) (peer (843207243 9 4674134))
     (allow_legacy_peer true) (Ok 9))
    |}];
  (* A header with an empty range section advertises only its individually listed
     versions. *)
  case ~us:[ krb; 1; range_marker ] ~peer:[ krb; range_marker; 1; 5 ];
  [%expect
    {|
    ((us (843207243 1 4674134)) (peer (843207243 4674134 1 5))
     (allow_legacy_peer false) (Ok 1))
    ((us (843207243 1 4674134)) (peer (843207243 4674134 1 5))
     (allow_legacy_peer true) (Ok 1))
    |}];
  case ~us:[ krb; 1; range_marker ] ~peer:[ krb; range_marker; 2; 5 ];
  [%expect
    {|
    ((us (843207243 1 4674134)) (peer (843207243 4674134 2 5))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (us_ranges ()) (peer_versions ()) (peer_ranges ((2 5)))
       (protocol Krb))))
    ((us (843207243 1 4674134)) (peer (843207243 4674134 2 5))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (us_ranges ()) (peer_versions ()) (peer_ranges ((2 5)))
       (protocol Krb))))
    |}];
  (* Several ranges. *)
  case
    ~us:[ krb; range_marker; 1; 3; 7; 9 ]
    ~peer:[ krb; range_marker; 4; 6; 8; 8; 20; 30 ];
  [%expect
    {|
    ((us (843207243 4674134 1 3 7 9)) (peer (843207243 4674134 4 6 8 8 20 30))
     (allow_legacy_peer false) (Ok 8))
    ((us (843207243 4674134 1 3 7 9)) (peer (843207243 4674134 4 6 8 8 20 30))
     (allow_legacy_peer true) (Ok 8))
    |}];
  (* Ranges with no versions in common. *)
  case ~us:[ krb; range_marker; 1; 3 ] ~peer:[ krb; range_marker; 5; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 1 3)) (peer (843207243 4674134 5 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ((1 3))) (peer_versions ())
       (peer_ranges ((5 9))) (protocol Krb))))
    ((us (843207243 4674134 1 3)) (peer (843207243 4674134 5 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ((1 3))) (peer_versions ())
       (peer_ranges ((5 9))) (protocol Krb))))
    |}];
  case
    ~us:[ krb; 1; range_marker; 3; 4; 7; 9 ]
    ~peer:[ krb; 2; range_marker; 5; 6; 20; 30 ];
  [%expect
    {|
    ((us (843207243 1 4674134 3 4 7 9)) (peer (843207243 2 4674134 5 6 20 30))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (us_ranges ((3 4) (7 9))) (peer_versions (2))
       (peer_ranges ((5 6) (20 30))) (protocol Krb))))
    ((us (843207243 1 4674134 3 4 7 9)) (peer (843207243 2 4674134 5 6 20 30))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1)) (us_ranges ((3 4) (7 9))) (peer_versions (2))
       (peer_ranges ((5 6) (20 30))) (protocol Krb))))
    |}]
;;

let%expect_test "one peer predates version ranges" =
  (* A peer without the marker sees the range endpoints as ordinary versions. Those really
     are versions we support, so negotiating one of them is correct... *)
  case ~us:[ krb; 1; 2; 3; range_marker; 5; 9 ] ~peer:[ krb; 9 ];
  [%expect
    {|
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 9))
     (allow_legacy_peer false) (Ok 9))
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 9))
     (allow_legacy_peer true) (Ok 9))
    |}];
  case ~us:[ krb; 1; 2; 3; range_marker; 5; 9 ] ~peer:[ krb; 5 ];
  [%expect
    {|
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 5))
     (allow_legacy_peer false) (Ok 5))
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 5))
     (allow_legacy_peer true) (Ok 5))
    |}];
  (* ...but such a peer can't see the versions strictly inside a range, so those are never
     negotiated. *)
  case ~us:[ krb; 1; 2; 3; range_marker; 5; 9 ] ~peer:[ krb; 7 ];
  [%expect
    {|
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 7))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1 2 3 5 9)) (peer_versions (7)) (protocol Krb))))
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 7))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (1 2 3 5 9)) (peer_versions (7)) (protocol Krb))))
    |}];
  case ~us:[ krb; 7 ] ~peer:[ krb; 1; 2; 3; range_marker; 5; 9 ];
  [%expect
    {|
    ((us (843207243 7)) (peer (843207243 1 2 3 4674134 5 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (7)) (peer_versions (1 2 3 5 9)) (protocol Krb))))
    ((us (843207243 7)) (peer (843207243 1 2 3 4674134 5 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions (7)) (peer_versions (1 2 3 5 9)) (protocol Krb))))
    |}];
  case ~us:[ krb; 1; 2; 3; range_marker; 5; 9 ] ~peer:[ krb; 3; 7 ];
  [%expect
    {|
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 3 7))
     (allow_legacy_peer false) (Ok 3))
    ((us (843207243 1 2 3 4674134 5 9)) (peer (843207243 3 7))
     (allow_legacy_peer true) (Ok 3))
    |}]
;;

let%expect_test "malformed range sections" =
  (* An odd number of elements after the marker. *)
  case ~us:[ krb; range_marker; 1 ] ~peer:[ krb; range_marker; 2; 3 ];
  [%expect
    {|
    ((us (843207243 4674134 1)) (peer (843207243 4674134 2 3))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1)))))
    ((us (843207243 4674134 1)) (peer (843207243 4674134 2 3))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1)))))
    |}];
  (* A range whose lower bound exceeds its upper bound. *)
  case ~us:[ krb; range_marker; 9; 5 ] ~peer:[ krb; range_marker; 5; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 9 5)) (peer (843207243 4674134 5 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 9 5)))))
    ((us (843207243 4674134 9 5)) (peer (843207243 4674134 5 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 9 5)))))
    |}];
  (* Ranges out of order. *)
  case ~us:[ krb; range_marker; 5; 9; 1; 3 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 5 9 1 3)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 5 9 1 3)))))
    ((us (843207243 4674134 5 9 1 3)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 5 9 1 3)))))
    |}];
  (* Overlapping ranges. *)
  case ~us:[ krb; range_marker; 1; 5; 3; 9 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 1 5 3 9)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1 5 3 9)))))
    ((us (843207243 4674134 1 5 3 9)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1 5 3 9)))))
    |}];
  (* Ranges that touch. Adjacent ranges are fine, but sharing an endpoint is not. *)
  case ~us:[ krb; range_marker; 1; 3; 3; 5 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 1 3 3 5)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1 3 3 5)))))
    ((us (843207243 4674134 1 3 3 5)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1 3 3 5)))))
    |}];
  case ~us:[ krb; range_marker; 1; 2; 3; 4 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 1 2 3 4)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false) (Ok 4))
    ((us (843207243 4674134 1 2 3 4)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true) (Ok 4))
    |}];
  (* An odd number of elements because a magic number cuts the section short. *)
  case ~us:[ krb; range_marker; 1; unknown_magic; 2 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 1 1000001 2)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1 1000001 2)))))
    ((us (843207243 4674134 1 1000001 2)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Malformed version range section. This indicates a bug in the construction of this header rather than a genuine version mismatch."
       (which_header Us) (header (843207243 4674134 1 1000001 2)))))
    |}];
  (* Against a peer with no marker, the same malformed headers negotiate fine. Neither
     side reads the range section as ranges, so the problem is never noticed: the peer
     treats the endpoints as plain versions, and so do we. *)
  case ~us:[ krb; range_marker; 1 ] ~peer:[ krb; 1 ];
  [%expect
    {|
    ((us (843207243 4674134 1)) (peer (843207243 1)) (allow_legacy_peer false)
     (Ok 1))
    ((us (843207243 4674134 1)) (peer (843207243 1)) (allow_legacy_peer true)
     (Ok 1))
    |}];
  case ~us:[ krb; range_marker; 9; 5 ] ~peer:[ krb; 5 ];
  [%expect
    {|
    ((us (843207243 4674134 9 5)) (peer (843207243 5)) (allow_legacy_peer false)
     (Ok 5))
    ((us (843207243 4674134 9 5)) (peer (843207243 5)) (allow_legacy_peer true)
     (Ok 5))
    |}];
  case ~us:[ krb; range_marker; 1; unknown_magic; 2 ] ~peer:[ krb; 2 ];
  [%expect
    {|
    ((us (843207243 4674134 1 1000001 2)) (peer (843207243 2))
     (allow_legacy_peer false) (Ok 2))
    ((us (843207243 4674134 1 1000001 2)) (peer (843207243 2))
     (allow_legacy_peer true) (Ok 2))
    |}]
;;

let%expect_test "the range section ends at the next magic number" =
  (* Whatever follows that magic number is taken to be a section this version of the
     library doesn't know about, and is ignored. Here [unknown_magic] stands in for the
     marker of such a future section. *)
  case
    ~us:[ krb; range_marker; 5; 9; unknown_magic; 20; 30 ]
    ~peer:[ krb; range_marker; 7; 12 ];
  [%expect
    {|
    ((us (843207243 4674134 5 9 1000001 20 30)) (peer (843207243 4674134 7 12))
     (allow_legacy_peer false) (Ok 9))
    ((us (843207243 4674134 5 9 1000001 20 30)) (peer (843207243 4674134 7 12))
     (allow_legacy_peer true) (Ok 9))
    |}];
  (* In particular, the contents of the unknown section are not read as versions... *)
  case
    ~us:[ krb; range_marker; 5; 9; unknown_magic; 12 ]
    ~peer:[ krb; range_marker; 12; 12 ];
  [%expect
    {|
    ((us (843207243 4674134 5 9 1000001 12)) (peer (843207243 4674134 12 12))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ((5 9))) (peer_versions ())
       (peer_ranges ((12 12))) (protocol Krb))))
    ((us (843207243 4674134 5 9 1000001 12)) (peer (843207243 4674134 12 12))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ((5 9))) (peer_versions ())
       (peer_ranges ((12 12))) (protocol Krb))))
    |}];
  (* ...except by a peer with no marker, which reads every version on the wire, so we do
     the same. *)
  case ~us:[ krb; range_marker; 5; 9; unknown_magic; 12 ] ~peer:[ krb; 12 ];
  [%expect
    {|
    ((us (843207243 4674134 5 9 1000001 12)) (peer (843207243 12))
     (allow_legacy_peer false) (Ok 12))
    ((us (843207243 4674134 5 9 1000001 12)) (peer (843207243 12))
     (allow_legacy_peer true) (Ok 12))
    |}];
  (* A magic number right after the marker makes the range section empty. *)
  case ~us:[ krb; range_marker; unknown_magic; 5 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 1000001 5)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ()) (peer_versions ()) (peer_ranges ((1 9)))
       (protocol Krb))))
    ((us (843207243 4674134 1000001 5)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ()) (peer_versions ()) (peer_ranges ((1 9)))
       (protocol Krb))))
    |}];
  (* A repeated marker is just another magic number, so it ends the section too. *)
  case ~us:[ krb; range_marker; range_marker; 5 ] ~peer:[ krb; range_marker; 1; 9 ];
  [%expect
    {|
    ((us (843207243 4674134 4674134 5)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer false)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ()) (peer_versions ()) (peer_ranges ((1 9)))
       (protocol Krb))))
    ((us (843207243 4674134 4674134 5)) (peer (843207243 4674134 1 9))
     (allow_legacy_peer true)
     (Error
      ("[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
       (us_versions ()) (us_ranges ()) (peer_versions ()) (peer_ranges ((1 9)))
       (protocol Krb))))
    |}]
;;

let%expect_test "range markers in headers that [create_exn] would never build" =
  (* Garbage, including a negative "version", before the marker. *)
  case ~us:[ 3; krb; -1; 1; range_marker; 5; 6 ] ~peer:[ krb; range_marker; 1; 2 ];
  [%expect
    {|
    ((us (3 843207243 -1 1 4674134 5 6)) (peer (843207243 4674134 1 2))
     (allow_legacy_peer false) (Ok 1))
    ((us (3 843207243 -1 1 4674134 5 6)) (peer (843207243 4674134 1 2))
     (allow_legacy_peer true) (Ok 1))
    |}];
  case ~us:[ krb; -1; range_marker ] ~peer:[ krb; -1; range_marker ];
  [%expect
    {|
    ((us (843207243 -1 4674134)) (peer (843207243 -1 4674134))
     (allow_legacy_peer false) (Ok -1))
    ((us (843207243 -1 4674134)) (peer (843207243 -1 4674134))
     (allow_legacy_peer true) (Ok -1))
    |}];
  case ~us:[ krb; unknown_magic; 3; range_marker; 5; 6 ] ~peer:[ krb; range_marker; 6; 8 ];
  [%expect
    {|
    ((us (843207243 1000001 3 4674134 5 6)) (peer (843207243 4674134 6 8))
     (allow_legacy_peer false) (Ok 6))
    ((us (843207243 1000001 3 4674134 5 6)) (peer (843207243 4674134 6 8))
     (allow_legacy_peer true) (Ok 6))
    |}]
;;
