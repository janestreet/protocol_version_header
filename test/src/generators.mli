open! Core

(** Half the time, permutes the elements of an encoded header, so that tests also cover
    headers that aren't in the shape [create_exn] builds. *)
val conditionally_permute
  :  Protocol_version_header.t
  -> Protocol_version_header.t Quickcheck.Generator.t

(** Generator for protocol version headers with configurable version count range.
    Generates a tuple of (protocol, supported_versions, additional_magic_numbers) suitable
    for passing to [Protocol_version_header.create_exn]. *)
val header_generator
  :  min_versions:int
  -> max_versions:int
  -> (Protocol_version_header.Known_protocol.t * int list * int list)
       Quickcheck.Generator.t

(** Generates the version arguments for [Protocol_version_header.create_exn]: a handful of
    [supported_versions] and [supported_version_ranges]. Versions are drawn from a small
    range so that two independently generated headers are likely to have some in common. *)
val advertised_versions : (int list * (int * int) list) Quickcheck.Generator.t

(** Generates the kinds of headers [negotiate] has to handle: headers built by
    [create_exn] (with and without version ranges), headers from peers that predate
    version ranges, random permutations of either, and hand-built garbage. *)
val negotiation_header : Protocol_version_header.t Quickcheck.Generator.t

(** Generates headers built by [create_exn] from a handful of low version numbers and no
    version ranges. *)
val header_without_ranges : Protocol_version_header.t Quickcheck.Generator.t
