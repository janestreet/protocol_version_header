@@ portable

open! Core

(** This library offers a lightweight way for applications protocols to version
    themselves. The more protocols that add themselves to [Known_protocol], the nicer
    error messages we will get when connecting to a service while using the wrong
    protocol.

    The library also offers a mechanism for some data to be shared in the connection
    header. A header can contain [additional_magic_numbers] that are not used in the
    version negotiation but can be used in some way by the peer (e.g. the kerberos rpc
    library could use this to indicate whether the server is doing any form of
    authorization).

    On the wire, a header is a list of ints laid out like this:

    {v
      [ protocol magic number
      ; additional magic numbers...
      ; supported versions...
      ; range marker
      ; lo1; hi1; lo2; hi2; ...
      ]
    v}

    The range marker (see [For_test.range_magic_number]) is a magic number that separates
    the individually listed versions from the [supported_version_ranges], which follow it
    as inclusive [(lo, hi)] pairs. Magic numbers, the marker included, are all greater
    than [For_test.max_supported_version]; that is how they are told apart from versions.

    Sending the marker means "I understand version ranges". [create_exn] always sends it,
    even when there are no ranges. A peer built before version ranges existed doesn't know
    the marker, so it ignores it like any other unknown magic number and treats each [lo]
    and [hi] as an ordinary supported version. Both sides of a connection must reach the
    same answer, so [negotiate] only treats ranges as ranges when both headers carry the
    marker. Otherwise it, too, treats the endpoints as ordinary versions.

    When both headers carry the marker, [negotiate] takes the range section to be
    everything after the first marker up to the next magic number, or to the end of the
    header if there is none, and pairs it up into [(lo, hi)] ranges. The ranges must be
    sorted and must not overlap. The section is malformed if it has an odd number of
    elements or breaks that ordering. [create_exn] never builds such a header, so a
    malformed section is reported as its own error rather than as a version mismatch. When
    only one header carries the marker, the section is never read as ranges, so it can't
    be malformed.

    The list of supported ranges is taken to be all values that occur in the list
    following the range marker, up until the end of the list or the next magic number
    (that is, the next number larger than [max_supported_version]). This way, if we need
    to add a new "section" in the future, we can add it after the list of ranges, such
    that the current version of the library will ignore them. *)

type t : immutable_data [@@deriving bin_io ~localize, globalize, sexp, stable_witness]

val bin_read_t__local : t Bin_prot.Read.reader__local [@@zero_alloc opt arity 2]

(** [create_exn] lists each of [supported_versions] individually, and each of
    [supported_version_ranges] as an inclusive [(lo, hi)] pair. Ranges are sorted, and
    overlapping or adjacent ranges are merged. A range may overlap the versions in
    [supported_versions].

    It raises if any of the following is true:
    - the header would hold more than 100 elements (see the wire format above: the range
      marker takes one element and each range takes two)
    - [supported_versions] contains a version greater than [max_supported_version]
    - [supported_version_ranges] contains an endpoint greater than
      [max_supported_version], or a range whose [lo] is greater than its [hi]
    - [additional_magic_numbers] contains a number not greater than
      [max_supported_version], which would be read as a version
    - [additional_magic_numbers] contains a magic number reserved by this library. *)
val create_exn
  :  ?additional_magic_numbers:int list
  -> ?supported_version_ranges:(int * int) list
  -> protocol:Known_protocol.t
  -> supported_versions:int list
  -> unit
  -> t

(** [negotiate ~allow_legacy_peer ~us ~peer] inspects the magic numbers of [us] and
    [peer]. If the magic numbers match, the highest shared version number is returned.

    If [allow_legacy_peer] then the magic number of [peer] is assumed to be [us] if no
    magic number exists.

    Ranges are treated as ranges only if both headers carry the range marker; otherwise
    their endpoints count as ordinary versions (see the wire format above). If both
    headers carry the marker but one has a malformed range section, the result is an
    error. *)
val negotiate : allow_legacy_peer:bool -> us:t -> peer:t -> int Or_error.t @ portable

(** [contains_magic_prefix] reads a bin_protted value of type [t] and returns a boolean
    saying whether this magic number was observed. *)
val contains_magic_prefix : protocol:Known_protocol.t -> bool Bin_prot.Type_class.reader

(** [any_magic_prefix] and [any_magic_prefix_from_six_bytes] read the magic number for one
    of the known protocols. They differ in how many bytes they read.

    [any_magic_prefix] reads the entire [Protocol_version_header.t] and then checks if
    there's any magic prefix in there. The number of bytes in the bin_io representation of
    a [Protocol_version_header.t] is not fixed because it depends on the supported
    versions of the protocol.

    [any_magic_prefix_from_six_bytes] reads exactly 6 bytes to check magic prefix.

    [any_magic_prefix_from_six_bytes_bin_size] is 6: the number of bytes that
    [any_magic_prefix_from_six_bytes] reads. *)
val any_magic_prefix : Known_protocol.t option Bin_prot.Type_class.reader @@ nonportable

(** See [any_magic_prefix] *)
val any_magic_prefix_from_six_bytes
  : Known_protocol.t option Bin_prot.Type_class.reader
  @@ nonportable

(** See [any_magic_prefix] *)
val any_magic_prefix_from_six_bytes_bin_size : int

module Known_protocol = Known_protocol
module Reserved_magic_numbers = Reserved_magic_numbers

(** This is useful for passing around a pair of [t]s that will eventually be passed to
    [negotiate]. *)
module Pair : sig
  type nonrec t =
    { us : t
    ; peer : t
    }
end

module Expert : sig
  (** Gets the raw list of version numbers contained in the header. *)
  val raw_version_list : t -> int list

  val none : t
  val is_none : t -> bool
end

module For_test : sig
  module Make_list_with_max_len (Config : List_with_max_len.Config) : List_with_max_len.S

  (** The number of bytes in the bin_io representation of a magic number. All magic
      numbers are represented in this fixed number of bytes. *)
  val magic_number_bin_size : int

  (** Max version that's possible to be supported ever. Any numbers bigger than this are
      considered metadata that's shared in the header. *)
  val max_supported_version : int

  (** The magic number [create_exn] puts in every header to say that it understands
      version ranges. See the wire format at the top of this file. *)
  val range_magic_number : int

  val create_unvalidated : int list -> t
end
