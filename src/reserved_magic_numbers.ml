open! Core

let all =
  Known_protocol.all_magic_numbers_including_retired @ [ Range_section.magic_number ]
;;
