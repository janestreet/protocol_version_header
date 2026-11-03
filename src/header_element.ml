open! Core

let max_supported_version = 1_000_000
let[@zero_alloc] is_magic_number element = element > max_supported_version
let[@zero_alloc] is_version_number element = not (is_magic_number element)
