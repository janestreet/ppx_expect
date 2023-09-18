open! Base
open Types

(** The callback expected by [f], which should convert the input to patches and is
    allowed to access the contents of the original file while doing so. *)
module Patch_with_file_contents : sig
  type 'a t = original_file_contents:string -> 'a -> (Compact_loc.t * string) list
end

(** Build a list of diffs to a file using [with_], then apply them to the file contents
    and write a [.corrected] file.

    Exported for use by toplevel tests. *)
val f
  :  use_color:bool
  -> in_place:bool
  -> diff_command:string option
  -> diff_path_prefix:string option
  -> filename:string
  -> with_:'a Patch_with_file_contents.t
  -> 'a
  -> Ppx_inline_test_lib.Test_result.t
