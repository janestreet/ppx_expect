open StdLabels

(* Make sure we use a tail rec version of List.map. *)
module List = struct
  include List
  let map ~f l =
    List.rev (List.rev_map ~f l)
end
