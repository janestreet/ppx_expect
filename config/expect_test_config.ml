module IO = struct
  type 'a t = 'a

  let return x = x
end

let sanitize s = s
let run f = f ()
let flushed () = true (* the runtime flushed [stdout] before calling this function *)
let upon_unreleasable_issue = `CR
