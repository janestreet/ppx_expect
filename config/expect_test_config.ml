module IO = struct
  type 'a t = 'a

  let return x = x
end

let sanitize s = s
let run f = f ()
let upon_unreleasable_issue = `CR
