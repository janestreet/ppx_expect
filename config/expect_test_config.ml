module type S = sig
  module IO : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
  val flush : unit -> unit IO.t
  val run : (unit -> unit IO.t) -> unit
end

module IO = struct
  type 'a t = 'a
  let return x = x
  let bind t f = f t
end

let flush () = () (* the runtime already flushes [stdout] *)
let run f = f ()

