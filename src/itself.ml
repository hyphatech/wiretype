(* One lock for every description: one is built in the time its module's
   combinators take, once, so it is never waited on long. A domain that
   holds it forces what it reaches directly, since a description is built
   inside another and the lock is not taken twice. *)
let building = Mutex.create ()
let holding = Domain.DLS.new_key (fun () -> false)

let locked l =
  Mutex.protect building (fun () ->
      Domain.DLS.set holding true;
      Fun.protect
        ~finally:(fun () -> Domain.DLS.set holding false)
        (fun () -> Lazy.force l))

(* [Lazy.is_val] is true of a lazy another domain is forcing as well as of
   one forced, and forcing that one is [Lazy.Undefined]: its builder holds
   the lock until it is built, so waiting for the lock is waiting for it. *)
let force l =
  if Domain.DLS.get holding then Lazy.force l
  else if Lazy.is_val l then
    match Lazy.force l with v -> v | exception Lazy.Undefined -> locked l
  else locked l
