(* Some useful extensions for the List module *)

module List = struct
  include List

  let map_and_foldl func acc as' =
    let _aux (acc', bs) a =
      let acc'', b = func acc' a in
      (acc'', b :: bs)
    in
    let acc', bs = List.fold_left _aux (acc, []) as' in
    (acc', List.rev bs)

  let map_opt (func : 'a -> 'b option) (as' : 'a list) : 'b list option =
    let ( let* ) = Option.bind in
    let _aux (a : 'a) (acc_opt : 'b list option) =
      let* b = func a in
      let* acc = acc_opt in
      Some (b :: acc)
    in
    List.fold_right _aux as' (Some [])
end
