(* Some useful extensions for the List module *)

module List = struct
  include List

  let map_and_foldl func acc as' =
    let _aux =
     fun (acc', bs) a ->
      let acc'', b = func acc' a in
      (acc'', b :: bs)
    in
    let acc', bs = List.fold_left _aux (acc, []) as' in
    (acc', List.rev bs)
end
