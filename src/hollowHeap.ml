
(** http://cs.au.dk/~tdh/papers/Hollow-heaps.pdf *)

module Make (Ord:Map.OrderedType) = struct

  type key = Ord.t

  type 'a holloption =
    | Full of key * 'a
    | Hollow

  module Node = struct

    type 'a t = {
      mutable elt : 'a item holloption;
      (* Children are mutable in particular to allow dag-shaped
         heaps. May possibly be improved by "inlining" the list as two
         field "child" and "next" both of type ['a t] as described in
         Section 6 of the original article. *)
      mutable children : 'a t list;
      (* [rank] is used to guide the linking strategy during
         deletes. It is a non-negative int. *)
      mutable rank : int
    }
    and 'a item = { self: 'a ; mutable node: 'a t}

    (* Used as a temporary place-holder for [make]. *)
    let dummy_node = { elt = Hollow ; children = [] ; rank = 0 }

    let make_with ?(rank=0) k xi =
      let u = { elt = Full (k,xi); children = [] ; rank } in
      let () = xi.node <- u in
      u

    let make k x =
      let xi = { self = x ; node = dummy_node } in
      xi, make_with k xi

    let link u v =
      (* Remark: it is an invariant of nodes that the root is always full. *)
      match u.elt , v.elt with
      | Full (ku,_), Full (kv,_) ->
        if ku >= kv then
          let () = v.children <- u::v.children in
          v
        else
          let () = u.children <- v::u.children in
          u
      | _ -> assert false

    (** [link_above w u v] links [u] and [v] with the following
        assumptions: [v] is an ancestor of [w] and [u] has a smaller
        key than every full node which descend from [w]. So if [u] is
        placed below [v], we also add [w] as a child of [u] to record
        the assumption in the structure of the heap. My guess is that
        the more links there are, the less likely it is that [delete]
        be costly. Therefore it is worth it recording all the
        knowledge we have. *)
    let link_above w u v =
      match u.elt , v.elt with
      | Full (ku,_), Full (kv,_) ->
        if ku >= kv then
          let () = v.children <- u::v.children in
          let () = u.children <- w::u.children in
          v
        else
          let () = u.children <- v::u.children in
          u
      | _ -> assert false

    let merge = link

    let insert u k x =
      let (xi,v) = make k x in
      xi , merge u v

    let find_min u =
      (* Remark: it is an invariant of nodes that the root is always full. *)
      match u.elt with
      | Full (_,xi) -> xi
      | _ -> assert false

    let update_key u k =
      (* Remark: applies only to that the root. It is an invariant of nodes
         the root is always full.*)
      match u.elt with
      | Full (_,xi) -> u.elt <- Full (k,xi)
      | _ -> assert false

    let decrease_key ({ node=u } as xi) k h =
      if u == h then
        let () = update_key h k in
        h
      else
        let rank = max 0 (u.rank-2) in
        let v = make_with k xi ~rank in
        let () = u.elt <- Hollow in
        link_above u v h

  end

  type 'a t = 'a Node.t option

end
