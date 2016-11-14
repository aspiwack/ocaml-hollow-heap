
module type Key = sig
  type t = int
  val compare : t -> t -> int
  val name : string
  val decrease : int -> int -> int
end

module Test (Key:Key) = struct

module BlackBox = HollowHeap.Make(Key)
module GlassBox = HollowHeapInternal.Make(Key)

type op =
  | Insert of int*int
  | DeleteMin
  | DecreaseKey of int*int (** Decrease the [i]-th inserted key which is still live. *)
  | Delete of int (** Deletes the [i]-th inserted key which is still live. *)


let op_pp = function
  | Insert (k,x) -> Printf.sprintf "insert(%d,%d)" k x
  | DeleteMin -> Printf.sprintf "delete_min"
  | DecreaseKey (i,k) -> Printf.sprintf "decrease_key(%d,%d)" i k
  | Delete i -> Printf.sprintf "delete %d" i

let gen_ops n =
  let open QCheck.Gen in
  let gen_op =
    frequency
      [ 4, return (fun k x -> Insert (k,x)) <*> small_int <*> small_int
      ; 1, return DeleteMin
      ; 1, return (fun i k -> DecreaseKey (i,k)) <*> small_int <*> small_int
      ; 1, return (fun i -> Delete i) <*> small_int
      ]
  in
  list_size (0--n) gen_op

(* The generator produces a more stable sequence of actions for
   comparison of implementation. In case where several items have the
   same key, [delete_min] will delete an implementation-dependent one
   of them, then [decrease_key] will have a different effect depending
   on which item was deleted, and one may end up even with heaps of
   different size. Instead, in this generator, we do all the
   [decrease_key] before any [delete_min]. This is not as random a
   sequence of action, but it should go a long way to stress the
   implementation. *)
let gen_ops_stable n =
  let open QCheck.Gen in
  let gen_op_decrease =
    frequency
      [ 2, return (fun k x -> Insert (k,x)) <*> small_int <*> small_int
      ; 1, return (fun i k -> DecreaseKey (i,k)) <*> small_int <*> small_int
      ]
  in
  let gen_op_delete =
    frequency
      [ 4, return (fun k x -> Insert (k,x)) <*> small_int <*> small_int
      ; 1, return DeleteMin
      ; 1, return (fun i -> Delete i) <*> small_int
      ]
  in
  map2 (@) (list_size (0--n/2) gen_op_decrease) (list_size (0--n/2) gen_op_delete)

let shrink_op o =
  let open QCheck.Iter in
  match o with
  | Insert (k,x) ->
    return (fun k x -> Insert (k,x)) <*> QCheck.Shrink.int k <*> QCheck.Shrink.int x
  | DeleteMin -> empty
  | DecreaseKey (i,k) ->
    return (fun i k -> DecreaseKey (i,k)) <*> QCheck.Shrink.int i <*> QCheck.Shrink.int k
  | Delete i ->
    return (fun i -> Delete i) <*> QCheck.Shrink.int i

let arb_ops n : op list QCheck.arbitrary =
  let shrink = QCheck.Shrink.list ~shrink:shrink_op in
  let print = QCheck.Print.list op_pp in
  QCheck.make ~shrink ~print (gen_ops n)

let arb_ops_stable n : op list QCheck.arbitrary =
  let shrink = QCheck.Shrink.list ~shrink:shrink_op in
  let print = QCheck.Print.list op_pp in
  QCheck.make ~shrink ~print (gen_ops_stable n)


module Run (H : HollowHeap.S with type key = Key.t) = struct

  let f ops =
    let h = H.create () in
    let rec heap_ops items = function
      | [] -> ()
      | Insert (k,x) :: r ->
        let xi = H.insert h k x in
        heap_ops (xi::items) r
      | DeleteMin :: r ->
        let () = H.delete_min h in
        heap_ops items r
      | DecreaseKey (i,k)::r ->
        let filtered = List.filter H.live items in
        begin match List.nth filtered i with
        | xi ->
          let k0 = H.get_key xi in
          let () = H.decrease_key h xi (Key.decrease k0 k) in
          heap_ops filtered r
        | exception _ ->
          heap_ops filtered r
        end
      | Delete i::r ->
        let filtered = List.filter H.live items in
        begin match List.nth filtered i with
        | xi ->
          let () = H.delete h xi in
          heap_ops filtered r
        | exception _ ->
          heap_ops filtered r
        end
    in
    let () = heap_ops [] ops in
    h

end

module RunBlackBox = Run(BlackBox)
module RunGlassBox = Run(GlassBox)


(** {6 Glass-box testing} *)

open OUnit2

let heap_invariants =
  let open GlassBox in

  (** The dag has the min-heap property. *)
  let heap_invariant h =
    let rec heap_inariant_node u k =
      let open Node in
      match u.elt with
      | Full (ku,_) when Key.compare ku k >= 0 ->
        List.for_all (fun v -> heap_inariant_node v ku) u.children
      | Full _ -> false
      | Hollow ->
        List.for_all (fun v -> heap_inariant_node v k) u.children
    in
    match !h with
    | None -> true
    | Some u -> heap_inariant_node u min_int
  in

  (** The root is always full. *)
  let root_invariant h =
    match !h with
    | None -> true
    | Some u ->
      match u.Node.elt with
      | Full _ -> true
      | Hollow -> false
  in

  (** full nodes have a single parent. *)
  let full_node_one_parent h =
    let rec full_node_one_parent u =
      let open Node in
      match u.elt , u.sp with
      | Full _ , true -> false
      | _ -> List.for_all full_node_one_parent u.children
    in
    match !h with
    | None -> true
    | Some u -> full_node_one_parent u
  in

  (** The back pointer of an item is indeed its containing node. *)
  let item_back_pointer h =
    let rec item_back_pointer_node u =
      let open Node in
      begin match u.elt with
        | Full (_,xi) -> xi.node == u
        | Hollow -> true
      end &&
      List.for_all item_back_pointer_node u.children
    in
    match !h with
    | None -> true
    | Some u -> item_back_pointer_node u
  in


  let make ~name inv =
    QCheck.Test.make ~name (arb_ops 300) (fun ops -> inv (RunGlassBox.f ops))
  in


  (Format.sprintf "Heap invariants (%s)" Key.name) >::: QCheck_runner.to_ounit2_test_list [
    make ~name:"No assert failure" (fun _ -> true);
    make ~name:"Heap invariant" heap_invariant;
    make ~name:"Root is full" root_invariant;
    make ~name:"Full nodes have at most one parent" full_node_one_parent;
    make ~name:"Item back-pointers correct" item_back_pointer;
  ]

(** {6 Black-box testing} *)

module Reference = struct

  type key = Key.t
  (** [id] is a unique identifier to ensure that all items are
      distincts. *)
  type 'a item = { self:'a ; id:int ; heap: 'a t; mutable live:bool }
  (** The list is assumed sorted with respect to keys. *)
  and 'a t = (Key.t*'a item) list ref

  let gen_sym =
    let count = ref 0 in
    fun () -> count := !count+1; !count

  let get { self ; _ } = self
  let get_key ({ heap ; _ } as xi) =
    let (rk,_) = List.find (fun (_,yi) -> xi == yi) !heap in
    rk
  let live { live ; _ } = live

  let create () = ref []

  let insert_item h k xi =
    let rec insert_item k xi = function
      | [] -> [(k,xi)]
      | (ka,ai)::l ->
        if Key.compare k ka <= 0 then (k,xi)::(ka,ai)::l
        else (ka,ai)::insert_item k xi l
    in
    h := insert_item k xi !h

  let insert h k x =
    let xi = { self=x ; id=gen_sym() ; heap = h ; live=true } in
    let () = insert_item h k xi in
    xi

  let merge h1 h2 =
    let () = List.iter (fun (k,xi) -> insert_item h1 k xi) !h2 in
    h2 := []

  let find_min h =
    match !h with
    | [] -> None
    | (_,xi)::_ -> Some xi

  let delete_min h =
    match !h with
    | [] -> ()
    | (_,xi)::l ->
      h := l;
      xi.live <- false

  let decrease_key h xi k =
    let h' = List.filter (fun (_,yi) -> not (xi==yi)) !h in
    let () = h := h' in
    insert_item h k xi

  let delete h xi =
    let h' = List.filter (fun (_,yi) -> not (xi==yi)) !h in
    let () = h := h' in
    let () = xi.live <- false in
    ()

end

module RunReference = Run(Reference)

let compare_impl
    (module H1 : HollowHeap.S with type key=Key.t)
    (module H2 : HollowHeap.S with type key=Key.t)
    ops =
  let module RunH1 = Run(H1) in
  let module RunH2 = Run(H2) in
  let map f = function
    | Some x -> Some (f x)
    | None -> None
  in
  (* Compares the keys rather than the values because find_min can
     output _any_ of the items with minimum key. *)
  H1.(RunH1.f ops |> find_min |> map get_key) = H2.(RunH2.f ops |> find_min |> map get_key)

let functional_correctness =
  (Format.sprintf "Functional_correctness (%s)" Key.name) >::: QCheck_runner.to_ounit2_test_list QCheck.Test.[
      make ~name:"Compare to reference implementation" (arb_ops_stable 300)
        (compare_impl (module BlackBox) (module Reference));
    ]

end

(** {6 Running the tests} *)

module NatInt = struct
  type t = int
  let compare = (Pervasives.compare:int->int->int)
  let name = "int, natord"
  let decrease k0 k = min (k0-1) k
end

module RevInt = struct
  type t = int
  let compare x y = (Pervasives.compare:int->int->int) y x
  let name = "int, revord"
  let decrease k0 k = max (k0+1) k
end

module Test1 = Test(NatInt)
module Test2 = Test(RevInt)

open OUnit2

let () =
  run_test_tt_main @@ test_list [
    Test1.heap_invariants;
    Test1.functional_correctness;
    Test2.heap_invariants;
    Test2.functional_correctness;
  ]
