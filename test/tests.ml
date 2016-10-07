
module Key = struct
  type t = int
  let compare = (Pervasives.compare:int->int->int)
end

module BlackBox = HollowHeap.Make(Key)
module GlassBox = HollowHeapInternal.Make(Key)

type op =
  | Insert of int*int
  | DeleteMin


let op_pp = function
  | Insert (k,x) -> Printf.sprintf "insert(%d,%d)" k x
  | DeleteMin -> Printf.sprintf "delete_min"

let gen_ops n =
  let open QCheck.Gen in
  let gen_op =
    frequency
      [ 2, return (fun k x -> Insert (k,x)) <*> small_int <*> small_int
      ; 1, return DeleteMin
      ]
  in
  list_size (0--n) gen_op

let arb_ops n : op list QCheck.arbitrary =
  let shrink_op o =
    let open QCheck.Iter in
    match o with
    | Insert (k,x) ->
      return (fun k x -> Insert (k,x)) <*> QCheck.Shrink.int k <*> QCheck.Shrink.int x
    | DeleteMin -> empty
  in
  let shrink =
    QCheck.Shrink.list ~shrink:shrink_op in
  let print = QCheck.Print.list op_pp in
QCheck.make ~shrink ~print (gen_ops n)

module Run (H : HollowHeap.S with type key = Key.t) = struct

  let f ops =
    let h = H.create () in
    let rec heap_ops = function
      | [] -> ()
      | Insert (k,x) :: r ->
        let _ = H.insert h k x in
        heap_ops r
      | DeleteMin :: r ->
        let () = H.delete_min h in
        heap_ops r
    in
    let () = heap_ops ops in
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
      | Full _ , Some _ -> false
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


  "Heap invariants" >::: QCheck_runner.to_ounit2_test_list [
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
  type 'a item = { self:'a ; id:int ; heap: 'a t}
  (** The list is assumed sorted with respect to keys. *)
  and 'a t = (Key.t ref*'a item) list ref

  let gen_sym =
    let count = ref 0 in
    fun () -> count := !count+1; !count

  let get { self } = self
  let get_key ({ heap } as xi) =
    let (rk,_) = List.find (fun (_,yi) -> xi == yi) !heap in
    !rk

  let create () = ref []

  let insert_item h k xi =
    let rec insert_item k xi = function
      | [] -> [(ref k,xi)]
      | (ka,ai)::l ->
        if Key.compare k !ka <= 0 then (ref k,xi)::(ka,ai)::l
        else (ka,ai)::insert_item k xi l
    in
    h := insert_item k xi !h

  let insert h k x =
    let xi = { self=x ; id=gen_sym() ; heap = h } in
    let () = insert_item h k xi in
    xi

  let merge h1 h2 =
    let () = List.iter (fun (k,xi) -> insert_item h1 !k xi) !h2 in
    h2 := []

  let find_min h =
    match !h with
    | [] -> None
    | (_,xi)::_ -> Some xi

  let delete_min h =
    match !h with
    | [] -> ()
    | _::l -> h := l

  let decrease_key h xi k =
    let (rk,_) = List.find (fun (_,yi) -> xi == yi) !h in
    rk := k

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
  H1.(RunH1.f ops |> find_min |> map get) = H2.(RunH2.f ops |> find_min |> map get)

let functional_correctness =
  "Functional_correctness" >::: QCheck_runner.to_ounit2_test_list QCheck.Test.[
      make ~name:"Compare to reference implementation" (arb_ops 300)
        (compare_impl (module BlackBox) (module Reference));
    ]


(** {6 Running the tests} *)

let () =
  run_test_tt_main @@ test_list [
    heap_invariants;
    functional_correctness;
  ]
