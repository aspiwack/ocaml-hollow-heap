
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

module Run (H : HollowHeap.S with type key = int) = struct

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



open OUnit2

let heap_invariants =
  let open GlassBox in

  (** The tree has the min-heap property. *)
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


  let make ~name inv =
    QCheck.Test.make ~name (arb_ops 300) (fun ops -> inv (RunGlassBox.f ops))
  in


  "Heap invariants" >::: QCheck_runner.to_ounit2_test_list [
    make ~name:"No assert failure" (fun _ -> true);
    make ~name:"Heap invariant" heap_invariant;
    make ~name:"Root is full" root_invariant;
  ]

let () =
  run_test_tt_main @@ test_list [
    heap_invariants;
  ]
