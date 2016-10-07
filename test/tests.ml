
module Int = struct
  type t = int
  let compare = (Pervasives.compare:int->int->int)
end

module BlackBox = HollowHeap.Make(Int)
module GlassBox = HollowHeapInternal.Make(Int)

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
  "Heap invariants" >::: QCheck_runner.to_ounit2_test_list QCheck.Test.[
    make ~name:"No assert failure" (arb_ops 300) (fun ops -> let _ = RunGlassBox.f ops in true)
  ]

let () =
  run_test_tt_main @@ test_list [
    heap_invariants;
  ]
