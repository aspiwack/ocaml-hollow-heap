(** [heap_ops (n,d) len] generates a random sequence of Heap
    operations of size [len], [n/d] of them, on average, being removes. *)
let heap_ops (module H : HollowHeap.S with type key = int) (n,d) len =
  let int () = Random.int (10*len) in
  let op h =
    if Random.int d < n then H.delete_min h
    else ignore (H.insert h (int ()) (int ()))
  in
  let rec loop h = function
    | 0 -> ()
    | n -> op h ; loop h (n-1)
  in
  let h = H.create () in
  loop h len

(** [heap_adds len] generates a random sequence of [add] of size [len], then [len] [remove_min]. *)
let heap_adds (module H : HollowHeap.S with type key = int) len =
  let int () = Random.int (20*len) in
  let rec loop_add h = function
    | 0 -> ()
    | n -> ignore @@ H.insert h (int()) (int ()) ; loop_add h (n-1)
  in
  let rec loop_remove h = function
    | 0 -> ()
    | n -> H.delete_min h ; loop_remove h (n-1)
  in
  let h = H.create () in
  let () = loop_add h len in
  loop_remove h len


let heap_ops_bench name rat h =
  Core_bench.Std.Bench.Test.create_indexed
    ~name
    ~args:[1000]
    (fun len -> Core.Std.Staged.stage (fun () -> heap_ops h rat len))

let heap_adds_bench name h =
  Core_bench.Std.Bench.Test.create_indexed
    ~name
    ~args:[500]
    (fun len -> Core.Std.Staged.stage (fun () -> heap_adds h len))

module IntHollowHeap = HollowHeap.Make(struct type t = int let compare = (compare : int -> int -> int) end)

let () =
  Core.Std.Command.run @@ Core_bench.Std.Bench.make_command [
    heap_adds_bench "HollowHeap (ordered)" (module IntHollowHeap);
    heap_ops_bench "HollowHeap (1/3)" (1,3) (module IntHollowHeap);
]
