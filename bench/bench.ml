(** [heap_ops (i,dk,dm) len] generates a random sequence of Heap
    operations of size [len], for each [i] insertion, there is, on
    average, [dk] decreases and [dm] deletes.. *)
let heap_ops (module H : HollowHeap.S with type key = int) (i,dk,dm) len =
  let int () = Random.int (10*len) in
  let insert items h =
    let nw = H.insert h (int ()) (int ()) in
    items := nw::!items
  in
  let dkey items h =
    let l = List.length !items in
    let i = Random.int l in
    let ix = List.nth !items i in
    if H.live ix then
      let k = H.get_key ix in
      H.decrease_key h ix (k-1)
    else
      ()
  in
  let op items h =
    let flip = Random.int (i+dk+dm) in
    if flip < dm then H.delete_min h
    else if i < dm+dk then dkey items h
    else insert items h
  in
  let rec loop items h = function
    | 0 -> ()
    | n -> op items h ; loop items h (n-1)
  in
  let h = H.create () in
  loop (ref[]) h len

(** [heap_adds len] generates a random sequence of [insert] of size
    [len], then [len/2] [decrease_key], then [len] [delete_min]. *)
let heap_adds (module H : HollowHeap.S with type key = int) len =
  let int () = Random.int (20*len) in
  let insert items h =
    let nw = H.insert h (int ()) (int ()) in
    items := nw::!items
  in
  let dkey items h =
    let l = List.length !items in
    let i = Random.int l in
    let ix = List.nth !items i in
    if H.live ix then
      let k = H.get_key ix in
      H.decrease_key h ix (k-1)
    else
      ()
  in
  let rec loop_add items h = function
    | 0 -> ()
    | n -> insert items h ; loop_add items h (n-1)
  in
  let rec loop_decrease_key items h = function
    | 0 -> ()
    | n -> dkey items h ; loop_decrease_key items h (n-1)
  in
  let rec loop_remove h = function
    | 0 -> ()
    | n -> H.delete_min h ; loop_remove h (n-1)
  in
  let h = H.create () in
  let items = ref [] in
  let () = loop_add items h len in
  let () = loop_decrease_key items h (len/2) in
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
    heap_ops_bench "HollowHeap (3-1-1)" (3,1,1) (module IntHollowHeap);
]
