(*** Sparse, variable-size arrays. A simple, standard implementation,
     using a bit-array as a mask. ***)


type 'a t = {
  mutable arr : 'a array;
  (** The actual array containing the data. Mutable so that it can be
      extended. *)
  mutable last_index: int;
  (** Position of the last populated index. [-1] if there are no such
      index. *)
  dummy: 'a;
  (** Holes (and reserved space) need to have values in them. Ideally
      it would be a null-pointer, but there is no canonical and safe
      way to do it in Ocaml, instead we keep a distinguished value. No
      populated index should contain the dummy element (up to physical
      equality). *)
}

(** [empty dummy capacity] creates an empty vector with initial
    capacity [capacity]. Holes are filled with [dummy]. *)
let empty dummy capacity = {
  arr = Array.make capacity dummy;
  last_index = -1;
  dummy = dummy;
}

exception NoElem

let find {arr;dummy;_} i =
  let () = if i >= Array.length arr then raise NoElem in
  let r = arr.(i) in
  if r != dummy then r
  else raise NoElem

let mem  {arr;dummy;_} i =
  (i < Array.length arr) && (arr.(i) != dummy)

(* Extend the capacity of [a] so that it has capacity at least [n]
   elements. *)
let extend a n =
  let l = Array.length a.arr in
  if n <= l then ()
  else
    let arr' = Array.make (2*n) a.dummy in
    let () = Array.blit a.arr 0 arr' 0 l in
    a.arr <- arr'

let add a i v =
  let () = extend a (i+1) in
  let () = a.arr.(i) <- v in
  a.last_index <- max (a.last_index) i

(* Search backwards in mask (from i) to find the last [true]
   bit. There are more efficient implementations of this function. *)
let rec previous_last_index a i =
  if i < 0 then
    -1
  else if a.arr.(i) != a.dummy then
    i
  else
    previous_last_index a (i-1)

let remove a i =
  if i > a.last_index then
    ()
  else
    let () = a.arr.(i) <- a.dummy in
    if i = a.last_index then a.last_index <- previous_last_index a i

let is_empty {last_index;_} = last_index = -1

let fold1 f {arr;last_index;dummy;_} =
  if last_index = -1 then
    None
  else
    let rec first_index i =
      if arr.(i) != dummy then i
      else first_index (i+1)
    in
    let i = first_index 0 in
    let res = ref arr.(i) in
    for j = i+1 to last_index do
      if arr.(j) != dummy then
        res := f !res arr.(j)
    done;
    Some !res
