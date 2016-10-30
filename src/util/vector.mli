(** Maps with [int] as key implemented as arrays (therefore
    imperative, the name of the operations have been chosen to
    resemble those of {!Hashtbl} in the Ocaml standard library). The
    choice of implementation and operations is {i ad hoc} and tailored
    to the requirements of the implementation of hollow heaps. *)

(** An ['a t] is a map from [int] to ['a]. *)
type 'a t

(** [empty dummy capacity] creates an empty mapping. [capacity] is the
    size of the underlying array which is allocated initially. [dummy]
    is the element which fills the holes in the array (initially all
    the array). ATTENTION: physical equality to the [dummy] element is
    used in the implementation to test whether an index is a
    hole. This means that one should {i never} add the [dummy] element
    in the array (up to physical equality). *)
val empty : 'a -> int -> 'a t

exception NoElem

(** [find a i] returns [x] if [(i,x)] is a binding of [a].

    @raise NoElem when there is no binding for [i] in [a]. *)
val find : 'a t -> int -> 'a

val mem : 'a t -> int -> bool

(** [add a i x] adds [(i,x)] as a binding in [a]. If there was a
    previous binding for [i] then it is erased. *)
val add : 'a t -> int -> 'a -> unit

(** [remove a i] removes the binding for [i] in [a] if it exists
    (otherwise is a no-op).*)
val remove : 'a t -> int -> unit

(** [is_empty a] returns [true] if and only if there is at least a binding in [a] *)
val is_empty : 'a t -> bool

(** [fold1 f a] assumes that there is at least a binding in [a] and
    returns [Some v] where [v] is the "product" of (the values of) the
    bindings in [a], in unspecified order. If there are no bindings,
    returns [None]. *)
val fold1 : ('a->'a->'a) -> 'a t -> 'a option
