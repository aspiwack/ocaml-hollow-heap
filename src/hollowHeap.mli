
(** This module is an implementation of
    {{:http://cs.au.dk/~tdh/papers/Hollow-heaps.pdf} hollow heaps} a
    mutable heap structure with O(1) [decrease_key]. Only [delete_min]
    is not O(1). *)

module type S = sig

  (** [key] is the type of {e keys} (a.k.a {e priorities}): elements
      in the heap are ordered according to the order of their keys.  *)
  type key

  (** An ['a item] is essentially an ['a]. It also represents its
      "location" in the heap, so one can decrease its key with
      {!decrease_key}. It is a mistake to call [decrease_key] on an
      [item] which was previously deleted by {!delete_min}. *)
  type 'a item

  (** The type of heaps. Note that these are mutable heaps. *)
  type 'a t

  (** [get xi] extract the element contained in [xi]. *)
  val get : 'a item -> 'a

  (** [create ()] creates a new, empty, heap. *)
  val create : unit -> 'a t

  (** [insert h k x] inserts [x] with key [k] in [h], and returns a
      handle to [x] in [h]. See {!decrease_key}. *)
  val insert : 'a t -> key -> 'a -> 'a item

  (** [merge h1 h2] merges the elements of [h2] into [h1]. After
      [merge], [h2] is empty. *)
  val merge : 'a t -> 'a t -> unit

  (** [find_min h] returns (a handle to) the element with lowest key in [h]. If [h]
      is empty, returns [None]. *)
  val find_min : 'a t -> 'a item option

  (** [delete_min h] removes the element with the lowest key from
      [h]. If [h] is empty, [delete_min h] is a no-op. *)
  val delete_min : 'a t -> unit

  (** [decrease_key h xi k] changes the key of the element of [h]
      pointed to by [xi] to [k]. Precondition [k] must be no greater
      than the current key of [xi] (hence {e decrease}). *)
  val decrease_key : 'a t -> 'a item -> key -> unit

end

module Make (Ord:Map.OrderedType) : S with type key = Ord.t
