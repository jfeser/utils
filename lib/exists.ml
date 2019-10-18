(* Swiped from https://gist.github.com/jonsterling/a1c7932bc99651145d3a *)

(** an abstract signature for instantiations of the existential quantifier *)
module type S = sig
  type 'a phi
  (** the predicate *)

  type t
  (** the existential type *)

  val pack : 'a phi -> t
  (** the introduction rule *)

  type 'b cont = { run : 'a. 'a phi -> 'b }
  (** we use a record to wrap this polymorphic function, working
   * around a limitation of OCaml *)

  val spread : 'b cont -> t -> 'b
  (** eliminate an existential by providing a polymorphic function
   * and an existential package *)
end

module Make (Phi : sig
  type 'a t
end) : S with type 'a phi = 'a Phi.t = struct
  type 'a phi = 'a Phi.t

  type 'b cont = { run : 'a. 'a Phi.t -> 'b }

  (* We use OCaml's new support for generalized algebraic datatypes to define an
   * inductive type whose _constructor_ quantifies over types. *)
  type t = Pack : 'a Phi.t -> t

  let pack x = Pack x

  let spread k (Pack x) = k.run x
end
