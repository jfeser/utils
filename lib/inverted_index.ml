open! Core

(** An inverted index maps sets to values so that queries can be
    performed that select all super- or sub-sets of the query set. *)
module InvertedIndex (KeyElem : sig
  type t

  val t_of_sexp : Sexplib.Sexp.t -> t

  val sexp_of_t : t -> Sexplib.Sexp.t

  val compare : t -> t -> int
end) (Value : sig
  type t
end) =
struct
  module KMap = Map.Make (KeyElem)
  module KSet = Set.Make (KeyElem)

  module KVPair = struct
    type t = KSet.t * Value.t

    let compare (x : t) (y : t) =
      let (x', _), (y', _) = (x, y) in
      KSet.compare x' y'
  end

  module IntPairSet = Set.Make (struct
    type t = int * int [@@deriving sexp, compare]
  end)

  type perf_counters = {
    mutable total_lookups : int;
    mutable total_full_lookups : int;
    mutable total_set_ops : int;
    mutable total_results_examined : int;
  }

  type t = {
    mutable index : IntPairSet.t KMap.t;
    store : KVPair.t Int.Table.t;
    fresh : Fresh.t;
    perf : perf_counters;
  }

  let create () : t =
    {
      index = KMap.empty;
      store = Int.Table.create ();
      fresh = Fresh.create ();
      perf =
        {
          total_lookups = 0;
          total_full_lookups = 0;
          total_set_ops = 0;
          total_results_examined = 0;
        };
    }

  let add (i : t) (k : KSet.t) (v : Value.t) : unit =
    let kv_key = Fresh.int i.fresh in
    let kv_key_pair = (kv_key, Set.length k) in
    (* Generate a new index where the list mapped to each element in k
       contains the reference to the (k, v) pair *)
    let index' =
      List.fold_left (Set.to_list k) ~init:i.index ~f:(fun i e ->
          match KMap.find i e with
          | Some s -> KMap.set i ~key:e ~data:(IntPairSet.add s kv_key_pair)
          | None -> KMap.set i ~key:e ~data:(IntPairSet.singleton kv_key_pair))
    in
    (* Update the index. *)
    i.index <- index';
    (* Update the key-value store. *)
    Hashtbl.add_exn i.store ~key:kv_key ~data:(k, v)

  (* Merge a list of result lists. *)
  let merge_results = IntPairSet.union_list

  let store_lookup store id =
    match Hashtbl.find store id with
    | Some x -> x
    | None -> failwith "Index contains reference to nonexistent item."

  let exists_subset_or_superset (i : t) (s : KSet.t) (subset_v : Value.t)
      (superset_v : Value.t) : Value.t option =
    let len = Set.length s in
    (* For each value in the query set, use the index to get
       references to the sets that contain that value. *)
    let result_ref_lists =
      List.filter_map (Set.to_list s) ~f:(fun elem ->
          match KMap.find i.index elem with
          | Some refs as r ->
              if Set.length refs = Hashtbl.length i.store then None else r
          | None -> None)
    in
    (* Merge the result lists. *)
    let result_refs = merge_results result_ref_lists in
    (* Update performance counters *)
    i.perf.total_lookups <- i.perf.total_lookups + 1;
    if Set.length result_refs = Hashtbl.length i.store then
      i.perf.total_full_lookups <- i.perf.total_full_lookups + 1;
    i.perf.total_results_examined <-
      i.perf.total_results_examined + Set.length result_refs;
    Set.find_map result_refs ~f:(fun (id, len') ->
        let s', v' = store_lookup i.store id in
        if len' < len then
          if Poly.(v' = subset_v) && Set.is_subset s' ~of_:s then (
            i.perf.total_set_ops <- i.perf.total_set_ops + 1;
            Some subset_v )
          else None
        else if len' = len then
          if Poly.(v' = subset_v) && Set.is_subset s' ~of_:s then (
            i.perf.total_set_ops <- i.perf.total_set_ops + 1;
            Some subset_v )
          else if Poly.(v' = superset_v) && Set.is_subset s ~of_:s' then (
            i.perf.total_set_ops <- i.perf.total_set_ops + 1;
            Some superset_v )
          else None
        else if Poly.(v' = superset_v) && Set.is_subset s ~of_:s' then (
          i.perf.total_set_ops <- i.perf.total_set_ops + 1;
          Some superset_v )
        else None)

  (* Return a summary of the performance counters suitable for writing to a log. *)
  let log_summary (i : t) : string =
    sprintf "Total set operations: %d\n" i.perf.total_set_ops
    ^ sprintf "Full lookups/Total lookups: %d/%d\n" i.perf.total_full_lookups
        i.perf.total_lookups
    ^ sprintf "Average results per lookup: %f\n"
        ( Float.of_int i.perf.total_results_examined
        /. Float.of_int i.perf.total_lookups )
    ^ sprintf "Distinct set elements: %d\n" (Map.length i.index)
    ^ sprintf "Total sets stored: %d\n" (Hashtbl.length i.store)
end
