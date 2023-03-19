open Sexplib.Std
open Sugar

type t =
  | Ty_any (* parsing only *)
  | Ty_unknown (* parsing only *)
  | Ty_var of string (* parsing only *)
  | Ty_unit
  | Ty_int
  | Ty_bool
  | Ty_list of t
  | Ty_arrow of Leff.t * t * t
  | Ty_tuple of t list
  | Ty_constructor of (string * t list)
[@@deriving sexp]

let is_basic_tp = function Ty_unit | Ty_int | Ty_bool -> true | _ -> false
let is_dt = function Ty_list _ | Ty_constructor _ -> true | _ -> false

let eq x y =
  let rec aux (x, y) =
    match (x, y) with
    | Ty_any, Ty_any -> true
    | Ty_unknown, Ty_unknown -> true
    | Ty_var x, Ty_var y -> String.equal x y
    | Ty_unit, Ty_unit -> true
    | Ty_int, Ty_int -> true
    | Ty_bool, Ty_bool -> true
    | Ty_list x, Ty_list y -> aux (x, y)
    | Ty_arrow (lx, x, x'), Ty_arrow (ly, y, y') ->
        Leff.eq lx ly && aux (x, y) && aux (x', y')
    | Ty_tuple xs, Ty_tuple ys ->
        if List.length xs == List.length ys then
          List.for_all aux @@ List.combine xs ys
        else false
    | Ty_constructor (id1, args1), Ty_constructor (id2, args2) ->
        String.equal id1 id2
        && List.length args1 == List.length args2
        && List.for_all2 (fun a b -> aux (a, b)) args1 args2
    | _ -> false
  in
  aux (x, y)

let destruct_normal_tp tp =
  let rec aux = function
    | Ty_arrow (None, t1, t2) ->
        let argsty, bodyty = aux t2 in
        (t1 :: argsty, bodyty)
    | Ty_arrow (_, _, _) -> _failatwith __FILE__ __LINE__ "?"
    | ty -> ([], ty)
  in
  aux tp

let rec construct_normal_tp = function
  | [], retty -> retty
  | h :: t, retty -> Ty_arrow (None, h, construct_normal_tp (t, retty))

let to_smtty t =
  let aux = function
    | Ty_bool -> Smtty.Bool
    | Ty_int -> Smtty.Int
    | Ty_list _ | Ty_constructor _ -> Smtty.Dt
    | _ ->
        let () =
          Printf.printf "t: %s\n" @@ Sexplib.Sexp.to_string @@ sexp_of_t t
        in
        _failatwith __FILE__ __LINE__ "not a basic type"
  in
  aux t

let default_ty = Ty_unknown
let unit_ty = Ty_unit
let int_ty = Ty_int
let bool_ty = Ty_bool
let mk_arr ?(lb = None) t1 t2 = Ty_arrow (lb, t1, t2)

let get_argty = function
  | Ty_arrow (_, t1, _) -> t1
  | _ -> _failatwith __FILE__ __LINE__ "?"

let get_retty = function
  | Ty_arrow (_, _, t2) -> t2
  | _ -> _failatwith __FILE__ __LINE__ "?"

(* type unification *)
open Zzdatatype.Datatype

let subst t (id, ty) =
  let rec aux t =
    match t with
    | Ty_unknown | Ty_any | Ty_unit | Ty_int | Ty_bool -> t
    | Ty_var x -> if String.equal x id then ty else t
    | Ty_list x -> Ty_list (aux x)
    | Ty_arrow (lb, t1, t2) -> Ty_arrow (lb, aux t1, aux t2)
    | Ty_tuple xs -> Ty_tuple (List.map aux xs)
    | Ty_constructor (id, args) -> Ty_constructor (id, List.map aux args)
  in
  aux t

let subst_m m t = StrMap.fold (fun id ty t -> subst t (id, ty)) m t
let layout t = Sexplib.Sexp.to_string @@ sexp_of_t t

let _type_unify_ file line m t1 t2 =
  (* let () = Printf.printf "unify %s --> %s\n" (layout t1) (layout t2) in *)
  let rec unify m (t1, t2) =
    let t1 = subst_m m t1 in
    (* let () = Printf.printf "one %s --> %s\n" (layout t1) (layout t2) in *)
    match (t1, t2) with
    | Ty_any, _ -> (m, t2)
    | Ty_unknown, _ -> (m, t2)
    | Ty_var n, t2 -> (
        match StrMap.find_opt m n with
        | Some _ -> _failatwith __FILE__ __LINE__ ""
        | None ->
            let m = StrMap.add n t2 m in
            (m, t2))
    | Ty_list t1, Ty_list t2 ->
        let m, t = unify m (t1, t2) in
        (m, Ty_list t)
    | Ty_constructor (id1, ts1), Ty_constructor (id2, ts2) ->
        let id = _check_equality file line String.equal id1 id2 in
        (* let () = *)
        (*   Printf.printf "(%s) v.s. (%s)\n" *)
        (*     (List.split_by_comma layout ts1) *)
        (*     (List.split_by_comma layout ts2) *)
        (* in *)
        let m, ts =
          List.fold_left
            (fun (m, ts) (t1, t2) ->
              let m, t = unify m (t1, t2) in
              (m, ts @ [ t ]))
            (m, []) (List.combine ts1 ts2)
        in
        (m, Ty_constructor (id, ts))
    | Ty_arrow (l1, t11, t12), Ty_arrow (l2, t21, t22) when Leff.eq l1 l2 ->
        let m, t1 = unify m (t11, t21) in
        let m, t2 = unify m (t12, t22) in
        (m, Ty_arrow (l1, t1, t2))
    | Ty_tuple ts1, Ty_tuple ts2 when List.length ts1 == List.length ts2 ->
        let m, ts =
          List.fold_left
            (fun (m, ts) (t1, t2) ->
              let m, t = unify m (t1, t2) in
              (m, ts @ [ t ]))
            (m, []) (List.combine ts1 ts2)
        in
        (m, Ty_tuple ts)
    | _, Ty_any -> (m, t1)
    | _, Ty_unknown -> (m, t1)
    | _, Ty_var _ ->
        (* (m, t1) *)
        _failatwith file line "argment should not contain type var"
    | _, _ -> (
        ( m,
          try _check_equality file line eq t1 t2
          with e ->
            Printf.printf "%s != %s\n" (layout t1) (layout t2);
            raise e ))
  in
  try unify m (t1, t2)
  with e ->
    Printf.printf "Type unify error: %s ==> %s\n" (layout t1) (layout t2);
    raise e

let _type_unify file line t1 t2 =
  snd @@ _type_unify_ file line StrMap.empty t1 t2
