open Sexplib.Std
open Sugar

type t = Nt.t option [@@deriving sexp]

let is_basic_tp = function
  | Some t -> Nt.is_basic_tp t
  | None -> _failatwith __FILE__ __LINE__ "?"

let is_dt = function
  | Some t -> Nt.is_dt t
  | None -> _failatwith __FILE__ __LINE__ "?"

let eq x y =
  match (x, y) with
  | None, None -> true
  | Some x, Some y -> Nt.eq x y
  | _, _ -> false

let destruct_normal_tp = function
  | Some t ->
      let t1, t2 = Nt.destruct_normal_tp t in
      (List.map (fun x -> Some x) t1, Some t2)
  | None -> _failatwith __FILE__ __LINE__ "?"

let construct_normal_tp (t1, t2) =
  let t1 =
    List.map
      (fun x ->
        match x with None -> _failatwith __FILE__ __LINE__ "?" | Some x -> x)
      t1
  in
  match t2 with
  | Some t2 -> Some (Nt.construct_normal_tp (t1, t2))
  | _ -> _failatwith __FILE__ __LINE__ "?"

let to_smtty = function
  | Some t -> Nt.to_smtty t
  | None -> _failatwith __FILE__ __LINE__ "?"

let default_ty = None
let unit_ty = Some Nt.Ty_unit
let int_ty = Some Nt.Ty_int
let bool_ty = Some Nt.Ty_bool

let mk_arr ?(lb = None) t1 t2 =
  match (t1, t2) with
  | Some t1, Some t2 -> Some (Nt.Ty_arrow (lb, t1, t2))
  | _, _ -> None

let mk_tuple ts =
  let* ts = Sugar.opt_list_to_list_opt ts in
  Some (Nt.mk_tuple ts)

let get_argty = function
  | Some (Nt.Ty_arrow (_, t1, _)) -> Some t1
  | _ -> _failatwith __FILE__ __LINE__ "?"

let get_retty = function
  | Some (Nt.Ty_arrow (_, _, t2)) -> Some t2
  | _ -> _failatwith __FILE__ __LINE__ "?"

let layout = function None -> "None" | Some t -> Nt.layout t

let _type_unify file line t1 t2 =
  match (t1, t2) with
  | Some t1, Some t2 -> Some (Nt._type_unify file line t1 t2)
  | _, _ -> _failatwith __FILE__ __LINE__ "?"
