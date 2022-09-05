module Smtty = struct
  type t = Bool | Int | Dt [@@deriving sexp]

  let smtty_eq = function
    | Bool, Bool | Int, Int -> true
    | Dt, Dt -> true
    | _ -> false

  let eq a b = smtty_eq (a, b)
  let layout = function Bool -> "B" | Int -> "I" | Dt -> "D"

  let pretty_typed_layout str = function
    | Bool -> Printf.sprintf "(%s:𝓑 )" str
    | Dt -> Printf.sprintf "(%s:𝓓 )" str
    | Int -> str

  let is_dt = function Dt -> true | _ -> false
end

module T = struct
  open Sexplib.Std
  open Sugar

  type id = string [@@deriving sexp]

  type t =
    | Ty_unknown
    | Ty_var of string
    | Ty_unit
    | Ty_int
    | Ty_bool
    | Ty_list of t
    | Ty_tree of t
    | Ty_arrow of t * t
    | Ty_tuple of t list
    | Ty_constructor of (id * t list)
  [@@deriving sexp]

  let is_basic_tp = function Ty_unit | Ty_int | Ty_bool -> true | _ -> false

  let is_dt = function
    | Ty_list _ | Ty_tree _ | Ty_constructor _ -> true
    | _ -> false

  let eq x y =
    let rec aux (x, y) =
      match (x, y) with
      | Ty_unknown, Ty_unknown -> true
      | Ty_var x, Ty_var y -> String.equal x y
      | Ty_unit, Ty_unit -> true
      | Ty_int, Ty_int -> true
      | Ty_bool, Ty_bool -> true
      | Ty_list x, Ty_list y -> aux (x, y)
      | Ty_tree x, Ty_tree y -> aux (x, y)
      | Ty_arrow (x, x'), Ty_arrow (y, y') -> aux (x, y) && aux (x', y')
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

  let destruct_arrow_tp tp =
    let rec aux = function
      | Ty_arrow (t1, t2) ->
          let argsty, bodyty = aux t2 in
          (t1 :: argsty, bodyty)
      | ty -> ([], ty)
    in
    aux tp

  let rec construct_arrow_tp = function
    | [], retty -> retty
    | h :: t, retty -> Ty_arrow (h, construct_arrow_tp (t, retty))

  let to_smtty t =
    let aux = function
      | Ty_bool -> Smtty.Bool
      | Ty_int -> Smtty.Int
      | Ty_list _ | Ty_tree _ | Ty_constructor _ -> Smtty.Dt
      | _ -> _failatwith __FILE__ __LINE__ "not a basic type"
    in
    aux t
end

module NotatedT = struct
  open Sexplib.Std

  type t = string option * T.t [@@deriving sexp]

  let eq (a1, b1) (a2, b2) =
    match (a1, a2) with
    | None, None -> T.eq b1 b2
    | Some a1, Some a2 when String.equal a1 a2 -> T.eq b1 b2
    | _ -> false
end

module NT = T

module Ntyped = struct
  type 'a typed = { x : 'a; ty : NT.t } [@@deriving sexp]

  let map (f : 'a -> 'b) { x; ty } = { x = f x; ty }
  let eq a b = String.equal a.x b.x && NT.eq a.ty b.ty
end

module SMTtyped = struct
  type 'a typed = { x : 'a; ty : Smtty.t } [@@deriving sexp]

  let map (f : 'a -> 'b) { x; ty } = { x = f x; ty }
  let eq a b = String.equal a.x b.x && Smtty.eq a.ty b.ty
end

module NNtyped = struct
  type 'a typed = { x : 'a; ty : NotatedT.t } [@@deriving sexp]

  let map (f : 'a -> 'b) { x; ty } = { x = f x; ty }
  let eq a b = String.equal a.x b.x && NotatedT.eq a.ty b.ty
end
