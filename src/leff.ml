open Sexplib.Std

type _t = EffArr | HdArr [@@deriving sexp]
type t = _t option [@@deriving sexp]

let eq x y =
  match (x, y) with
  | None, None | Some EffArr, Some EffArr | Some HdArr, Some HdArr -> true
  | _, _ -> false
