module SMTtyped = struct
  include Smtty

  type 'a typed = { x : 'a; ty : t } [@@deriving sexp]

  let map (f : 'a -> 'b) { x; ty } = { x = f x; ty }
  let typed_eq a b = String.equal a.x b.x && eq a.ty b.ty
end

module type T = sig
  include T.T

  type 'a typed = { x : 'a; ty : t } [@@deriving sexp]

  val mk_noty : 'a -> 'a typed
  val ( #: ) : 'a -> t -> 'a typed
  val xmap : ('a -> 'b) -> 'a typed -> 'b typed
  val layout_typed : ('a -> string) -> 'a typed -> string
  val layout_typed_l : ('a -> string) -> 'a typed list -> string
  val to_smttyped : 'a typed -> 'a SMTtyped.typed
end

module F (Ty : T.T) : T with type t = Ty.t = struct
  include Ty

  type 'a typed = { x : 'a; ty : Ty.t } [@@deriving sexp]

  let ( #: ) x ty = { x; ty }
  let mk_noty x = x #: Ty.default_ty
  let xmap f { x; ty } = { x = f x; ty }
  let layout_typed f { x; ty } = Printf.sprintf "%s:%s" (f x) (Ty.layout ty)

  let layout_typed_l f l =
    Zzdatatype.Datatype.List.split_by_comma (layout_typed f) l

  let to_smttyped { x; ty } = SMTtyped.{ x; ty = to_smtty ty }
end

module Nt = struct
  include Nt

  let layout = Frontend.layout
  let layout_l = Frontend.layout_l
  let layout_coretype = Frontend.layout_
  let of_string = Frontend.of_string
end

module Ntopt = struct
  include Ntopt

  let layout = function None -> "None" | Some t -> Frontend.layout t
  let layout_l l = Zzdatatype.Datatype.List.split_by_comma layout l
  let layout_coretype = Frontend.layout_
  let of_string = Some Frontend.of_string
end

module Ntyped = struct
  include F (Nt)
  include NT
end

module NOpttyped = struct
  include F (Ntopt)
  include Ntopt
end
