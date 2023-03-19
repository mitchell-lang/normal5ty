module type T = sig
  type t [@@deriving sexp]

  val is_basic_tp : t -> bool
  val is_dt : t -> bool
  val eq : t -> t -> bool
  val destruct_normal_tp : t -> t list * t
  val construct_normal_tp : t list * t -> t
  val to_smtty : t -> Smtty.t
  val default_ty : t
  val unit_ty : t
  val int_ty : t
  val bool_ty : t
  val mk_arr : ?lb:Leff.t -> t -> t -> t
  val get_argty : t -> t
  val get_retty : t -> t
  val layout : t -> string
  val _type_unify : string -> int -> t -> t -> t
end
