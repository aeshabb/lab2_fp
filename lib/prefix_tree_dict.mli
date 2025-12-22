(** Сигнатура для типа ключа *)
module type KEY = sig
  type t
  type elem

  val to_list : t -> elem list
  val of_list : elem list -> t
  val compare_elem : elem -> elem -> int
end

(** Функтор для создания prefix tree словаря *)
module Make (Key : KEY) : sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val singleton : Key.t -> 'a -> 'a t
  val insert : Key.t -> 'a -> 'a t -> 'a t
  val find : Key.t -> 'a t -> 'a
  val find_opt : Key.t -> 'a t -> 'a option
  val mem : Key.t -> 'a t -> bool
  val remove : Key.t -> 'a t -> 'a t
  val to_list : 'a t -> (Key.t * 'a) list
  val of_list : (Key.t * 'a) list -> 'a t
  val size : 'a t -> int
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (Key.t -> 'a -> 'b) -> 'a t -> 'b t
  val filter : (Key.t -> 'a -> bool) -> 'a t -> 'a t
  val filter_values : ('a -> bool) -> 'a t -> 'a t
  val fold_left : ('b -> Key.t -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : (Key.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (Key.t -> 'a -> unit) -> 'a t -> unit
  val merge_with : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val merge : 'a t -> 'a t -> 'a t
  val concat : 'a t list -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val keys : 'a t -> Key.t list
  val values : 'a t -> 'a list
  val find_prefix : Key.t -> 'a t -> (Key.t * 'a) list
  val has_prefix : Key.t -> 'a t -> bool
end

(** Модуль для строковых ключей *)
module StringKey : KEY with type t = string and type elem = char

(** Стандартная инстанциация для строковых ключей *)
type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val singleton : string -> 'a -> 'a t
val insert : string -> 'a -> 'a t -> 'a t
val find : string -> 'a t -> 'a
val find_opt : string -> 'a t -> 'a option
val mem : string -> 'a t -> bool
val remove : string -> 'a t -> 'a t
val to_list : 'a t -> (string * 'a) list
val of_list : (string * 'a) list -> 'a t
val size : 'a t -> int
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (string -> 'a -> 'b) -> 'a t -> 'b t
val filter : (string -> 'a -> bool) -> 'a t -> 'a t
val filter_values : ('a -> bool) -> 'a t -> 'a t
val fold_left : ('b -> string -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : (string -> 'a -> unit) -> 'a t -> unit
val merge_with : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
val merge : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val keys : 'a t -> string list
val values : 'a t -> 'a list
val find_prefix : string -> 'a t -> (string * 'a) list
val has_prefix : string -> 'a t -> bool
