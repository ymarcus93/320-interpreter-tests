type constant =
  | Int of int
  | Bool of bool
  | Error
  | String of string
  | Name of string
  | Unit
[@@deriving show { with_path = false }]

type command =
  | Push of constant
  | Swap
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Quit
  | Cat
  | And
  | Or
  | Not
  | Eq
  | Lte
  | Lt
  | Gte
  | Gt
  | Bnd
  | Env of command list
[@@deriving show { with_path = false }]

let is_int = function
  | Int _ -> true
  | _ -> false
;;

let is_string = function
  | String _ -> true
  | _ -> false
;;

let is_bool = function
  | Bool _ -> true
  | _ -> false
;;

let is_name = function
  | Name _ -> true
  | _ -> false
;;

let is_error = function
  | Error -> true
  | _ -> false
;;
