open Types

let ( #% ) = Printf.sprintf
let implode (cl : char list) : string = String.concat "" (List.map (String.make 1) cl)

let explode (s : string) : char list =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

let constant_output = function
  | Int i -> string_of_int i
  | String s -> explode s |> List.filter (fun c -> c <> '\"') |> implode
  | Name n -> n
  | Bool b -> "<%s>" #% (string_of_bool b)
  | Error -> "<error>"
  | Unit -> "<unit>"
;;

let constant = function
  | Int i -> string_of_int i
  | String s -> s
  | Name n -> n
  | Bool b -> "<%s>" #% (string_of_bool b)
  | Error -> "<error>"
  | Unit -> "<unit>"
;;

let rec command = function
  | Push c -> "Push %s" #% (constant c)
  | Swap -> "Swap"
  | Pop -> "Pop"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | Neg -> "Neg"
  | Quit -> "Quit"
  | Cat -> "Cat"
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"
  | Eq -> "Eq"
  | Lte -> "Lte"
  | Lt -> "Lt"
  | Gte -> "Gte"
  | Gt -> "Gt"
  | Bnd -> "Bnd"
  | Env cmds ->
    let helper cmds =
      List.fold_left (fun acc el -> "%s%s\n" #% acc (command el)) "" cmds
    in
    "Begin\n%sEnd\n" #% (helper cmds)
;;

let commands (cmds : command list) = List.map command cmds
