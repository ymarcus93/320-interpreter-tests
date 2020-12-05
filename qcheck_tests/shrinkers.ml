open Types

let ( #% ) = Printf.sprintf
let implode (cl : char list) : string = String.concat "" (List.map (String.make 1) cl)

let explode (s : string) : char list =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []
;;

module Parsers = struct
  let is_alpha = function
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  (* 1. defining a parser *)
  type 'a parser = Parser of (string -> 'a option * string)

  (* 2. defining parse function *)
  let parse (p : 'a parser) (s : string) : 'a option * string =
    match p with
    | Parser f -> f s
  ;;

  (* 3. defining item parser that simply consumes the first character
   from the input*)
  let item_p : char parser =
    Parser
      (fun s ->
        match explode s with
        | [] -> (None, "") (*check whether the string is empty *)
        | x :: xs -> (Some x, implode xs))
  ;;

  (* 4. defining empty parser that simply returns back empty list
   always*)
  let empty_p : 'a parser = Parser (fun _ -> (None, ""))

  (* 5. defining return parser*)
  let return_p (x : 'a) : 'a parser = Parser (fun s -> (Some x, s))

  (* 6. defining choice parser*)
  let ( <|> ) (p : 'a parser) (q : 'a parser) : 'a parser =
    Parser
      (fun s ->
        match parse p s with
        | None, _ ->
          parse q s (*return the result of the second parser if the first parser fails *)
        | d -> d
        (* return the result of the first parser *))
  ;;

  (* 7. defining let* function *)
  let ( let* ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
    Parser
      (fun s ->
        match parse p s with
        | None, _ -> (None, s)
        | Some v, rs -> parse (f v) rs)
  ;;

  let sat_p (p : char -> bool) : char parser =
    let* x = item_p in
    if p x then return_p x else empty_p
  ;;

  (* 8. defining digit parser*)
  let digit_p : char parser = sat_p is_digit

  (* 9. defining letter parser*)
  let letter_p : char parser = sat_p is_alpha

  (* 10. defining character parser*)
  let char_p (x : char) : char parser = sat_p (fun y -> y = x)

  (* 11. defining a recursive parser*)
  let rec many_p (p : 'a parser) : 'a list parser =
    (let* a = p in
     let* ls = many_p p in
     return_p (a :: ls))
    <|> return_p []
  ;;

  (* 12. defining a recursive parser which fails on empty*)
  let many1_p (p : 'a parser) : 'a list parser =
    let* a = p in
    let* ls = many_p p in
    return_p (a :: ls)
  ;;

  let name_p : constant parser =
    let underscore_p = char_p '_' in
    let many_underscore_p =
      let* xs = many_p underscore_p in
      return_p (implode xs)
    in
    let many_trail_name_p =
      let trail_name_p = letter_p <|> digit_p <|> underscore_p in
      let* xs = many_p trail_name_p in
      return_p (implode xs)
    in
    let* begPart = many_underscore_p in
    let* middlePart = letter_p in
    let* endPart = many_trail_name_p in
    let finalized = begPart ^ String.make 1 middlePart ^ endPart in
    return_p (Name finalized)
  ;;

  let string_p : constant parser =
    let many1_simple_ascii_p =
      let simple_ascii_g =
        let is_simple_ascii =
          let is_ascii (c : char) = Char.code c < 128 in
          function
          | '\\' | '\"' -> false
          | _ as c -> is_ascii c
        in
        sat_p is_simple_ascii
      in
      let* xs = many1_p simple_ascii_g in
      return_p (implode xs)
    in
    let* _ = char_p '\"' in
    let* str = many1_simple_ascii_p in
    let* _ = char_p '\"' in
    return_p (String str)
  ;;
end

let string_shrinker s =
  let open QCheck in
  let open Parsers in
  Shrink.filter
    (fun s ->
      match parse string_p s with
      | Some _, _ -> true
      | _ -> false)
    Shrink.string
    s
;;

let name_shrinker n =
  let open Parsers in
  let open QCheck in
  Shrink.filter
    (fun n ->
      match parse name_p n with
      | Some _, _ -> true
      | _ -> false)
    Shrink.string
    n
;;

let constant_shrinker =
  let open QCheck in
  function
  | Int i -> Iter.map (fun i' -> Int i') (Shrink.int i)
  | String s -> Iter.map (fun s' -> String s') (string_shrinker s)
  | Name n -> Iter.map (fun n' -> Name n') (name_shrinker n)
  | Bool _ | Error | Unit -> Iter.empty
;;

let rec command_shrinker =
  let open QCheck in
  function
  | Push c -> Iter.map (fun c' -> Push c') (constant_shrinker c)
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
  | Bnd -> Iter.empty
  | Env cmds ->
    let open Iter in
    let shrink_to_one cmds =
      Iter.filter (fun l -> List.length l = 1) (Shrink.list ~shrink:command_shrinker cmds)
    in
    Iter.return (fun cmds' -> Env cmds') <*> shrink_to_one cmds
;;
