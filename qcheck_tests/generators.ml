open Types
open QCheck

module Constants = struct
  let simple_ascii_char =
    let open Gen in
    let a = char_range '#' '[' in
    let b = char_range ']' '~' in
    oneof [ pure ' '; pure '!'; a; b ]
  ;;

  let string_gen =
    let open Gen in
    let* simple_ascii_str = string_size (int_range 1 5) ~gen:simple_ascii_char in
    return ("\"" ^ simple_ascii_str ^ "\"")
  ;;

  let name_gen =
    let open Gen in
    let underscore_gen = string_size (int_range 0 5) ~gen:(pure '_') in
    let letter_gen = oneof [ char_range 'a' 'z'; char_range 'A' 'Z' ] in
    let digit_gen = char_range '0' '9' in
    let* beginPart = underscore_gen in
    let* middlePart = letter_gen in
    let* endPart =
      oneof
        [ string_size (int_range 0 5) ~gen:letter_gen
        ; string_size (int_range 0 5) ~gen:digit_gen
        ; underscore_gen
        ]
    in
    return (beginPart ^ String.make 1 middlePart ^ endPart)
  ;;

  let constant_int = Gen.(map (fun i -> Int i) (small_int_corners ()).gen)
  let constant_bool = Gen.(map (fun b -> Bool b) bool)
  let constant_error = Gen.(return Error)
  let constant_string = Gen.(map (fun s -> String s) string_gen)
  let constant_name = Gen.(map (fun s -> Name s) name_gen)
  let constant_unit = Gen.(return Unit)

  let constant_gens =
    [ constant_int
    ; constant_bool
    ; constant_error
    ; constant_string
    ; constant_name
    ; constant_unit
    ]
  ;;

  let boundable_literal_constant_gen =
    Gen.oneof [ constant_int; constant_string; constant_bool; constant_unit ]
  ;;

  let constant_gen = Gen.oneof constant_gens
end

module Commands = struct
  (* Pure command generators *)
  let command_push = Gen.(map (fun const -> Push const) Constants.constant_gen)
  let command_swap = Gen.(return Swap)
  let command_pop = Gen.(return Pop)
  let command_add = Gen.(return Add)
  let command_sub = Gen.(return Sub)
  let command_mul = Gen.(return Mul)
  let command_div = Gen.(return Div)
  let command_rem = Gen.(return Rem)
  let command_neg = Gen.(return Neg)
  let command_quit = Gen.(return Quit)
  let command_cat = Gen.(return Cat)
  let command_and = Gen.(return And)
  let command_or = Gen.(return Or)
  let command_not = Gen.(return Not)
  let command_eq = Gen.(return Eq)
  let command_lte = Gen.(return Lte)
  let command_lt = Gen.(return Lt)
  let command_gte = Gen.(return Gte)
  let command_gt = Gen.(return Gt)
  let command_bnd = Gen.(return Bnd)

  let part1_pure_command_gens =
    [ command_push
    ; command_swap
    ; command_pop
    ; command_add
    ; command_sub
    ; command_mul
    ; command_div
    ; command_rem
    ; command_neg
    ; command_quit
    ]
  ;;

  let part2_pure_command_gens =
    [ command_cat
    ; command_and
    ; command_or
    ; command_not
    ; command_eq
    ; command_lte
    ; command_lt
    ; command_gte
    ; command_gt
    ; command_bnd
    ]
  ;;

  let part1_2_pure_command_gens =
    List.flatten [ part1_pure_command_gens; part2_pure_command_gens ]
  ;;

  let part1_pure_command_gen = Gen.oneof part1_pure_command_gens
  let part2_pure_command_gen = Gen.oneof part2_pure_command_gens
  let part1_2_pure_command_gen = Gen.oneof part1_2_pure_command_gens
end
