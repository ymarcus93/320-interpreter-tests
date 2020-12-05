open QCheck

module type BinaryCmdTestSpec = sig
  type t
  type t_expected

  val cmd : Types.command
  val precond_pos : (t -> t -> bool) option
  val expected_result_pos : t -> t -> t_expected
  val is_correct_const : Types.constant -> bool
  val extract_from_const : Types.constant -> t
  val wrap_into_const : t_expected -> Types.constant
  val gen : Types.constant arbitrary
end

module type UnaryCmdTestSpec = sig
  type t
  type t_expected

  val cmd : Types.command
  val expected_result_pos : t -> t_expected
  val is_correct_const : Types.constant -> bool
  val extract_from_const : Types.constant -> t
  val wrap_into_const : t_expected -> Types.constant
  val gen : Types.constant arbitrary
end

module BinaryIntArithmeticOpSpec = struct
  type t = int
  type t_expected = int

  let extract_from_const c =
    match c with
    | Types.Int x -> x
    | _ -> failwith "expected int"
  ;;

  let is_correct_const c = Types.is_int c
  let wrap_into_const t = Types.Int t
  let gen = Arbitraries.constant_int_arb
end

module CommonAddSubMulSpec = struct
  include BinaryIntArithmeticOpSpec

  let precond_pos = None
end

module AddSpec : BinaryCmdTestSpec = struct
  include CommonAddSubMulSpec

  let cmd = Types.Add
  let expected_result_pos = ( + )
end

module SubSpec : BinaryCmdTestSpec = struct
  include CommonAddSubMulSpec

  let cmd = Types.Sub
  let expected_result_pos = ( - )
end

module MulSpec : BinaryCmdTestSpec = struct
  include CommonAddSubMulSpec

  let cmd = Types.Mul
  let expected_result_pos = ( * )
end

module NegSpec : UnaryCmdTestSpec = struct
  type t = int
  type t_expected = int

  let cmd = Types.Neg
  let expected_result_pos v = -1 * v

  let extract_from_const c =
    match c with
    | Types.Int x -> x
    | _ -> failwith "expected int"
  ;;

  let is_correct_const c = Types.is_int c
  let wrap_into_const t = Types.Int t
  let gen = Arbitraries.constant_int_arb
end

module CommonDivRemSpec = struct
  include BinaryIntArithmeticOpSpec

  let precond_pos = Some (fun _ dem -> dem != 0)
end

module DivSpec : BinaryCmdTestSpec = struct
  include CommonDivRemSpec

  let cmd = Types.Div
  let expected_result_pos = ( / )
end

module RemSpec : BinaryCmdTestSpec = struct
  include CommonDivRemSpec

  let cmd = Types.Rem
  let expected_result_pos = ( mod )
end

module CatSpec : BinaryCmdTestSpec = struct
  type t = string
  type t_expected = string

  let cmd = Types.Cat
  let expected_result_pos fst snd = fst ^ snd
  let precond_pos = None

  let extract_from_const c =
    match c with
    | Types.String x -> x
    | _ -> failwith "expected string"
  ;;

  let is_correct_const c = Types.is_string c
  let wrap_into_const t = Types.String t
  let gen = Arbitraries.constant_string_arb
end

module BinaryBoolCompareOpSpec = struct
  type t = bool
  type t_expected = bool

  let precond_pos = None

  let extract_from_const c =
    match c with
    | Types.Bool x -> x
    | _ -> failwith "expected bool"
  ;;

  let is_correct_const c = Types.is_bool c
  let wrap_into_const t = Types.Bool t
  let gen = Arbitraries.constant_bool_arb
end

module AndSpec : BinaryCmdTestSpec = struct
  include BinaryBoolCompareOpSpec

  let cmd = Types.And
  let expected_result_pos fst snd = fst && snd
end

module OrSpec : BinaryCmdTestSpec = struct
  include BinaryBoolCompareOpSpec

  let cmd = Types.Or
  let expected_result_pos fst snd = fst || snd
end

module NotSpec : UnaryCmdTestSpec = struct
  type t = bool
  type t_expected = bool

  let cmd = Types.Not
  let expected_result_pos v = not v

  let extract_from_const c =
    match c with
    | Types.Bool x -> x
    | _ -> failwith "expected bool"
  ;;

  let is_correct_const c = Types.is_bool c
  let wrap_into_const t = Types.Bool t
  let gen = Arbitraries.constant_bool_arb
end

module EqSpec : BinaryCmdTestSpec = struct
  type t = int
  type t_expected = bool

  let precond_pos = None
  let cmd = Types.Eq
  let expected_result_pos fst snd = fst = snd

  let extract_from_const c =
    match c with
    | Types.Int x -> x
    | _ -> failwith "expected int"
  ;;

  let is_correct_const c = Types.is_int c
  let wrap_into_const t = Types.Bool t
  let gen = Arbitraries.constant_int_arb
end

module BinaryIntCompareOpSpec = struct
  type t = int
  type t_expected = bool

  let precond_pos = None

  let extract_from_const c =
    match c with
    | Types.Int x -> x
    | _ -> failwith "expected int"
  ;;

  let is_correct_const c = Types.is_int c
  let wrap_into_const t = Types.Bool t
  let gen = Arbitraries.constant_int_arb
end

module LteSpec : BinaryCmdTestSpec = struct
  include BinaryIntCompareOpSpec

  let cmd = Types.Lte
  let expected_result_pos fst snd = fst <= snd
end

module LtSpec : BinaryCmdTestSpec = struct
  include BinaryIntCompareOpSpec

  let cmd = Types.Lt
  let expected_result_pos fst snd = fst < snd
end

module GteSpec : BinaryCmdTestSpec = struct
  include BinaryIntCompareOpSpec

  let cmd = Types.Gte
  let expected_result_pos fst snd = fst >= snd
end

module GtSpec : BinaryCmdTestSpec = struct
  include BinaryIntCompareOpSpec

  let cmd = Types.Gt
  let expected_result_pos fst snd = fst > snd
end
