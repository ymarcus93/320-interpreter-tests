open QCheck
open Types
open Shrinkers
open Generators.Constants
open Generators.Commands

let constant_int_arb = make ~shrink:constant_shrinker ~print:show_constant constant_int

let constant_string_arb =
  make ~shrink:constant_shrinker ~print:show_constant constant_string
;;

let constant_name_arb = make ~shrink:constant_shrinker ~print:show_constant constant_name
let constant_bool_arb = make ~shrink:constant_shrinker ~print:show_constant constant_bool

let constant_error_arb =
  make ~shrink:constant_shrinker ~print:show_constant constant_error
;;

let constant_unit_arb = make ~shrink:constant_shrinker ~print:show_constant constant_unit

let boundable_literal_constant_arb =
  make ~shrink:constant_shrinker ~print:show_constant boundable_literal_constant_gen
;;

let constant_arb = make ~shrink:constant_shrinker ~print:show_constant constant_gen

let part1_command_arb =
  make ~shrink:command_shrinker ~print:show_command part1_pure_command_gen
;;

let part2_command_arb =
  make ~shrink:command_shrinker ~print:show_command part2_pure_command_gen
;;

let part1_2_command_arb =
  make ~shrink:command_shrinker ~print:show_command part1_2_pure_command_gen
;;
