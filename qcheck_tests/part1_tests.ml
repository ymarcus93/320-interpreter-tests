open Common
open QCheck
open Assertions

(* Add,Sub,Mul *)
module AddTests = Common.MakeBinaryCmdTests (Test_specs.AddSpec)
module SubTests = Common.MakeBinaryCmdTests (Test_specs.SubSpec)
module MulTests = Common.MakeBinaryCmdTests (Test_specs.MulSpec)

module CommonDivRemTests = struct
  let rem_div_dem_0_no_bnd ~name cmd =
    QCheck.Test.make ~long_factor ~name (small_int_corners ()) (fun i ->
        let final_stack = interpret [ Push (Int 0); Push (Int i); cmd ] in
        assert_stack_is [ Error; Int i; Int 0 ] final_stack)
  ;;
end

module DivTests = struct
  include Common.MakeBinaryCmdTests (Test_specs.DivSpec)
  include CommonDivRemTests

  let div_op_dem_zero =
    let open Common.Tests in
    rem_div_dem_0_no_bnd ~name:("%s_%s" #% negative_prefix "Div_op_dem_zero") Div
  ;;

  let old_tests = tests
  let tests = old_tests @ [ div_op_dem_zero ]
end

module RemTests = struct
  include Common.MakeBinaryCmdTests (Test_specs.RemSpec)
  include CommonDivRemTests

  let rem_op_dem_zero =
    let open Common.Tests in
    rem_div_dem_0_no_bnd ~name:("%s_%s" #% negative_prefix "Rem_op_dem_zero") Rem
  ;;

  let old_tests = tests
  let tests = old_tests @ [ rem_op_dem_zero ]
end

module NegTests = Common.MakeUnaryCmdTests (Test_specs.NegSpec)

module SwapTests = struct
  let swap_no_bnd =
    let open Arbitraries in
    QCheck.Test.make
      ~long_factor
      ~name:"%s_Swap_op" #% Common.Tests.positive_prefix
      (pair constant_arb constant_arb)
      (fun (a, b) ->
        let final_stack = interpret [ Push b; Push a; Swap ] in
        assert_stack_is [ b; a ] final_stack)
  ;;

  let swap_op_one_value =
    let open Common.Tests in
    Negative.any_op_one_value
      ~name:("%s_%s" #% negative_prefix "Swap_op_one_value")
      Swap
  ;;

  let swap_op_empty_stack =
    let open Common.Tests in
    Negative.any_op_empty_stack
      ~name:("%s_%s" #% negative_prefix "Swap_op_empty_stack")
      Swap
  ;;

  let tests = [ swap_no_bnd; swap_op_one_value; swap_op_empty_stack ]
end

module PushTests = struct
  let push_no_bnd =
    let open Arbitraries in
    QCheck.Test.make
      ~long_factor
      ~name:"%s_Push_op" #% Common.Tests.positive_prefix
      constant_arb
      (fun a ->
        let final_stack = interpret [ Push a ] in
        assert_stack_is [ a ] final_stack)
  ;;

  let tests = [ push_no_bnd ]
end

module PopTests = struct
  let pop_no_bnd =
    let open Arbitraries in
    QCheck.Test.make
      ~long_factor
      ~name:"%s_Pop_op" #% Common.Tests.positive_prefix
      constant_arb
      (fun a ->
        let final_stack = interpret [ Push a; Pop ] in
        assert_stack_is [] final_stack)
  ;;

  let pop_on_empty_stack =
    let open Common.Tests in
    Negative.any_op_empty_stack ~name:"%s_Pop_op_empty_stack" #% negative_prefix Pop
  ;;

  let tests = [ pop_no_bnd; pop_on_empty_stack ]
end

module QuitTests = struct
  let quit_no_bnd =
    Common.Tests.QuitTests.quit ~name_suffix:"no_bnd" Arbitraries.part1_command_arb
  ;;

  let tests = [ quit_no_bnd ]
end

let suite =
  List.map
    QCheck_alcotest.to_alcotest
    (List.flatten
       [ PushTests.tests
       ; PopTests.tests
       ; AddTests.tests
       ; SubTests.tests
       ; MulTests.tests
       ; DivTests.tests
       ; RemTests.tests
       ; NegTests.tests
       ; SwapTests.tests
       ; QuitTests.tests
       ])
;;
