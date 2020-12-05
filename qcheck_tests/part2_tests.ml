open Common
module CatTests = Common.MakeBinaryCmdTests (Test_specs.CatSpec)
module AndTests = Common.MakeBinaryCmdTests (Test_specs.AndSpec)
module OrTests = Common.MakeBinaryCmdTests (Test_specs.OrSpec)
module NotTests = Common.MakeUnaryCmdTests (Test_specs.NotSpec)
module EqTests = Common.MakeBinaryCmdTests (Test_specs.EqSpec)
module LteTests = Common.MakeBinaryCmdTests (Test_specs.LteSpec)
module LtTests = Common.MakeBinaryCmdTests (Test_specs.LtSpec)
module GteTests = Common.MakeBinaryCmdTests (Test_specs.GteSpec)
module GtTests = Common.MakeBinaryCmdTests (Test_specs.GtSpec)

module BndTests = struct
  open QCheck
  open Assertions

  let pos_bnd_literal_values =
    let open Arbitraries in
    Test.make
      ~long_factor
      ~name:"%s_Bnd_op_literal_values" #% Common.Tests.positive_prefix
      (pair boundable_literal_constant_arb constant_name_arb)
      (fun (bound_value, name) ->
        let final_stack = interpret [ Push bound_value; Push name; Bnd ] in
        assert_stack_is [ Unit ] final_stack)
  ;;

  let pos_bnd_value_of_name =
    let open Arbitraries in
    Test.make
      ~long_factor
      ~name:"%s_Bnd_op_value_of_name" #% Common.Tests.positive_prefix
      (triple boundable_literal_constant_arb constant_name_arb constant_name_arb)
      (fun (bound_value, name1, name2) ->
        let final_stack =
          interpret [ Push bound_value; Push name1; Bnd; Push name1; Push name2; Bnd ]
        in
        assert_stack_is [ Unit; Unit ] final_stack)
  ;;

  let neg_bnd_empty_stack =
    let open Common.Tests in
    Negative.any_op_empty_stack
      ~name:("%s_%s" #% negative_prefix "Bnd_op_empty_stack")
      Bnd
  ;;

  let neg_bnd_stack_one_value =
    let open Common.Tests in
    Negative.any_op_one_value ~name:("%s_%s" #% negative_prefix "Bnd_op_one_value") Bnd
  ;;

  let neg_bnd_unbound_name =
    let open Arbitraries in
    Test.make
      ~long_factor
      ~name:"%s_Bnd_op_unbound_name" #% Common.Tests.negative_prefix
      (pair constant_name_arb constant_name_arb)
      (fun (name1, name2) ->
        let final_stack = interpret [ Push name1; Push name2; Bnd ] in
        assert_stack_is [ Error; name2; name1 ] final_stack)
  ;;

  let neg_bnd_invalid_first =
    let open Arbitraries in
    let open Types in
    Test.make
      ~long_factor
      ~name:"%s_Bnd_op_invalid_first" #% Common.Tests.negative_prefix
      (pair constant_arb constant_arb)
      (fun (a, b) ->
        assume (not (is_name b));
        let final_stack = interpret [ Push a; Push b; Bnd ] in
        assert_stack_is [ Error; b; a ] final_stack)
  ;;

  let neg_bnd_invalid_snd =
    let open Arbitraries in
    let open Types in
    Test.make
      ~long_factor
      ~name:"%s_Bnd_op_invalid_snd" #% Common.Tests.negative_prefix
      (pair constant_error_arb constant_name_arb)
      (fun (a, b) ->
        let final_stack = interpret [ Push a; Push b; Bnd ] in
        assert_stack_is [ Error; b; a ] final_stack)
  ;;

  let tests =
    [ pos_bnd_literal_values
    ; pos_bnd_value_of_name
    ; neg_bnd_empty_stack
    ; neg_bnd_stack_one_value
    ; neg_bnd_unbound_name
    ; neg_bnd_invalid_first
    ; neg_bnd_invalid_snd
    ]
  ;;
end

module MakeUnaryCmdTestsWithBnd (Spec : Test_specs.UnaryCmdTestSpec) = struct
  let op_name = "%s_op" #% (Types.show_command Spec.cmd)

  let t_pos_with_bnd =
    let open Arbitraries in
    let open QCheck in
    Test.make
      ~long_factor
      ~name:("%s_%s_with_bnd" #% Tests.positive_prefix op_name)
      (pair Spec.gen constant_name_arb)
      (fun (value, name) ->
        let expected_result = Spec.expected_result_pos (Spec.extract_from_const value) in
        let final_stack =
          interpret [ Push value; Push name; Bnd; Pop; Push name; Spec.cmd ]
        in
        Assertions.assert_stack_is [ Spec.wrap_into_const expected_result ] final_stack)
  ;;

  let t_pos_overwritten_bnd =
    let open Arbitraries in
    let open QCheck in
    Test.make
      ~long_factor
      ~name:("%s_%s_with_bnd_overwritten" #% Tests.positive_prefix op_name)
      (triple Spec.gen constant_name_arb Spec.gen)
      (fun (value, name, value') ->
        let expected_result = Spec.expected_result_pos (Spec.extract_from_const value') in
        let final_stack =
          interpret
            [ Push value
            ; Push name
            ; Bnd
            ; Pop
            ; Push value'
            ; Push name
            ; Bnd
            ; Pop
            ; Push name
            ; Spec.cmd
            ]
        in
        Assertions.assert_stack_is [ Spec.wrap_into_const expected_result ] final_stack)
  ;;

  let t_neg_not_t_with_bnd =
    let open Arbitraries in
    let open Types in
    let open QCheck in
    Test.make
      ~long_factor
      ~name:("%s_%s_not_t_with_bnd" #% Tests.negative_prefix op_name)
      (pair constant_arb constant_name_arb)
      (fun (value, name) ->
        assume (not (Spec.is_correct_const value));
        (* Invariant of bnd is that value cannot be name or error *)
        assume (not (is_name value));
        assume (not (is_error value));
        let final_stack =
          interpret [ Push value; Push name; Bnd; Pop; Push name; Spec.cmd ]
        in
        Assertions.assert_stack_is [ Error; name ] final_stack)
  ;;

  let t_neg_name_unbound =
    let open Arbitraries in
    QCheck.Test.make
      ~long_factor
      ~name:("%s_%s_with_bnd_name_unbound" #% Tests.negative_prefix op_name)
      constant_name_arb
      (fun name ->
        let final_stack = interpret [ Push name; Spec.cmd ] in
        Assertions.assert_stack_is [ Error; name ] final_stack)
  ;;

  let tests =
    [ t_pos_with_bnd; t_pos_overwritten_bnd; t_neg_not_t_with_bnd; t_neg_name_unbound ]
  ;;
end

module MakeBinaryCmdTestsWithBnd (Spec : Test_specs.BinaryCmdTestSpec) = struct
  let op_name = "%s_op" #% (Types.show_command Spec.cmd)

  let t_pos_with_bnd =
    let open Arbitraries in
    let open QCheck in
    Test.make
      ~long_factor
      ~name:("%s_%s_with_bnd" #% Tests.positive_prefix op_name)
      (quad Spec.gen constant_name_arb Spec.gen constant_name_arb)
      (fun (value1, name1, value2, name2) ->
        (* Without this assumption, our expected value will be incorrect, as the
         overwrite will conflict with name2 *)
        assume (name1 <> name2);
        let _ =
          match Spec.precond_pos with
          | None -> ()
          | Some f ->
            assume (f (Spec.extract_from_const value1) (Spec.extract_from_const value2))
        in
        let expected_result =
          Spec.expected_result_pos
            (Spec.extract_from_const value1)
            (Spec.extract_from_const value2)
        in
        let final_stack =
          interpret
            [ Push value1
            ; Push name1
            ; Bnd
            ; Pop
            ; Push value2
            ; Push name2
            ; Bnd
            ; Pop
            ; Push name2
            ; Push name1
            ; Spec.cmd
            ]
        in
        Assertions.assert_stack_is [ Spec.wrap_into_const expected_result ] final_stack)
  ;;

  let t_pos_overwritten_bnd =
    let open Arbitraries in
    let open QCheck in
    let value_name_value_gen = triple Spec.gen constant_name_arb Spec.gen in
    let value_name_gen = pair Spec.gen constant_name_arb in
    QCheck.Test.make
      ~long_factor
      ~name:("%s_%s_with_bnd_overwritten" #% Tests.positive_prefix op_name)
      (pair value_name_value_gen value_name_gen)
      (fun ((value1, name1, value1'), (value2, name2)) ->
        assume (name1 <> name2);
        assume (value1 <> value1');
        let _ =
          match Spec.precond_pos with
          | None -> ()
          | Some f ->
            assume (f (Spec.extract_from_const value1) (Spec.extract_from_const value2));
            assume (f (Spec.extract_from_const value1') (Spec.extract_from_const value2))
        in
        let helper a b =
          Spec.expected_result_pos (Spec.extract_from_const a) (Spec.extract_from_const b)
        in
        let (bnd_orig_values : Types.command list) =
          [ Push value1; Push name1; Bnd; Pop; Push value2; Push name2; Bnd; Pop ]
        in
        let (overwrite_first : Types.command list) =
          [ Push value1'; Push name1; Bnd; Pop ]
        in
        let (execuete_op_cmds : Types.command list) =
          [ Push name2; Push name1; Spec.cmd ]
        in
        let expected_result = helper value1' value2 in
        let final_stack =
          interpret (bnd_orig_values @ overwrite_first @ execuete_op_cmds)
        in
        Assertions.assert_stack_is [ Spec.wrap_into_const expected_result ] final_stack)
  ;;

  let t_neg_either_not_t_with_bnd =
    let open Arbitraries in
    let open Types in
    let open QCheck in
    Test.make
      ~long_factor
      ~name:("%s_%s_either_not_t_with_bnd" #% Tests.negative_prefix op_name)
      (quad constant_arb constant_name_arb constant_arb constant_name_arb)
      (fun (value1, name1, value2, name2) ->
        let helper value =
          assume (not (Spec.is_correct_const value));
          (* Invariant of bnd is that value cannot be name or error *)
          assume (not (is_name value));
          assume (not (is_error value))
        in
        helper value1;
        helper value2;
        let final_stack =
          interpret
            [ Push value1
            ; Push name1
            ; Bnd
            ; Pop
            ; Push value2
            ; Push name2
            ; Bnd
            ; Pop
            ; Push name2
            ; Push name1
            ; Spec.cmd
            ]
        in
        Assertions.assert_stack_is [ Error; name1; name2 ] final_stack)
  ;;

  let t_neg_name_unbound =
    let open Arbitraries in
    let open QCheck in
    Test.make
      ~long_factor
      ~name:("%s_%s_with_bnd_name_unbound" #% Tests.negative_prefix op_name)
      (pair constant_name_arb constant_name_arb)
      (fun (name1, name2) ->
        let final_stack = interpret [ Push name1; Push name2; Spec.cmd ] in
        Assertions.assert_stack_is [ Error; name2; name1 ] final_stack)
  ;;

  let tests =
    [ t_pos_with_bnd
    ; t_pos_overwritten_bnd
    ; t_neg_either_not_t_with_bnd
    ; t_neg_name_unbound
    ]
  ;;
end

(* Part 1 commands *)
module AddTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.AddSpec)
module SubTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.SubSpec)
module MulTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.MulSpec)
module DivTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.DivSpec)
module RemTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.RemSpec)
module NegTestsWithBnd = MakeUnaryCmdTestsWithBnd (Test_specs.NegSpec)
module CatTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.CatSpec)
module AndTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.AndSpec)
module OrTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.OrSpec)
module NotTestsWithBnd = MakeUnaryCmdTestsWithBnd (Test_specs.NotSpec)
module EqTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.EqSpec)
module LteTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.LteSpec)
module LtTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.LtSpec)
module GteTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.GteSpec)
module GtTestsWithBnd = MakeBinaryCmdTestsWithBnd (Test_specs.GtSpec)

module QuitTestsWithBnd = struct
  let quit_with_bnd =
    Common.Tests.QuitTests.quit ~name_suffix:"with_bnd" Arbitraries.part1_2_command_arb
  ;;

  let tests = [ quit_with_bnd ]
end

(* module QuitTestsWithEnv = struct *)
(*   open QCheck;; *)
(*   open Assertions;; *)
(*   let quit = *)
(*     let rec fold_until f p acc = function *)
(*       | x :: xs when p x -> acc *)
(*       | x :: xs -> fold_until f p (f acc x) xs *)
(*       | [] -> acc *)
(*     in *)
(*     let open Common.Types in *)
(*     let open Arbitraries in *)
(*     QCheck.Test.make *)
(*       ~long_factor *)
(*       ~name:"%s_Quit_op" #% Common.Tests.positive_prefix *)
(*       (list_of_size (int_range 1 10).gen part2_command_arb) *)
(*       (fun cmds -> *)
(*         let rec helper cmds = *)
(*           match cmds with *)
(*           | [] -> false *)
(*           | Env cmds :: tl -> helper cmds && helper tl *)
(*           | Quit :: _ -> true *)
(*           | _ -> false *)
(*         in *)
(*         assume (helper cmds); *)
(*         let cmds_until_quit = *)
(*           fold_until *)
(*             (fun acc el -> el :: acc) *)
(*             (fun cmd -> *)
(*               match cmd with *)
(*               | Quit -> true *)
(*               | Env cmds -> helper cmds *)
(*               | _ -> false) *)
(*             [] *)
(*             cmds *)
(*           |> List.rev *)
(*         in *)
(*         if cmds_until_quit = [] *)
(*         then assert_stack_is [] (interpret cmds) *)
(*         else ( *)
(*           let expected_stack = interpret cmds_until_quit in *)
(*           let got_stack = interpret cmds in *)
(*           assert_stack_is_as_str expected_stack got_stack)) *)
(*   ;; *)
(*   let tests = [ quit ] *)
(* end *)

let suite =
  List.map
    QCheck_alcotest.to_alcotest
    (List.flatten
       [ CatTests.tests
       ; AndTests.tests
       ; OrTests.tests
       ; NotTests.tests
       ; EqTests.tests
       ; LteTests.tests
       ; LtTests.tests
       ; GteTests.tests
       ; GtTests.tests
       ; BndTests.tests
       ; AddTestsWithBnd.tests
       ; SubTestsWithBnd.tests
       ; MulTestsWithBnd.tests
       ; DivTestsWithBnd.tests
       ; RemTestsWithBnd.tests
       ; NegTestsWithBnd.tests
       ; QuitTestsWithBnd.tests
       ; CatTestsWithBnd.tests
       ; AndTestsWithBnd.tests
       ; OrTestsWithBnd.tests
       ; NotTestsWithBnd.tests
       ; EqTestsWithBnd.tests
       ; LteTestsWithBnd.tests
       ; LtTestsWithBnd.tests
       ; GteTestsWithBnd.tests
       ; GtTestsWithBnd.tests
       ])
;;
