open QCheck

let ( #% ) = Printf.sprintf
let long_factor = 100

let rec fold_until f p acc = function
  | x :: _ when p x -> acc
  | x :: xs -> fold_until f p (f acc x) xs
  | [] -> acc
;;

let interpret cmds =
  let stack = cmds |> SUT_type_converters.commands |> Interpreter.interpret in
  stack
;;

module Tests = struct
  open Assertions

  let positive_prefix = "positive"
  let negative_prefix = "negative"

  module QuitTests = struct
    let quit ~name_suffix (command_arb : Types.command arbitrary) =
      let open Types in
      QCheck.Test.make
        ~long_factor
        ~name:("%s_Quit_op_%s" #% positive_prefix name_suffix)
        (list_of_size (int_range 1 10).gen command_arb)
        (fun cmds ->
          assume (List.mem Quit cmds);
          let cmds_until_quit =
            fold_until (fun acc el -> el :: acc) (fun cmd -> cmd = Quit) [] cmds
            |> List.rev
          in
          if cmds_until_quit = []
          then assert_stack_is [] (interpret cmds)
          else (
            let expected_stack = interpret cmds_until_quit in
            let got_stack = interpret cmds in
            assert_stack_is_as_str expected_stack got_stack))
    ;;
  end

  module Negative = struct
    let any_op_one_value ~name cmd =
      let open QCheck.Test in
      make ~long_factor ~name Arbitraries.constant_arb (fun x ->
          let final_stack = interpret [ Push x; cmd ] in
          assert_stack_is [ Error; x ] final_stack)
    ;;

    let any_op_empty_stack ~name cmd =
      let open QCheck.Test in
      make ~long_factor ~name unit (fun _ ->
          let final_stack = interpret [ cmd ] in
          assert_stack_is [ Error ] final_stack)
    ;;
  end
end

module MakeUnaryCmdTests (Spec : Test_specs.UnaryCmdTestSpec) = struct
  let op_name = "%s_op" #% (Types.show_command Spec.cmd)

  let t_pos_no_bnd =
    QCheck.Test.make
      ~long_factor
      ~name:("%s_%s" #% Tests.positive_prefix op_name)
      Spec.gen
      (fun x ->
        let expected_result = Spec.expected_result_pos (Spec.extract_from_const x) in
        let final_stack = interpret [ Push x; Spec.cmd ] in
        Assertions.assert_stack_is [ Spec.wrap_into_const expected_result ] final_stack)
  ;;

  let t_neg_not_t =
    let open Arbitraries in
    QCheck.Test.make
      ~long_factor
      ~name:("%s_%s_not_t" #% Tests.negative_prefix op_name)
      constant_arb
      (fun x ->
        assume (not (Spec.is_correct_const x));
        let final_stack = interpret [ Push x; Spec.cmd ] in
        Assertions.assert_stack_is [ Error; x ] final_stack)
  ;;

  let t_neg_no_bnd_empty_stack =
    Tests.Negative.any_op_empty_stack
      ~name:("%s_%s_empty_stack" #% Tests.negative_prefix op_name)
      Spec.cmd
  ;;

  let tests = [ t_pos_no_bnd; t_neg_not_t; t_neg_no_bnd_empty_stack ]
end

module MakeBinaryCmdTests (Spec : Test_specs.BinaryCmdTestSpec) = struct
  let op_name = "%s_op" #% (Types.show_command Spec.cmd)

  let t_pos_no_bnd =
    QCheck.Test.make
      ~long_factor
      ~name:("%s_%s" #% Tests.positive_prefix op_name)
      (pair Spec.gen Spec.gen)
      (fun (fst, snd) ->
        let _ =
          match Spec.precond_pos with
          | None -> ()
          | Some f ->
            assume (f (Spec.extract_from_const fst) (Spec.extract_from_const snd))
        in
        let expected_result =
          Spec.expected_result_pos
            (Spec.extract_from_const fst)
            (Spec.extract_from_const snd)
        in
        let final_stack = interpret [ Push snd; Push fst; Spec.cmd ] in
        Assertions.assert_stack_is [ Spec.wrap_into_const expected_result ] final_stack)
  ;;

  let t_neg_no_bnd_either_not_t =
    let open Types in
    let open Arbitraries in
    QCheck.Test.make
      ~long_factor
      ~name:("%s_%s_either_not_t" #% Tests.negative_prefix op_name)
      (QCheck.pair constant_arb constant_arb)
      (fun (a, b) ->
        assume ((not (Spec.is_correct_const a)) || not (Spec.is_correct_const b));
        let final_stack = interpret [ Push a; Push b; Spec.cmd ] in
        Assertions.assert_stack_is [ Error; b; a ] final_stack)
  ;;

  let t_neg_no_bnd_one_value =
    Tests.Negative.any_op_one_value
      ~name:("%s_%s_one_value" #% Tests.negative_prefix op_name)
      Spec.cmd
  ;;

  let t_neg_no_bnd_empty_stack =
    Tests.Negative.any_op_empty_stack
      ~name:("%s_%s_empty_stack" #% Tests.negative_prefix op_name)
      Spec.cmd
  ;;

  let tests =
    [ t_pos_no_bnd
    ; t_neg_no_bnd_either_not_t
    ; t_neg_no_bnd_one_value
    ; t_neg_no_bnd_empty_stack
    ]
  ;;
end
