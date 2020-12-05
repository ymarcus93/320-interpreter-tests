let ( #% ) = Printf.sprintf

let fail reason (stack : string list) =
  QCheck.Test.fail_report
    ("failure-reason: %s\nfinal stack outtputed: %s" #% reason
       (QCheck.Print.list QCheck.Print.string stack))
;;

let assert_rs condition reason (stack : string list) =
  if not condition then fail reason stack else true
;;

let assert_stack_is_as_str (expected : string list) (stack : string list) =
  if stack = expected
  then true
  else
    QCheck.Test.fail_report
      ("expected stack: %s\ngot: %s" #% (QCheck.Print.list QCheck.Print.string expected)
         (QCheck.Print.list QCheck.Print.string stack))
;;

let assert_stack_is (expected : Types.constant list) (stack : string list) =
  let expected_as_str_list =
    List.map (fun el -> SUT_type_converters.constant_output el) expected
  in
  assert_stack_is_as_str expected_as_str_list stack
;;
