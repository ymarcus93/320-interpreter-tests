let () =
  let open Alcotest in
  run
    "Tests"
    [ ("part1-property-tests", Interp_prop_tests.Part1_tests.suite)
    ; ("part2-property-tests", Interp_prop_tests.Part2_tests.suite)
    ; ("vector-tests", Vector_tests.vector_tests)
    ]
;;
