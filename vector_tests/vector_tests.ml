let read_file file = Core.In_channel.read_lines file
let is_vector fname = Core.Filename.check_suffix fname ".txt"

let read_filenames_in_dir (path : string) : string list =
  let open Unix in
  let rec helper (d : dir_handle) (acc : string list) : string list =
    match Core.Unix.readdir_opt d with
    | None -> acc
    | Some fname when is_vector fname -> helper d (fname :: acc)
    | Some _ -> helper d acc
  in
  let handler = Unix.opendir path in
  let result = helper handler [] in
  let _ = Unix.closedir handler in
  result
;;

let dict_of_list l =
  let res = Hashtbl.create (List.length l) in
  let () = List.iter (fun x -> Hashtbl.add res x ()) l in
  res
;;

type vector =
  { name : string
  ; input_file : string
  ; expected_output : string list
  }

let load_vectors input_dir output_dir =
  let vector_input_files = read_filenames_in_dir input_dir in
  let dict_of_output_files = dict_of_list (read_filenames_in_dir output_dir) in
  let accumulate_vector (acc : vector list) (vector_name : string) =
    let vector_output_name = "out" ^ vector_name in
    if Hashtbl.mem dict_of_output_files vector_output_name
    then (
      let expected_output = read_file (output_dir ^ "/" ^ vector_output_name) in
      { name = vector_name; input_file = input_dir ^ "/" ^ vector_name; expected_output }
      :: acc)
    else failwith ("Could not find vector output with name: " ^ vector_output_name)
  in
  List.fold_left accumulate_vector [] vector_input_files
;;

let vector_tests =
  let open Alcotest in
  let test_creator acc vector =
    let interp_result =
      try Interpreter.interpret_file vector.input_file with
      | _ as e ->
        Alcotest.fail
          ("interpreter failed with exn: "
          ^ Printexc.to_string e
          ^ " on vector: "
          ^ vector.name)
    in
    test_case vector.input_file `Quick (fun () ->
        Alcotest.(check (list string)) vector.name vector.expected_output interp_result)
    :: acc
  in
  List.fold_left
    test_creator
    []
    (load_vectors "vector_tests/vectors/input" "vector_tests/vectors/output")
;;
