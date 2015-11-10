open Json_predicate

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  let () = really_input ic s 0 n in
  let () = close_in ic in
  s

let test js jp b =
  let pred = Yojson.Safe.from_file jp in
  let data = Yojson.Safe.from_file js in
  let result = Json_predicate.exec_exn pred data in
  Printf.printf "[%s] %s with %s\n%!" (if result = b then "ok" else "fail") js jp

let _ =
  let () = test "tests/contains.js" "tests/contains_true.jp" true in
  let () = test "tests/contains.js" "tests/contains_true2.jp" true in
  let () = test "tests/defined.js" "tests/defined_false.jp" false in
  let () = test "tests/defined.js" "tests/defined_true.jp" true in
  let () = test "tests/undefined.js" "tests/undefined_false.jp" false in
  let () = test "tests/undefined.js" "tests/undefined_true.jp" true in
  let () = test "tests/ends.js" "tests/ends_true.jp" true in
  let () = test "tests/ends.js" "tests/ends_true2.jp" true in
  let () = test "tests/in.js" "tests/in_true.jp" true in
  let () = test "tests/less.js" "tests/less_true.jp" true in
  let () = test "tests/matches.js" "tests/matches_true.jp" true in
  let () = test "tests/matches.js" "tests/matches_true2.jp" true in
  let () = test "tests/more.js" "tests/more_true.jp" true in
  let () = test "tests/starts.js" "tests/starts_true.jp" true in
  let () = test "tests/starts.js" "tests/starts_true2.jp" true in
  let () = test "tests/test.js" "tests/test_true.jp" true in
  let () = test "tests/type.js" "tests/type_true.jp" true in
  let () = test "tests/and.js" "tests/and_true.jp" true in
  let () = test "tests/and.js" "tests/and_false.jp" false in
  let () = test "tests/not.js" "tests/not_true.jp" true in
  let () = test "tests/not.js" "tests/not_false.jp" false in
  let () = test "tests/or.js" "tests/or_true.jp" true in
  let () = test "tests/or.js" "tests/or_false.jp" false in
  print_endline "done"

