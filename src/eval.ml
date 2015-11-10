open Json_predicate

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  let () = really_input ic s 0 n in
  let () = close_in ic in
  s

let _ =
  if Array.length Sys.argv != 3 then
    Printf.printf "Usage: %s PREDICATE-FILE DATA-FILE\n" Sys.argv.(0)
  else
    let pred = Yojson.Safe.from_file Sys.argv.(1) in
    let data = Yojson.Safe.from_file Sys.argv.(2) in
    let eval = Json_predicate.exec_exn pred in
    let rc = eval data in
    if rc then print_endline "yeah" else print_endline "nay"

