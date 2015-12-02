type case =
  | Case_sensitive
  | Case_insensitive
 
let json_to_string = function
  | `String v -> v
  | x -> Yojson.Safe.to_string x

let regexp = function
  | Case_insensitive -> Str.regexp_case_fold
  | Case_sensitive -> Str.regexp

let matches character_matching needle =
  let re = regexp character_matching needle in
  (fun str ->
    try
      Str.string_match re str 0
    with
    | Not_found -> false
  )

let contains character_matching needle =
  let s = Str.quote needle in
  let re = regexp character_matching s in
  (fun str ->
    try
      let _ = Str.search_forward re str 0 in true
    with
    | Not_found -> false
  )

let predicate f =
  (fun path data ->
    try
      let sub = Json_pointer.resolve_exn path data in
      f sub
    with
    | Not_found -> false
  )

let p_string f value =
  predicate (fun x -> f (json_to_string value) (json_to_string x))

let p_contains character_matching =
  p_string (contains character_matching)

let p_defined =
  predicate (fun _ -> true)

let p_in values =
  let elts =
    match values with
      | `List xs -> xs
      | _ -> raise (Invalid_argument "value not a list")
  in
  (fun path data ->
    try
      let elt = Json_pointer.resolve_exn path data in
      List.mem elt elts
    with
    | Not_found -> false
  )

let p_less value =
  predicate (fun x -> value > x)

let p_matches value character_matching =
  p_string (matches character_matching) value

let p_more value =
  predicate (fun x -> value < x)

let p_starts character_matching =
  let starts character_matching needle str =
    let needle_length = String.length needle in
    try
      let str = String.sub str 0 needle_length in
      let (str, needle) =
        if character_matching = Case_insensitive then (String.lowercase str, String.lowercase needle)
        else (str, needle)
      in
      str = needle
    with
    | Invalid_argument _ -> false
  in
  p_string (starts character_matching)

let p_ends character_matching =
  let ends character_matching needle str =
    let needle_length = String.length needle in
    try
      let str = String.sub str (String.length str - needle_length) needle_length in
      let (str, needle) =
        if character_matching = Case_insensitive then (String.lowercase str, String.lowercase needle)
        else (str, needle)
      in
      str = needle
    with 
    | Invalid_argument _ -> false
  in
  p_string (ends character_matching)

let p_test character_matching value =
  let ieq x y = String.lowercase x = String.lowercase y in
  let rec eq_character_matching (a : Yojson.Safe.json) (b : Yojson.Safe.json) =
    match (a, b) with
    | (x, y) when x = y -> true
    | (`Assoc xs, `Assoc ys) -> List.for_all2 (fun (s1, x) (s2, y) -> (ieq s1 s2) && (eq_character_matching x y)) xs ys
    | (`List xs, `List ys) -> List.for_all2 eq_character_matching xs ys
    | (`Tuple xs, `Tuple ys) -> List.for_all2 eq_character_matching xs ys
    | (`String x, `String y) -> ieq x y
    | _ -> false
  in
  match character_matching with
  | Case_sensitive -> predicate (fun x -> x = value)
  | Case_insensitive -> predicate (fun x -> try eq_character_matching value x with | Not_found -> false)

let p_type value =
  let f =
    match json_to_string value with
    | "number" -> (function | `Int _ -> true | `Float _ -> true | _ -> false)
    | "string" -> (function | `String _ -> true | _ -> false)
    | "boolean" -> (function | `Bool _ -> true | _ -> false)
    | "object" -> (fun _ -> false) (* TODO *)
    | "array" -> (function | `List _ -> true | _ -> false)
    | "null" -> (function | `Null -> true | _ -> false)
    | "undefined" -> (fun _ -> false) (* TODO *)
    | "date" -> (fun _ -> false) (* TODO *)
    | "date-time" -> (fun _ -> false) (* TODO *)
    | "time" -> (fun _ -> false) (* TODO *)
    | "lang" -> (fun _ -> false) (* TODO *)
    | "lang-range" -> (fun _ -> false) (* TODO *)
    | "iri"  -> (fun _ -> false) (* TODO *)
    | "absolute-iri" -> (fun _ -> false) (* TODO *)
    | x -> raise (Invalid_argument (Printf.sprintf "'%s' not a valid type" x))
  in
  predicate f

let p_undefined p data =
  let path = Json_pointer.path p in
  let rec crawl ps jd =
    match ps with
    | [] -> false
    | x::xs ->
      begin match jd with
        | `Assoc js -> crawl xs (List.assoc x js)
        | _ -> true
      end
  in
  try
    crawl path data
  with
  | Not_found -> true

let getstr (xs : (string * Yojson.Safe.json) list) x =
  match List.assoc x xs with
  | `String v -> v
  | _ -> raise (Invalid_argument (Printf.sprintf "value '%s' not a string" x))

let rec eval (pred : Yojson.Safe.json) =
(*
  let () = Printf.printf "eval '%s'\n%!" (json_to_string pred) in
*)
  begin match pred with
  | `Assoc (xs : (string * Yojson.Safe.json) list) ->
    let get = fun x -> try List.assoc x xs with Not_found -> raise (Invalid_argument (Printf.sprintf "'%s' not found" x)) in
    let getstr = getstr xs in
    let e () = raise (Invalid_argument "'path' not found") in
    let p def = try Json_pointer.of_string_exn (getstr "path") with Not_found -> (def ()) in
    begin match getstr "op" with
    | "contains" -> p_contains Case_sensitive (get "value") (p e)
    | "contains-" -> p_contains Case_insensitive (get "value") (p e)
    | "defined" -> p_defined (p e)
    | "ends" -> p_ends Case_sensitive (get "value") (p e)
    | "ends-" -> p_ends Case_insensitive (get "value") (p e)
    | "in" -> p_in (get "value") (p e)
    | "less" -> p_less (get "value") (p e)
    | "matches" -> p_matches (get "value") Case_sensitive (p e)
    | "matches-" -> p_matches (get "value") Case_insensitive (p e)
    | "more" -> p_more (get "value") (p e)
    | "starts" -> p_starts Case_sensitive (get "value") (p e)
    | "starts-" -> p_starts Case_insensitive (get "value") (p e)
    | "test" -> p_test Case_sensitive (get "value") (p e)
    | "test-" -> p_test Case_insensitive (get "value") (p e)
    | "type" -> p_type (get "value") (p e)
    | "undefined" -> p_undefined (p e)
    | "and" -> p_and (p (fun () -> Json_pointer.empty)) (get "apply")
    | "not" -> p_not (p (fun () -> Json_pointer.empty)) (get "apply")
    | "or" -> p_or (p (fun () -> Json_pointer.empty)) (get "apply")
    | _ -> raise (Invalid_argument "invalid operation")
    end
  | _ -> raise (Invalid_argument "not a json-predicate")
  end
and p_and p apply data =
  match apply with
  | `List xs ->
    begin
      let sub = Json_pointer.resolve_exn p data in
      List.for_all (fun x -> eval x sub) xs
    end
  | _ -> false
and p_not p apply data =
  match apply with
  | `List xs ->
    begin
      let sub = Json_pointer.resolve_exn p data in
      List.for_all (fun x -> false = eval x sub) xs
    end
  | _ -> false
and p_or p apply data =
  match apply with
  | `List xs ->
    begin
      let sub = Json_pointer.resolve_exn p data in
      List.exists (fun x -> eval x sub) xs
    end
  | _ -> false

let exec pred =
  let f = try Result.Ok (eval pred) with | Invalid_argument x -> Result.Error (`EInvalid_argument x) in
  f

let exec_exn pred data =
  eval pred data

