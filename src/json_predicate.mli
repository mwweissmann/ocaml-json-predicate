val exec : Yojson.Safe.json -> (Yojson.Safe.json -> bool, [> `EInvalid_argument of string]) Result.result
(** [exec predicate document] evalutes the JSON predicate [predicate] on the
    JSON document [document]. In case of an error in [predicate], [Result.Error]
    is returned; else an evaluation function is returned that will verify if
    the given JSON document meets the predicate.
    Expressions of the predicate that cannot be evaluated e.g. because of a
    dangling pointer expression, will evaluate to [false].
*)

val exec_exn : Yojson.Safe.json -> (Yojson.Safe.json -> bool)
(** [exec_exn] behaves identical to [exec] but raises [Invalid_argument] in
    case of an error in the JSON predicate. *)

