# ocaml-json-predicate
This OCaml library implements [JSON predicate](https://tools.ietf.org/html/draft-snell-json-test-07)

JSON Predicates defines a syntax for serializing various predicate expressions as JSON Objects.

Given the following JSON document
```Javascript
{
  "a" : {
    "b" : "foo",
    "c" : {
      "d": 10
    }
  }
}
```

and the following JSON predicate
```Javascript
{
  "op": "and",
  "apply": [
    {
      "op": "defined",
      "path": "/a/b"
    },
    {
      "op": "less",
      "path": "/a/c/d",
      "value": 15
    }
  ]
}
```

you can evalute the predicate for the document -- which in this case is valid.

```ocaml
let _ =
  let predicate = Yojson.Safe.from_file the_predicate_file in
  let document = Yojson.Safe.from_file the_document_file in
  let result = Json_predicate.exec_exn predicate document in
  print_endline (if result then "true" else "false")
```

There is basically only one function ```exec```:
```ocaml
val exec : Yojson.Safe.json ->
    (Yojson.Safe.json -> bool, [> `EInvalid_argument of string]) Result.result

val exec_exn : Yojson.Safe.json -> (Yojson.Safe.json -> bool)
```
```exec predicate document``` evalutes the JSON predicate ```predicate``` on the
JSON document ```document```. In case of an error in ```predicate```, ```Result.Error```
is returned; else an evaluation function is returned that will verify if
the given JSON document meets the predicate.
Expressions of the predicate that cannot be evaluated e.g. because of a
dangling pointer expression, will evaluate to ```false```.
```exec_exn``` behaves identical to ```exec``` but raises ```Invalid_argument``` in
case of an error in the JSON predicate.
Both functions can be partially applied to create pre-compiled predicate functions
which can be executed multiple times.

The json-pointer library is written by [Markus Weissmann](http://www.mweissmann.de).

The source-code of json-predicate is available under the MIT license.

