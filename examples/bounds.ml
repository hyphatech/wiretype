module J = Wiretype
module B = Bounded

let read name description text =
  match J.decode description text with
  | Ok _ -> Printf.printf "%-8s %s: read\n" name text
  | Error problems ->
      Printf.printf "%-8s %s: %s\n" name text
        (J.Problem.list_to_string problems)

let () =
  read "quantity" B.quantity "0";
  read "quantity" B.quantity "1e1";
  read "even" B.even "7";
  read "price" B.price "19.99";
  read "price" B.price "19.999";
  read "ratio" B.ratio "1";
  read "name" B.name {|"café"|};
  read "name" B.name {|"cafés"|};
  read "tags" B.tags {|["a", "b", "c"]|};
  read "scores" B.scores "{}"
