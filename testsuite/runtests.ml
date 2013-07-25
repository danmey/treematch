(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  { empty with
    tool = "treematch.native";
    suffixes = [".tm"];
    options = Some "-special";
    dirs = [
      "treematch";
    ];
  };
])
