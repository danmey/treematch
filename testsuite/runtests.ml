(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "treematch.native";
    suffixes = [".tm"];
    options = Some "-special";
    dirs = [
      "treematch";
    ];
  };
])
