(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "treematch";
    suffixes = [".tm"];
    options = Some "-special";
    dirs = [
      "treematch";
    ];
  };
])
