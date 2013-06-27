ast ListA {
List: Cons Integer List
      | Nil
Integer: int
}

ast ListB {
List: Cons Integer List
      | Nil
Integer: int
}

map double : ListA => ListB {
 List: Cons x xs => Cons x (Cons x xs)
}

map reverse_args : ListA => ListB {
 List: Cons x xs => (Cons xs x)
}

map rename_tag : ListA => ListB {
 List: Cons x xs => ConsRev xs x
}
