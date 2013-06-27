ast Program {
Program:     | Program Definition Program
             | End
Definition:  | Function Function
             | Global Global
Function:    | Function Identifer Arguments Expression
Arguments:   | Argument Identifer Arguments
             | End
Expression:  | Let Identifer Expression Expression
             | Operator Operator Expression Expression
             | Call Name Expressions
             | Variable Identifier
             | Constant Integer
Expressions: | Expression Expressions
             | End
Operator:    | Plus
             | Minus
             | Mult
Global:      | Global Identifer
Identifer: string
Integer: int
}
