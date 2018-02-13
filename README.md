# Sym_diff
Symbolic differentiation tool in Haskell. 

The abstract data type used to represent mathematical expressions is the ME. 
Some examples of internal representation:
"3+4" becomes Add (Num 3) (Num 4)
"3*x" becomes Mul (Num 3) (Var 'x')
"-x"  becomes Neg (Var 'x')
"x^3" becomes Pow (Var 'x') (3)

The recursive definition of ME allows for nesting of expressions:
"(3*x^4 + 5*x + 4)*((x+1)^2)" becomes
Mul (Group (Add Add (Mul (Num 3) (Pow (Var 'x') 4)) (Mul (Num 5) (Var x)) (Num 4))) (Group (Pow (Add (Var 'x') (Num 1)) 2))

The derivative functions perform differentiation on a given ME according to specific rules. The Result is unsimplified.

The simplification functions produce an equivalent ME with a different form.

The unparser takes a given ME, for example Add (Var 'x') (Num 3) and returns a character string "x+3".

The parser takes a character string and gives the internal representation. For example, "x**3" returns Pow (Var 'x') (3)
