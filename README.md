A Haskell module for symbolic mathematics, including differentiation, evaluation, and series approximation.

## Features
- Symbolic Expressions: Represent numbers, variables, and operations like +, *, /, sin, cos, log, exp.
- Differentiation: Compute derivatives symbolically.
- Evaluation: Evaluate expressions with variable assignments.
- Maclaurin Series: Approximate expressions using series expansions.
- Pretty Printing: Convert expressions to readable form.
- Infinite Series Arithmetic: Work with infinite lists using numeric operations.

## Example Usage
```
import Calculus
import Data.Map (fromList)

-- Expression: x^2 + 3x + 5
let expr = (Id "x" * Id "x") + (Val 3 * Id "x") + Val 5

-- Symbolic derivative
diff "x" expr
-- Output: (x*1 + 1*x) + (3*1 + 0) + 0

-- Evaluate at x = 2
eval (fromList [("x", 2)]) expr
-- Output: 15

-- Maclaurin approximation for e^x at x=1, 10 terms
maclaurin (exp (Id "x")) 1 10
-- Output: Just 2.7182818

-- Pretty print
pretty expr
-- Output: "((x*x)+(3*x)+5)"

-- Pi approximation with 1000 terms
pi 1000
```
