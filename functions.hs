-- Symbolic Differentiation

import Data.Char

-- Internal representation of algebraic expressions
-- Examples:
-- "x + y" := Add (Var 'x') (Var 'y')
-- "x^3"   := Pow (Var 'x') 3
-- "3x"    := Mul (Num 3) (Var 'x')
data ME = Num Int | Var Char | Group ME | Add ME ME | Sub ME ME | Mul ME ME | Pow ME Int | Neg ME deriving (Show, Ord, Eq)

-- Differentiation rules

-- differentiate with respect to Char
deriv :: ME -> Char -> ME
-- dn/dx = 0 for numeral n
deriv (Num n) x = Num 0

-- dx/dx = 0 for var x
--derive (Var x) 'x' = Num 1

-- dy/dx = 0 for vars x, y
--derive (Var y) x = Num 0

deriv (Var x) y
 | x == y    = Num 1
 | otherwise = Num 0

-- d(-f)/dx = -df/dx
deriv (Neg f) x = Neg (deriv f x)

-- d(f+g)/dx = df/dx + dg/dx
deriv (Add f g) x = Add (deriv f x) (deriv g x)

-- d(f*g)/dx = g*df/dx + f*dg/dx
deriv (Mul f g) x = Add (Mul g (deriv f x)) (Mul f (deriv g x))

-- Extra implementations

-- d/dx power(f,n) = n*power(f,n-1)(df/dx)
deriv (Pow f (n)) x  = Mul (Mul (Num n) (Pow f (n-1))) (deriv f x)


-- Simplification rules

-- inside out simplification
simplifyME :: ME -> ME
simplifyME (Var x)   = makeVar x
simplifyME (Num n)   = makeNum n
simplifyME (Group f) = makeGroup (simplifyME f)
simplifyME (Add f g) = makeAdd (simplifyME f) (simplifyME g)
simplifyME (Sub f g) = makeSub (simplifyME f) (simplifyME g)
simplifyME (Mul f g) = makeMul (simplifyME f) (simplifyME g)
simplifyME (Pow f n) = makePow (simplifyME f) n
simplifyME (Neg f)   = makeNeg (simplifyME f)

-- simplification rules
-- m,n integers, f,g,h mathematical expressions
makeVar :: Char -> ME
makeVar x = Var x

makeNum :: Int -> ME
makeNum n = Num n

makeGroup :: ME -> ME
makeGroup f = f

makeAdd :: ME -> ME -> ME
-- m+n = k
makeAdd (Num m) (Num n) = Num k
                        where k = m+n
-- f+m+n = f+k                              
makeAdd (Add f (Num m)) (Num n) = Add f (Num k)
                                where k = m+n
-- n + f = f + n                                      
makeAdd (Num n) f = Add f (Num n)

-- f+n+g = f+g+n
makeAdd (Add f (Num n)) g = Add (Add f g) (Num n)

-- f+0=f
makeAdd f (Num 0) = f

-- mf + nf = kf
makeAdd (Mul (Num m) f) (Mul (Num n) g)
  | f == g    = makeMul (Num k) f
  | otherwise = Add (makeMul (Num m) f) (makeMul (Num n) g)
  where k = m+n

-- default
makeAdd f g = Add f g  

makeSub :: ME -> ME -> ME

-- m - n = k
makeSub (Num m) (Num n) = Num k
                        where k = m-n

-- 0 - f = -f                              
makeSub (Num 0) f = Neg f

-- f - 0 = f
makeSub f (Num 0) = f

-- m - f = -f + m
makeSub (Num m) f = Add (Neg f) (Num m)

-- f - m - n = f - k
makeSub (Sub f (Num m)) (Num n) = Sub f (Num k)
                                      where k = m+n

-- default                                            
makeSub f g = Sub f g


makeMul :: ME -> ME -> ME

-- m*n = k
makeMul (Num m) (Num n) = Num k
                        where k = m*n

-- 0*f = 0                              
makeMul (Num 0) f       = Num 0

-- 1*f = f
makeMul (Num 1) f       = f

-- f*n = n*f
makeMul f (Num n)       = Mul (Num n) f

--f*(g*h) = (f*g)*h
makeMul f (Mul g h)     = Mul (Mul f g) h

-- f^m * f^n = f^k
makeMul (Pow f m) (Pow g n)
  | f == g    = Pow f k
  | otherwise = Mul (Pow f m) (Pow g n)
  where k = m+n

-- f^m * g = g * f^m  
makeMul (Pow f m) g     = Mul g (Pow f m)

-- default
makeMul f g             = Mul f g

makePow :: ME -> Int -> ME

-- f^0 = 1
makePow f 0       = Num 1

-- f^1 = f
makePow f 1       = f

-- m^n = k
makePow (Num m) n = Num k
                  where k = m^n

-- default                        
makePow f m       = Pow f m

makeNeg :: ME -> ME
makeNeg f = Neg f


-- BNF grammars for mathematical expressions: 

-- <expression> ::= <signed-term> | <expression> "+" <term> | <expression> "-" <term>
-- <signed-term> ::= "-" <term> | <term>
-- <term> ::= <factor> | <term> * <factor> 
-- <factor> ::= <element> | <element> ** <numeral>
-- <element> ::= <variable> | <numeral> | "(" <expression> ")"
-- <variable> ::= [a-z]
-- <numeral> ::= [0-9]+

-- Extra: Unparsing

-- recursive unparsing, adding groups if necessary
unparseME :: ME -> [Char]
unparseME (Add m1 m2) = (unparseME (addGroups m1)) ++ "+" ++ (unparseME (addGroups m2))
unparseME (Sub m1 m2) = (unparseME (addGroups m1)) ++ "-" ++ (unparseME (addGroups m2))
unparseME (Mul m1 m2) = (unparseME (addGroups m1)) ++ "*" ++ (unparseME (addGroups m2))
unparseME (Pow f n)   = (unparseME (addGroups f)) ++ "**" ++ (show n) 
unparseME (Neg f)     = "-" ++ (unparseME (addGroups f))
unparseME (Group f)   = "(" ++ (unparseME (addGroups f)) ++ ")"
unparseME (Var f)     = f:[]
unparseME (Num n)     = (intToDigit n):[]

-- main idea based on BNF grammar
-- force ME to fall into a specific category
-- of the BNF grammar
-- eg. a signed term cannot be an Add or Sub,
-- so put it in a Group
forceSiTerm :: ME -> ME
forceSiTerm (Add m1 m2) = Group (Add m1 m2)
forceSiTerm (Sub m1 m2) = Group (Sub m1 m2)
forceSiTerm f           = f

forceTerm :: ME -> ME
forceTerm (Add m1 m2) = Group (Add m1 m2)
forceTerm (Sub m1 m2) = Group (Sub m1 m2)
forceTerm (Neg m)     = Group (Neg m)
forceTerm f           = f

forceFact :: ME -> ME
forceFact (Add m1 m2) = Group (Add m1 m2)
forceFact (Sub m1 m2) = Group (Sub m1 m2)
forceFact (Neg m)     = Group (Neg m)
forceFact (Mul m1 m2) = Group (Mul m1 m2)
forceFact f           = f

forceEl :: ME -> ME
forceEl (Add m1 m2) = Group (Add m1 m2)
forceEl (Sub m1 m2) = Group (Sub m1 m2)
forceEl (Neg m)     = Group (Neg m)
forceEl (Mul m1 m2) = Group (Mul m1 m2)
forceEl (Pow m n)   = Group (Pow m n)
forceEl f           = f

-- eg. Add is <expr> '+' <term>
-- so force m2 to be <term>
addGroups :: ME -> ME
addGroups (Add m1 m2) = Add m1 (forceTerm m2)
addGroups (Sub m1 m2) = Sub m1 (forceTerm m2)
addGroups (Neg m)     = Neg (forceSiTerm m)
addGroups (Mul m1 m2) = Mul (forceTerm m1) (forceFact m2)
addGroups (Pow m n)   = Pow (forceEl m) n
addGroups (Var f)     = Var f
addGroups (Num n)     = Num n
addGroups (Group f)   = Group f

-- Extra: Parsing
-- follows structure of parsing in RE3.hs closely

parseExp :: [Char] -> Maybe(ME, [Char])
parseSiTerm :: [Char] -> Maybe(ME, [Char])
parseTerm :: [Char] -> Maybe(ME, [Char])
parseFact :: [Char] -> Maybe(ME, [Char])
parseEl :: [Char] -> Maybe(ME, [Char])
parseBase :: [Char] -> Maybe(ME, [Char])

-- wrap Nums and Vars in a single parse function since they have the same form
parseBase [] = Nothing
parseBase (c:s)
  | (isDigit c) && elem c (map intToDigit [0..9]) = Just ((Num (digitToInt c)), s)
  | elem c ['a'..'z']                             = Just ((Var c), s)
  | otherwise                                     = Nothing

-- detect bracketed Expr
parseEl ('(':more) =
  case parseExp(more) of
    Just (me, ')':yet_more) -> Just (Group me, yet_more)
    _ -> Nothing
-- otherwise El is a Var or Num    
parseEl s = parseBase s

parseFact s =
  case parseEl(s) of
    -- if we see a star, look for another one
    Just (me, '*':more) -> sndStar (me, more)
    Just (me, more) -> Just (me, more)
    _ -> Nothing


sndStar :: (ME, [Char]) -> Maybe (ME, [Char])
sndStar (me, '*':more) = extendFact (me, more)
-- if there was only one star, it must be part of a Mul
-- leave the star in so it can be found in parseTerm 
sndStar (me, more) = Just (me,'*':more) 

-- handle stuff after **
extendFact :: (ME, [Char]) -> Maybe (ME, [Char])
extendFact (m, afterStar) =
  case parseBase(afterStar) of
    Just (Num n, more) -> Just (Pow m n, more)
    _ -> Nothing


parseTerm s =
  case parseFact(s) of
    Just (me, more) -> extendTerm (me, more)
    _ -> Nothing

-- look for more stuff
extendTerm :: (ME, [Char]) -> Maybe (ME, [Char])
-- if there's a star it's Mul
extendTerm (m1, '*':afterStar) =
  case parseFact(afterStar) of
    Just (m2, more) -> extendTerm (Mul m1 m2, more)
    _ -> Just (m1, afterStar)
extendTerm (m1, more) = Just (m1, more)    

-- if we see a '-' first
parseSiTerm ('-':more) =
  case parseTerm(more) of
    Just (m, yet_more) -> Just (Neg m, yet_more)
    _ -> Nothing
-- if just a normal term
parseSiTerm s = parseTerm s

parseExp s =
  case parseSiTerm(s) of
    Just (m, more_chars) -> extendExp(m, more_chars)
    _ -> Nothing

extendExp :: (ME, [Char]) -> Maybe (ME, [Char])
extendExp (m1, []) = Just (m1, [])
extendExp (m1, '+':after_plus) =
  case parseTerm(after_plus) of
    Just (m2, more) -> extendExp(Add m1 m2, more)
    _ -> Nothing
extendExp (m1, '-':after_minus) =
  case parseTerm(after_minus) of
    Just (m2, more) -> extendExp(Sub m1 m2, more)
    _ -> Nothing
extendExp (m1, c:more) = Just (m1, c:more)    

parseME :: [Char] -> Maybe ME
parseME s =
  case parseExp s of
    Just (m, []) -> Just m
    _ -> Nothing
