data V = X | Y | Z

instance Show V
  where show X = "x"
        show Y = "y"
        show Z = "z"

instance Eq V
  where (==) X X = True
        (==) Y Y = True
        (==) Z Z = True
        (==) _ _ = False

data Expr = Const Int | Var V | Join Expr Op Expr | Func Fn Expr
data Op = Add | Sub | Pow | Mul | Div deriving (Eq, Show)
data Fn = Ln | Sin | Cos | Tg | Exp deriving (Eq, Show)

instance Show Expr
  where
        show (Const a) = show a
        show (Var a) = show a
        show (Join a Add b) = "("++(show a) ++ "+" ++ (show b)++")"
        show (Join a Sub b) = "("++(show a) ++ "-" ++ (show b)++")"
        show (Join a Pow b) = "("++(show a) ++ "^" ++ (show b)++")"
        show (Join a Mul b) =  "("++(show a) ++ "*" ++ (show b)++")"
        show (Join a Div b) = "("++(show a) ++ "/" ++ (show b)++")"
        show (Func Ln a) = "ln(" ++ (show a) ++ ")"
        show (Func Sin a) = "sin(" ++ (show a) ++ ")"
        show (Func Cos a) = "cos(" ++ (show a) ++ ")"
        show (Func Tg a) = "tg(" ++ (show a) ++ ")"
        show (Func Exp a) = "exp(" ++ (show a) ++ ")"


instance Eq Expr
  where
        (==) (Const a) (Const b) = a == b
        (==) (Var a) (Var b) = a == b
        (==) (Join a b c) (Join d e f) = (a == d) && (b == e) && (c == f)
        (==) (Func a b) (Func c d) = (a == c) && (b == d)
        (==) _ _ = False


-- one iteration of clean up
minim :: Expr -> Expr
minim (Join (Const a) Add (Const b)) = Const (a+b)
minim (Join (Const a) Sub (Const b)) = Const (a-b)
minim (Join (Const a) Mul (Const b)) = Const (a*b)
minim (Join (Const a) Div (Const b)) = Const (div a b)
minim (Join (Const a) Pow (Const b)) = Const (a^b)

minim (Join (Const 0) Add a) = minim a
minim (Join (Const 0) Mul _) = Const 0
minim (Join (Const 0) Sub a) = Join (Const (-1)) Mul (minim a)
minim (Join (Const 0) Div _) = Const 0
minim (Join (Const 0) Pow _) = Const 0

minim (Join _ Pow (Const 0)) = Const 1
minim (Join a Add (Const 0)) = minim a
minim (Join a Mul (Const 0)) = Const 0
minim (Join a Sub (Const 0)) = minim a

minim (Join (Const 1) Mul a) = minim a
minim (Join a Mul (Const 1)) = minim a
minim (Join a Pow (Const 1)) = minim a
minim (Join (Const 1) Pow _) = Const 1

minim (Join a Mul (Const b)) = Join (Const b) Mul a
minim (Join a Mul (Join b Div c)) = Join (Join a Mul b) Div c

minim (Join a b c) = Join (minim a) b (minim c)

minim (Func Ln (Const 1)) = Const 0
minim (Func Exp (Const 0)) = Const 1
minim (Func a b) = Func a (minim b)
minim a = a

-- helper function to "clean up" results a bit - evaluate operations where possible
minimal :: Expr -> Expr
--minimal a = head (dropWhile (\x -> x /= (minim x) ) (iterate minim a))
--minimal a = head (((dropWhile (\x -> x /= (minim x))) . (iterate minim)) a)
--minimal a = (head . ((dropWhile (\x -> x /= (minim x))) . (iterate minim))) a
minimal = head . ((dropWhile (\x -> x /= (minim x))) . (iterate minim))


-- partial derivative of V
der :: V -> Expr -> Expr
der x (Const a) = (Const 0)
der x (Var y) | x == y = Const 1
              | otherwise = Const 0
der x (Join a Add b) = Join (der x a) Add (der x b)
der x (Join a Sub b) = Join (der x a) Sub (der x b)

der x (Join (Const a) Pow (Var y)) | x == y = Join (Join (Const a) Pow (Var x)) Mul (Func Ln (Const a))
                                   | otherwise = Const 0
der x (Join (Var y) Pow (Const a)) | x == y = Join (Const a) Mul (Join (Var x) Pow (Const (a-1)))
                                   | otherwise = Const 0
der x (Join a Pow b) = Const 0
der x (Join (Var y) Mul (Const b)) | x == y = Const b
                                   | otherwise = Const 0
der x (Join (Const a) Mul (Var y)) | x == y = Const a
                                   | otherwise = Const 0
der x (Join a Mul b) = Join (Join (der x a) Mul b) Add (Join a Mul (der x b))
der x (Join a Div b) = Join (Join (Join (der x a) Mul b) Sub (Join a Mul (der x b))) Div (Join b Pow (Const 2))
der x (Func Ln a) = Join (Join (Const 1) Div a) Mul (der x a)
der x (Func Sin a) = Join (Func Cos a) Mul (der x a)
der x (Func Cos a) = Join (Join (Const (-1)) Mul (Func Sin a)) Mul (der x a)
der x (Func Tg a) = Join (Join (Const 1) Div (Join (Func Cos a) Pow (Const 2))) Mul (der x a)
der x (Func Exp a) = Join (Func Exp a) Mul (der x a)


mapVar :: String -> Expr
mapVar "x" = Var X
mapVar "y" = Var Y
mapVar "z" = Var Z
mapVar _ = error "invalid variable"

mapOp :: String -> Op
mapOp "+" = Add
mapOp "-" = Sub
mapOp "*" = Mul
mapOp "/" = Div
mapOp "^" = Pow
mapOp _ = error "invalid operator"

mapFn :: String -> Fn
mapFn "sin" = Sin
mapFn "cos" = Cos
mapFn "tg" = Tg
mapFn "exp" = Exp
mapFn "ln" = Ln
mapFn _ = error "invalid function"

isVar :: String -> Bool
isVar "x" = True
isVar "y" = True
isVar "z" = True
isVar _ = False

isFunc :: String -> Bool
isFunc "sin" = True
isFunc "cos" = True
isFunc "tg" = True
isFunc "exp" = True
isFunc "ln" = True
isFunc _ = False


-- operator's precedence
prOp :: String -> Int
prOp "+" = 2
prOp "-" = 2
prOp "*" = 3
prOp "/" = 3
prOp "^" = 3

-- is operator's associativity = left
leftAs :: String -> Bool
leftAs "+" = True
leftAs "-" = True
leftAs "*" = True
leftAs "/" = True
leftAs "^" = False

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

-- helper function to extract number from the string (as in continuous digits)
number :: String -> String
number [] = []
number (x:xs) | isDigit x = x:(number xs)
              | otherwise = []

-- helper function parsing String with equation into an array of parts - variables, numbers, functions and operators
toArray :: String -> [String]
toArray ('x':xs) = ["x"] ++ (toArray xs)
toArray ('y':xs) = ["y"] ++ (toArray xs)
toArray ('z':xs) = ["z"] ++ (toArray xs)
toArray ('(':xs) = ["("] ++ (toArray xs)
toArray (')':xs) = [")"] ++ (toArray xs)
toArray ('+':xs) = ["+"] ++ (toArray xs)
toArray ('-':xs) = ["-"] ++ (toArray xs)
toArray ('*':xs) = ["*"] ++ (toArray xs)
toArray ('/':xs) = ["/"] ++ (toArray xs)
toArray ('^':xs) = ["^"] ++ (toArray xs)
toArray ('s':'i':'n':xs) = ["sin"] ++ (toArray xs)
toArray ('c':'o':'s':xs) = ["cos"] ++ (toArray xs)
toArray ('t':'g':xs) = ["tg"] ++ (toArray xs)
toArray ('e':'x':'p':xs) = ["exp"] ++ (toArray xs)
toArray ('l':'n':xs) = ["ln"] ++ (toArray xs)
toArray (x:xs) | isDigit x =  [number (x:xs)] ++ (toArray (drop (length (number (x:xs))) (x:xs)))
         | otherwise = error "invalid equation"
toArray _ = []

-- pop operators up to left parenthesis
popOpsP :: [String] -> [String]
popOpsP [] = error "invalid expression"
popOpsP (x:xs) | x == "(" = []
               | otherwise = x:(popOpsP xs)

-- pop operators for given new operator
popOpsO :: [String] -> String -> [String]
popOpsO [] op = []
popOpsO (x:xs) op | isFunc x = x:(popOpsO xs op)
                  | x == "(" = []
                  | (prOp x) > (prOp op) = x:(popOpsO xs op)
                  | ((prOp x) == (prOp op)) && (leftAs op) = x:(popOpsO xs op)
                  | otherwise = []

-- infix to rpn: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
-- equation string, operator stack, output
rpn :: [String] -> [String] -> [String]
rpn [] [] = []
rpn [] (q:qs) | q == "(" = error "inavlid expression: mismatched brackets"
              | otherwise = q:(rpn [] qs)


rpn (x:xs) q | ((isVar x) || (isDigit (head x))) = x:(rpn xs q)
             | isFunc x = rpn xs (x:q)
             | x == "(" = rpn xs (x:q)
             | x == ")" = (popOpsP q) ++ (rpn xs (drop (length (popOpsP q) +1) q))
             | otherwise = (popOpsO q x) ++ (rpn xs (x:(drop (length (popOpsO q x)) q)))


-- map equation in rpn to expr data structure
toExpr :: [String] -> [Expr] -> [Expr]
toExpr [] s | length s > 1 = error "invalid expression: missing operator"
            | otherwise = s

toExpr (x:xs) [] | isVar x = toExpr xs [mapVar x]
                 | isDigit (head x) = toExpr xs [Const (read x :: Int)]
                 | otherwise = error "invalid expression: missing variable"

toExpr (x:xs) [q] | isVar x = toExpr xs ((mapVar x):[q])
                     | isDigit (head x) = toExpr xs ((Const (read x :: Int)):[q])
                     | isFunc x = toExpr xs [Func (mapFn x) q]
                     | otherwise = error "invalid expression: missing variable"

toExpr (x:xs) (q:qs) | isVar x = toExpr xs ((mapVar x):q:qs)
                     | isDigit (head x) = toExpr xs ((Const (read x :: Int)):q:qs)
                     | isFunc x = toExpr xs ((Func (mapFn x) q):qs)
                     | otherwise = toExpr xs ((Join (head qs) (mapOp x) q):(tail qs))


-- parse String with equation to Expr data structure
parse :: String -> Expr
--parse s = head (toExpr (rpn (toArray s) []) [])
--parse s = head (toExpr ((flip rpn [] . toArray) s) [])
--parse s = head (((flip toExpr []) . (flip rpn [] . toArray)) s)
--parse s = (head . (((flip toExpr []) . (flip rpn [] . toArray)))) s
parse = head . (((flip toExpr []) . (flip rpn [] . toArray)))


main = do
         putStrLn "Welcome to the Partial Derivative Calculator!"
         putStrLn "Supported operators: +, -, /, *, ^."
         putStrLn "Supported functions: sin, cos, tg, ln, exp."
         putStrLn "Supported variables: x, y, z."
         putStrLn "Please insert your equation:"
         eq<-getLine
         putStr $ "df/dx = "
         putStrLn $ show (minimal (der X (parse eq)))
         putStr $ "df/dy = "
         putStrLn $ show (minimal (der Y (parse eq)))
         putStr $ "df/dz = "
         putStrLn $ show (minimal (der Z (parse eq)))