module CSE230.Parse where

import Text.Parsec hiding (State, between)
import Text.Parsec.String
import qualified CSE230.Types as H

{- As you can see, it is rather tedious to write the above tests! 
   They correspond to the code in the files `test.imp` and `fact.imp`. 
   It is rather tedious to have to specify individual programs as Haskell
   values. For this problem, you will use parser combinators to build a parser
   for the WHILE language from the previous problem.
-}

parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p s = runParser p () "DUMMY" s

-- >>> parseFromString varP "X45"
-- Right "X"
--

-------------------------------------------------------------------------------
-- | Parsing Constants
-------------------------------------------------------------------------------

-- First, we will write parsers for the `Value` type

valueP :: Parser H.Value
valueP = intP <|> boolP

-- First, fill in the implementation of `intP`. You can assume that the numbers
-- in our language are non-negative.

intP :: Parser H.Value
intP = do
         n <- many1 digit
         return (H.IntVal (read n))

-- Next, define a parser that will accept a particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x = do
               string s
               return x

-- Use the above to define a parser for boolean values 
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser H.Value
boolP = constP "true" (H.BoolVal True) <|> constP "false" (H.BoolVal True)

-- Continue to use the above to parse the binary operators

opP :: Parser H.Bop
opP =     constP "+"  H.Plus
      <|> constP "-"  H.Minus
      <|> constP "*"  H.Times
      <|> constP "/"  H.Divide
      <|> constP ">"  H.Gt
      <|> constP ">=" H.Ge
      <|> constP "<"  H.Lt
      <|> constP "<=" H.Le

-------------------------------------------------------------------------------
-- | Parsing Expressions 
-------------------------------------------------------------------------------

-- The following is a parser for variables, which are one-or-more uppercase letters. 

varP :: Parser H.Variable
varP = many1 upper

-- Use the above to write a parser for `Expression` values. Assume that
-- operators are right associative, and they all have the same precedence.

exprP :: Parser H.Expression
exprP   = choice [try exprOp, exprVal, exprVar]

exprVal :: Parser H.Expression
exprVal = do
            H.Val <$> valueP

exprVar :: Parser H.Expression
exprVar = do
            H.Var <$> varP

exprOp :: Parser H.Expression
exprOp = do
            x <- exprVar <|> exprVal <|> parenthesesP exprOp
            spaceP
            o <- opP
            spaceP
            y <- exprVar <|> exprVal <|> parenthesesP exprOp
            return (H.Op o x y)

spaceP :: Parser ()
spaceP = skipMany space

parenthesesP :: Parser p -> Parser p
parenthesesP p = do
                    string "("
                    r <- p
                    string ")"
                    return r

-------------------------------------------------------------------------------
-- | Parsing Statements 
-------------------------------------------------------------------------------

-- Next, use the expression parsers to build a statement parser

statementP :: Parser H.Statement
statementP = choice [try sequenceP, assignP, ifP, whileP, skipP]

assignP :: Parser H.Statement
assignP = do
            spaceP
            x <- varP
            spaceP
            string ":="
            spaceP
            H.Assign x <$> exprP

ifP :: Parser H.Statement
ifP = do
            spaceP
            string "if"
            spaceP
            e <- exprP
            spaceP
            string "then"
            spaceP
            s1 <- statementP
            spaceP
            string "else"
            spaceP
            s2 <- statementP
            spaceP
            string "endif"
            spaceP
            return (H.If e s1 s2)

whileP :: Parser H.Statement
whileP = do
            spaceP
            string "while"
            spaceP
            e <- exprP
            spaceP
            string "do"
            spaceP
            s <- statementP
            spaceP
            string "endwhile"
            spaceP
            return (H.While e s)

sequenceP :: Parser H.Statement
sequenceP = do
              spaceP
              s1 <- assignP <|> ifP <|> whileP <|> skipP
              string ";"
              spaceP
              H.Sequence s1 <$> statementP


skipP :: Parser H.Statement
skipP = do
            spaceP
            string "skip"
            return H.Skip


-- When you are done, we can put the parser and evaluator together 
-- in the end-to-end interpreter function `runFile` in `Main.hs`

-- | Parsing Files 

-------------------------------------------------------------------------------
parseFile :: FilePath -> IO (Either ParseError H.Statement)
-------------------------------------------------------------------------------
-- >>> ((Right H.w_fact) ==) <$> parseFile "test/in/fact.imp"
-- True
-- >>> ((Right H.w_test) == ) <$> parseFile "test/in/test.imp"
-- True
-- >>> ((Right H.w_abs) == ) <$> parseFile "test/in/abs.imp"
-- True
-- >>> ((Right H.w_times) == ) <$> parseFile "test/in/times.imp"
-- True


parseFile f = parseFromFile statementP f
