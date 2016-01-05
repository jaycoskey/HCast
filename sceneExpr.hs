module SceneExpr where

-- Note: Contains some sample code from wiki.haskell.org.

-- First steps toward an expression parser to be used in a ray tracer scene file
-- Note: This will not be used for the entire scene file --- only certain exprs.
-- For details on using Parsec, see
--   http://wiki.haskell.org/Parsing_a_simple_imperative_language

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- TODO: Add ray-tracer-specific language features

-- Boolean, arithmetic, and relational operators
-- Bool  ::= true | false | not b | b opBool b | a opRel a
--     opBool  ::= and | or
--     opRel   ::= < | <= | > | >= | == | !=
--
-- Arith ::= x | n | - a | a opArith a
--     opArith ::= + | - | * | /
--
-- Statements
--     S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S

-- Binary booloan operators
data BoolBinOp = And | Or deriving (Show)

-- Booloan expressions
data BoolExpr = BoolConst Bool
              | Not BoolExpr
              | BoolBinary BoolBinOp BoolExpr  BoolExpr
              | RelBinary  RelBinOp  ArithExpr ArithExpr
              deriving (Show)

-- Arithmetic operators
data ArithBinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

-- Arithmetic expressions
data ArithExpr = Var String
               | IntConst Integer
               | Neg ArithExpr
               | ArithBinary ArithBinOp ArithExpr ArithExpr
               deriving (Show)

-- Relational operators
data RelBinOp = LessThan
              | LessThanOrEqualTo
              | GreaterThan
              | GreaterThanOrEqualTo
              | EqualTo
              | NotEqualTo
              deriving (Show)

-- Statements
data Stmt = Seq [Stmt]
          | Assign String ArithExpr
          | If BoolExpr Stmt Stmt
          | While BoolExpr Stmt
          | Skip
          deriving (Show)

-- Note: The constructor emptyDef is from Text.ParserCombinators.Parsec.Language

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", ":="
                                     , "<", "<=", ">", ">=", "==", "!="
                                     , "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
                                    -- then uses arg to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semicolon  = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt
 
sequenceOfStmt =
  do list <- (sepBy1 statement' semicolon)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- boolExpr
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
 
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- boolExpr
     reserved "do"
     stmt <- statement
     return $ While cond stmt
 
assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- arithExpr
     return $ Assign var expr
 
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

arithExpr :: Parser ArithExpr
arithExpr = buildExpressionParser arithOperators arithTerm
 
boolExpr :: Parser BoolExpr
boolExpr = buildExpressionParser boolOperators boolTerm

arithOperators =
    [ [ Prefix (reservedOp "-"   >> return (Neg                 ))
      ]
    , [ Infix  (reservedOp "*"   >> return (ArithBinary Multiply)) AssocLeft
      , Infix  (reservedOp "/"   >> return (ArithBinary Divide  )) AssocLeft
      ]
    , [ Infix  (reservedOp "+"   >> return (ArithBinary Add     )) AssocLeft
      , Infix  (reservedOp "-"   >> return (ArithBinary Subtract)) AssocLeft
      ]
    ]

boolOperators =
    [ [ Prefix (reservedOp "not" >> return (Not                ))
      ]
    , [ Infix  (reservedOp "and" >> return (BoolBinary And     )) AssocLeft
      , Infix  (reservedOp "or"  >> return (BoolBinary Or      )) AssocLeft
      ]
    ]

arithTerm =   parens arithExpr
          <|> liftM Var identifier
          <|> liftM IntConst integer

boolTerm =   parens boolExpr
         <|> (reserved "true"  >> return (BoolConst True ))
         <|> (reserved "false" >> return (BoolConst False))
         <|> relExpr

relExpr =
  do a1 <- arithExpr
     op <- relOpExpr
     a2 <- arithExpr
     return $ RelBinary op a1 a2
 
relOpExpr =   (reservedOp "<" >> return LessThan)
          <|> (reservedOp ">" >> return LessThanOrEqualTo)
          <|> (reservedOp ">" >> return GreaterThan)
          <|> (reservedOp ">" >> return GreaterThanOrEqualTo)
          <|> (reservedOp ">" >> return EqualTo)
          <|> (reservedOp ">" >> return NotEqualTo)

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
 
parseFile :: String -> IO Stmt
parseFile file =
  do program <- readFile file
     case parse whileParser "" program of
       Left  e -> print e >> fail "parse error"
       Right r -> return r

-- ast <- parseFile "<filename>"

-- main :: IO ()
-- main = do
    -- putStrLn "Hello, world"
    -- putStrLn $ show $ parseString "x:=hello"