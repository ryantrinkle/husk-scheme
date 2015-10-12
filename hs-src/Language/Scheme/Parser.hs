{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Language.Scheme.Parser
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module implements parsing of Scheme code.
-}

module Language.Scheme.Parser 
    (
      lispDef
    -- *Higher level parsing
    , mainParser
    , readOrThrow
    , readExpr
    , readExprList 
    -- *Low level parsing
    , parseExpr 
    , parseAtom
    , parseBool
    , parseChar
    , parseOctalNumber 
    , parseBinaryNumber
    , parseHexNumber
    , parseDecimalNumber
    , parseNumber 
    , parseRealNumber
    , parseRationalNumber 
    , parseComplexNumber 
    , parseEscapedChar 
    , parseString 
    , parseVector
    , parseByteVector
    , parseHashTable
    , parseList
    , parseDottedList
    , parseQuoted
    , parseQuasiQuoted 
    , parseUnquoted 
    , parseUnquoteSpliced 
    ) where
import Language.Scheme.Types
import Control.Monad.Except
import Data.Array
import qualified Data.ByteString as BS
import qualified Data.Char as DC
import Data.Complex
import qualified Data.Map
import Data.Ratio
import Data.Word
import Numeric
import Text.Parsec hiding (Parser, spaces)
import Text.Parsec.Text
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
#if __GLASGOW_HASKELL__ >= 702
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)
#endif
import Data.Text (Text)
import qualified Data.Text as T

-- This was added by pull request #63 as part of a series of fixes
-- to get husk to build on ghc 7.2.2
--
-- For now this has been removed to allow husk to support the older
-- GHC 6.x.x series.
--
--import Data.Functor.Identity (Identity)

-- |Language definition for Scheme
lispDef :: GenLanguageDef Text () Identity
lispDef 
  = P.LanguageDef
  { P.commentStart   = "#|"
  , P.commentEnd     = "|#"
  , P.commentLine    = ";"
  , P.nestedComments = True
  , P.identStart     = letter <|> symbol
  , P.identLetter    = letter <|> digit <|> symbol
  , P.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.reservedOpNames = []
  , P.reservedNames  = []
  , P.caseSensitive  = True
  } 

#if __GLASGOW_HASKELL__ >= 702
lexer :: P.GenTokenParser Text () Data.Functor.Identity.Identity
#endif
lexer = P.makeTokenParser lispDef

#if __GLASGOW_HASKELL__ >= 702
dot :: ParsecT Text () Identity Text
#endif
dot = liftM T.pack $ P.dot lexer

#if __GLASGOW_HASKELL__ >= 702
parens :: ParsecT Text () Identity a -> ParsecT Text () Identity a
#endif
parens = P.parens lexer

#if __GLASGOW_HASKELL__ >= 702
brackets :: ParsecT Text () Identity a -> ParsecT Text () Identity a
#endif
brackets = P.brackets lexer

#if __GLASGOW_HASKELL__ >= 702
identifier :: ParsecT Text () Identity Text
#endif
identifier = liftM T.pack $ P.identifier lexer

#if __GLASGOW_HASKELL__ >= 702
whiteSpace :: ParsecT Text () Identity ()
#endif
whiteSpace = P.whiteSpace lexer

#if __GLASGOW_HASKELL__ >= 702
lexeme :: ParsecT Text () Identity a -> ParsecT Text () Identity a
#endif
lexeme = P.lexeme lexer

-- |Match a special character
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

-- |Parse an atom (scheme symbol)
parseAtom :: Parser (LispVal m r)
parseAtom = do
  atom <- identifier
  if atom == "."
     then parserZero -- Do not match this form
     else return $ Atom atom

-- |Parse a boolean
parseBool :: Parser (LispVal m r)
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False
                          _ -> Bool False

-- |Parse a character
parseChar :: Parser (LispVal m r)
parseChar = do
  _ <- try (string "#\\")
  c <- anyChar
  r <- many (letter <|> digit)
  case c : r of
    "space"     -> return $ Char ' '
    "newline"   -> return $ Char '\n'
    "alarm"     -> return $ Char '\a' 
    "backspace" -> return $ Char '\b' 
    "delete"    -> return $ Char '\DEL'
    "escape"    -> return $ Char '\ESC' 
    "null"      -> return $ Char '\0' 
    "return"    -> return $ Char '\n' 
    "tab"       -> return $ Char '\t'
    [ch] -> return $ Char ch
    'x' : hexs -> do
        rv <- parseHexScalar hexs
        return $ Char rv
    _ -> parserZero

-- |Parse an integer in octal notation, base 8
parseOctalNumber :: Parser (LispVal m r)
parseOctalNumber = do
  _ <- try (string "#o")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01234567")
  case (length sign) of
     0 -> return $ Number $ fst $ head (Numeric.readOct num)
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readOct num)
     _ -> parserZero

-- |Parse an integer in binary notation, base 2
parseBinaryNumber :: Parser (LispVal m r)
parseBinaryNumber = do
  _ <- try (string "#b")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01")
  case (length sign) of
     0 -> return $ Number $ fst $ head (Numeric.readInt 2 (`elem` ("01" :: String)) DC.digitToInt num)
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readInt 2 (`elem` ("01" :: String)) DC.digitToInt num)
     _ -> parserZero

-- |Parse an integer in hexadecimal notation, base 16
parseHexNumber :: Parser (LispVal m r)
parseHexNumber = do
  _ <- try (string "#x")
  sign <- many (oneOf "-")
  num <- many1 (digit <|> oneOf "abcdefABCDEF")
  case (length sign) of
     0 -> return $ Number $ fst $ head (Numeric.readHex num)
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readHex num)
     _ -> parserZero

-- |Parser for Integer, base 10
parseDecimalNumber :: Parser (LispVal m r)
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 digit
  if (length sign) > 1
     then parserZero
     else return $ (Number . read) $ sign ++ num

-- |Parser for a base 10 Integer that will also
--  check to see if the number is followed by
--  an exponent (scientific notation). If so,
--  the integer is converted to a float of the
--  given magnitude.
parseDecimalNumberMaybeExponent :: Parser (LispVal m r)
parseDecimalNumberMaybeExponent = do
  num <- parseDecimalNumber
  parseNumberExponent num

-- |Parse an integer in any base
parseNumber :: Parser (LispVal m r)
parseNumber = parseDecimalNumberMaybeExponent <|>
              parseHexNumber <|>
              parseBinaryNumber <|>
              parseOctalNumber <?>
              "Unable to parse number"

-- |Parse a floating point number
parseRealNumber :: Parser (LispVal m r)
parseRealNumber = do
  sign <- many (oneOf "-+")
  num <- many digit
  _ <- char '.'
  frac <- many1 digit
  let dec = if not (null num)
               then num ++ "." ++ frac
               else "0." ++ frac
  f <- case (length sign) of
     0 -> return $ Float $ fst $ head (Numeric.readFloat dec)
          -- Bit of a hack, but need to support the + sign as well as the minus.
     1 -> if sign == "-" 
             then return $ Float $ (*) (-1.0) $ fst $ head (Numeric.readFloat dec)
             else return $ Float $ fst $ head (Numeric.readFloat dec)
     _ -> parserZero
  parseNumberExponent f

-- | Parse the exponent section of a floating point number
--   in scientific notation. Eg "e10" from "1.0e10"
parseNumberExponent :: LispVal m r -> Parser (LispVal m r)
parseNumberExponent n = do 
  expnt <- many $ oneOf "Ee"
  case (length expnt) of
    0 -> return n
    1 -> do
      num <- try parseDecimalNumber
      case num of
        Number nexp -> buildResult n nexp
        _ -> parserZero
    _ -> parserZero
 where 
  buildResult (Number num) nexp = return $ Float $ (fromIntegral num) * (10 ** (fromIntegral nexp))
  buildResult (Float num) nexp = return $ Float $ num * (10 ** (fromIntegral nexp))
  buildResult _ _ = parserZero

-- |Parse a rational number
parseRationalNumber :: Parser (LispVal m r)
parseRationalNumber = do
  pnumerator <- parseDecimalNumber
  case pnumerator of
    Number n -> do
      _ <- char '/'
      sign <- many (oneOf "-")
      num <- many1 digit
      if (length sign) > 1
         then parserZero
         else do
             let pdenominator = read $ sign ++ num
             if pdenominator == 0
                then return $ Number 0 -- TODO: Prevents a div-by-zero error, but not really correct either
                else return $ Rational $ n % pdenominator
    _ -> parserZero

-- |Parse a complex number
parseComplexNumber :: Parser (LispVal m r)
parseComplexNumber = do
  lispreal <- (try parseRealNumber <|> try parseRationalNumber <|> parseDecimalNumber)
  let real = case lispreal of
                  Number n -> fromInteger n
                  Rational r -> fromRational r
                  Float f -> f
                  _ -> 0
  _ <- char '+'
  lispimag <- (try parseRealNumber <|> try parseRationalNumber <|> parseDecimalNumber)
  let imag = case lispimag of
                  Number n -> fromInteger n
                  Rational r -> fromRational r
                  Float f -> f
                  _ -> 0 -- Case should never be reached
  _ <- char 'i'
  return $ Complex $ real :+ imag

-- |Parse an escaped character
parseEscapedChar :: forall st .
                    GenParser st Char
parseEscapedChar = do
  _ <- char '\\'
  c <- anyChar
  case c of
    'a' -> return '\a'
    'b' -> return '\b'
    'n' -> return '\n'
    't' -> return '\t'
    'r' -> return '\r'
    'x' -> do
        num <- many $ letter <|> digit
        _ <- char ';'
        parseHexScalar num
    _ -> return c

-- |Parse a hexidecimal scalar
parseHexScalar :: Monad m => String -> m Char
parseHexScalar num = do
    let ns = Numeric.readHex num
    case ns of
        [] -> fail $ "Unable to parse hex value " ++ show num
        _ -> return $ DC.chr $ fst $ head ns

-- |Parse a string
parseString :: Parser (LispVal m r)
parseString = do
  _ <- char '"'
  x <- many (parseEscapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ Text $ T.pack x

-- |Parse a vector
parseVector :: Parser (LispVal m r)
parseVector = do
  vals <- sepBy parseExpr whiteSpace
  return $ Vector (listArray (0, (length vals - 1)) vals)

-- |Parse a bytevector
parseByteVector :: Parser (LispVal m r)
parseByteVector = do
  ns <- sepBy parseNumber whiteSpace
  return $ ByteVector $ BS.pack $ map conv ns
 where 
   conv (Number n) = fromInteger n :: Word8
   conv _ = 0 :: Word8

-- |Parse a hash table. The table is either empty or is made up of
--  an alist (associative list)
parseHashTable :: Parser (LispVal m r)
parseHashTable = do
  -- This function uses explicit recursion to loop over the parsed list:
  -- As long as it is an alist, the members are appended to an accumulator
  -- so they can be added to the hash table. However, if the input list is
  -- determined not to be an alist, Nothing is returned, letting the parser
  -- know that a valid hashtable was not read.
  let f :: [(LispVal m r, LispVal m r)] -> [LispVal m r] -> Maybe [(LispVal m r, LispVal m r)]
      f acc [] = Just acc
      f acc (List [a, b] :ls) = f (acc ++ [(a, b)]) ls
      f acc (DottedList [a] b :ls) = f (acc ++ [(a, b)]) ls
      f _ (_:_) = Nothing
  vals <- sepBy parseExpr whiteSpace
  let mvals = f [] vals
  case mvals of
    Just m -> return $ HashTable $ Data.Map.fromList m
    Nothing -> parserZero

-- |Parse a list
parseList :: Parser (LispVal m r)
parseList = liftM List $ sepBy parseExpr whiteSpace
-- TODO: wanted to use endBy (or a variant) above, but it causes an error such that dotted lists are not parsed

-- |Parse a dotted list (scheme pair)
parseDottedList :: Parser (LispVal m r)
parseDottedList = do
  phead <- endBy parseExpr whiteSpace
  case phead of
    [] -> parserZero -- car is required; no match   
    _ -> do
      ptail <- dot >> parseExpr
      case ptail of
        DottedList ls l -> return $ DottedList (phead ++ ls) l 
        -- Issue #41
        -- Improper lists are tricky because if an improper list ends in a 
        -- proper list, then it becomes proper as well. The following cases 
        -- handle that, as well as preserving necessary functionality when 
        -- appropriate, such as for unquoting.
        --
        -- FUTURE: I am not sure if this is complete, in fact the "unquote" 
        -- seems like it could either be incorrect or one special case among 
        -- others. Anyway, for the 3.3 release this is good enough to pass all
        -- test cases. It will be revisited later if necessary.
        --
        List (Atom "unquote" : _) -> return $ DottedList phead ptail 
        List ls -> return $ List $ phead ++ ls
        {- Regarding above, see 
           http://community.schemewiki.org/?scheme-faq-language#dottedapp
         
           Note, however, that most Schemes expand literal lists occurring in 
           function applications, e.g. (foo bar . (1 2 3)) is expanded into 
           (foo bar 1 2 3) by the reader. It is not entirely clear whether this 
           is a consequence of the standard - the notation is not part of the 
           R5RS grammar but there is strong evidence to suggest a Scheme 
           implementation cannot comply with all of R5RS without performing this
           transformation. -}
        _ -> return $ DottedList phead ptail

-- |Parse a quoted expression
parseQuoted :: Parser (LispVal m r)
parseQuoted = do
  _ <- lexeme $ char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- |Parse a quasi-quoted expression
parseQuasiQuoted :: Parser (LispVal m r)
parseQuasiQuoted = do
  _ <- lexeme $ char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

-- |Parse an unquoted expression (a quasiquotated expression preceded
--  by a comma)
parseUnquoted :: Parser (LispVal m r)
parseUnquoted = do
  _ <- try (lexeme $ char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- |Parse an unquote-spliced expression
parseUnquoteSpliced :: Parser (LispVal m r)
parseUnquoteSpliced = do
  _ <- try (lexeme $ string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

-- FUTURE: should be able to use the grammar from R5RS
-- to make parsing more efficient (mostly by minimizing
-- or eliminating the number of try's below)

-- |Parse an expression
parseExpr :: Parser (LispVal m r)
parseExpr =
      try (lexeme parseComplexNumber)
  <|> try (lexeme parseRationalNumber)
  <|> try (lexeme parseRealNumber)
  <|> try (lexeme parseNumber)
  <|> lexeme parseChar
  <|> parseUnquoteSpliced
  <|> do _ <- try (lexeme $ string "#(")
         x <- parseVector
         _ <- lexeme $ char ')'
         return x
  <|> do _ <- try (lexeme $ string "#u8(")
         x <- parseByteVector
         _ <- lexeme $ char ')'
         return x
--  <|> do _ <- try (lexeme $ string "#hash(")
--         x <- parseHashTable
--         _ <- lexeme $ char ')'
--         return x
  <|> try parseAtom
  <|> lexeme parseString
  <|> lexeme parseBool
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnquoted
  <|> try (parens parseList)
  <|> parens parseDottedList
  <|> try (brackets parseList)
  <|> brackets parseDottedList
  <?> "Expression"

-- |Initial parser used by the high-level parse functions
mainParser :: Parser (LispVal m r)
mainParser = do
    _ <- whiteSpace
    parseExpr

-- |Use a parser to parse the given text, throwing an error
--  if there is a problem parsing the text.
readOrThrow :: Parser a -> Text -> ThrowsError m r a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- |Parse an expression from a string of text
readExpr :: Text -> ThrowsError m r (LispVal m r)
readExpr = readOrThrow mainParser

-- |Parse many expressions from a string of text
readExprList :: Text -> ThrowsError m r [LispVal m r]
readExprList = readOrThrow (endBy mainParser whiteSpace)

