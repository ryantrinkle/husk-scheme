{-# LANGUAGE CPP #-}

{- |
Module      : Language.Scheme.Compiler.Types
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains data types used by the compiler.
-}

module Language.Scheme.Compiler.Types 
    (
    -- * Data types
      CompOpts (..)
    , CompLibOpts (..)
    , defaultCompileOptions
    , HaskAST (..)
    -- * Utility functions
    , ast2Str
    , asts2Str
    , createAstFunc 
    , createAstCont 
    , joinL 
    , moduleRuntimeVar
    , showValAST
    -- * Headers appended to output file
    , header
    , headerComment
    , headerModule
    , headerImports
    )
where 
import qualified Language.Scheme.Core as LSC (version) 
import Language.Scheme.Types
import qualified Language.Scheme.Util (escapeBackslashes)
import qualified Data.Array
import qualified Data.ByteString as BS
import qualified Data.Complex as DC
import qualified Data.List
import qualified Data.Map
import qualified Data.Ratio as DR

-- |A type to store options passed to compile.
--  Eventually all of this might be able to be 
--  integrated into a Compile monad.
data CompOpts = CompileOptions {
    coptsThisFunc :: String,        
    -- ^Immediate name to use when creating a compiled function.
    --  Presumably there is other code that is expecting
    --  to call into it.

    coptsThisFuncUseValue :: Bool,
    -- ^Whether to include the /value/ parameter in the current function
    
    coptsThisFuncUseArgs :: Bool,
    -- ^Whether to include the /args/ parameter in the current function
    
    coptsNextFunc :: Maybe String
    -- ^The name to use for the next function after the current
    --  compiler recursion is finished. For example, after compiling
    --  a block of code, the control flow would be expected to go
    --  to this function.
    }

-- |The default compiler options
defaultCompileOptions :: String -> CompOpts
defaultCompileOptions thisFunc = CompileOptions thisFunc False False Nothing

-- |Options passed to the compiler library module
data CompLibOpts m r = CompileLibraryOptions {
    compBlock :: String -> Maybe String -> Env m r 
              -> [HaskAST] -> [LispVal m r] -> IOThrowsError m r [HaskAST],
    compLisp :: Env m r -> String -> String -> Maybe String 
              -> IOThrowsError m r [HaskAST]
    }

-- |Runtime reference to module data structure
moduleRuntimeVar :: String
moduleRuntimeVar = " modules "

-- |Create code for a function
createAstFunc 
  :: CompOpts  -- ^ Compilation options
  -> [HaskAST] -- ^ Body of the function
  -> HaskAST -- ^ Complete function code
createAstFunc (CompileOptions thisFunc useVal useArgs _) funcBody = do
  let val = if useVal then "value" else "_"
      args = if useArgs then "(Just args)" else "_"
  AstFunction thisFunc (" env cont " ++ val ++ " " ++ args ++ " ") funcBody

-- |Create code for a continutation
createAstCont 
  :: CompOpts -- ^ Compilation options
  -> String -- ^ Value to send to the continuation
  -> String -- ^ Extra leading indentation (or blank string if none)
  -> HaskAST -- ^ Generated code
createAstCont (CompileOptions _ _ _ (Just nextFunc)) var indentation = do
  AstValue $ indentation ++ "  " ++ nextFunc ++ " env cont " ++ var ++ " (Just [])"
createAstCont (CompileOptions _ _ _ Nothing) var indentation = do
  AstValue $ indentation ++ "  continueEval env cont " ++ var ++ " Nothing"


--  FUTURE: is this even necessary? Would just a string be good enough?

-- |A very basic type to store a Haskell AST.
data HaskAST = AstAssignM String HaskAST
  | AstFunction {astfName :: String,
--                 astfType :: String,
                 astfArgs :: String,
                 astfCode :: [HaskAST]
                } 
 | AstValue String
 | AstRef String
 | AstContinuation {astcNext :: String,
                    astcArgs :: String
                   }

-- |Generate code based on the given Haskell AST
showValAST :: HaskAST -> String
showValAST (AstAssignM var val) = "  " ++ var ++ " <- " ++ show val
showValAST (AstFunction name args code) = do
  let typeSig = "\n" ++ name ++ " :: Env m r -> LispVal m r -> LispVal m r -> Maybe [LispVal m r] -> IOThrowsError r (LispVal m r) "
  let fheader = "\n" ++ name ++ args ++ " = do "
  let fbody = unwords . map (\x -> '\n' : x ) $ map showValAST code
#ifdef UseDebug
  let appendArg arg = do
        if Data.List.isInfixOf arg args
           then " ++ \" \" ++ " ++ (show arg) ++ 
                " ++ \" [\" ++ (show " ++ arg ++ ")" ++ 
                " ++ \"] \""
           else ""
  let fdebug = "\n  _ <- liftIO $ (trace (\"" ++ 
               name ++ "\"" ++ 
               (appendArg "value") ++ 
               (appendArg "args") ++ 
               ") getCPUTime)"
  typeSig ++ fheader ++ fdebug ++ fbody 
#else
  typeSig ++ fheader ++ fbody 
#endif
showValAST (AstValue v) = v
showValAST (AstRef v) = v
showValAST (AstContinuation nextFunc args) =
    "  continueEval env (makeCPSWArgs env cont " ++ 
       nextFunc ++ " " ++ args ++ ") (Nil \"\") Nothing "

instance Show HaskAST where show = showValAST

-- |A utility function to join list members together
joinL 
  :: forall a. [[a]] -- ^ Original list-of-lists
  -> [a] -- ^ Separator 
  -> [a] -- ^ Joined list
joinL ls sep = Data.List.intercalate sep ls

-- |Convert abstract syntax tree to a string
ast2Str :: LispVal m r -> String 
ast2Str (String s) = "String " ++ show s
ast2Str (Char c) = "Char " ++ show c
ast2Str (Atom a) = "Atom " ++ show a
ast2Str (Number n) = "Number (" ++ show n ++ ")"
ast2Str (Complex c) = "Complex $ (" ++ (show $ DC.realPart c) ++ ") :+ (" ++ (show $ DC.imagPart c) ++ ")"
ast2Str (Rational r) = "Rational $ (" ++ (show $ DR.numerator r) ++ ") % (" ++ (show $ DR.denominator r) ++ ")"
ast2Str (Float f) = "Float (" ++ show f ++ ")"
ast2Str (Bool True) = "Bool True"
ast2Str (Bool False) = "Bool False"
ast2Str (HashTable ht) = do
 let ls = Data.Map.toList ht 
     conv (a, b) = "(" ++ ast2Str a ++ "," ++ ast2Str b ++ ")"
 "HashTable $ Data.Map.fromList $ [" ++ joinL (map conv ls) "," ++ "]"
ast2Str (Vector v) = do
  let ls = Data.Array.elems v
      size = (length ls) - 1
  "Vector (listArray (0, " ++ show size ++ ")" ++ "[" ++ joinL (map ast2Str ls) "," ++ "])"
ast2Str (ByteVector bv) = do
  let ls = BS.unpack bv
  "ByteVector ( BS.pack " ++ "[" ++ joinL (map show ls) "," ++ "])"
ast2Str (List ls) = "List [" ++ joinL (map ast2Str ls) "," ++ "]"
ast2Str (DottedList ls l) = 
  "DottedList [" ++ joinL (map ast2Str ls) "," ++ "] $ " ++ ast2Str l
ast2Str l = show l -- Error?

-- |Convert a list of abstract syntax trees to a list of strings
asts2Str :: [LispVal m r] -> String
asts2Str ls = do
    "[" ++ (joinL (map ast2Str ls) ",") ++ "]"

-- |Header comment used at the top of a Haskell program generated
--  by the compiler
headerComment:: [String]
headerComment = [
   "--"
 , "-- This file was automatically generated by the husk scheme compiler (huskc)"
 , "--"
 , "--  http://justinethier.github.io/husk-scheme "
 , "--  (c) 2010 Justin Ethier "
 , "--  Version " ++ LSC.version
 , "--"]

-- |Main module used in a compiled Haskell program
headerModule :: [String]
headerModule = ["module Main where "]

-- |Imports used for a compiled program
headerImports :: [String]
headerImports = [
   "Language.Scheme.Core "
 , "Language.Scheme.Numerical "
 , "Language.Scheme.Macro "
 , "Language.Scheme.Primitives "
 , "Language.Scheme.Types     -- Scheme data types "
 , "Language.Scheme.Variables -- Scheme variable operations "
 , "Control.Monad.Error "
 , "Data.Array "
 , " qualified Data.ByteString as BS "
 , "Data.Complex "
 , " qualified Data.Map "
 , "Data.Ratio "
 , "Data.Word "
 , "System.IO "
#ifdef UseDebug
 , "System.CPUTime "
 , "Debug.Trace "
#endif
 ]

-- |Block of code used in the header of a Haskell program 
--  generated by the compiler.
header :: String -> Bool -> String -> [String]
header filepath useCompiledLibs langRev = do
  let env = if useCompiledLibs
            then "primitiveBindings"
            else case langRev of
                   "7" -> "r7rsEnv"
                   _ -> "r5rsEnv"
      initSrfi55 = 
        case langRev of
          "7" -> []
          _ -> [ "exec55_3 env cont _ _ = do "
               , "  liftIO $ registerExtensions env getDataFileName' "
               , "  continueEval env (makeCPSWArgs env cont exec []) (Nil \"\") Nothing"]
  [ " "
    , " "
    , "-- |Get variable at runtime "
    , "getRTVar env var = do " 
    , "  v <- getVar env var " 
    , "  return $ case v of "
    , "    List _ -> Pointer var env "
    , "    DottedList _ _ -> Pointer var env "
    , "    String _ -> Pointer var env "
    , "    Vector _ -> Pointer var env "
    , "    ByteVector _ -> Pointer var env "
    , "    HashTable _ -> Pointer var env "
    , "    _ -> v "
    , " "
-- TODO:  this is just a temporary function until calls to continueEval can be purged from the compiler
    , "continueEval' env cont value = continueEval env cont value Nothing "
    , " "
    , "applyWrapper env cont (Nil _) (Just (a:as))  = do "
    , "  apply cont a as "
    , " "
    , "applyWrapper env cont value (Just (a:as))  = do "
    , "  apply cont a $ as ++ [value] "
    , " "
    , "getDataFileName' :: FilePath -> IO FilePath "
    , "getDataFileName' name = return $ \"" ++ (Language.Scheme.Util.escapeBackslashes filepath) ++ "\" ++ name "
    , " "]
    ++ initSrfi55 ++ 
    [ " "
    , "main :: IO () "
    , "main = do "
    , "  env <- " ++ env ++ " "
    , "  result <- (runIOThrows $ liftM show $ hsInit env (makeNullContinuation env) (Nil \"\") Nothing) "
    , "  case result of "
    , "    Just errMsg -> putStrLn errMsg "
    , "    _ -> return () "
    , " "
    , "hsInit env cont _ _ = do "
    , "  _ <- defineVar env \"" ++ moduleRuntimeVar ++ "\" $ HashTable $ Data.Map.fromList [] "
    , "  run env cont (Nil \"\") (Just [])"
    , " "]

