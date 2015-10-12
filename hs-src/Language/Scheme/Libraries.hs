{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Language.Scheme.Libraries
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains code to handle R7RS libraries.
NOTE: Libraries are usually referred to as /modules/ in the husk source code.

-}

module Language.Scheme.Libraries
    (
      findModuleFile
    , moduleImport
    ) where
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid

-- |Get the full path to a module file
findModuleFile 
    :: (ReadRef r m, PtrEq m r)
    => [LispVal m r]
    -> IOThrowsError m r (LispVal m r)
findModuleFile [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= findModuleFile
findModuleFile [Text file] = do
    -- Good enough now that load searches @lib@ if file not found
    return $ Text file
findModuleFile _ = return $ Bool False

-- |Import definitions from one environment into another
moduleImport 
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r  -- ^ Environment to import into
    -> Env m r  -- ^ Environment to import from
    -> [LispVal m r] -- ^ Identifiers to import
    -> IOThrowsError m r (LispVal m r)
moduleImport to from (p@(Pointer _ _) : is) = do
  i <- derefPtr p
  moduleImport to from (i : is)
moduleImport to from (Atom i : is) = do
  _ <- divertBinding to from i i
  moduleImport to from is
moduleImport to from (DottedList [Atom iRenamed] (Atom iOrig) : is) = do
  _ <- divertBinding to from iOrig iRenamed
  moduleImport to from is
moduleImport to _ [] = do
  return $ LispEnv to
moduleImport _ _ err = do
  throwError $ Default $ "Unexpected argument to moduleImport: " <> T.pack (show err)

-- |Copy a binding from one env to another
divertBinding
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r  -- ^ Environment to import into
    -> Env m r  -- ^ Environment to import from
    -> Text -- ^ Name of the binding in @from@
    -> Text -- ^ Name to use for the binding in @to@
    -> IOThrowsError m r (LispVal m r)
divertBinding to from nameOrig nameNew = do
  isMacroBound <- lift $ isNamespacedRecBound from Macro nameOrig
  namespace <- lift $ if isMacroBound then return Macro
                                      else return Var
  m <- getNamespacedVar from namespace nameOrig
  defineNamespacedVar to namespace nameNew m

