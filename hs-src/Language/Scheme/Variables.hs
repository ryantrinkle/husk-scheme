{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Language.Scheme.Variables
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains code for working with Scheme variables,
and the environments that contain them.

-}

module Language.Scheme.Variables 
    (
    -- * Environments
      printEnv
    , recPrintEnv
    , recExportsFromEnv 
    , exportsFromEnv 
    , copyEnv
    , extendEnv
    , importEnv
    , topmostEnv
    , nullEnvWithParent
    , findNamespacedEnv
    -- * Getters
    , getVar
    , getVar'
    , getNamespacedVar 
    , getNamespacedVar' 
    , getNamespacedRef 
    -- * Setters
    , defineVar
    , defineNamespacedVar
    , setVar
    , setNamespacedVar
    , updateObject 
    , updateNamespacedObject 
    -- * Predicates
    , isBound
    , isRecBound
    , isNamespacedRecBound 
    -- * Pointers
    , derefPtr
--    , derefPtrs
    , recDerefPtrs
    , safeRecDerefPtrs
    , recDerefToFnc
    ) where
import Language.Scheme.Types
import Control.Monad.Except
import Data.Array
import qualified Data.Map
-- import Debug.Trace

-- Experimental code:
-- From: http://rafaelbarreto.com/2011/08/21/comparing-objects-by-memory-location-in-haskell/
--
-- import Foreign
-- isMemoryEquivalent :: a -> a -> IO Bool
-- isMemoryEquivalent obj1 obj2 = do
--   obj1Ptr <- newStablePtr obj1
--   obj2Ptr <- newStablePtr obj2
--   let result = obj1Ptr == obj2Ptr
--   freeStablePtr obj1Ptr
--   freeStablePtr obj2Ptr
--   return result
-- 
-- -- Using above, search an env for a variable definition, but stop if the upperEnv is
-- -- reached before the variable
-- isNamespacedRecBoundWUpper :: Env -> Env -> String -> String -> IO Bool
-- isNamespacedRecBoundWUpper upperEnvRef envRef namespace var = do 
--   areEnvsEqual <- liftIO $ isMemoryEquivalent upperEnvRef envRef
--   if areEnvsEqual
--      then return False
--      else do
--          found <- liftIO $ isNamespacedBound envRef namespace var
--          if found
--             then return True 
--             else case parentEnv envRef of
--                       (Just par) -> isNamespacedRecBoundWUpper upperEnvRef par namespace var
--                       Nothing -> return False -- Var never found
--

-- |Create a variable's name in an environment using given arguments
getVarName :: Namespace -> String -> (Namespace, String)
getVarName namespace name = (namespace, name)

-- |Show the contents of an environment
printEnv :: ReadRef r m
         => Env m r   -- ^Environment
         -> m String   -- ^Contents of the env as a string
printEnv env = do
  binds <- readRef $ bindings env
  l <- mapM showVar $ Data.Map.toList binds 
  return $ unlines l
 where 
  showVar ((namespace, name), val) = do
    v <- readRef val
    return $ "[" ++ show namespace ++ " " ++ name ++ "]" ++ ": " ++ show v

-- |Recursively print an environment to string
recPrintEnv :: ReadRef r m => Env m r -> m String
recPrintEnv env = do
  envStr <- printEnv env

  case parentEnv env of
    Just par -> do
        parEnvStr <- recPrintEnv par
        return $ envStr ++ "\n" ++ parEnvStr
    Nothing -> return envStr

-- |Recursively find all exports from the given environment
recExportsFromEnv :: ReadRef r m => Env m r -> m [LispVal m r]
recExportsFromEnv env = do
  xs <- exportsFromEnv env

  case parentEnv env of
    Just par -> do
        pxs <- recExportsFromEnv par
        return $ xs ++ pxs
    Nothing -> return xs

-- |Return a list of symbols exported from an environment
exportsFromEnv :: ReadRef r m
               => Env m r 
               -> m [LispVal m r]
exportsFromEnv env = do
  binds <- readRef $ bindings env
  return $ getExports [] $ fst $ unzip $ Data.Map.toList binds 
 where 
  getExports acc ((Macro, b) : bs) = getExports (Atom b:acc) bs
  getExports acc ((Var, b) : bs) = getExports (Atom b:acc) bs
  getExports acc (_ : bs) = getExports acc bs
  getExports acc [] = acc

-- |Create a deep copy of an environment
copyEnv :: (ReadRef r m, NewRef r m)
        => Env m r      -- ^ Source environment
        -> m (Env m r)   -- ^ A copy of the source environment
copyEnv env = do
  ptrs <- readRef $ pointers env
  ptrList <- newRef ptrs

  binds <- readRef $ bindings env
  bindingListT <- mapM addBinding $ Data.Map.toList binds 
  bindingList <- newRef $ Data.Map.fromList bindingListT
  return $ Environment (parentEnv env) bindingList ptrList
 where addBinding (name, val) = do 
         x <- readRef val
         ref <- newRef x
         return (name, ref)

-- |Perform a deep copy of an environment's contents into
--  another environment.
--
--  The destination environment is modified!
--
importEnv 
  :: (ReadRef r m, WriteRef r m)
  => Env m r -- ^ Destination environment
  -> Env m r -- ^ Source environment
  -> m (Env m r)
importEnv dEnv sEnv = do
  sPtrs <- readRef $ pointers sEnv
  dPtrs <- readRef $ pointers dEnv
  writeRef (pointers dEnv) $ Data.Map.union sPtrs dPtrs

  sBinds <- readRef $ bindings sEnv
  dBinds <- readRef $ bindings dEnv
  writeRef (bindings dEnv)  $ Data.Map.union sBinds dBinds

  case parentEnv sEnv of
    Just ps -> importEnv dEnv ps 
    Nothing -> return dEnv 

-- |Extend given environment by binding a series of values to a new environment.
extendEnv :: (NewRef r m)
          => Env m r -- ^ Environment 
          -> [((Namespace, String), LispVal m r)] -- ^ Extensions to the environment
          -> m (Env m r) -- ^ Extended environment
extendEnv envRef abindings = do 
  bindinglistT <- mapM addBinding abindings -- >>= newRef
  bindinglist <- newRef $ Data.Map.fromList bindinglistT
  nullPointers <- newRef $ Data.Map.fromList []
  return $ Environment (Just envRef) bindinglist nullPointers
 where addBinding ((namespace, name), val) = do ref <- newRef val
                                                return (getVarName namespace name, ref)

-- |Find the top-most environment
topmostEnv :: Env m r -> Env m r
topmostEnv envRef = do
    case parentEnv envRef of
        Just p -> topmostEnv p
        Nothing -> envRef

-- |Create a null environment with the given environment as its parent.
nullEnvWithParent :: NewRef r m => Env m r -> m (Env m r) 
nullEnvWithParent p = do
  Environment _ binds ptrs <- nullEnv
  return $ Environment (Just p) binds ptrs

-- |Recursively search environments to find one that contains the given variable.
findNamespacedEnv 
    :: ReadRef r m
    => Env m r      -- ^Environment to begin the search; 
                --  parent env's will be searched as well.
    -> Namespace     -- ^Namespace
    -> String   -- ^Variable
    -> m (Maybe (Env m r)) -- ^Environment, or Nothing if there was no match.
findNamespacedEnv envRef namespace var = do
  found <- isNamespacedBound envRef namespace var
  if found
     then return (Just envRef)
     else case parentEnv envRef of
               (Just par) -> findNamespacedEnv par namespace var
               Nothing -> return Nothing

-- |Determine if a variable is bound in the default namespace
isBound :: ReadRef r m
        => Env m r      -- ^ Environment
        -> String   -- ^ Variable
        -> m Bool  -- ^ True if the variable is bound
isBound envRef = isNamespacedBound envRef Var

-- |Determine if a variable is bound in the default namespace, 
--  in this environment or one of its parents.
isRecBound :: ReadRef r m
           => Env m r      -- ^ Environment
           -> String   -- ^ Variable
           -> m Bool  -- ^ True if the variable is bound
isRecBound envRef = isNamespacedRecBound envRef Var

-- |Determine if a variable is bound in a given namespace
isNamespacedBound 
    :: ReadRef r m
    => Env m r      -- ^ Environment
    -> Namespace    -- ^ Namespace
    -> String   -- ^ Variable
    -> m Bool  -- ^ True if the variable is bound
isNamespacedBound envRef namespace var = 
    readRef (bindings envRef) >>= return . Data.Map.member (getVarName namespace var)

-- |Determine if a variable is bound in a given namespace
--  or a parent of the given environment.
isNamespacedRecBound 
    :: ReadRef r m
    => Env m r      -- ^ Environment
    -> Namespace     -- ^ Namespace
    -> String   -- ^ Variable
    -> m Bool  -- ^ True if the variable is bound
isNamespacedRecBound envRef namespace var = do
  env <- findNamespacedEnv envRef namespace var
  case env of
    (Just e) -> isNamespacedBound e namespace var
    Nothing -> return False

-- |Retrieve the value of a variable defined in the default namespace
getVar :: ReadRef r m
       => Env m r       -- ^ Environment
       -> String    -- ^ Variable
       -> IOThrowsError m r (LispVal m r) -- ^ Contents of the variable
getVar envRef = getNamespacedVar envRef Var

-- |Retrieve the value of a variable defined in the default namespace,
--  or Nothing if it is not defined
getVar' :: ReadRef r m
        => Env m r       -- ^ Environment
        -> String    -- ^ Variable
        -> IOThrowsError m r (Maybe (LispVal m r)) -- ^ Contents of the variable
getVar' envRef = getNamespacedVar' envRef Var

-- |Retrieve an ioRef defined in a given namespace
getNamespacedRef :: ReadRef r m
                 => Env m r     -- ^ Environment
                 -> Namespace    -- ^ Namespace
                 -> String  -- ^ Variable
                 -> IOThrowsError m r (r (LispVal m r))
getNamespacedRef envRef
                 namespace
                 var = do
  v <- getNamespacedObj' envRef namespace var return
  case v of
    Just a -> return a
    Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)

-- |Retrieve the value of a variable defined in a given namespace
getNamespacedVar :: ReadRef r m
                 => Env m r     -- ^ Environment
                 -> Namespace    -- ^ Namespace
                 -> String  -- ^ Variable
                 -> IOThrowsError m r (LispVal m r) -- ^ Contents of the variable
getNamespacedVar envRef
                 namespace
                 var = do
  v <- getNamespacedVar' envRef namespace var
  case v of
    Just a -> return a
    Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)

-- |Retrieve the value of a variable defined in a given namespace,
--  or Nothing if it is not defined
getNamespacedVar' :: ReadRef r m
                  => Env m r     -- ^ Environment
                  -> Namespace    -- ^ Namespace
                  -> String  -- ^ Variable
                  -> IOThrowsError m r (Maybe (LispVal m r)) -- ^ Contents of the variable, if found
getNamespacedVar' envRef
                 namespace
                 var = do 
    getNamespacedObj' envRef namespace var readRef

getNamespacedObj' :: ReadRef r m
                  => Env m r     -- ^ Environment
                  -> Namespace    -- ^ Namespace
                  -> String  -- ^ Variable
                  -> (r (LispVal m r) -> m a)
                  -> IOThrowsError m r (Maybe a) -- ^ Contents of the variable, if found
getNamespacedObj' envRef
                 namespace
                 var 
                 unpackFnc = do 
    binds <- readRef $ bindings envRef
    case Data.Map.lookup (getVarName namespace var) binds of
      (Just a) -> do
          v <- lift $ unpackFnc a
          return $ Just v
      Nothing -> case parentEnv envRef of
                   (Just par) -> getNamespacedObj' par namespace var unpackFnc
                   Nothing -> return Nothing

-- |Set a variable in the default namespace
setVar
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r      -- ^ Environment
    -> String   -- ^ Variable
    -> LispVal m r  -- ^ Value
    -> IOThrowsError m r (LispVal m r) -- ^ Value
setVar envRef = setNamespacedVar envRef Var

-- |Set a variable in a given namespace
setNamespacedVar 
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r      -- ^ Environment 
    -> Namespace     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal m r  -- ^ Value
    -> IOThrowsError m r (LispVal m r)   -- ^ Value
setNamespacedVar envRef
                 namespace
                 var value = do 
  -- Issue #98 - Need to detect circular references
  --
  -- TODO:
  -- Note this implementation is rather simplistic since
  -- it does not take environments into account. The same
  -- variable name could refer to 2 different variables in
  -- different environments.
  case value of
    Pointer p _ -> do
      if p == var 
          then return value
          else next
    _ -> next

  where 
    next = do
      _ <- updatePointers envRef namespace var 
      _setNamespacedVar envRef namespace var value

-- |An internal function that does the actual setting of a 
--  variable, without all the extra code that keeps pointers
--  in sync when a variable is re-binded
--
--  Note this function still binds reverse pointers
--  for purposes of book-keeping.
_setNamespacedVar 
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r      -- ^ Environment 
    -> Namespace     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal m r  -- ^ Value
    -> IOThrowsError m r (LispVal m r)   -- ^ Value
_setNamespacedVar envRef
                 namespace
                 var value = do 
  -- Set the variable to its new value
  valueToStore <- getValueToStore namespace var envRef value
  _setNamespacedVarDirect envRef namespace var valueToStore

-- |Do the actual /set/ operation, with NO pointer operations.
--  Only call this if you know what you are doing!
_setNamespacedVarDirect
    :: (ReadRef r m, WriteRef r m)
    => Env m r      -- ^ Environment 
    -> Namespace     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal m r  -- ^ Value
    -> IOThrowsError m r (LispVal m r)   -- ^ Value
_setNamespacedVarDirect envRef
                 namespace
                 var valueToStore = do 
  env <- readRef $ bindings envRef
  case Data.Map.lookup (getVarName namespace var) env of
    (Just a) -> do
      writeRef a valueToStore
      return valueToStore
    Nothing -> case parentEnv envRef of
      (Just par) -> _setNamespacedVarDirect par namespace var valueToStore
      Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var

-- |This helper function is used to keep pointers in sync when
--  a variable is bound to a different value.
updatePointers :: forall m r. (ReadRef r m, WriteRef r m, NewRef r m) => Env m r -> Namespace -> String -> IOThrowsError m r (LispVal m r)
updatePointers envRef namespace var = do
  ptrs <- readRef $ pointers envRef
  case Data.Map.lookup (getVarName namespace var) ptrs of
    (Just valIORef) -> do
      val <- readRef valIORef
      case val of 
        -- If var has any pointers, then we need to 
        -- assign the first pointer to the old value
        -- of x, and the rest need to be updated to 
        -- point to that first var

        -- This is the first pointer to (the old) var
        (Pointer pVar pEnv : ps) -> do
          -- Since var is now fresh, reset its pointers list
          writeRef valIORef []

          -- The first pointer now becomes the old var,
          -- so its pointers list should become ps
          _ <- movePointers pEnv namespace pVar ps

          -- Each ps needs to be updated to point to pVar
          -- instead of var
          _ <- pointToNewVar pEnv namespace pVar ps

          -- Set first pointer to existing value of var
          existingValue <- getNamespacedVar envRef namespace var
          _setNamespacedVar pEnv namespace pVar existingValue

        -- No pointers, so nothing to do
        [] -> return $ Nil ""
        _ -> throwError $ InternalError
               "non-pointer value found in updatePointers"
    Nothing -> return $ Nil ""
 where
  -- |Move the given pointers (ptr) to the list of
  --  pointers for variable (var)
  movePointers :: Env m r -> Namespace -> String -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
  movePointers envRef' namespace' var' ptrs = do
    env <- readRef $ pointers envRef'
    case Data.Map.lookup (getVarName namespace' var') env of
      Just ps' -> do
        -- Append ptrs to existing list of pointers to var
        ps <- readRef ps'
        writeRef ps' $ ps ++ ptrs
        return $ Nil ""
      Nothing -> do
        -- var does not have any pointers; create new list
        valueRef <- newRef ptrs
        writeRef (pointers envRef') (Data.Map.insert (getVarName namespace var') valueRef env)
        return $ Nil ""

  -- |Update each pointer's source to point to pVar
  pointToNewVar pEnv namespace' pVar' (Pointer v e : ps) = do
    _ <- _setNamespacedVarDirect e namespace' v (Pointer pVar' pEnv)
    pointToNewVar pEnv namespace' pVar' ps
  pointToNewVar _ _ _ [] = return $ Nil ""
  pointToNewVar _ _ _ _ = throwError $ InternalError "pointToNewVar"

-- |A wrapper for updateNamespaceObject that uses the variable namespace.
updateObject :: (ReadRef r m, WriteRef r m, NewRef r m) => Env m r -> String -> LispVal m r -> IOThrowsError m r (LispVal m r)
updateObject env = 
  updateNamespacedObject env Var

-- |This function updates the object that the variable refers to. If it is
--  a pointer, that means this function will update that pointer (or the last
--  pointer in the chain) to point to the given /value/ object. If the variable
--  is not a pointer, the result is the same as a setVar (but without updating
--  any pointer references, see below).
--
--  Note this function only updates the object, it does not
--  update any associated pointers. So it should probably only be
--  used internally by husk, unless you really know what you are
--  doing!
updateNamespacedObject :: (ReadRef r m, WriteRef r m, NewRef r m)
                       => Env m r                   -- ^ Environment
                       -> Namespace                  -- ^ Namespace
                       -> String                -- ^ Variable
                       -> LispVal m r               -- ^ Value
                       -> IOThrowsError m r (LispVal m r) -- ^ Value
updateNamespacedObject env namespace var value = do
  varContents <- getNamespacedVar env namespace var
  obj <- findPointerTo varContents
  case obj of
    Pointer pVar pEnv -> do
      _setNamespacedVar pEnv namespace pVar value
    _ -> _setNamespacedVar env namespace var value

-- |Bind a variable in the default namespace
defineVar
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r      -- ^ Environment
    -> String   -- ^ Variable
    -> LispVal m r  -- ^ Value
    -> IOThrowsError m r (LispVal m r) -- ^ Value
defineVar envRef = defineNamespacedVar envRef Var 

-- |Bind a variable in the given namespace
defineNamespacedVar
    :: (ReadRef r m, WriteRef r m, NewRef r m)
    => Env m r      -- ^ Environment 
    -> Namespace     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal m r  -- ^ Value
    -> IOThrowsError m r (LispVal m r)   -- ^ Value
defineNamespacedVar envRef
                    namespace
                    var value = do
  alreadyDefined <- lift $ isNamespacedBound envRef namespace var
  if alreadyDefined
    then setNamespacedVar envRef namespace var value >> return value
    else do
      --
      -- Future optimization:
      -- don't change anything if (define) is to existing pointer
      -- (IE, it does not really change anything)
      --


      -- If we are assigning to a pointer, we need a reverse lookup to 
      -- note that the pointer @value@ points to @var@
      -- 
      -- So run through this logic to figure out what exactly to store,
      -- both for bindings and for rev-lookup pointers
      valueToStore <- getValueToStore namespace var envRef value
      -- Write new value binding
      valueRef <- newRef valueToStore
      env <- readRef $ bindings envRef
      writeRef (bindings envRef) (Data.Map.insert (getVarName namespace var) valueRef env)
      return valueToStore

-- |An internal helper function to get the value to save to an env
--  based on the value passed to the define/set function. Normally this
--  is straightforward, but there is book-keeping involved if a
--  pointer is passed, depending on if the pointer resolves to an object.
getValueToStore :: (ReadRef r m, WriteRef r m, NewRef r m) => Namespace -> String -> Env m r -> LispVal m r -> IOThrowsError m r (LispVal m r)
getValueToStore namespace var env (Pointer p pEnv) = do
  addReversePointer namespace p pEnv namespace var env
getValueToStore _ _ _ value = return value

-- |Accept input for a pointer (ptrVar) and a variable that the pointer is going
--  to be assigned to. If that variable is an object then we setup a reverse lookup
--  for future book-keeping. Otherwise, we just look it up and return it directly, 
--  no booking-keeping required.
addReversePointer :: (ReadRef r m, WriteRef r m, NewRef r m) => Namespace -> String -> Env m r -> Namespace -> String -> Env m r -> IOThrowsError m r (LispVal m r)
addReversePointer namespace var envRef ptrNamespace ptrVar ptrEnvRef = do
   env <- readRef $ bindings envRef
   case Data.Map.lookup (getVarName namespace var) env of
     (Just a) -> do
       v <- readRef a
       if isObject v
          then do
            -- Store a reverse pointer for book keeping
            ptrs <- readRef $ pointers envRef
            
            -- Lookup ptr for var
            case Data.Map.lookup (getVarName namespace var) ptrs of
              -- Append another reverse ptr to this var
              -- FUTURE: make sure ptr is not already there, 
              --         before adding it to the list again?
              (Just valueRef) -> do
                value <- readRef valueRef
                writeRef valueRef (value ++ [Pointer ptrVar ptrEnvRef])
                return $ Pointer var envRef 

              -- No mapping, add the first reverse pointer
              Nothing -> do
                valueRef <- newRef [Pointer ptrVar ptrEnvRef]
                writeRef (pointers envRef) (Data.Map.insert (getVarName namespace var) valueRef ptrs)
                return $ Pointer var envRef -- Return non-reverse ptr to caller
          else return v -- Not an object, return value directly
     Nothing -> case parentEnv envRef of
       (Just par) -> addReversePointer namespace var par ptrNamespace ptrVar ptrEnvRef
       Nothing -> throwError $ UnboundVar "Getting an unbound variable: " var

-- |Return a value with a pointer dereferenced, if necessary
derefPtr :: ReadRef r m => LispVal m r -> IOThrowsError m r (LispVal m r)
-- Try dereferencing again if a ptr is found
--
-- Not sure if this is the best solution; it would be 
-- nice if we did not have to worry about multiple levels
-- of ptrs, especially since I believe husk only needs to 
-- have one level. but for now we will go with this to
-- move forward.
--
derefPtr (Pointer p env) = do
    result <- getVar env p
    derefPtr result
derefPtr v = return v

-- -- |Return the given list of values, but if any of the
-- --  original values is a pointer it will be dereferenced
-- derefPtrs :: [LispVal m r] -> IOThrowsError m r (LispVal m r)
-- derefPtrs lvs = mapM (liftThrows $ derefPtr) lvs

-- |Recursively process the given data structure, dereferencing
--  any pointers found along the way. 
-- 
--  This could potentially be expensive on large data structures 
--  since it must walk the entire object.
recDerefPtrs :: (ReadRef r m, PtrEq m r) => LispVal m r -> IOThrowsError m r (LispVal m r)
recDerefPtrs = safeRecDerefPtrs []

-- |Attempt to dereference pointers safely, without being caught in a cycle
safeRecDerefPtrs :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> LispVal m r -> IOThrowsError m r (LispVal m r)
#ifdef UsePointers
safeRecDerefPtrs ps (List l) = do
    result <- mapM (safeRecDerefPtrs ps) l
    return $ List result
safeRecDerefPtrs ps (DottedList ls l) = do
    ds <- mapM (safeRecDerefPtrs ps) ls
    d <- safeRecDerefPtrs ps l
    return $ DottedList ds d
safeRecDerefPtrs ps (Vector v) = do
   let vs = elems v
   ds <- mapM (safeRecDerefPtrs ps) vs
   return $ Vector $ listArray (0, length vs - 1) ds
safeRecDerefPtrs ps (HashTable ht) = do
    ks <- mapM (safeRecDerefPtrs ps)$ map (\ (k, _) -> k) $ Data.Map.toList ht
    vs <- mapM (safeRecDerefPtrs ps)$ map (\ (_, v) -> v) $ Data.Map.toList ht
    return $ HashTable $ Data.Map.fromList $ zip ks vs
#endif
safeRecDerefPtrs ps ptr@(Pointer p env) = do
    if containsPtr ps ptr
       then return ptr -- Avoid cycle
       else do
            result <- getVar env p
            safeRecDerefPtrs (ptr : ps) result 
safeRecDerefPtrs _ v = return v

containsPtr :: PtrEq m r => [LispVal m r] -> LispVal m r -> Bool
containsPtr ((Pointer pa ea):ps) p@(Pointer pb eb) = do
    let found = (pa == pb) && ((bindings ea) == (bindings eb))
    found || containsPtr ps p
containsPtr _ _ = False

-- |A helper to recursively dereference all pointers and
--  pass results to a function
recDerefToFnc :: (ReadRef r m, PtrEq m r) => ([LispVal m r] -> ThrowsError m r (LispVal m r)) -> [LispVal m r] 
            -> IOThrowsError m r (LispVal m r)
recDerefToFnc fnc lvs = do
    List result <- recDerefPtrs $ List lvs 
    liftThrows $ fnc result

-- |A predicate to determine if the given lisp value 
--  is an /object/ that can be pointed to.
isObject :: LispVal m r -> Bool
isObject (List _) = True
isObject (DottedList _ _) = True
isObject (String _) = True
isObject (Vector _) = True
isObject (HashTable _) = True
isObject (ByteVector _) = True
isObject (Pointer _ _) = True
isObject _ = False

-- |Same as dereferencing a pointer, except we want the
--  last pointer to an object (if there is one) instead
--  of the object itself
findPointerTo :: ReadRef r m => LispVal m r -> IOThrowsError m r (LispVal m r)
findPointerTo ptr@(Pointer p env) = do
    result <- getVar env p
    case result of
      (Pointer _ _) -> findPointerTo result
      _ -> return ptr
findPointerTo v = return v

