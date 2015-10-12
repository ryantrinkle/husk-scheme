{-# LANGUAGE CPP #-}
{-# Language ExistentialQuantification #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}
{-# Language OverloadedStrings #-}

{- |
Module      : Language.Scheme.Primitives
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains primitive functions written in Haskell.
Most of these map directly to an equivalent Scheme function.

-}

module Language.Scheme.Primitives (
 -- * Pure functions
 -- ** List
   car
 , cdr 
 , cons
 , eq
 , equal 
 , makeList
 , listCopy
 -- ** Vector
 , buildVector 
 , vectorLength 
 , vectorRef 
 , vectorCopy
 , vectorToList 
 , listToVector
 , makeVector
 -- ** Bytevectors
 , makeByteVector
 , byteVector
 , byteVectorLength
 , byteVectorRef
 , byteVectorCopy
 , byteVectorAppend
 , byteVectorUtf2Str
 , byteVectorStr2Utf
 -- ** Hash Table
 , hashTblExists 
 , hashTblRef
 , hashTblSize 
 , hashTbl2List
 , hashTblKeys
 , hashTblValues 
 , hashTblCopy
 , hashTblMake
 , wrapHashTbl
 , wrapLeadObj
 -- ** Text
 , buildString
 , makeString
 , doMakeString
 , stringLength
 , stringRef
 , substring
 , stringCIEquals 
 , stringCIBoolBinop 
 , stringAppend 
 , stringToNumber
 , stringToList 
 , listToString
 , stringToVector
 , vectorToString
 , stringCopy 
 , symbol2String 
 , string2Symbol

 -- ** Character
 , charCIBoolBinop 
 , charPredicate
 , charUpper
 , charLower
 , charDigitValue
 , char2Int
 , int2Char

 -- ** Predicate
 , isHashTbl
 , isChar 
 , isString 
 , isBoolean 
 , isBooleanEq
 , isSymbolEq
 , isDottedList 
 , isProcedure 
 , isList 
 , isVector 
 , isRecord
 , isByteVector
 , isNull 
 , isEOFObject 
 , isSymbol 

 -- ** Utility functions
 , Unpacker ()
 , unpackEquals 
 , boolBinop 
 , unaryOp 
 , unaryOp'
 , strBoolBinop 
 , charBoolBinop 
 , boolBoolBinop
 , unpackStr 
 , unpackBool
 -- * Impure functions
 -- |All of these functions must be executed within the IO monad.
 
 -- ** Input / Output 
 , makePort 
 , makeBufferPort
 , openInputString
 , openOutputString
 , getOutputString
 , openInputByteVector
 , openOutputByteVector
 , getOutputByteVector
 , closePort
 , flushOutputPort
 , currentOutputPort 
 , currentInputPort 
 , isTextPort
 , isBinaryPort
 , isOutputPort 
 , isInputPort
 , isInputPortOpen
 , isOutputPortOpen
 , isCharReady
 , readProc 
 , readCharProc 
 , readByteVector
 , readString
 , writeProc 
 , writeCharProc
 , writeByteVector
 , writeString
 , readContents
 , load
 , readAll
 , fileExists
 , deleteFile
 , eofObject 
 -- ** Symbol generation
 , gensym
 , _gensym
 -- ** Time
 , currentTimestamp
 -- ** System
 , system
 , getEnvVars
-- , systemRead

 , MonadSerial (..)
 , MonadFilesystem (..)
 , MonadStdin (..)
 ) where
import Prelude hiding (readFile, getContents)
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Types
import Language.Scheme.Variables
--import qualified Control.Exception
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char hiding (isSymbol)
import Data.Array
import Data.IORef
import qualified Data.Knob as DK
--import qualified Data.List as DL
import qualified Data.Map
import qualified Data.Time.Clock.POSIX
import Data.Word
import qualified System.Directory as IO (doesFileExist, removeFile)
import qualified System.Environment as SE
import System.Exit (ExitCode(..))
import System.IO hiding (readFile, getContents)
import qualified System.IO as IO
import System.IO.Error
import qualified System.Process
--import System.Process (readProcess)
--import Debug.Trace
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding
import Data.Monoid

import System.IO.Unsafe (unsafePerformIO) -- For constructing the global serialRef

#if __GLASGOW_HASKELL__ < 702
try' = try
#else
try' :: IO a -> IO (Either IOError a)
try' = tryIOError
#endif

class Monad m => MonadSerial m where
  newSerial :: m Integer

{-# NOINLINE serialRef #-}
serialRef :: IORef Integer
serialRef = unsafePerformIO $ newIORef 1

instance MonadSerial IO where
  newSerial = atomicModifyIORef' serialRef $ \x -> (succ x, x)

instance MonadSerial m => MonadSerial (ExceptT e m) where
  newSerial = lift newSerial

---------------------------------------------------
-- I/O Primitives
-- These primitives all execute within the IO monad
---------------------------------------------------

-- |Open the given file
--
--   LispVal Arguments:
--
--   * Text - filename
--
--   Returns: Port
--
makePort
    :: (MonadIO m, ReadRef r m, PtrEq m r)
    => (FilePath -> IOMode -> IO Handle)
    -> IOMode
    -> [LispVal m r]
    -> IOThrowsError m r (LispVal m r)
makePort openFnc mode [Text filename] = do
    h <- liftIO $ openFnc (T.unpack filename) mode
    return $ Port h Nothing
makePort fnc mode [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= makePort fnc mode
makePort _ _ [] = throwError $ NumArgs (Just 1) []
makePort _ _ args@(_ : _) = throwError $ NumArgs (Just 1) args

-- |Create an memory-backed port
makeBufferPort :: MonadIO m => Maybe (LispVal m r) -> IOThrowsError m r (LispVal m r)
makeBufferPort buf = do
    let mode = case buf of
                 Nothing -> WriteMode
                 _ -> ReadMode
    bs <- case buf of
--        Just (p@(Pointer {})] = recDerefPtrs p >>= box >>= openInputString
        Just (Text s)-> return $ encodeUtf8 s
        Just (ByteVector bv)-> return bv
        Just err -> throwError $ TypeMismatch "string or bytevector" err
        Nothing -> return $ BS.pack []
    k <- DK.newKnob bs
    h <- liftIO $ DK.newFileHandle k "temp.buf" mode
    return $ Port h (Just k)

-- |Read byte buffer from a given port
getBufferFromPort :: MonadIO m => LispVal m r -> IOThrowsError m r BSU.ByteString
getBufferFromPort (Port h (Just k)) = do
    _ <- liftIO $ hFlush h
    DK.getContents k
getBufferFromPort args = do
    throwError $ TypeMismatch "output-port" args

-- |Create a new input string buffer
openInputString :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
openInputString [p@(Pointer {})] = recDerefPtrs p >>= box >>= openInputString
openInputString [buf@(Text _)] = makeBufferPort (Just buf)
openInputString args = if length args == 1
    then throwError $ TypeMismatch "(string)" $ List args
    else throwError $ NumArgs (Just 1) args

-- |Create a new output string buffer
openOutputString :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
openOutputString _ = makeBufferPort Nothing

-- |Create a new input bytevector buffer
openInputByteVector :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
openInputByteVector [p@(Pointer {})] = recDerefPtrs p >>= box >>= openInputByteVector
openInputByteVector [buf@(ByteVector _)] = makeBufferPort (Just buf)
openInputByteVector args = if length args == 1
    then throwError $ TypeMismatch "(bytevector)" $ List args
    else throwError $ NumArgs (Just 1) args

-- |Create a new output bytevector buffer
openOutputByteVector :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
openOutputByteVector _ = makeBufferPort Nothing


-- |Get string written to string-output-port
getOutputString :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
getOutputString [p@(Pointer {})] = recDerefPtrs p >>= box >>= getOutputString
getOutputString [p@(Port port _)] = do
    o <- liftIO $ hIsOpen port
    if o then do 
            bytes <- getBufferFromPort p
            return $ Text $ decodeUtf8 bytes 
         else return $ Text ""
getOutputString args = do
    throwError $ TypeMismatch "output-port" $ List args

-- |Get bytevector written to bytevector-output-port
getOutputByteVector :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
getOutputByteVector [p@(Pointer {})] = recDerefPtrs p >>= box >>= getOutputByteVector
getOutputByteVector [p@(Port port _)] = do
    o <- liftIO $ hIsOpen port
    if o then do bytes <- getBufferFromPort p
                 return $ ByteVector bytes 
         else return $ ByteVector $ BS.pack []
getOutputByteVector args = do
    throwError $ TypeMismatch "output-port" $ List args

-- |Close the given port
--
--   Arguments:
--
--   * Port
--
--   Returns: Bool - True if the port was closed, false otherwise
--
closePort :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
closePort [p@(Pointer {})] = recDerefPtrs p >>= box >>= closePort
closePort [Port port _] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False


{- FUTURE: For now, these are just hardcoded to the standard i/o ports.
a future implementation that includes with-*put-from-file
would require a more involved implementation here as well as
other I/O functions hooking into these instead of std* -}

-- |Return the current input port
--
--   LispVal Arguments: (None)
--
--   Returns: Port
--
currentInputPort :: Monad m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
currentInputPort _ = return $ Port stdin Nothing
-- |Return the current input port
--
--   LispVal Arguments: (None)
--
--   Returns: Port
--
currentOutputPort :: Monad m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
currentOutputPort _ = return $ Port stdout Nothing

-- | Flush the given output port
flushOutputPort :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
flushOutputPort [] = liftIO $ hFlush stdout >> (return $ Bool True)
flushOutputPort [p@(Pointer {})] = recDerefPtrs p >>= box >>= flushOutputPort
flushOutputPort [p@(Port _ _)] = 
    withOpenPort p $ \port -> liftIO $ hFlush port >> (return $ Bool True)
flushOutputPort _ = return $ Bool False

-- | Determine if the given port is a text port.
--
--   Arguments
--
--   * Port
--
--   Returns: Bool
isTextPort :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isTextPort [p@(Pointer {})] = recDerefPtrs p >>= box >>= isTextPort
isTextPort [Port port _] = do
    val <- liftIO $ isTextPort' port
    return $ Bool val
isTextPort _ = return $ Bool False

-- | Determine if the given port is a binary port.
--
--   Arguments
--
--   * Port
--
--   Returns: Bool
isBinaryPort :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isBinaryPort [p@(Pointer {})] = recDerefPtrs p >>= box >>= isBinaryPort
isBinaryPort [Port port _] = do
    val <- liftIO $ isTextPort' port
    return $ Bool $ not val
isBinaryPort _ = return $ Bool False

-- | Determine if a file handle is in text mode
isTextPort' :: Handle -> IO Bool
isTextPort' port = do
    textEncoding <- hGetEncoding port
    case textEncoding of
        Nothing -> return False
        _ -> return True

-- | Determine if the given port is open
--
--   Arguments
--
--   * Port
--
--   Returns: Bool
isInputPortOpen :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isInputPortOpen [p@(Pointer {})] = recDerefPtrs p >>= box >>= isInputPortOpen
isInputPortOpen [p@(Port _ _)] = do
  withOpenPort p $ \port -> do
    r <- liftIO $ hIsReadable port
    o <- liftIO $ hIsOpen port
    return $ Bool $ r && o
isInputPortOpen _ = return $ Bool False

-- | Helper function to ensure a port is open, to prevent Haskell errors
withOpenPort :: (MonadIO m, ReadRef r m, PtrEq m r) => LispVal m r -> (Handle -> IOThrowsError m r (LispVal m r)) -> IOThrowsError m r (LispVal m r)
withOpenPort p@(Pointer {}) proc = do
    obj <- recDerefPtrs p 
    withOpenPort obj proc
withOpenPort (Port port _) proc = do
    o <- liftIO $ hIsOpen port
    if o then proc port
         else return $ Bool False
withOpenPort _ _ = return $ Bool False

-- | Determine if the given port is open
--
--   Arguments
--
--   * Port
--
--   Returns: Bool
isOutputPortOpen :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isOutputPortOpen [p@(Pointer {})] = recDerefPtrs p >>= box >>= isOutputPortOpen
isOutputPortOpen [p@(Port _ _)] = do
  withOpenPort p $ \port -> do
    w <- liftIO $ hIsWritable port
    o <- liftIO $ hIsOpen port
    return $ Bool $ w && o
isOutputPortOpen _ = return $ Bool False

-- |Determine if the given objects is an input port
--
--   LispVal Arguments:
--
--   * Port
--
--   Returns: Bool - True if an input port, false otherwise
--
isInputPort :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isInputPort [p@(Pointer {})] = recDerefPtrs p >>= box >>= isInputPort
isInputPort [p@(Port _ _)] = 
  withOpenPort p $ \port -> liftM Bool $ liftIO $ hIsReadable port
isInputPort _ = return $ Bool False

-- |Determine if the given objects is an output port
--
--   LispVal Arguments:
--
--   * Port
--
--   Returns: Bool - True if an output port, false otherwise
--
isOutputPort :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isOutputPort [p@(Pointer {})] = recDerefPtrs p >>= box >>= isOutputPort
isOutputPort [p@(Port _ _)] = 
    withOpenPort p $ \port -> liftM Bool $ liftIO $ hIsWritable port
isOutputPort _ = return $ Bool False

-- |Determine if a character is ready on the port
--
--   LispVal Arguments:
--
--   * Port
--
--   Returns: Bool
--
isCharReady :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isCharReady [p@(Pointer {})] = recDerefPtrs p >>= box >>= isCharReady
isCharReady [Port port _] = do --liftM Bool $ liftIO $ hReady port
    result <- liftIO $ try' (liftIO $ hReady port)
    case result of
        Left e -> if isEOFError e
                     then return $ Bool False
                     else throwError $ Default "I/O error reading from port" -- FUTURE: ioError e
        Right _ -> return $ Bool True
isCharReady _ = return $ Bool False

-- |Read from the given port
--
--   LispVal Arguments:
--
--   * Port
--
--   Returns: LispVal
--
readProc :: (MonadIO m, ReadRef r m, PtrEq m r) => Bool -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
readProc mode [] = readProc mode [Port stdin Nothing]
readProc mode [p@(Pointer {})] = recDerefPtrs p >>= box >>= readProc mode
readProc mode [Port port _] = do
    input <- liftIO $ try' (liftIO $ hGetLine port)
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port" -- FUTURE: ioError e
        Right inpStr -> do
            liftThrows $ 
                case mode of
                    True -> readExpr $ T.pack inpStr
                    _ -> return $ Text $ T.pack inpStr
readProc _ args = if length args == 1
                     then throwError $ TypeMismatch "port" $ List args
                     else throwError $ NumArgs (Just 1) args

-- |Read character from port
--
--   LispVal Arguments:
--
--   * Port
--
--   Returns: Char
--
readCharProc :: (MonadIO m, ReadRef r m, PtrEq m r) => (Handle -> IO Char) -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
readCharProc func [p@(Pointer {})] = recDerefPtrs p >>= box >>= readCharProc func
readCharProc func [] = readCharProc func [Port stdin Nothing]
readCharProc func [p@(Port _ _)] = do
  withOpenPort p $ \port -> do
    liftIO $ hSetBuffering port NoBuffering
    input <- liftIO $ try' (liftIO $ func port)
    liftIO $ hSetBuffering port LineBuffering
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port"
        Right inpChr -> do
            return $ Char inpChr
readCharProc _ args = if length args == 1
                         then throwError $ TypeMismatch "port" $ List args
                         else throwError $ NumArgs (Just 1) args

-- | Read a byte vector from the given port
--
--   Arguments
--
--   * Number - Number of bytes to read
--   * Port - Port to read from
--
--   Returns: ByteVector
readByteVector :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
readByteVector args = readBuffer args ByteVector

-- | Read a string from the given port
--
--   Arguments
--
--   * Number - Number of bytes to read
--   * Port - Port to read from
--
--   Returns: Text
readString :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
readString args = readBuffer args (Text . decodeUtf8)

-- |Helper function to read n bytes from a port into a buffer
readBuffer :: MonadIO m => [LispVal m r] -> (BSU.ByteString -> LispVal m r) -> IOThrowsError m r (LispVal m r)
readBuffer [Number n, Port port _] rvfnc = do
    input <- liftIO $ try' (liftIO $ BS.hGet port $ fromInteger n)
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port"
        Right inBytes -> do
            if BS.null inBytes
               then return $ EOF
               else return $ rvfnc inBytes
readBuffer args _ = if length args == 2
                       then throwError $ TypeMismatch "(k port)" $ List args
                       else throwError $ NumArgs (Just 2) args

-- |Write to the given port
--
--   LispVal Arguments:
--
--   * LispVal
--
--   * Port (optional)
--
--   Returns: (None)
--
{- writeProc :: --forall a (m :: * -> *).
             (MonadIO m, MonadError LispError m) =>
             (Handle -> LispVal m r -> IO a) -> [LispVal m r] -> m (LispVal m r) -}
writeProc :: (MonadIO m, ReadRef r m, PtrEq m r)
          => (Handle -> LispVal m r -> IO a)
          -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
writeProc func [obj] = do
    dobj <- recDerefPtrs obj -- Last opportunity to do this before writing
    writeProc func [dobj, Port stdout Nothing]
writeProc func [obj, Port port _] = do
    dobj <- recDerefPtrs obj -- Last opportunity to do this before writing
    output <- liftIO $ try' (liftIO $ func port dobj)
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeProc _ other = if length other == 2
                     then throwError $ TypeMismatch "(value port)" $ List other
                     else throwError $ NumArgs (Just 2) other

-- |Write character to the given port
--
--   Arguments:
--
--   * Char - Value to write
--
--   * Port (optional) - Port to write to, defaults to standard output
--
--   Returns: (None)
--
writeCharProc :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
writeCharProc [obj] = writeCharProc [obj, Port stdout Nothing]
writeCharProc [obj@(Char _), Port port _] = do
    output <- liftIO $ try' (liftIO $ (hPutStr port $ show obj))
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeCharProc other = if length other == 2
                     then throwError $ TypeMismatch "(character port)" $ List other
                     else throwError $ NumArgs (Just 2) other

-- | Write a byte vector to the given port
--
--   Arguments
--
--   * ByteVector
--   * Port
--
--   Returns: (unspecified)
writeByteVector :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
writeByteVector args = writeBuffer args bv2b
  where
    bv2b obj = do
        ByteVector bs <- recDerefPtrs obj -- Last opportunity to do this before writing
        return bs

-- | Write a string to the given port
--
--   Arguments
--
--   * Text
--   * Port
--
--   Returns: (unspecified)
writeString :: (MonadIO m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
writeString args = writeBuffer args str2b
  where
    str2b obj = do
        Text str <- recDerefPtrs obj -- Last opportunity to do this before writing
        return $ encodeUtf8 str

-- |Helper function to write buffer-based data to output port
writeBuffer :: MonadIO m => [LispVal m r] -> (LispVal m r -> IOThrowsError m r BSU.ByteString) -> IOThrowsError m r (LispVal m r)
writeBuffer [obj, Port port _] getBS = do
    bs <- getBS obj
    output <- liftIO $ try' (liftIO $ BS.hPut port bs)
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeBuffer other _ = 
    if length other == 2
       then throwError $ TypeMismatch "(bytevector port)" $ List other
       else throwError $ NumArgs (Just 2) other

-- |Determine if the given file exists
--
--   Arguments:
--
--   * Text - Filename to check
--
--   Returns: Bool - True if file exists, false otherwise
--
fileExists :: (MonadFilesystem m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
fileExists [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= fileExists
fileExists [Text filename] = do
    exists <- doesFileExist $ T.unpack filename
    return $ Bool exists
fileExists [] = throwError $ NumArgs (Just 1) []
fileExists args@(_ : _) = throwError $ NumArgs (Just 1) args

-- |Delete the given file
--
--   Arguments:
--
--   * Text - Filename to delete
--
--   Returns: Bool - True if file was deleted, false if an error occurred
--
deleteFile :: (MonadFilesystem m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
deleteFile [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= deleteFile
deleteFile [Text filename] = liftM Bool $ removeFile $ T.unpack filename
deleteFile [] = throwError $ NumArgs (Just 1) []
deleteFile args@(_ : _) = throwError $ NumArgs (Just 1) args

-- |Read the given file and return the raw string content 
--
--   Arguments:
--
--   * Text - Filename to read
--
--   Returns: Text - Actual text read from the file
--
readContents :: (MonadFilesystem m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
readContents [Text filename] = liftM Text $ readFile $ T.unpack filename
readContents [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= readContents
readContents [] = throwError $ NumArgs (Just 1) []
readContents args@(_ : _) = throwError $ NumArgs (Just 1) args

class Monad m => MonadFilesystem m where
  doesFileExist :: FilePath -> m Bool
  readFile :: FilePath -> m Text
  removeFile :: FilePath -> m Bool

instance MonadFilesystem IO where
  doesFileExist = IO.doesFileExist
  readFile = T.readFile
  removeFile filename = do
    output <- try' $ removeFile filename
    case output of
        Left _ -> return False
        Right _ -> return True

instance MonadFilesystem m => MonadFilesystem (ExceptT e m) where
  doesFileExist = lift . doesFileExist
  readFile = lift . readFile
  removeFile = lift . removeFile

class Monad m => MonadStdin m where
  getContents :: m Text

instance MonadStdin IO where
  getContents = T.getContents

instance MonadStdin m => MonadStdin (ExceptT e m) where
  getContents = lift getContents

-- |Parse the given file and return a list of scheme expressions
--
--   Arguments:
--
--   * Text - Filename to read
--
--   Returns: [LispVal m r] - Raw contents of the file parsed as scheme code
--
load :: MonadFilesystem m => Text -> IOThrowsError m r [LispVal m r]
load filename = do
  result <- doesFileExist $ T.unpack filename
  if result
     then do
        f <- readFile $ T.unpack filename

        case T.lines f of
            -- Skip comment header for shell scripts
            -- TODO: this could be much more robust
            (h : ls)
              | any (`T.isPrefixOf` h) ["#!/", "#! /"]
                -> liftThrows . readExprList $ T.unlines ls
            _ -> (liftThrows . readExprList) f
     else throwError $ Default $ "File does not exist: " <> filename

-- | Read the contents of the given scheme source file into a list
--
--   Arguments:
--
--   * Text - Filename to read
--
--   Returns: List - Raw contents of the file parsed as scheme code
--
readAll :: (MonadFilesystem m, MonadStdin m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
readAll [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= readAll
readAll [Text filename] = liftM List $ load filename
readAll [] = do -- read from stdin
    input <- getContents
    lisp <- (liftThrows . readExprList) input
    return $ List lisp
readAll args@(_ : _) = throwError $ NumArgs (Just 1) args

-- |Version of gensym that can be conveniently called from Haskell.
_gensym :: MonadSerial m => Text -> IOThrowsError m r (LispVal m r)
_gensym prefix = do
    u <- newSerial
    return $ Atom $ prefix <> (T.pack $ show $ Number u)

-- |Generate a (reasonably) unique symbol, given an optional prefix.
--  This function is provided even though it is not part of R5RS.
--
--   Arguments:
--
--   * Text - Prefix of the unique symbol
--
--   Returns: Atom
--
gensym :: (MonadSerial m, ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
gensym [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= gensym
gensym [Text prefix] = _gensym prefix
gensym [] = _gensym " g"
gensym args@(_ : _) = throwError $ NumArgs (Just 1) args


---------------------------------------------------
-- "Pure" primitives
---------------------------------------------------

-- List primitives

-- | Retrieve the first item from a list
--
--   Arguments:
--
--   * List (or DottedList)
--
--   Returns: LispVal - First item in the list
--
car :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
car [p@(Pointer _ _)] = derefPtr p >>= box >>= car
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Return the /tail/ of a list, with the first element removed
--
--   Arguments:
--
--   * List (or DottedList)
--
--   Returns: List (or DottedList)
--
cdr :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
cdr [p@(Pointer _ _)] = derefPtr p >>= box >>= cdr
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs (Just 1) badArgList

-- | The LISP @cons@ operation - create a list from two values
--
--   Arguments:
--
--   * LispVal
--
--   * LispVal
--
--   Returns: List (or DottedList) containing new value(s)
--
cons :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
cons [x, p@(Pointer _ _)] = do
  y <- derefPtr p
  cons [x, y]
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Create a new list
--
--   Arguments
--
--   * Number - Length of the list
--   * LispVal - Object to fill the list with (optional)
--
--   Returns: List
makeList :: [LispVal m r] -> ThrowsError m r (LispVal m r)
makeList [(Number n)] = makeList [Number n, List []]
makeList [(Number n), a] = do
  let l = replicate (fromInteger n) a
  return $ List l
makeList [badType] = throwError $ TypeMismatch "integer" badType
makeList badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a copy of a list
--
--   Arguments
--
--   * List
--
--   Returns: List
listCopy :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
listCopy [p@(Pointer _ _)] = do
  l <- derefPtr p
  listCopy [l]
listCopy [(List ls)] = return $ List ls
listCopy [badType] = return badType
listCopy badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a copy of a vector
--
--   Arguments
--
--   * Vector
--   * Number - Start copying the vector from this element (optional)
--   * Number - Stop copying the vector at this element (optional)
--
--   Returns: Vector
vectorCopy :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
vectorCopy (p@(Pointer _ _) : args) = do
  v <- derefPtr p
  vectorCopy (v : args)
vectorCopy [Vector vs] = do
    let l = elems vs
    return $ Vector $ listArray (0, length l - 1) l 
vectorCopy [Vector vs, Number start] = do
    let l = drop (fromInteger start) $ 
              elems vs
    return $ Vector $ listArray (0, length l - 1) l 
vectorCopy [Vector vs, Number start, Number end] = do
    let l = take (fromInteger $ end - start) $
              drop (fromInteger start) $ 
                elems vs
    return $ Vector $ listArray (0, length l - 1) l 
vectorCopy [badType] = return badType
vectorCopy badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Use pointer equality to compare two objects if possible, otherwise
--   fall back to the normal equality comparison
eq :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
eq [(Pointer pA envA), (Pointer pB envB)] = do
    return $ Bool $ (pA == pB) && ((bindings envA) == (bindings envB))
--    if pA == pB 
--       then do
--         refA <- getNamespacedRef envA varNamespace pA
--         refB <- getNamespacedRef envB varNamespace pB
--         return $ Bool $ refA == refB
--       else return $ Bool False
eq args = recDerefToFnc eqv args

-- | Recursively compare two LispVals for equality
--
--   Arguments:
--
--   * LispVal
--
--   * LispVal
--
--   Returns: Bool - True if equal, false otherwise
--
equal :: [LispVal m r] -> ThrowsError m r (LispVal m r)
equal [(Vector arg1), (Vector arg2)] = eqvList equal [List $ (elems arg1), List $ (elems arg2)]
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs <> [x], List $ ys <> [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs (Just 2) badArgList

-- ------------ Vector Primitives --------------

-- | Create a new vector
--
--   Arguments:
--
--   * Number - Length of the vector
--
--   * LispVal - Value to fill the vector with
--
--   Returns: Vector
--
makeVector :: [LispVal m r] -> ThrowsError m r (LispVal m r)
makeVector [(Number n)] = makeVector [Number n, List []]
makeVector [(Number n), a] = do
  let l = replicate (fromInteger n) a
  return $ Vector $ (listArray (0, length l - 1)) l
makeVector [badType] = throwError $ TypeMismatch "integer" badType
makeVector badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a vector from the given lisp values
--
--   Arguments:
--
--   * LispVal (s)
--
--   Returns: Vector
--
buildVector :: [LispVal m r] -> ThrowsError m r (LispVal m r)
buildVector lst@(_ : _) = do
  return $ Vector $ (listArray (0, length lst - 1)) lst
buildVector badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Determine the length of the given vector
--
--   Arguments:
--
--   * Vector
--
--   Returns: Number
--
vectorLength :: [LispVal m r] -> ThrowsError m r (LispVal m r)
vectorLength [(Vector v)] = return $ Number $ toInteger $ length (elems v)
vectorLength [badType] = throwError $ TypeMismatch "vector" badType
vectorLength badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Retrieve the object at the given position of a vector
--
--   Arguments:
--
--   * Vector
--
--   * Number - Index of the vector to retrieve
--
--   Returns: Object at the given index
--
vectorRef :: [LispVal m r] -> ThrowsError m r (LispVal m r)
vectorRef [(Vector v), (Number n)] = do
    let len = toInteger $ (length $ elems v) - 1
    if n > len || n < 0
       then throwError $ Default "Invalid index"
       else return $ v ! (fromInteger n)
vectorRef [badType] = throwError $ TypeMismatch "vector integer" badType
vectorRef badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Convert the given vector to a list
--
--   Arguments:
--
--   * Vector
--
--   Returns: List
--
vectorToList :: [LispVal m r] -> ThrowsError m r (LispVal m r)
vectorToList [(Vector v)] = return $ List $ elems v
vectorToList [badType] = throwError $ TypeMismatch "vector" badType
vectorToList badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Convert the given list to a vector
--
--   Arguments:
--
--   * List to convert
--
--   Returns: Vector
--
listToVector :: [LispVal m r] -> ThrowsError m r (LispVal m r)
listToVector [(List l)] = return $ Vector $ (listArray (0, length l - 1)) l
listToVector [badType] = throwError $ TypeMismatch "list" badType
listToVector badArgList = throwError $ NumArgs (Just 1) badArgList

-- ------------ Bytevector Primitives --------------

-- | Create a new bytevector
--
--   Arguments:
--
--   * Number - Length of the new bytevector
--
--   * Number (optional) - Byte value to fill the bytevector with
--
--   Returns: ByteVector - A new bytevector
--
makeByteVector :: [LispVal m r] -> ThrowsError m r (LispVal m r)
makeByteVector [(Number n)] = do
  let ls = replicate (fromInteger n) (0 :: Word8)
  return $ ByteVector $ BS.pack ls
makeByteVector [Number n, Number byte] = do
  let ls = replicate (fromInteger n) (fromInteger byte :: Word8)
  return $ ByteVector $ BS.pack ls
makeByteVector [badType] = throwError $ TypeMismatch "integer" badType
makeByteVector badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Create new bytevector containing the given data
--
--   Arguments:
--
--   * Objects - Objects to convert to bytes for the bytevector
--
--   Returns: ByteVector - A new bytevector
--
byteVector :: [LispVal m r] -> ThrowsError m r (LispVal m r)
byteVector bs = do
 return $ ByteVector $ BS.pack $ map conv bs
 where 
   conv (Number n) = fromInteger n :: Word8
   conv _ = 0 :: Word8

byteVectorCopy :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)

-- | Create a copy of the given bytevector
--
--   Arguments:
--
--   * ByteVector - Bytevector to copy
--
--   * Number (optional) - Start of the region to copy
--
--   * Number (optional) - End of the region to copy
--
--   Returns: ByteVector - A new bytevector containing the copied region
--
byteVectorCopy (p@(Pointer _ _) : lvs) = do
    bv <- derefPtr p
    byteVectorCopy (bv : lvs)
byteVectorCopy [ByteVector bv] = do
    return $ ByteVector $ BS.copy
        bv
byteVectorCopy [ByteVector bv, Number start] = do
    return $ ByteVector $ BS.drop 
        (fromInteger start)
        bv
byteVectorCopy [ByteVector bv, Number start, Number end] = do
    return $ ByteVector $ BS.take 
        (fromInteger $ end - start)
        (BS.drop 
            (fromInteger start)
            bv)
byteVectorCopy [badType] = throwError $ TypeMismatch "bytevector" badType
byteVectorCopy badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Append many bytevectors into a new bytevector
--
--   Arguments:
--
--   * ByteVector (one or more) - Bytevectors to concatenate
--
--   Returns: ByteVector - A new bytevector containing the values
--
byteVectorAppend :: forall m r. ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
byteVectorAppend bs = do
    let conv :: LispVal m r -> IOThrowsError m r BSU.ByteString
        conv p@(Pointer _ _) = derefPtr p >>= conv
        conv (ByteVector bvs) = return bvs
        conv _ = return BS.empty
    bs' <- mapM conv bs
    return $ ByteVector $ BS.concat bs'
-- TODO: error handling

-- | Find the length of a bytevector
--
--   Arguments:
--
--   * ByteVector
--
--   Returns: Number - Length of the given bytevector
--
byteVectorLength :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
byteVectorLength [p@(Pointer _ _)] = derefPtr p >>= box >>= byteVectorLength
byteVectorLength [(ByteVector bv)] = return $ Number $ toInteger $ BS.length bv
byteVectorLength [badType] = throwError $ TypeMismatch "bytevector" badType
byteVectorLength badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Return object at the given index of a bytevector
--
--   Arguments:
--
--   * ByteVector
--
--   * Number - Index of the bytevector to query
--
--   Returns: Object at the index
--
byteVectorRef :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
byteVectorRef (p@(Pointer _ _) : lvs) = do
    bv <- derefPtr p
    byteVectorRef (bv : lvs)
byteVectorRef [(ByteVector bv), (Number n)] = do
    let len = toInteger $ (BS.length bv) - 1
    if n > len || n < 0
       then throwError $ Default "Invalid index"
       else return $ Number $ toInteger $ BS.index bv (fromInteger n)
byteVectorRef [badType] = throwError $ TypeMismatch "bytevector integer" badType
byteVectorRef badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Convert a bytevector to a string
--
--   Arguments:
--
--   * ByteVector
--
--   Returns: Text
--
byteVectorUtf2Str :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
byteVectorUtf2Str [p@(Pointer _ _)] = derefPtr p >>= box >>= byteVectorUtf2Str
byteVectorUtf2Str [(ByteVector bv)] = do
    return $ Text $ decodeUtf8 bv 
-- TODO: need to support other overloads of this function
byteVectorUtf2Str [badType] = throwError $ TypeMismatch "bytevector" badType
byteVectorUtf2Str badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Convert a string to a bytevector
--
--   Arguments:
--
--   * Text
--
--   Returns: ByteVector
--
byteVectorStr2Utf :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
byteVectorStr2Utf [p@(Pointer _ _)] = derefPtr p >>= box >>= byteVectorStr2Utf
byteVectorStr2Utf [(Text s)] = do
    return $ ByteVector $ encodeUtf8 s
-- TODO: need to support other overloads of this function
byteVectorStr2Utf [badType] = throwError $ TypeMismatch "string" badType
byteVectorStr2Utf badArgList = throwError $ NumArgs (Just 1) badArgList


-- ------------ Ptr Helper Primitives --------------

-- | A helper function to allow a pure function to work with pointers, by
--   dereferencing the leading object in the argument list if it is
--   a pointer. This is a special hash-table specific function that will
--   also dereference a hash table key if it is included.
wrapHashTbl :: (ReadRef r m, PtrEq m r) => ([LispVal m r] -> ThrowsError m r (LispVal m r)) -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
wrapHashTbl fnc [p@(Pointer _ _)] = do
  val <- derefPtr p
  liftThrows $ fnc [val]
wrapHashTbl fnc (p@(Pointer _ _) : key : args) = do
  ht <- derefPtr p
  k <- recDerefPtrs key
  liftThrows $ fnc (ht : k : args)
wrapHashTbl fnc args = liftThrows $ fnc args

-- | A helper function to allow a pure function to work with pointers, by
--   dereferencing the leading object in the argument list if it is
--   a pointer.
wrapLeadObj :: ReadRef r m => ([LispVal m r] -> ThrowsError m r (LispVal m r)) -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
wrapLeadObj fnc [p@(Pointer _ _)] = do
  val <- derefPtr p
  liftThrows $ fnc [val]
wrapLeadObj fnc (p@(Pointer _ _) : args) = do
  obj <- derefPtr p
  liftThrows $ fnc (obj : args)
wrapLeadObj fnc args = liftThrows $ fnc args

-- ------------ Hash Table Primitives --------------

-- Future: support (equal?), (hash) parameters

-- | Create a new hashtable
--
--   Arguments: (None)
--
--   Returns: HashTable
--
hashTblMake :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblMake _ = return $ HashTable $ Data.Map.fromList []

-- | Determine if a given object is a hashtable
--
--   Arguments:
--
--   * Object to inspect
--
--   Returns: Bool - True if arg was a hashtable, false otherwise
--
isHashTbl :: [LispVal m r] -> ThrowsError m r (LispVal m r)
isHashTbl [(HashTable _)] = return $ Bool True
isHashTbl _ = return $ Bool False

-- | Determine if the given key is found in the hashtable
--
--   Arguments:
--
--   * HashTable to search
--
--   * Key to search for
--
--   Returns: Bool - True if found, False otherwise
--
hashTblExists :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblExists [(HashTable ht), key] = do
  case Data.Map.lookup key ht of
    Just _ -> return $ Bool True
    Nothing -> return $ Bool False
hashTblExists [] = throwError $ NumArgs (Just 2) []
hashTblExists args@(_ : _) = throwError $ NumArgs (Just 2) args

-- | Retrieve the value from the hashtable for the given key.
--   An error is thrown if the key is not found.
--
--   Arguments:
--
--   * HashTable to copy
--
--   * Object that is the key to query the table for
--
--   Returns: Object containing the key's value
--
hashTblRef :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblRef [(HashTable ht), key] = do
  case Data.Map.lookup key ht of
    Just val -> return val
    Nothing -> throwError $ BadSpecialForm "Hash table does not contain key" key
hashTblRef [(HashTable ht), key, Func {}] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ NotImplemented "thunk"
{- FUTURE: a thunk can optionally be specified, this drives definition of /default
Nothing -> apply thunk [] -}
hashTblRef [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblRef badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Return the number of key/value associations in the hashtable
--
--   Arguments:
--
--   * HashTable
--
--   Returns: Number - number of associations
--
hashTblSize :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblSize [(HashTable ht)] = return $ Number $ toInteger $ Data.Map.size ht
hashTblSize [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblSize badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a list containing all key/value pairs in the hashtable
--
--   Arguments:
--
--   * HashTable
--
--   Returns: List of (key, value) pairs
--
hashTbl2List :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTbl2List [(HashTable ht)] = do
  return $ List $ map (\ (k, v) -> List [k, v]) $ Data.Map.toList ht
hashTbl2List [badType] = throwError $ TypeMismatch "hash-table" badType
hashTbl2List badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a list containing all keys in the hashtable
--
--   Arguments:
--
--   * HashTable
--
--   Returns: List containing the keys
--
hashTblKeys :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblKeys [(HashTable ht)] = do
  return $ List $ map fst $ Data.Map.toList ht
hashTblKeys [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblKeys badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a list containing all values in the hashtable
--
--   Arguments:
--
--   * HashTable
--
--   Returns: List containing the values
--
hashTblValues :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblValues [(HashTable ht)] = do
  return $ List $ map snd $ Data.Map.toList ht
hashTblValues [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblValues badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Create a new copy of a hashtable
--
--   Arguments:
--
--   * HashTable to copy
--
--   Returns: HashTable
--
hashTblCopy :: [LispVal m r] -> ThrowsError m r (LispVal m r)
hashTblCopy [(HashTable ht)] = do
  return $ HashTable $ Data.Map.fromList $ Data.Map.toList ht
hashTblCopy [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblCopy badArgList = throwError $ NumArgs (Just 1) badArgList

-- ------------ Text Primitives --------------

-- | Convert a list of characters to a string
--
--   Arguments:
--
--   * Character (one or more) - Character(s) to add to the string
--
--   Returns: Text - new string built from given chars
--
buildString :: [LispVal m r] -> ThrowsError m r (LispVal m r)
buildString [(Char c)] = return $ Text $ T.singleton c
buildString (Char c : rest) = do
  cs <- buildString rest
  case cs of
    Text s -> return $ Text $ T.singleton c <> s
    badType -> throwError $ TypeMismatch "character" badType
buildString [badType] = throwError $ TypeMismatch "character" badType
buildString badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Make a new string
--
--   Arguments:
--
--   * Number - number of characters in the string
--
--   * Char (optional) - Character to fill in each position of string.
--                       Defaults to space
--
--   Returns: Text - new string
--
makeString :: [LispVal m r] -> ThrowsError m r (LispVal m r)
makeString [(Number n)] = return $ doMakeString n ' ' ""
makeString [(Number n), (Char c)] = return $ doMakeString n c ""
makeString badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Helper function
doMakeString :: forall a m r . (Num a, Eq a) => a -> Char -> Text -> LispVal m r
doMakeString n char s =
    if n == 0
       then Text s
       else doMakeString (n - 1) char (s <> T.singleton char)

-- | Determine the length of the given string
--
--   Arguments:
--
--   * Text - Text to examine
--
--   Returns: Number - Length of the given string
--
stringLength :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringLength [p@(Pointer _ _)] = derefPtr p  >>= box >>= stringLength
stringLength [Text s] = return $ Number $ toInteger $ T.length s
stringLength [badType] = throwError $ TypeMismatch "string" badType
stringLength badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Get character at the given position of a string
--
--   Arguments:
--
--   * Text - Text to examine
--
--   * Number - Get the character at this position
--
--   Returns: Char
--
stringRef :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringRef [p@(Pointer _ _), k@(Number _)] = do
    s <- derefPtr p 
    stringRef [s, k]
stringRef [(Text s), (Number k)] = do
    let len = toInteger $ (T.length s) - 1
    if k > len || k < 0
       then throwError $ Default $ "Invalid index " <> (T.pack $ show k)
       else return $ Char $ s `T.index` fromInteger k
stringRef [badType] = throwError $ TypeMismatch "string number" badType
stringRef badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Get a part of the given string
--
--   Arguments:
--
--   * Text - Original string
--
--   * Number - Starting position of the substring
--
--   * Number - Ending position of the substring
--
--   Returns: Text - substring of the original string
--
substring :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
substring (p@(Pointer _ _) : lvs) = do
  s <- derefPtr p
  substring (s : lvs)
substring [(Text s), (Number start), (Number end)] =
  do let slength = fromInteger $ end - start
     let begin = fromInteger start
     return $ Text $ (T.take slength . T.drop begin) s
substring [badType] = throwError $ TypeMismatch "string number number" badType
substring badArgList = throwError $ NumArgs (Just 3) badArgList

-- | Perform a case insensitive comparison of the given strings
--
--   Arguments:
--
--   * Text - Text to compare
--
--   * Text - Text to compare
--
--   Returns: Bool - True if strings are equal, false otherwise
--
stringCIEquals :: (ReadRef r m, PtrEq m r) => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringCIEquals args = do
  List dargs <- recDerefPtrs $ List args
  case dargs of
    [(Text str1), (Text str2)] -> return $ Bool $ T.toCaseFold str1 /= T.toCaseFold str2
    [badType] -> throwError $ TypeMismatch "string string" badType
    badArgList -> throwError $ NumArgs (Just 2) badArgList

-- |Helper function
stringCIBoolBinop :: (ReadRef r m, PtrEq m r) => (Text -> Text -> Bool) -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringCIBoolBinop op args = do 
  List dargs <- recDerefPtrs $ List args -- Deref any pointers
  case dargs of
    [(Text s1), (Text s2)] ->
      liftThrows $ boolBinop unpackStr op [(Text $ T.toCaseFold s1), (Text $ T.toCaseFold s2)]
    [badType] -> throwError $ TypeMismatch "string string" badType
    badArgList -> throwError $ NumArgs (Just 2) badArgList

toCaseFoldChar :: Char -> Char
toCaseFoldChar = (`T.index` 0) . T.toCaseFold . T.singleton

-- |Helper function
charCIBoolBinop :: (Char -> Char -> Bool) -> [LispVal m r] -> ThrowsError m r (LispVal m r)
charCIBoolBinop op [(Char s1), (Char s2)] = boolBinop unpackChar op [(Char $ toCaseFoldChar s1), (Char $ toCaseFoldChar s2)]
charCIBoolBinop _ [badType] = throwError $ TypeMismatch "character character" badType
charCIBoolBinop _ badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Append all given strings together into a single string
--
--   Arguments:
--
--   * Text (one or more) - Text(s) to concatenate
--
--   Returns: Text - all given strings appended together as a single string
--
stringAppend :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringAppend (p@(Pointer _ _) : lvs) = do
  s <- derefPtr p
  stringAppend (s : lvs)
stringAppend [(Text s)] = return $ Text s -- Needed for "last" string value
stringAppend (Text st : sts) = do
  rest <- stringAppend sts
  case rest of
    Text s -> return $ Text $ st <> s
    other -> throwError $ TypeMismatch "string" other
stringAppend [] = return $ Text ""
stringAppend [badType] = throwError $ TypeMismatch "string" badType
stringAppend badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Convert given string to a number
--
--   Arguments:
--
--   * Text - Text to convert
--
--   * Number (optional) - Number base to convert from, defaults to base 10 (decimal)
--
--   Returns: Numeric type, actual type will depend upon given string
--
stringToNumber :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringToNumber (p@(Pointer _ _) : lvs) = do
  s <- derefPtr p
  stringToNumber (s : lvs)
stringToNumber [(Text s)] = do
  result <- liftThrows $ readExpr s
  case result of
    n@(Number _) -> return n
    n@(Rational _) -> return n
    n@(Float _) -> return n
    n@(Complex _) -> return n
    _ -> return $ Bool False
stringToNumber [(Text s), Number radix] = do
  case radix of
    2 -> stringToNumber [Text $ "#b" <> s]
    8 -> stringToNumber [Text $ "#o" <> s]
    10 -> stringToNumber [Text s]
    16 -> stringToNumber [Text $ "#x" <> s]
    _ -> throwError $ Default $ "Invalid radix: " <> T.pack (show radix)
stringToNumber [badType] = throwError $ TypeMismatch "string" badType
stringToNumber badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Convert the given string to a list of chars
--
--   Arguments:
--
--   * Text - string to deconstruct
--
--   Returns: List - list of characters
--
stringToList :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringToList (p@(Pointer _ _) : ps) = do
    p' <- derefPtr p 
    stringToList (p' : ps)
stringToList [(Text s)] = return $ List $ map Char $ T.unpack s
stringToList [Text s, Number start] = 
    return $ List $ map Char $ T.unpack $ trimStartText start s
stringToList [Text s, Number start, Number end] = 
    return $ List $ map Char $ T.unpack $ trimStartEndText start end s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Utility function to trim from the start of a Text
trimStartText :: Integer -> Text -> Text
trimStartText start = T.drop (fromInteger start)

-- |Utility function to trim from start/end of a Text
trimStartEndText :: Integer -> Integer -> Text -> Text
trimStartEndText start end ls = 
  T.take (fromInteger $ end - start) $ T.drop (fromInteger start) ls

-- |Utility function to trim from the start of a Vector
trimStart :: Integer -> [a] -> [a]
trimStart start = drop (fromInteger start)

-- |Utility function to trim from start/end of a Vector
trimStartEnd :: Integer -> Integer -> [a] -> [a]
trimStartEnd start end ls = 
  take (fromInteger $ end - start) $ drop (fromInteger start) ls

-- | Convert the given list of characters to a string
--
--   Arguments:
--
--   * List - list of chars to convert
--
--   Returns: Text - Resulting string
--
listToString :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
listToString [p@(Pointer _ _)] = derefPtr p >>= box >>= listToString
listToString [(List [])] = return $ Text ""
listToString [(List l)] = liftThrows $ buildString l
listToString [badType] = throwError $ TypeMismatch "list" badType
listToString [] = throwError $ NumArgs (Just 1) []
listToString args@(_ : _) = throwError $ NumArgs (Just 1) args

-- | Convert a string to a vector
--
--   Arguments
--
--   * Text
--
--   Returns: Vector
stringToVector :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringToVector args = do
    List l <- stringToList args
    return $ Vector $ listArray (0, length l - 1) l

-- | Convert a vector to a string
--
--   Arguments
--
--   * Vector
--
--   Returns: Text
vectorToString :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
vectorToString (p@(Pointer _ _) : ps) = do
    p' <- derefPtr p
    vectorToString (p' : ps)
vectorToString [(Vector v)] = do
    let l = elems v
    case l of
        [] -> return $ Text ""
        _ -> liftThrows $ buildString l
vectorToString [Vector v, Number start] = do
    listToString [List $ trimStart start (elems v)]
vectorToString [Vector v, Number start, Number end] = do
    listToString [List $ trimStartEnd start end (elems v)]
vectorToString [badType] = throwError $ TypeMismatch "vector" badType
vectorToString [] = throwError $ NumArgs (Just 1) []
vectorToString args@(_ : _) = throwError $ NumArgs (Just 1) args

-- | Create a copy of the given string
--
--   Arguments:
--
--   * Text - Text to copy
--
--   Returns: Text - New copy of the given string
--
stringCopy :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
stringCopy (p@(Pointer _ _) : args) = do
    s <- derefPtr p 
    stringCopy (s : args)
stringCopy [Text s] = return $ Text s
stringCopy [Text s, Number start] = do
    return $ Text $ trimStartText start s
stringCopy [Text s, Number start, Number end] = do
    return $ Text $ trimStartEndText start end s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs (Just 2) badArgList

-- | Determine if given object is an improper list
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if improper list, False otherwise
--
isDottedList :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isDottedList ([p@(Pointer _ _)]) = derefPtr p >>= box >>= isDottedList
isDottedList ([DottedList _ _]) = return $ Bool True
-- Must include lists as well since they are made up of /chains/ of pairs
isDottedList ([List []]) = return $ Bool False
isDottedList ([List _]) = return $ Bool True
isDottedList _ = return $ Bool False

-- | Determine if given object is a procedure
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if procedure, False otherwise
--
isProcedure :: [LispVal m r] -> ThrowsError m r (LispVal m r)
isProcedure ([Continuation {}]) = return $ Bool True
isProcedure ([PrimitiveFunc _]) = return $ Bool True
isProcedure ([Func {}]) = return $ Bool True
isProcedure ([HFunc {}]) = return $ Bool True
isProcedure ([IOFunc _]) = return $ Bool True
isProcedure ([EvalFunc _]) = return $ Bool True
isProcedure ([CustFunc _]) = return $ Bool True
isProcedure _ = return $ Bool False

-- | Determine if given object is a vector
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if vector, False otherwise
--
isVector :: ReadRef r m => LispVal m r -> IOThrowsError m r (LispVal m r)
isVector p@(Pointer _ _) = derefPtr p >>= isVector
isVector (Vector vs) = do
    case elems vs of
        -- Special exception for record types
        ((Atom "  record-marker  ") : _) -> return $ Bool False
        _ -> return $ Bool True
isVector _ = return $ Bool False

-- | Determine if given object is a record
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if record, False otherwise
--
isRecord :: ReadRef r m => LispVal m r -> IOThrowsError m r (LispVal m r)
isRecord p@(Pointer _ _) = derefPtr p >>= isRecord
isRecord (Vector vs) = do
    case (elems vs) of
        -- Special exception for record types
        ((Atom "  record-marker  ") : _) -> return $ Bool True
        _ -> return $ Bool False
isRecord _ = return $ Bool False

-- | Determine if given object is a bytevector
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if bytevector, False otherwise
--
isByteVector :: ReadRef r m => LispVal m r -> IOThrowsError m r (LispVal m r)
isByteVector p@(Pointer _ _) = derefPtr p >>= isVector
isByteVector (ByteVector _) = return $ Bool True
isByteVector _ = return $ Bool False

-- | Determine if given object is a list
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if list, False otherwise
--
isList :: ReadRef r m => LispVal m r -> IOThrowsError m r (LispVal m r)
isList p@(Pointer _ _) = derefPtr p >>= isList
isList (List _) = return $ Bool True
isList _ = return $ Bool False

-- | Determine if given object is the null list
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if null list, False otherwise
--
isNull :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isNull ([p@(Pointer _ _)]) = derefPtr p >>= box >>= isNull
isNull ([List []]) = return $ Bool True
isNull _ = return $ Bool False

-- | Determine if given object is the EOF marker
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if EOF, False otherwise
--
isEOFObject :: [LispVal m r] -> ThrowsError m r (LispVal m r)
isEOFObject ([EOF]) = return $ Bool True
isEOFObject _ = return $ Bool False

-- | Return the EOF object
eofObject :: [LispVal m r] -> ThrowsError m r (LispVal m r)
eofObject _ = return $ EOF

-- | Determine if given object is a symbol
--
--   Arguments:
--
--   * Value to check
--
--   Returns: Bool - True if a symbol, False otherwise
--
isSymbol :: [LispVal m r] -> ThrowsError m r (LispVal m r)
isSymbol ([Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

-- | Convert the given symbol to a string
--
--   Arguments:
--
--   * Atom - Symbol to convert
--
--   Returns: Text
--
symbol2String :: [LispVal m r] -> ThrowsError m r (LispVal m r)
symbol2String ([Atom a]) = return $ Text a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = throwError $ NumArgs (Just 1) []
symbol2String args@(_ : _) = throwError $ NumArgs (Just 1) args

-- | Convert a string to a symbol
--
--   Arguments:
--
--   * Text (or pointer) - Text to convert
--
--   Returns: Atom
--
string2Symbol :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
string2Symbol ([p@(Pointer _ _)]) = derefPtr p >>= box >>= string2Symbol
string2Symbol ([Text s]) = return $ Atom s
string2Symbol [] = throwError $ NumArgs (Just 1) []
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol args@(_ : _) = throwError $ NumArgs (Just 1) args

-- | Convert a character to uppercase
--
--   Arguments:
--
--   * Char
--
--   Returns: Char - Character in uppercase
--
charUpper :: [LispVal m r] -> ThrowsError m r (LispVal m r)
charUpper [Char c] = return $ Char $ toUpper c
charUpper [notChar] = throwError $ TypeMismatch "char" notChar
charUpper args = throwError $ NumArgs (Just 1) args

-- | Convert a character to lowercase
--
--   Arguments:
--
--   * Char
--
--   Returns: Char - Character in lowercase
--
charLower :: [LispVal m r] -> ThrowsError m r (LispVal m r)
charLower [Char c] = return $ Char $ toCaseFoldChar c
charLower [notChar] = throwError $ TypeMismatch "char" notChar
charLower args = throwError $ NumArgs (Just 1) args

-- | Return integer value of a char digit
--
--   Arguments
--
--   * Char
--
--   Returns: Number, or False
charDigitValue :: [LispVal m r] -> ThrowsError m r (LispVal m r)
charDigitValue [Char c] = do
    -- This is not really good enough, since unicode chars
    -- are supposed to be processed, and r7rs does not
    -- spec hex chars, but it is a decent start for now...
    if isHexDigit c
       then return $ Number $ toInteger $ digitToInt c
       else return $ Bool False
charDigitValue [notChar] = throwError $ TypeMismatch "char" notChar
charDigitValue args = throwError $ NumArgs (Just 1) args

-- | Convert from a charater to an integer
--
--   Arguments:
--
--   * Char
--
--   Returns: Number
--
char2Int :: [LispVal m r] -> ThrowsError m r (LispVal m r)
char2Int [Char c] = return $ Number $ toInteger $ ord c 
char2Int [notChar] = throwError $ TypeMismatch "char" notChar
char2Int args = throwError $ NumArgs (Just 1) args

-- | Convert from an integer to a character
--
--   Arguments:
--
--   * Number
--
--   Returns: Char
--
int2Char :: [LispVal m r] -> ThrowsError m r (LispVal m r)
int2Char [Number n] = return $ Char $ chr $ fromInteger n 
int2Char [notInt] = throwError $ TypeMismatch "integer" notInt
int2Char args = throwError $ NumArgs (Just 1) args

-- |Determine if given character satisfies the given predicate
charPredicate :: (Char -> Bool) -> [LispVal m r] -> ThrowsError m r (LispVal m r)
charPredicate cpred ([Char c]) = return $ Bool $ cpred c 
charPredicate _ _ = return $ Bool False

-- | Determine if the given value is a character
--
--   Arguments:
--
--   * LispVal m r to check
--
--   Returns: Bool - True if the argument is a character, False otherwise
--
isChar :: [LispVal m r] -> ThrowsError m r (LispVal m r)
isChar ([Char _]) = return $ Bool True
isChar _ = return $ Bool False

-- | Determine if the given value is a string
--
--   Arguments:
--
--   * LispVal m r to check
--
--   Returns: Bool - True if the argument is a string, False otherwise
--
isString :: ReadRef r m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
isString [p@(Pointer _ _)] = derefPtr p >>= box >>= isString
isString ([Text _]) = return $ Bool True
isString _ = return $ Bool False

-- | Determine if the given value is a boolean
--
--   Arguments:
--
--   * LispVal m r to check
--
--   Returns: Bool - True if the argument is a boolean, False otherwise
--
isBoolean :: [LispVal m r] -> ThrowsError m r (LispVal m r)
isBoolean ([Bool _]) = return $ Bool True
isBoolean _ = return $ Bool False

-- | Determine if multiple boolean values are the same
--
--   Arguments
--
--   * A list of Bool values
--
--   Returns: True if the list contains booleans that are the same, False otherwise
isBooleanEq :: [LispVal m r] -> LispVal m r
isBooleanEq (Bool a : Bool b : bs)
    | a == b = isBooleanEq (Bool b : bs)
    | otherwise = Bool False
isBooleanEq [Bool _] = Bool True
isBooleanEq _ = Bool False

-- | Determine if multiple symbols values are the same
--
--   Arguments
--
--   * A list of Atom values
--
--   Returns: True if all of the symbols are the same, False otherwise
isSymbolEq :: [LispVal m r] -> LispVal m r
isSymbolEq (Atom a : Atom b : bs)
    | a == b = isSymbolEq (Atom b : bs)
    | otherwise = Bool False
isSymbolEq [Atom _] = Bool True
isSymbolEq _ = Bool False

-- |Utility type for unpackEquals
data Unpacker m r = forall a . Eq a => AnyUnpacker (LispVal m r -> ThrowsError m r a)

-- |Determine if two lispval's are equal
unpackEquals :: LispVal m r -> LispVal m r -> Unpacker m r -> ThrowsError m r Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

-- |Helper function to perform a binary logic operation on two LispVal arguments.
boolBinop :: (LispVal m r -> ThrowsError m r a) -> (a -> a -> Bool) -> [LispVal m r] -> ThrowsError m r (LispVal m r)
boolBinop unpacker op args = if length args < 2
                             then throwError $ NumArgs (Just 2) args
                             else do
                                 result <- cmp (head args) (tail args)
                                 return $ Bool result
 where 
    cmp b1 (b2 : bs) = do
      b1' <- unpacker b1
      b2' <- unpacker b2
      let result = op b1' b2'
      if result
         then cmp b2 bs
         else return False
    cmp _ _ = return True
       

-- |Perform the given function against a single LispVal argument
unaryOp :: (LispVal m r -> ThrowsError m r (LispVal m r)) -> [LispVal m r] -> ThrowsError m r (LispVal m r)
unaryOp f [v] = f v
unaryOp _ [] = throwError $ NumArgs (Just 1) []
unaryOp _ args@(_ : _) = throwError $ NumArgs (Just 1) args

-- |Same as unaryOp but in the IO monad
unaryOp' :: Monad m => (LispVal m r -> IOThrowsError m r (LispVal m r)) -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
unaryOp' f [v] = f v
unaryOp' _ [] = throwError $ NumArgs (Just 1) []
unaryOp' _ args@(_ : _) = throwError $ NumArgs (Just 1) args

-- |Perform boolBinop against two string arguments
strBoolBinop :: (ReadRef r m, PtrEq m r) => (Text -> Text -> Bool) -> [LispVal m r] -> IOThrowsError m r (LispVal m r)
strBoolBinop fnc args = do
  List dargs <- recDerefPtrs $ List args -- Deref any pointers
  liftThrows $ boolBinop unpackStr fnc dargs

-- |Perform boolBinop against two char arguments
charBoolBinop :: (Char -> Char -> Bool)
              -> [LispVal m r] -> ThrowsError m r (LispVal m r)
charBoolBinop = boolBinop unpackChar

-- |Perform boolBinop against two boolean arguments
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal m r] -> ThrowsError m r (LispVal m r)
boolBoolBinop = boolBinop unpackBool

-- | Unpack a LispVal char
--
--   Arguments:
--
--   * Char - Character to unpack
--
unpackChar :: LispVal m r -> ThrowsError m r Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "character" notChar

-- | Unpack a LispVal Text
--
--   Arguments:
--
--   * Text - Text to unpack
--
unpackStr :: LispVal m r -> ThrowsError m r Text
unpackStr (Text s) = return s
unpackStr (Number s) = return $ T.pack $ show s
unpackStr (Bool s) = return $ T.pack $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- | Unpack a LispVal boolean
--
--   Arguments:
--
--   * Bool - Boolean to unpack
--
unpackBool :: LispVal m r -> ThrowsError m r Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- | Return the current time, in seconds
--
--   Arguments: (None)
--
--   Returns: Current UNIX timestamp in seconds
currentTimestamp :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
currentTimestamp _ = do
    cur <- liftIO $ Data.Time.Clock.POSIX.getPOSIXTime
    return $ Float $ realToFrac cur

-- | Execute a system command on the underlying OS.
--
--   Arguments:
--
--   * Text - Command to execute
--
--   Returns: Integer - program return status
--
system :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
system [Text cmd] = do
    result <- liftIO $ System.Process.system $ T.unpack cmd
    case result of
        ExitSuccess -> return $ Number 0
        ExitFailure code -> return $ Number $ toInteger code
system err = throwError $ TypeMismatch "string" $ List err

-- | Retrieve all environment variables
--
--   Arguments: (none)
--
--   Returns: List - list of key/value alists
--
getEnvVars :: MonadIO m => [LispVal m r] -> IOThrowsError m r (LispVal m r)
getEnvVars _ = do
    vars <- liftIO $ SE.getEnvironment
    return $ List $ map (\(k, v) -> DottedList [Text $ T.pack k] (Text $ T.pack v)) vars

-- FUTURE (?):
-- systemRead :: [LispVal m r] -> IOThrowsError m r (LispVal m r)
-- systemRead ((Text cmd) : args) = do
--   let args' = map conv args
--   result <- liftIO $ readProcess cmd args' ""
--   return $ Text result
--  where
--    conv (Text s) = s
--    conv _ = ""
