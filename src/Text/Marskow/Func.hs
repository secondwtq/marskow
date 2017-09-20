
module Text.Marskow.Func where

import Text.Marskow.Types (MarskowT(..), blocks, liftInterpreter, runMarskowTWithInterpreter)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask, catch)
import Control.Monad.State (modify, get)

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import qualified Cheapskate.Types as C.T
import qualified Language.Haskell.Interpreter as I
import Text.XML.HXT.Core

block :: (Monad m, MonadIO m, MonadMask m, Typeable m) => C.T.Block -> MarskowT m ()

data MarskowRuntimeError
  = ScriptError I.InterpreterError
  | OtherError
  deriving (Show)

exceptionHandler :: (I.MonadInterpreter m)
  => I.InterpreterError
  -> m (Either MarskowRuntimeError a)
exceptionHandler e = return $ Left $ ScriptError e

interpretCatched :: (I.MonadInterpreter m, Typeable a)
  => String
  -> a
  -> m (Either MarskowRuntimeError a)
interpretCatched expr wit = catch (Right <$> I.interpret expr wit) exceptionHandler

type ReturnT = Integer
type MResultT a = Either MarskowRuntimeError a

interpret :: (Monad m, MonadIO m, MonadMask m, Typeable m)
  => String
  -> MarskowT m (MResultT (MarskowT m ReturnT))
interpret code = liftInterpreter $ interpretCatched code I.infer

block bl@(C.T.HtmlBlock t) = do
  liftIO $ print t
  let comments = runLA (xread >>> getCmt) $ T.unpack t
  case listToMaybe comments of
    Nothing -> justPushBlock bl
    Just code -> do
      maybeResult <- interpret code
      case maybeResult of
        Left err -> liftIO $ print err
        Right resultM -> do
          result <- resultM
          liftIO $ putStrLn $ "OUTPUT: " ++ show result
          return ()

  -- liftIO $ print maybeCommentCode
  -- val <- catch (maybe (return 0) (\d -> liftInterpreter $ I.interpret d (I.as :: Int)) maybeCommentCode) ((>> return 0) . liftIO . (print :: I.InterpreterError -> IO ()))
  -- liftIO $ print val
  -- return ()

block (C.T.List tight listType items) = do
  state <- get
  let initialState = state { blocks = mempty }
  newItems <- MarskowT (lift $ foldl foldf (return [ ]) items)
  let listBlock = C.T.List tight listType newItems
  justPushBlock listBlock
  where
    foldf itemsI item = do
      items <- itemsI
      (_, state) <- runMarskowTWithInterpreter $ foldBlocks item
      return $ item : items

block bl = justPushBlock bl

justPushBlock :: (Monad m) => C.T.Block -> MarskowT m ()
justPushBlock bl = modify (\s -> s { blocks = bl : blocks s })

foldBlocks :: (Monad m, MonadIO m, MonadMask m, Typeable m) => C.T.Blocks -> MarskowT m ()
foldBlocks = foldl bindSeq (return ())
  where bindSeq m bl = m >> block bl

tag :: Text -> MarskowT m ()
tag = undefined

marskowTest :: (Monad m, MonadIO m) => MarskowT m ()
marskowTest = liftIO $ putStrLn "marskowTest!"
