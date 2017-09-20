{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Marskow.Types where

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.State (MonadState, StateT, runStateT)
import qualified Data.Sequence as S

import qualified Cheapskate.Types as C.T
import Language.Haskell.Interpreter as I

data MarskowState = MarskowState {
    -- lastCodeBlock :: Maybe C.T.Block,
    blocks        :: [C.T.Block] -- C.T.Blocks
}

{- Is that MonadIO necessary? -}
newtype MarskowT m a = MarskowT { runMT :: StateT MarskowState (I.InterpreterT m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState MarskowState,
              MonadThrow, MonadCatch, MonadMask)

instance MonadTrans MarskowT where
  lift = MarskowT . lift . lift

liftInterpreter :: (Monad m, MonadMask m) => I.InterpreterT m a -> MarskowT m a
liftInterpreter = MarskowT . lift

runMarskowTWithInterpreter :: (Monad m)
  => MarskowT m a
  -> I.InterpreterT m (a, MarskowState)
runMarskowTWithInterpreter mt =
  runStateT (runMT mt) initialState
    where initialState = MarskowState {
        -- lastCodeBlock = lastCodeBlock,
        blocks = mempty
    }

runMarskowTWithoutInterpreter
  :: (Monad m, MonadIO m, MonadMask m)
  => MarskowT m a
  -> m (Either I.InterpreterError (a, MarskowState))
runMarskowTWithoutInterpreter mt = I.runInterpreter (runMarskowTWithInterpreter mt)

runMarskowT
  :: (Monad m, MonadIO m, MonadMask m)
  => MarskowT m a
  -> m (Either I.InterpreterError (a, C.T.Blocks))
runMarskowT mt = fmap (extractState <$>) <$> runMarskowTWithoutInterpreter mt
  where
    extractState = S.fromList . reverse . blocks
