{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_monad_logger(0,3,8)
#else
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#endif
module Control.Monad.Trans.PureLogger (
    module Control.Monad.Trans.PureLogger
#if MIN_VERSION_monad_logger(0,3,8)
    , NoLoggingT(..)
#endif
    ) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Logger hiding (NoLoggingT, runNoLoggingT)
import Control.Applicative                  (Applicative)
import Data.Foldable                        (toList)
import Data.Monoid                          (Monoid, mempty)
import System.Log.FastLogger                (LogStr, toLogStr)

#if MIN_VERSION_monad_logger(0,3,8)
import Control.Monad.Logger                 (NoLoggingT(..), runNoLoggingT)
#else
import Control.Monad (liftM, ap)
import Control.Applicative                  (pure, (<*>))
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Control (MonadBaseControl (..), MonadTransControl (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource (liftResourceT), MonadThrow, monadThrow)
import Control.Monad.Base (MonadBase (liftBase))
#if MIN_VERSION_conduit_extra(1,1,0)
import Data.Conduit.Lazy (MonadActive, monadActive)
#endif

#if MIN_VERSION_resourcet(1,1,0)
import Control.Monad.Trans.Resource (throwM)
import Control.Monad.Catch (MonadCatch (..)
#if MIN_VERSION_exceptions(0,6,0)
    , MonadMask (..)
#endif
    )
#endif

#endif

import Data.Sequence                        (Seq)
import qualified Data.Sequence              as S

class QuickAppendable t where
    qappend :: a -> t a -> t a

    -- | from new to old
    descList :: t a -> [a]
    descList = reverse . ascList

    -- | from old to new
    ascList :: t a -> [a]
    ascList = reverse . descList

instance QuickAppendable [] where
    qappend = (:)
    descList = id

instance QuickAppendable Seq where
    qappend = flip (S.|>)
    ascList = toList
    descList = toList . S.reverse

newtype PureLoggingT t m a = PureLoggingT
                                { unPureLoggingT :: StateT (t LogStr) m a }

deriving instance MonadTrans (PureLoggingT t)
deriving instance Functor m => Functor (PureLoggingT t m)
deriving instance (Functor m, Monad m) => Applicative (PureLoggingT t m)
deriving instance Monad m => Monad (PureLoggingT t m)

instance (QuickAppendable t, Monad m) => MonadLogger (PureLoggingT t m) where
    monadLoggerLog loc src level msg = do
        PureLoggingT $ modify $ qappend $
                    defaultLogStr loc src level $ toLogStr msg

runPureLoggingT :: Monoid (t LogStr) => PureLoggingT t m a -> m (a, t LogStr)
runPureLoggingT = flip runStateT mempty . unPureLoggingT

#if MIN_VERSION_monad_logger(0,3,8)
#else
-- Workaround only
-- The following about NoLoggingT is copied from monad-logger project
-- It is here because monad-logger requires:
-- MonadIO m => MonadLogger (NoLoggingT m)
-- I just change that context to: Monad m
newtype NoLoggingT m a = NoLoggingT { runNoLoggingT :: m a }

instance Monad m => Functor (NoLoggingT m) where
    fmap = liftM

instance Monad m => Applicative (NoLoggingT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (NoLoggingT m) where
    return = NoLoggingT . return
    NoLoggingT ma >>= f = NoLoggingT $ ma >>= runNoLoggingT . f

instance MonadIO m => MonadIO (NoLoggingT m) where
    liftIO = Trans.lift . liftIO

#if MIN_VERSION_resourcet(1,1,0)
instance MonadThrow m => MonadThrow (NoLoggingT m) where
    throwM = Trans.lift . throwM

instance MonadCatch m => MonadCatch (NoLoggingT m) where
    catch (NoLoggingT m) c =
        NoLoggingT $ m `catch` \e -> runNoLoggingT (c e)
#if MIN_VERSION_exceptions(0,6,0)
instance MonadMask m => MonadMask (NoLoggingT m) where
#endif
    mask a = NoLoggingT $ mask $ \u -> runNoLoggingT (a $ q u)
      where q u (NoLoggingT b) = NoLoggingT $ u b
    uninterruptibleMask a =
        NoLoggingT $ uninterruptibleMask $ \u -> runNoLoggingT (a $ q u)
      where q u (NoLoggingT b) = NoLoggingT $ u b
#else
instance MonadThrow m => MonadThrow (NoLoggingT m) where
    monadThrow = Trans.lift . monadThrow
#endif

#if MIN_VERSION_conduit_extra(1,1,0)
instance MonadActive m => MonadActive (NoLoggingT m) where
    monadActive = Trans.lift monadActive
#endif

instance MonadResource m => MonadResource (NoLoggingT m) where
    liftResourceT = Trans.lift . liftResourceT

instance MonadBase b m => MonadBase b (NoLoggingT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans NoLoggingT where
    lift = NoLoggingT

instance MonadTransControl NoLoggingT where
    newtype StT NoLoggingT a = StIdent {unStIdent :: a}
    liftWith f = NoLoggingT $ f $ \(NoLoggingT t) -> liftM StIdent t
    restoreT = NoLoggingT . liftM unStIdent
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (NoLoggingT m) where
     newtype StM (NoLoggingT m) a = StMT' (StM m a)
     liftBaseWith f = NoLoggingT $
         liftBaseWith $ \runInBase ->
             f $ liftM StMT' . runInBase . (\(NoLoggingT r) -> r)
     restoreM (StMT' base) = NoLoggingT $ restoreM base

instance Monad m => MonadLogger (NoLoggingT m) where
    monadLoggerLog _ _ _ _ = return ()
#endif
