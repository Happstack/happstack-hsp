-- | This module contains orphan 'XMLGenT' instances for 'ServerMonad', 'FilterMonad', 'WebMonad', 'HasRqData', and 'Happstack'. It does not export any functions.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Happstack.Server.XMLGenT where

import Control.Applicative         (Alternative(..))
import Control.Monad               (MonadPlus(..))
import Control.Monad.Trans         (MonadIO(..))
import Happstack.Server.SimpleHTTP (ServerMonad(..), FilterMonad(..), WebMonad(..), HasRqData(..), Happstack(..), Response)
import HSP.XMLGenerator            (XMLGenT(..))
import HSP.Monad                   (HSPT(..))

instance (ServerMonad m) => ServerMonad (XMLGenT m) where
    askRq = XMLGenT askRq
    localRq f (XMLGenT m) = XMLGenT (localRq f m)

instance (FilterMonad a m) => FilterMonad a (XMLGenT m) where
    setFilter = XMLGenT . setFilter
    composeFilter f = XMLGenT (composeFilter f)
    getFilter (XMLGenT m) = XMLGenT (getFilter m)

instance (WebMonad a m) => WebMonad a (XMLGenT m) where
    finishWith r = XMLGenT $ finishWith r

instance (HasRqData m) => (HasRqData (XMLGenT m)) where
    askRqEnv = XMLGenT askRqEnv
    localRqEnv f (XMLGenT m) = XMLGenT (localRqEnv f m)
    rqDataError = XMLGenT . rqDataError

instance (Alternative m, MonadPlus m, Functor m, MonadIO m, ServerMonad m, FilterMonad a m, WebMonad a m, HasRqData m, a ~ Response) => Happstack (XMLGenT m)

instance (ServerMonad m) => ServerMonad (HSPT xml m) where
    askRq              = HSPT askRq
    localRq f (HSPT m) = HSPT (localRq f m)

instance (FilterMonad a m) => FilterMonad a (HSPT xml m) where
    setFilter          = HSPT . setFilter
    composeFilter f    = HSPT (composeFilter f)
    getFilter (HSPT m) = HSPT (getFilter m)

instance (WebMonad a m) => WebMonad a (HSPT xml m) where
    finishWith r       = HSPT $ finishWith r

instance (HasRqData m) => (HasRqData (HSPT xml m)) where
    askRqEnv              = HSPT askRqEnv
    localRqEnv f (HSPT m) = HSPT (localRqEnv f m)
    rqDataError           = HSPT . rqDataError

instance (Alternative m, MonadPlus m, Functor m, MonadIO m, ServerMonad m, FilterMonad a m, WebMonad a m, HasRqData m, a ~ Response) => Happstack (HSPT xml m)
