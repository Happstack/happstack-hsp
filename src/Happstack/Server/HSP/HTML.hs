-- | support for using HSP+Happstack for rendering HTML
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -F -pgmFhsx2hs #-}
module Happstack.Server.HSP.HTML
  ( defaultTemplate
  ) where

import Control.Monad.Trans (MonadIO(), liftIO)
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy (Text)

import Control.Monad (liftM)
import Happstack.Server
  ( ToMessage(toMessage, toContentType, toResponse)
  , Response
  )

import HSP
import HSP.HTML4

instance ToMessage (Maybe XMLMetaData, XML) where
    toContentType (Just md,_) = T.encodeUtf8 $ TL.toStrict (contentType md)
    toContentType _ = "text/html;charset=utf-8"

    toMessage (Just (XMLMetaData (showDt, dt) _ pr), xml) =
         TL.encodeUtf8 $ TL.toLazyText ((if showDt then ((TL.fromLazyText dt) <>) else id) (pr xml))

    toMessage (Nothing, xml) =
        TL.encodeUtf8 (renderAsHTML xml)


instance ToMessage XML where
    toContentType _ = "text/html;charset=utf-8"
    toMessage xml   = toMessage (html4Strict, xml)


-- | A generic webpage template
defaultTemplate :: ( XMLGenerator m, EmbedAsChild m headers
                   , EmbedAsChild m body, StringType m ~ Text) =>
                   TL.Text  -- ^ text to use in \<title\> tag
                -> headers  -- ^ extra headers to insert in \<head\> tag. Use @()@ if none.
                -> body     -- ^ content to put between the \<body\> tags.
                -> m (XMLType m)
defaultTemplate title headers body =
    unXMLGenT $
    <html>
     <head>
      <title><% title %></title>
      <% headers %>
     </head>
     <body>
      <% body %>
     </body>
    </html>
