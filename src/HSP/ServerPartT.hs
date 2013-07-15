-- |This module provides, @instance 'XMLGenerator' ('ServerPartT' m)@
{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSP.ServerPartT () where

import Control.Monad              (liftM)
import Data.Monoid                ((<>))
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as TL
import HSP.XML
import HSP.XMLGenerator
import Happstack.Server (ServerPartT)

instance (Monad m) => XMLGen (ServerPartT m) where
    type XMLType (ServerPartT m) = XML
    type StringType (ServerPartT m) = TL.Text
    newtype ChildType (ServerPartT m) = SChild { unSChild :: XML }
    newtype AttributeType (ServerPartT m) = SAttr { unSAttr :: Attribute }
    genElement n attrs children =
        do attribs <- map unSAttr `liftM` asAttr attrs
           childer <- (flattenCDATA . map unSChild) `liftM`asChild children
           return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = SChild
    pcdataToChild = xmlToChild . pcdata

flattenCDATA :: [XML] -> [XML]
flattenCDATA cxml =
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs
    where
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1<>s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)

{-
instance (Monad m) => IsAttrValue (ServerPartT m) T.Text where
    toAttrValue = toAttrValue . T.unpack

instance (Monad m) => IsAttrValue (ServerPartT m) TL.Text where
    toAttrValue = toAttrValue . TL.unpack
-}
instance (Functor m, Monad m) => EmbedAsAttr (ServerPartT m) Attribute where
    asAttr = return . (:[]) . SAttr

instance (Functor m, Monad m, IsName n TL.Text) => EmbedAsAttr (ServerPartT m) (Attr n Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Functor m, Monad m, IsName n TL.Text) => EmbedAsAttr (ServerPartT m) (Attr n String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal $ TL.pack str)

instance (Functor m, Monad m, IsName n TL.Text) => EmbedAsAttr (ServerPartT m) (Attr n Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Functor m, Monad m, IsName n TL.Text) => EmbedAsAttr (ServerPartT m) (Attr n Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (TL.pack $ show i))

instance (Functor m, Monad m, IsName n TL.Text) => (EmbedAsAttr (ServerPartT m) (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ a)

instance (Functor m, Monad m, IsName n TL.Text) => (EmbedAsAttr (ServerPartT m) (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.fromStrict a)

instance (Functor m, Monad m) => EmbedAsChild (ServerPartT m) Char where
    asChild = XMLGenT . return . (:[]) . SChild . pcdata . TL.singleton

instance (Functor m, Monad m) => EmbedAsChild (ServerPartT m) String where
    asChild = XMLGenT . return . (:[]) . SChild . pcdata . TL.pack

instance (Functor m, Monad m) => EmbedAsChild (ServerPartT m) Int where
    asChild = XMLGenT . return . (:[]) . SChild . pcdata . TL.pack . show

instance (Functor m, Monad m) => EmbedAsChild (ServerPartT m) Integer where
    asChild = XMLGenT . return . (:[]) . SChild . pcdata . TL.pack . show

instance (Functor m, Monad m) => EmbedAsChild (ServerPartT m) XML where
    asChild = XMLGenT . return . (:[]) . SChild

instance Monad m => EmbedAsChild (ServerPartT m) () where
  asChild () = return []

instance (Functor m, Monad m) => (EmbedAsChild (ServerPartT m) TL.Text) where
    asChild = asChild . TL.unpack

instance (Functor m, Monad m) => (EmbedAsChild (ServerPartT m) T.Text) where
    asChild = asChild . T.unpack

instance (Functor m, Monad m) => AppendChild (ServerPartT m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unSChild chs))

instance (Functor m, Monad m) => SetAttr (ServerPartT m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unSAttr attrs)) cs

instance (Functor m, Monad m) => XMLGenerator (ServerPartT m)
