{-# LANGUAGE CPP, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSP.Identity
    ( Ident
    , evalIdentity
    ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import HSP
import Control.Monad.Identity (Identity(Identity, runIdentity))
import HSX.XMLGenerator

instance XMLGenerator Identity

instance XMLGen Identity where
    type XMLType Identity = XML
    newtype ChildType Identity = IChild { unIChild :: XML }
    newtype AttributeType Identity = IAttr { unIAttr :: Attribute }
    genElement n attrs children = do
                           attrs'    <- asAttr attrs
                           children' <- asChild children
                           return $ Element (toName n) (map unIAttr attrs') (map unIChild children')
    xmlToChild = IChild
    pcdataToChild = xmlToChild . pcdata

instance IsAttrValue Identity T.Text where
    toAttrValue = toAttrValue . T.unpack

instance IsAttrValue Identity TL.Text where
    toAttrValue = toAttrValue . TL.unpack

instance EmbedAsAttr Identity Attribute where
    asAttr = return . (:[]) . IAttr

instance EmbedAsAttr Identity (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance EmbedAsAttr Identity (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance EmbedAsAttr Identity (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance EmbedAsAttr Identity (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (IsName n) => (EmbedAsAttr Identity (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.unpack a)

instance (IsName n) => (EmbedAsAttr Identity (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ T.unpack a)

instance EmbedAsChild Identity Char where
    asChild = XMLGenT . Identity . (:[]) . IChild . pcdata . (:[])

instance EmbedAsChild Identity String where
    asChild = XMLGenT . Identity . (:[]) . IChild . pcdata

instance (EmbedAsChild Identity TL.Text) where
    asChild = asChild . TL.unpack

instance (EmbedAsChild Identity T.Text) where
    asChild = asChild . T.unpack

instance EmbedAsChild Identity XML where
    asChild = XMLGenT . Identity . (:[]) . IChild

instance EmbedAsChild Identity () where
  asChild () = return []

instance AppendChild Identity XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map stripChild chs))

stripAttr :: AttributeType Identity -> Attribute
stripAttr  (IAttr a) = a

stripChild :: ChildType Identity -> XML
stripChild (IChild c) = c

instance SetAttr Identity XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr insert as (map stripAttr attrs)) cs

insert :: Attribute -> Attributes -> Attributes
insert = (:)

evalIdentity :: XMLGenT Identity XML -> XML
evalIdentity = runIdentity . unXMLGenT

type Ident = XMLGenT Identity
