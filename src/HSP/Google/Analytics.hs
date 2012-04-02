{-# LANGUAGE DeriveDataTypeable, PatternGuards, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module HSP.Google.Analytics
    ( UACCT(..)
    , analytics
    , addAnalytics
    , analyticsAsync
    ) where

import Data.Generics (Data, Typeable)
import HSP
import qualified HSX.XMLGenerator as HSX
import Prelude hiding (head)

newtype UACCT = UACCT String -- ^ The UACCT provided to you by Google (looks like: @UA-XXXXX-X@)
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- | create the google analytics asynchronous tracking script tag
--
-- NOTE: you must put this right before the \<\/head\> tag
analyticsAsync :: (XMLGenerator m) =>
                  UACCT     -- ^ web property ID (looks like: @UA-XXXXX-X@)
               -> GenXML m
analyticsAsync (UACCT uacct) =
    <script type="text/javascript">

      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', '<% uacct %>']);
      _gaq.push(['_trackPageview']);

      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();

    </script>

-- | create the (old) google analytics script tags
--
-- NOTE: you must put the <% analytics yourUACCT %> immediately before the </body> tag
--
-- You probably want to use 'analyticsAsync' instead.
--
-- See also: 'addAnalytics', 'analyticsAsync'
analytics :: (XMLGenerator m) => UACCT -> GenXMLList m
analytics (UACCT uacct) =
    do a <- <script type="text/javascript">
              var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script>
       b <- <script type="text/javascript">
              var pageTracker = _gat._getTracker("<% uacct %>");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
       return [a,b]

-- | automatically add the google analytics scipt tags immediately before the </body> element
-- NOTE: this function is not idepotent
addAnalytics :: ( AppendChild m XML
                , EmbedAsChild m XML
                , EmbedAsAttr m Attribute
                , XMLGenerator m
                , HSX.XML m ~ XML)
             => UACCT
             -> XMLGenT m XML
             -> GenXML m
addAnalytics uacct pg =
    do page <- pg
       a <- analytics uacct
       case page of
         <html hattrs><[ head, body ]></html> ->
             <html hattrs>
                <% head %>
                <% body <: (a :: [XML]) %>
             </html>
         o -> error ("Failed to add analytics." ++ show o)

{- Example Analytics Code from Google:

<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script type="text/javascript">
var pageTracker = _gat._getTracker("UA-4353757-1");
pageTracker._initData();
pageTracker._trackPageview();
</script>
-}
