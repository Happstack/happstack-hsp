{-# LANGUAGE DeriveDataTypeable, PatternGuards, FlexibleContexts, TypeFamilies, OverloadedStrings, QuasiQuotes #-}
module HSP.Google.Analytics
    ( UACCT(..)
    , analytics
    , analyticsAsync
    , universalAnalytics
    ) where

import Data.Generics (Data, Typeable)
import Data.Text.Lazy (Text,pack)
import HSP
import Prelude hiding (head)
import Language.Haskell.HSX.QQ (hsx)

newtype UACCT = UACCT String -- ^ The UACCT provided to you by Google (looks like: @UA-XXXXX-X@)
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- | create the google analytics asynchronous tracking script tag
--
-- This uses the now dubbed 'classic google analytics'
--
-- NOTE: you must put this right before the \<\/head\> tag
-- see also: universalAnalytics
analyticsAsync :: (XMLGenerator m, StringType m ~ Text) =>
                  UACCT     -- ^ web property ID (looks like: @UA-XXXXX-X@)
               -> GenXML m
analyticsAsync (UACCT uacct) = [hsx|
    <script type="text/javascript">

      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', '<% pack uacct %>']);
      _gaq.push(['_trackPageview']);

      (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
      })();

    </script> |]

universalAnalytics :: (XMLGenerator m, StringType m ~ Text) =>
                      UACCT     -- ^ web property ID (looks like: @UA-XXXXX-X@)
                   -> GenXML m
universalAnalytics (UACCT uacct) = [hsx|
    <script type="text/javascript">
     (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
     (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
     m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
     })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

     ga('create', '<% pack uacct %>', 'auto');
     ga('send', 'pageview');
    </script> |]


-- | create the (even older) google analytics script tags
--
-- NOTE: you must put the <% analytics yourUACCT %> immediately before the </body> tag
--
-- You probably want to use 'analyticsAsync' instead.
--
-- See also: 'addAnalytics', 'analyticsAsync', 'universalAnalytics'
analytics :: (XMLGenerator m, StringType m ~ Text) => UACCT -> GenXMLList m
analytics (UACCT uacct) =
    do a <- [hsx|
             <script type="text/javascript">
              var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
             </script> |]
       b <- [hsx|
             <script type="text/javascript">
               var pageTracker = _gat._getTracker("<% pack uacct %>");
               pageTracker._initData();
               pageTracker._trackPageview();
             </script> |]
       return [a,b]
{-
removed because the hsx qq does not currently support patterns.

-- | automatically add the google analytics scipt tags immediately before the </body> element
-- NOTE: this function is not idepotent
addAnalytics :: ( AppendChild m XML
                , EmbedAsChild m XML
                , EmbedAsAttr m Attribute
                , XMLGenerator m
                , XMLType m ~ XML
                , StringType m ~ Text
                )
             => UACCT
             -> XMLGenT m XML
             -> GenXML m
addAnalytics uacct pg =
    do page <- pg
       a <- analytics uacct
       case page of
         [hsx| <html hattrs><[ head, body ]></html> |] ->
                [hsx|
                 <html hattrs>
                  <% head %>
                  <% body <: (a :: [XML]) %>
                 </html>
                 |]
         o -> error ("Failed to add analytics." ++ show o)
-}
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
