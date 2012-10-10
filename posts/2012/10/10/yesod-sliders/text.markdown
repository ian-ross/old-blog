---
author: Ian
tags: haskell,yesod
timestamp: 16:26:06
title: Sliders for Yesod
---
Michael Snoyman recently had a [blog post][snoyman] about
composability in the [Yesod][yesod] web framework for Haskell, where
he responded to comments about it being difficult to build reusable
components for Yesod by, well, building one.  I'm pretty much a newbie
with Yesod, but the composability aspect of things had never looked
too difficult to me, so I thought I'd also have a go at the same kind
of exercise.

I decided to implement a slider form field, using one of the
[nicest jQuery-based slider][slider] widgets I've found.  All the code
for this example is available as a [Gist][gist].  I'll follow the
usual convention here of putting more or less all of the code in the
blog post too.

<!--MORE-->

# Sliders

The idea here is to hide some of the details of embedding components
into web pages so that a user can write a few lines of Haskell instead
of a mess of HTML, CSS, JavaScript plus whatever server-side code is
needed to talk to all of that.

We're going to put everything we need into a single Haskell module
called `JqSlider` (file `JqSlider.hs`).

### Setup

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, 
             QuasiQuotes, FlexibleContexts #-}

module JqSlider
    ( YesodJqSlider (..)
    , YesodJquery (..)
    , jqSliderField
    , JqSliderSettings (..)
    , JqSliderFormat (..)
    , JqSliderVal
    , Default (..)
    ) where
~~~~

We re-export a couple of things (`YesodJquery` and `Default`) for
convenience.

We have the usual pile of imports:

~~~~ {.haskell}
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Default

import Yesod
import Yesod.Form.Jquery (YesodJquery(..))
~~~~

And now we get into things properly.  The sliders in the plugin we're
using support both single value sliders and range sliders, so we
define a type synonym for the result of a slider field:

~~~~ {.haskell}
type JqSliderVal = Either Double (Double,Double)
~~~~

This isn't the prettiest way to do things (we should probably have
separate field types for single value sliders and range sliders) but
it'll do for now.

We need to be able to access the CSS and JavaScript files that
implement the sliders so, as is done in `Yesod.Jquery`, we implement a
type class to record where these things are.  Any application wanting
to make use of this component should declare its foundation data type
to be an instance of `YesodJqSlider`:

~~~~ {.haskell}
class YesodJqSlider master where
  -- | Routes to the jQuery Slider CSS and JS files.
  jqSliderCss :: master -> Either (Route master) Text
  jqSliderScript :: master -> Either (Route master) Text
~~~~

### Settings

Much of the work we have to do turns out to be marshalling values
between Haskell and JavaScript for setting the many options of the
sliders.  A simple case is a way of defining the locale-dependent
formatting of numeric values:

~~~~ {.haskell}
-- | Slider value display formatting.
data JqSliderFormat = JqSliderFormat
  { sfFormat :: Text
  , sfLocale :: Text } deriving (Eq, Show)
~~~~

Here, the marshalling is done by Aeson's default derivation of a
`ToJSON` instance:

~~~~ {.haskell}
$(deriveToJSON (map toLower . drop 2) ''JqSliderFormat)
~~~~

The overall slider settings are a bit more complicated, and it's nice
to have a more "Haskelly" view of things, which means we need to write
a custom `ToJSON` instance:

~~~~ {.haskell}
-- | Slider settings.
data JqSliderSettings = JqSliderSettings
  { ssRange :: (Double, Double)
  , ssStep :: Maybe Double
  , ssRound :: Maybe Int
  , ssFormat :: Maybe JqSliderFormat
  , ssHeterogeneity :: [(Double, Double)]
  , ssDimension :: Text
  , ssLimits :: Bool
  , ssScale :: [Text]
  , ssSkin :: Maybe Text
  } deriving (Eq, Show)

instance Default JqSliderSettings where
  def = JqSliderSettings
    { ssRange = (1, 10)
    , ssStep = Nothing
    , ssRound = Nothing
    , ssFormat = Nothing
    , ssHeterogeneity = []
    , ssDimension = ""
    , ssLimits = True
    , ssScale = []
    , ssSkin = Nothing
    }

instance ToJSON JqSliderSettings where
  toJSON s = Object $ H.fromList $ concat 
             [fromto, step, round, format, heterogeneity, 
              dimension, limits, scale, skin]
    where fromto = ["from" .= fst (ssRange s), "to" .= snd (ssRange s)]
          step = msingle "step" ssStep id
          round = msingle "round" ssRound id
          format = msingle "format" ssFormat toJSON
          heterogeneity = lsingle "heterogeneity" ssHeterogeneity
                          (\(p,v) -> concat [show p, "/", show v])
          dimension = case ssDimension s of
            "" -> []
            d -> ["dimension" .= d]
          limits = ["limits" .= ssLimits s]
          scale = lsingle "scale" ssScale id
          skin = maybe [] (single "skin") (ssSkin s)
          single k v = [k .= v]
          msingle k fld f = maybe [] (single k . f) (fld s)
          lsingle k fld f = case fld s of
            [] -> []
            vs -> [k .= (Array $ V.fromList $ map (toJSON . f) vs)]
~~~~

This feels a little clunky, but it's easy to use and the
implementation details are hidden away.

### A slider field

Finally, we get to the definition of a slider field.  This is
essentially a double field with a prettier user interface (apart from
the possibility of having two return values, for a range slider).  We
parse the return value appropriately (it should either be a single
real number, or a pair of real numbers separated by a semicolon).

~~~~ {.haskell}
jqSliderField :: (RenderMessage master FormMessage, 
                  YesodJquery master, YesodJqSlider master) =>
                 JqSliderSettings -> Field sub master (Either Double (Double,Double))
jqSliderField s = Field 
  { fieldParse = parseHelper $ \s ->
     case TR.double s of
       Right (a, "") -> Right (Left a)
       Right (a, s') -> if (T.head s' == ';') then
                          case TR.double (T.tail s') of
                            Right (b, "") -> Right (Right (a,b))
                            _ -> Left $ MsgInvalidNumber s
                        else Left $ MsgInvalidNumber s
       _ -> Left $ MsgInvalidNumber s
            
  , fieldView = \i n as v req -> do
       master <- lift getYesod
       addStylesheetEither $ jqSliderCss master
       addScriptEither $ urlJqueryJs master
       addScriptEither $ jqSliderScript master
       toWidget [whamlet|
<input id="#{i}" name="#{n}" *{as} type="slider" :req:required="" value="#{showVal v}">
|]
       toWidget [julius| $(function() { $("##{i}").slider(#{toJSON s}); }); |]
  }
 where showVal :: Either Text (Either Double (Double,Double)) -> Text
       showVal = either id (either (T.pack . show) (T.pack . (\(l,h) -> show l ++ ";" ++ show h)))
~~~~

To render the field, we just produce a HTML `INPUT` element of the
appropriate type and add in a bit of JavaScript to transform the input
into a slider at document load time.

(The ugliness in `showVal` with these repeated calls to `either` is
another indication that we should really have separate `jqSliderField`
and `jqRangeSliderField` field types.)


# A usage example

So, how do we use it?

### File layout

One slight ugliness we need to deal with up front is file layout.  We
need access to the CSS and JavaScript files for the slider plugin
(which we can point to using the methods in our `YesodJqSlider`
instance), but we also need access to the images used for theming the
sliders.  Unfortunately, as is pretty common, the paths to the images
are hard-coded in the CSS files.  For the moment, that means that if
the CSS is in `.../css/jquery.slider.min.css`, then the image files must
be in `.../img`.  I can imagine some sorts of solution to this problem
using Yesod's templating system, but that would make it tricky to
integrate directly with the original plugin, since we would have to
transform the original CSS.

Anyway, modulo this minor problem, we can now use slider fields
directly from Haskell with little or no pain (file
`jqslider-example.hs`).

### Setup

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, TypeFamilies,
             TemplateHaskell, QuasiQuotes, TupleSections #-}

import Yesod
import Yesod.Form
import Yesod.Static
import Yesod.Default.Util
import JqSlider
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import Text.Hamlet
~~~~

The usual imports are followed by our foundation data type definition.
Here, we're going to need to serve up some static resources (CSS,
JavaScript, image files) as well as the pages defined by our handlers,
so we need to include a static subsite.

~~~~ {.haskell}
data App = App { getStatic :: Static }
~~~~

Routes are simple: just a home page with some sliders, plus the static
subsite.

~~~~ {.haskell}
mkYesod "App" [parseRoutes|
/ HomeR GET POST
/static StaticR Static getStatic
|]
~~~~

### Handlers

The handlers are also pretty simple: generate a form using the
`sliderForm` function, then either just lay it out or process the
results (we have three sliders in the form and just add a message with
their values).

~~~~ {.haskell}
-- Basic handler: display form.
getHomeR :: Handler RepHtml
getHomeR = do
  (formWidget, formEnctype) <- generateFormPost sliderForm
  defaultLayout $ do
    setTitle "Sliders"
    $(widgetFileReload def "jqslider-example")

-- Process form result: just display a message.
postHomeR :: Handler RepHtml
postHomeR = do
  ((result, formWidget), formEnctype) <- runFormPost sliderForm
  case result of
    FormSuccess (a, b, c) -> 
      setMessage [shamlet|<p>Values: s1=#{showVal a}  s2=#{showVal b}  s3=#{showVal c}|]
    _ -> setMessage "Bad form response"
  defaultLayout $ do
    setTitle "Sliders"
    $(widgetFileReload def "jqslider-example")
  where showVal :: JqSliderVal -> T.Text
        showVal = either (T.pack . show) (T.pack . show)
~~~~

The page layout (in `jqslider-example.hamlet`) is straightforward:

~~~~ {.html}
$newline never
<h1>Jquery Sliders

<form method=post action=@{HomeR} enctype=#{formEnctype}>
  ^{formWidget}
  <input type="submit" value="Submit">
~~~~

### Form definition

Constructing the sliders is basically a matter of choosing settings.
Note that the result value for a slider form field is a `JqSliderVal`,
since we need to represent both single values and ranges.

~~~~ {.haskell}
-- Build a form with three sliders, two with single values and one
-- range slider.
sliderForm :: Html -> MForm App App
              (FormResult (JqSliderVal, JqSliderVal, JqSliderVal), GWidget App App ())
sliderForm = renderDivs $ (,,) 
             <$> areq (jqSliderField ss1) "Slider 1" (Just $ Left 15)
             <*> areq (jqSliderField ss2) "Slider 2" (Just $ Right (25,75))
             <*> areq (jqSliderField ss3) "Slider 3" (Just $ Left 50)
  where ss1 = def { ssRange = (5, 50), ssStep = Just 2.5, ssRound = Just 1,
                    ssFormat = Just (JqSliderFormat "##.0" "de"), ssDimension = "&nbsp;$" }
        ss2 = def { ssRange = (0, 500), ssStep = Just 1, ssLimits = False,
                    ssHeterogeneity = [(50,100), (75,250)],
                    ssScale = ["0", "|", "50", "|" , "100", "|", "250", "|", "500"],
                    ssDimension = "&nbsp;m<small>2</small>" }
        ss3 = def { ssRange = (0, 100), ssSkin = Just "round" }
~~~~

### Type class instances

We need to set up our static site and implement some type class
instances, and then we're ready to go.

~~~~ {.haskell}
$(staticFiles "static")

-- Typeclass instances.
instance Yesod App
instance YesodJquery App
instance YesodJqSlider App where
  jqSliderCss _ = Left (StaticR bin_jquery_slider_min_css)
  jqSliderScript _ = Left (StaticR bin_jquery_slider_min_js)
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage
~~~~

### Off we go...

The main program only has one wrinkle, in that we need to set up the
static site in our foundation object.

~~~~ {.haskell}
-- Off we go...
main :: IO ()
main = do
  s <- static "static"
  warpDebug 3000 (App s)
~~~~

And here's what it looks like:

<div class="img-full">![Screenshot](screenshot.png)</div>

# Conclusions

I think that wasn't too bad.  I like the way that it's possible to
bundle up the different parts of a component like this, and then to
hide the complexity from the user -- producing a form with sliders
looks pretty much the same as the basic form examples in the Yesod
book.


[snoyman]: http://www.yesodweb.com/blog/2012/10/jqplot-widget
[yesod]: http://www.yesodweb.com
[slider]: http://egorkhmelev.github.com/jslider
[gist]: https://gist.github.com/3866692
