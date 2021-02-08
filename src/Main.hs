module Main where

import           Control.Monad.Fix       (MonadFix)
import qualified Data.Text               as T
import           Diagrams.Backend.Reflex
import           Diagrams.Prelude        hiding (Time, el, text)
import           Reflex
import           Reflex.Dom
import           ValueSVG

main :: IO ()
main =
  mainWidgetWithHead headWidget bodyElement

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text "reflex-stone"

bodyElement :: MonadWidget t m => m ()
bodyElement = tsvg

stoneButton :: DomBuilder t m => m (Event t ())
stoneButton = do
  let attr = ("style" =: "font-size: 200%;")
  clickEvent $ elAttr' "button" attr stone

stone :: DomBuilder t m => m ()
stone =
  text "🗿"

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent w =
  fmap (fmap (const ()) . domEvent Click . fst) w

tsvg :: MonadWidget t m => m ()
tsvg = do
  ev <- last gif
  ct <- count . ffilter getAny $ diaMousedownEv ev
  dynText $ fmap counter ct
  return ()

counter :: Int -> T.Text
counter i = T.concat ["The circle has been clicked ", T.pack (show i), " times" ]
