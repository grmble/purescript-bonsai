module Test.JSDOM
  ( elementById
  , makeDocument )
where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Document, Element)

foreign import makeDocument :: forall eff. String -> Eff (dom::DOM|eff) Document

foreign import elementById :: forall eff. Document -> String -> Eff (dom::DOM|eff) Element
