-- | Bonsai HTML Internal module.
-- |
-- | Implements the HTML DSL.
-- | Heavily inspired by Smolder.
module Bonsai.Html.Internal
  ( Style(..)
  , Content
  , ContentF
  , ContentT
  , (!)
  , (!?)
  , (#!)
  , (#!?)
  , attribute
  , stringProperty
  , booleanProperty
  , text
  , emptyElement
  , element
  , keyedElement
  , render
  , withAttribute
  , withStyle
  , withOptionalAttribute
  , withOptionalStyle
  )
where


import Prelude
import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, execState, state)
import Data.Array (snoc, null)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | A style that will be collected for VD.style
data Style =
  Style
    { name :: String
    , value :: String
    }

-- | Create a custom attribute
attribute :: forall msg. String -> String -> VD.Property msg
attribute =
  VD.attribute

-- | Create a string property.
stringProperty :: forall msg. String -> String -> VD.Property msg
stringProperty =
  VD.property

-- | Create a boolean property.
booleanProperty :: forall msg. String -> Boolean -> VD.Property msg
booleanProperty =
  VD.property


-- | Content record type alias
type ContentRec msg =
  { name :: String
  , attribs :: Array (VD.Property msg)
  , styles :: Array Style
  , content :: ContentT msg
  }

-- | Content Functor for the DSL
data ContentF msg a
  = ContentF (ContentRec msg) a
  | VNodeF (VD.VNode msg) a
  | EmptyF a

instance functorContentF :: Functor (ContentF msg) where
  map f (ContentF rec x) = ContentF rec (f x)
  map f (VNodeF v x) = VNodeF v (f x)
  map f (EmptyF x) = EmptyF (f x)

type Content msg = Free (ContentF msg)
type ContentT msg = Content msg Unit

-- | Attach an attribute to a content node. Also !
withAttribute
  :: forall msg
  .  (ContentT msg -> ContentT msg)
  -> VD.Property msg
  -> (ContentT msg -> ContentT msg)
withAttribute cfn p =
  \child -> hoistFree go (cfn child)
  where
    go :: ContentF msg ~> ContentF msg
    go (ContentF rec x) = ContentF (rec { attribs = snoc rec.attribs p }) x
    go x = x

-- ! Attach an optional attribute. Also !?
withOptionalAttribute
  :: forall msg
  .  (ContentT msg -> ContentT msg)
  -> Maybe (VD.Property msg)
  -> (ContentT msg -> ContentT msg)
withOptionalAttribute cfn Nothing =
  cfn
withOptionalAttribute cfn (Just p) =
  withAttribute cfn p

-- | Attach a style to a content node. Also #!
withStyle
  :: forall msg
  .  (ContentT msg -> ContentT msg)
  -> Style
  -> (ContentT msg -> ContentT msg)
withStyle cfn st =
  \child -> hoistFree go (cfn child)
  where
    go :: ContentF msg ~> ContentF msg
    go (ContentF rec x) = ContentF (rec { styles = snoc rec.styles st }) x
    go x = x

-- | Attach an optional style to a content node. Also #!?
withOptionalStyle
  :: forall msg
  .  (ContentT msg -> ContentT msg)
  -> Maybe Style
  -> (ContentT msg -> ContentT msg)
withOptionalStyle cfn Nothing =
  cfn
withOptionalStyle cfn (Just st) =
  withStyle cfn st

infixl 4 withAttribute as !
infixl 4 withOptionalAttribute as !?
infixl 4 withStyle as #!
infixl 4 withOptionalStyle as #!?

-- | Create a text node.
text :: forall msg. String -> ContentT msg
text s =
  liftF $ VNodeF (VD.text s) unit

emptyContent :: forall msg. ContentT msg
emptyContent = liftF $ EmptyF unit

-- | Create a empty element - will not have children.
emptyElement :: forall msg. String -> ContentT msg
emptyElement name =
  liftF $ ContentF { name, attribs: [], styles: [], content: emptyContent } unit

-- | keyedElement renders to a VirtualDom keyedNode.
-- | The DSL does not work here to give it attributes and/or styles
keyedElement
  :: forall msg
  .  String
  -> Array (VD.Property msg)
  -> Array (Tuple String (VD.VNode msg))
  -> ContentT msg
keyedElement name attrs children =
  liftF $ VNodeF (VD.keyedNode name attrs children) unit

-- | Create an element with children.
element
  :: forall msg
  .  String
  -> ContentT msg
  -> ContentT msg
element name content =
  liftF $ ContentF { name, attribs: [], styles: [], content: content } unit


-- | Render the content DSL to a VirtualDom node.
render :: forall msg. ContentT msg -> VD.VNode msg
render elem =

  singleNode $ renderNodes elem

  where

    singleNode ns =
      case ns of
        [] ->
          VD.node "div" [] []
        [ n ] ->
          n
        x ->
          VD.node "div" [] x

    styles2Tups s = map (\(Style st) -> Tuple st.name st.value) s

    renderNodes content =
      execState (foldFree foldContentF content) []

    foldContentF :: ContentF msg ~> State (Array (VD.VNode msg))
    foldContentF (EmptyF x) =
      pure x
    foldContentF (VNodeF vnode x) =
      state \acc -> Tuple x (snoc acc vnode)
    foldContentF (ContentF rec x) =
      let
        c = renderNodes rec.content
        a = if null rec.styles
              then rec.attribs
              else rec.attribs <> [ VD.style $ styles2Tups rec.styles ]
      in
        state \acc -> Tuple x (snoc acc (VD.node rec.name a c))
