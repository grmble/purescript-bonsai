module Native.VirtualDom
  ( Node
  , Property
  , Options
  , JsonDecoder
  , Patch
  , Cmd(..)
  , Sub(..)
  , Program
  , Never
  , node
  , text
  , map
  , property
  , attribute
  , attributeNS
  , mapProperty
  , style
  , on
  , onWithOptions
  , lazy
  , lazy2
  , lazy3
  , keyedNode
  -- XXX
  , render
  , diff
  , applyPatches
  -- , program
  )
where

import Prelude

import Control.Monad.Eff
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn3, runFn4)
import Data.Foreign (Foreign, toForeign)
import Data.Tuple (Tuple)
import DOM
import DOM.Node.Types (Element)


-- | An immutable chunk of data representing a DOM node. This can be HTML or SVG.
newtype Node msg =
  Node Foreign

-- | Create a DOM node with a tag name, a list of HTML properties that can
-- | include styles and event listeners, a list of CSS properties like `color`, and
-- | a list of child nodes.
-- |
-- |    import Json.Encode as Json
-- |
-- |    hello : Node msg
-- |    hello =
-- |      node "div" [] [ text "Hello!" ]
-- |
-- |    greeting : Node msg
-- |    greeting =
-- |      node "div"
-- |        [ property "id" (Json.string "greeting") ]
-- |        [ text "Hello!" ]
node :: forall msg. String -> Array (Property msg) -> Array (Node msg) -> Node msg
node =
  runFn3 nodeFn3

foreign import nodeFn3 :: forall msg. Fn3 String (Array (Property msg)) (Array (Node msg)) (Node msg)


-- | Just put plain text in the DOM. It will escape the string so that it appears
-- | exactly as you specify.
-- |
-- |    text "Hello World!"
foreign import text :: forall msg. String -> Node msg

-- | This function is useful when nesting components with [the Elm
-- | Architecture](https://github.com/evancz/elm-architecture-tutorial/). It lets
-- | you transform the messages produced by a subtree.
-- |
-- | Say you have a node named `button` that produces `()` values when it is
-- | clicked. To get your model updating properly, you will probably want to tag
-- | this `()` value like this:
-- |
-- |     type Msg = Click | ...
-- |
-- |     update msg model =
-- |       case msg of
-- |         Click ->
-- |           ...
-- |
-- |     view model =
-- |       map (\_ -> Click) button
-- |
-- | So now all the events produced by `button` will be transformed to be of type
-- | `Msg` so they can be handled by your update function!
-- |
-- | map : (a -> msg) -> Node a -> Node msg
-- | map =
-- |   Native.VirtualDom.map
map :: forall a msg. (a -> msg) -> Node a-> Node msg
map =
  runFn2 mapFn2

foreign import mapFn2 :: forall a msg. Fn2 (a -> msg) (Node a) (Node msg)

-- PROPERTIES


-- | When using HTML and JS, there are two ways to specify parts of a DOM node.
-- |
-- |  1. Attributes &mdash; You can set things in HTML itself. So the `class`
-- |     in `<div class="greeting"></div>` is called an *attribute*.
-- |
-- |  2. Properties &mdash; You can also set things in JS. So the `className`
-- |     in `div.className = 'greeting'` is called a *property*.
-- |
-- | So the `class` attribute corresponds to the `className` property. At first
-- | glance, perhaps this distinction is defensible, but it gets much crazier.
-- | *There is not always a one-to-one mapping between attributes and properties!*
-- | Yes, that is a true fact. Sometimes an attribute exists, but there is no
-- | corresponding property. Sometimes changing an attribute does not change the
-- | underlying property. For example, as of this writing, the `webkit-playsinline`
-- | attribute can be used in HTML, but there is no corresponding property!

newtype Property msg =
  Property Foreign


-- | Create arbitrary *properties*.
-- |
-- |    import JavaScript.Encode as Json
-- |
-- |    greeting : Html
-- |    greeting =
-- |        node "div" [ property "className" (Json.string "greeting") ] [
-- |          text "Hello!"
-- |        ]
-- |
-- | Notice that you must give the *property* name, so we use `className` as it
-- | would be in JavaScript, not `class` as it would appear in HTML.
property :: forall a msg. String -> a -> Property msg
property key value =
  runFn2 propertyFn2 key (toForeign value)

foreign import propertyFn2 :: forall a msg. Fn2 String Foreign (Property msg)



-- | Create arbitrary HTML *attributes*. Maps onto JavaScript’s `setAttribute`
-- | function under the hood.
-- |
-- |     greeting : Html
-- |     greeting =
-- |         node "div" [ attribute "class" "greeting" ] [
-- |           text "Hello!"
-- |         ]
-- |
-- | Notice that you must give the *attribute* name, so we use `class` as it would
-- | be in HTML, not `className` as it would appear in JS.
attribute :: forall msg. String -> String -> Property msg
attribute =
  runFn2 attributeFn2

foreign import attributeFn2 :: forall msg. Fn2 String String (Property msg)


-- | Would you believe that there is another way to do this?! This corresponds
-- | to JavaScript's `setAttributeNS` function under the hood. It is doing pretty
-- | much the same thing as `attribute` but you are able to have "namespaced"
-- | attributes. This is used in some SVG stuff at least.
attributeNS :: forall msg. String -> String -> String -> Property msg
attributeNS = runFn3 attributeFn3

foreign import attributeFn3 :: forall msg. Fn3 String String String (Property msg)


-- | Transform the messages produced by a `Property`.
mapProperty :: forall a msg. (a -> msg) -> Property a -> Property msg
mapProperty =
  runFn2 mapPropertyFn2

foreign import mapPropertyFn2 :: forall a msg. Fn2 (a -> msg) (Property a) (Property msg)

-- | Specify a list of styles.
-- |
-- |     myStyle : Property msg
-- |     myStyle =
-- |       style
-- |         [ ("backgroundColor", "red")
-- |         , ("height", "90px")
-- |         , ("width", "100%")
-- |         ]
-- |
-- |     greeting : Node msg
-- |     greeting =
-- |       node "div" [ myStyle ] [ text "Hello!" ]
foreign import style :: forall msg. Array (Tuple String String) -> Property msg



-- EVENTS


-- | Create a custom event listener.
-- |
-- |     import Json.Decode as Json
-- |
-- |     onClick : msg -> Property msg
-- |     onClick msg =
-- |       on "click" (Json.succeed msg)
-- |
-- | You first specify the name of the event in the same format as with JavaScript’s
-- | `addEventListener`. Next you give a JSON decoder, which lets you pull
-- | information out of the event object. If the decoder succeeds, it will produce
-- | a message and route it to your `update` function.
on :: forall msg. String -> (JsonDecoder msg) -> Property msg
on eventName decoder =
  runFn3 onFn3 eventName defaultOptions decoder

foreign import onFn3 :: forall msg. Fn3 String Options (JsonDecoder msg) (Property msg)


-- | Same as `on` but you can set a few options.
onWithOptions :: forall msg. String -> Options -> JsonDecoder msg -> Property msg
onWithOptions =
  runFn3 onFn3


-- | Options for an event listener. If `stopPropagation` is true, it means the
-- | event stops traveling through the DOM so it will not trigger any other event
-- | listeners. If `preventDefault` is true, any built-in browser behavior related
-- | to the event is prevented. For example, this is used with touch events when you
-- | want to treat them as gestures of your own, not as scrolls.
type Options =
  { stopPropagation :: Boolean
  , preventDefault :: Boolean
  }


-- | Everything is `False` by default.
-- |
-- |     defaultOptions =
-- |         { stopPropagation = False
-- |         , preventDefault = False
-- |         }
defaultOptions :: Options
defaultOptions =
  { stopPropagation: false
  , preventDefault: false
  }



-- XXX: Json Decoder ?
newtype JsonDecoder msg =
  JsonDecoder Foreign



-- OPTIMIZATION


-- | A performance optimization that delays the building of virtual DOM nodes.
-- |
-- | Calling `(view model)` will definitely build some virtual DOM, perhaps a lot of
-- | it. Calling `(lazy view model)` delays the call until later. During diffing, we
-- | can check to see if `model` is referentially equal to the previous value used,
-- | and if so, we just stop. No need to build up the tree structure and diff it,
-- | we know if the input to `view` is the same, the output must be the same!
lazy :: forall a msg. (a -> Node msg) -> a -> Node msg
lazy =
  runFn2 lazyFn2

foreign import lazyFn2
  :: forall a msg
  .  Fn2 (a -> Node msg) a (Node msg)

-- | Same as `lazy` but checks on two arguments.
lazy2 :: forall a b msg. (a -> b -> Node msg) -> a -> b -> Node msg
lazy2 =
  runFn3 lazy2Fn3

foreign import lazy2Fn3
  :: forall a b msg
  .  Fn3 (a -> b -> Node msg) a b (Node msg)

-- | Same as `lazy` but checks on three arguments.
lazy3 :: forall a b c msg. (a -> b -> c -> Node msg) -> a -> b -> c -> Node msg
lazy3 =
  runFn4 lazy3Fn4

foreign import lazy3Fn4
  :: forall a b c msg
  .  Fn4 (a -> b -> c -> Node msg) a b c (Node msg)


-- | Works just like `node`, but you add a unique identifier to each child
-- | node. You want this when you have a list of nodes that is changing: adding
-- | nodes, removing nodes, etc. In these cases, the unique identifiers help make
-- | the DOM modifications more efficient.
keyedNode
  :: forall msg
  .  String
  -> Array (Property msg)
  -> Array (Tuple String (Node msg))
  -> Node msg
keyedNode =
  runFn3 keyedNodeFn3

foreign import keyedNodeFn3 ::
  forall msg.
  Fn3 String (Array (Property msg)) (Array (Tuple String (Node msg))) (Node msg)


-- XXXXXXXX

foreign import render
  :: forall msg
  .  Node msg
  -> Element

newtype Patch msg =
  Patch Foreign

diff :: forall msg. Node msg -> Node msg -> Patch msg
diff = runFn2 diffFn2

foreign import diffFn2
  :: forall msg
  .  Fn2 (Node msg) (Node msg) (Patch msg)

applyPatches
  :: forall msg eff
  .  Element
  -> Node msg
  -> Patch msg
  -> Eff (dom::DOM | eff) Element
applyPatches oldDom oldNode patch =
  pure $ runFn3 applyPatchesFn3 oldDom oldNode patch

foreign import applyPatchesFn3
  :: forall msg
  .  Fn3 Element (Node msg) (Patch msg) Element


-- PROGRAMS


-- | Check out the docs for [`Html.App.program`][prog].
-- | It works exactly the same way.
-- |
-- | [prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#program
program
  :: forall model msg
  .  { init :: (Tuple model (Cmd msg))
     , update :: msg -> model -> (Tuple model (Cmd msg))
     , subscriptions :: model -> Sub msg
     , view :: model -> Node msg
     }
  -> Program Never model msg
program =
  programNoDebug <<< toForeign
  -- runFn2 programFn2 Debug.wrap impl

foreign import programNoDebug
  :: forall model msg
  .  Foreign -> (Program Never model msg)

-- XXX: stubs for elm builtins

newtype Cmd msg =
  Cmd (Array msg)

newtype Sub msg =
  Sub (Array msg)

data Never =
  Never

newtype Program flag model msg = Program { program :: String }


-- | Check out the docs for [`Html.App.programWithFlags`][prog].
-- | It works exactly the same way.
-- |
-- | [prog]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-App#programWithFlags
{--
programWithFlags
  :: forall flags model msg
  .  { init :: (Tuple model (Cmd msg))
     , update :: msg -> model -> (Tuple model (Cmd msg))
     , subscriptions :: model -> Sub msg
     , view :: model -> Node msg
     }
  -> Program flags model msg
programWithFlags impl =
  runFn3 programWithFlagsFn2 Debug.wrapWithFlags impl
--}
