-- | Bonsai HTML Attribute Helpers
-- |
-- | Mostly transcribed from Elm with omissions - only the most frequently used.
module Bonsai.Html.Attributes
where

import Prelude
import Bonsai.VirtualDom as VD
import Bonsai.Html.Internal (Style, attribute, booleanProperty, stringProperty)
import Data.Array (intercalate, filter)
import Data.Tuple (Tuple, snd, fst)

-- | Create a style fragment for the Element DSL
style :: String -> String -> Style
style n v =
  { name: n, value: v }

-- | Sets space-separated class attributes for CSS
classList :: forall msg. Array (Tuple String Boolean) -> VD.Property msg
classList classes =
  let
    go = filter snd >>>
      map fst >>>
      intercalate ", " >>>
      cls
  in
    go classes

-- | Set the elements class for CSS.  Also class_
-- |
-- | The VirtualDom supports multiple cls/class_/className
-- | properties and builds a correct attribute by
-- | separating them with a space.
-- |
-- | This is special cased - normarlly, the last value
-- | of a property wins.
cls :: forall msg. String -> VD.Property msg
cls =
  stringProperty "className"

-- | Set the elements class for CSS.  Also class_
class_ :: forall msg. String -> VD.Property msg
class_ =
  cls

-- | Set the element's ID. Also id
id_ :: forall msg. String -> VD.Property msg
id_ =
  stringProperty "id"

-- | Set the element's ID. Also id_
id :: forall msg. String -> VD.Property msg
id =
  id_

-- | Hide or show the element
hidden :: forall msg. Boolean -> VD.Property msg
hidden =
  booleanProperty "hidden"

-- | Text that will displayed in a tooltip when hovering over the element
title :: forall msg. String -> VD.Property msg
title =
  stringProperty "title"


-- INPUT


-- | Defines the type of a button, input, or ...
typ :: forall msg. String -> VD.Property msg
typ =
  -- setting this as property causes ie errors
  -- https://github.com/grmble/purescript-bonsai/issues/24
  attribute "type"

-- | Defines the type of a button, input, or ...
type_ :: forall msg. String -> VD.Property msg
type_ =
  typ

-- | Defines a (default) value which will be displayed in a button, option, input, ...
value :: forall msg. String -> VD.Property msg
value =
  stringProperty "value"

-- | Defines an initial value which will be displayed in an input when
-- | it is added to the DOM. Unlike value, changing defaultValue after
-- | the input has been added to the DOM has no effect.
defaultValue :: forall msg. String -> VD.Property msg
defaultValue =
  stringProperty "defaultValue"

-- | Indicates if a checkbox is checked.
checked :: forall msg. Boolean -> VD.Property msg
checked =
  booleanProperty "checked"

-- | Provide a hint to the user what can be entered in an input or textarea
placeholder :: forall msg. String -> VD.Property msg
placeholder =
  stringProperty "placeholder"

-- | defines which option will be selected on page load.
selected :: forall msg. Boolean -> VD.Property msg
selected =
  booleanProperty "selected"

-- | Indicates if a form or input can have their values automatically
-- | completed by the browser.
autocomplete :: forall msg. Boolean -> VD.Property msg
autocomplete b =
  stringProperty "autocomplete" (if b then "on" else "off")

-- | The element should be automaticall focused after the page loads.
-- | For button, inpupt, keygen, select, and textarea.
autofocus :: forall msg. Boolean -> VD.Property msg
autofocus =
  booleanProperty "autofocus"

-- | Indicates whether the user can interact with a button, fieldset, input,
-- | keygen, optgroup, option select or textarea.
disabled :: forall msg. Boolean -> VD.Property msg
disabled =
  booleanProperty "disabled"

-- | Defines the minimum number of characters allowed in an input or textarea
minlength :: forall msg. Int -> VD.Property msg
minlength =
  attribute "minlength" <<< show

-- | Defines the minimum number of characters allowed in an input or textarea
maxlength :: forall msg. Int -> VD.Property msg
maxlength =
  attribute "maxlength" <<< show

-- | Indicates whether multiple values can be entered in an input of
-- | type email or file.  Can also indicate that you can select many options.
multiple :: forall msg. Boolean -> VD.Property msg
multiple =
  booleanProperty "multiple"

-- | Name of the element.
-- | Used by a server to identify the fields in the form submit.
name :: forall msg. String -> VD.Property msg
name =
  stringProperty "name"

-- | This indicates that a form should not be validated when submitted.
novalidate :: forall msg. Boolean -> VD.Property msg
novalidate =
  booleanProperty "noValidate"

-- | Defines a regular expression which an input will be validated against.
pattern :: forall msg.  String -> VD.Property msg
pattern =
  -- phantom patterns on reused input fiels
  -- https://github.com/grmble/purescript-bonsai/issues/24
  attribute "pattern"

-- | Indicates whether an input or textarea can be edited.
readonly :: forall msg. Boolean -> VD.Property msg
readonly =
  booleanProperty "readonly"

-- | Indicates wheter this element is required to be filled out.
-- | For inpupt, select and textarea.
required :: forall msg. Boolean -> VD.Property msg
required =
  booleanProperty "required"

-- | For input specifies the width of an input in characters.
-- | For select the number of visible options in dropdown list
size :: forall msg. Int -> VD.Property msg
size =
  attribute "size" <<< show

-- | The element ID described by this label.
for :: forall msg. String -> VD.Property msg
for =
  -- setting this as property can cause empty for attributes to show up
  -- this will prevent associated checkboxes from working
  -- stringProperty "htmlFor"
  attribute "size"

-- | The element ID of the form for this button, fieldset, inpupt, keygen, label, meter,
-- | object, output, progress, select or textarea
form :: forall msg. String -> VD.Property msg
form =
  attribute "form"

-- | Maximum value for value allowed. For input of type number or date,
-- | the max value must be a number or date. For input, meter and progress
max :: forall msg. String -> VD.Property msg
max =
  stringProperty "max"


-- | Minimum value for value allowed. For input of type number or date,
-- | the min value must be a number or date. For input, meter and progress
min :: forall msg. String -> VD.Property msg
min =
  stringProperty "min"

-- | Step size for an input.  Use step "any" to allow any floating point number.
step :: forall msg. String -> VD.Property msg
step =
  stringProperty "step"


-- | Number of columns in a textarea
cols :: forall msg. Int -> VD.Property msg
cols =
  attribute "cols" <<< show


-- | Number of rows in a textarea
rows :: forall msg. Int -> VD.Property msg
rows =
  attribute "rows" <<< show


-- | Indicates whether the text should be wrapped in a textarea.
-- | Possible values are 'soft' and 'hard'
wrap :: forall msg. String -> VD.Property msg
wrap =
  stringProperty "wrap"


-- | The URL of a linked resource, such as a, area, base, or link
href :: forall msg. String -> VD.Property msg
href =
  stringProperty "href"

-- | Specify where the result of clicking an a, area, base or form
-- | would appear.  _blank (new window or tab), _self (same frame, default),
-- | _partent (parent frame), _top (full body of the window).
-- | Also takes the name of any frame you created.  As if.
target :: forall msg. String -> VD.Property msg
target =
  stringProperty "target"

-- | Indicates that clicking an a and area will download the resource,
-- | and that the downloaded resource will have the given filename.
downloadAs :: forall msg. String -> VD.Property msg
downloadAs =
  stringProperty "download"

-- | Indicates that clicking an a or area will download the resource
download :: forall msg. Boolean -> VD.Property msg
download =
  booleanProperty "download"


-- TABLES


-- | Defines the number of columns a cell should span.
colspan :: forall msg. Int -> VD.Property msg
colspan =
  attribute "colspan" <<< show

-- | Defines the number of rows a cell should span.
rowspan :: forall msg. Int -> VD.Property msg
rowspan =
  attribute "rowspan" <<< show
