-- | Bonsai HTML Element Helpers
-- |
-- | These helper functions are borrowed from Elm,
-- | the actual DSL was "inspired" by Smolder.
module Bonsai.Html.Elements
where

import Prelude hiding (div)

import Bonsai.Html.Internal (MarkupT, leaf, parent)



-- BLOCK LEVEL ELEMENTS




-- | HTML body element
body :: MarkupT ~> MarkupT
body =
  parent "body"

-- | A (major) section in a document
section :: MarkupT ~> MarkupT
section =
  parent "section"

-- | A section that contains only navigation links
nav :: MarkupT ~> MarkupT
nav =
  parent "nav"

-- | Self-contained content that could exist independently.
article :: MarkupT ~> MarkupT
article =
  parent "article"

-- | Content that is loosely related to the page.
-- |
-- | If it is removed, the remaining content still makes sense.
aside :: MarkupT ~> MarkupT
aside =
  parent "aside"

-- | Html header
h1 :: MarkupT ~> MarkupT
h1 =
  parent "h1"

-- | Html header
h2 :: MarkupT ~> MarkupT
h2 =
  parent "h2"

-- | Html header
h3 :: MarkupT ~> MarkupT
h3 =
  parent "h3"

-- | Html header
h4 :: MarkupT ~> MarkupT
h4 =
  parent "h4"

-- | Html header
h5 :: MarkupT ~> MarkupT
h5 =
  parent "h5"

-- | Html header
h6 :: MarkupT ~> MarkupT
h6 =
  parent "h6"

-- | Header of a page or section.
header :: MarkupT ~> MarkupT
header =
  parent "header"

-- | Footer of a page or section.
footer :: MarkupT ~> MarkupT
footer =
  parent "footer"

-- | A section containing contact information.
address :: MarkupT ~> MarkupT
address =
  parent "address"

-- | Main or important content in the document.
-- |
-- | There is only one main element in the document.
main_ :: MarkupT ~> MarkupT
main_ =
  parent "main"

-- | Main or important content in the document.
-- |
-- | There is only one main element in the document.
main :: MarkupT ~> MarkupT
main =
  main_

-- | The content should be displayed as a paragraph.
p :: MarkupT ~> MarkupT
p =
  parent "p"

-- | A thematic break between paragraphs of a section or article or any longer content.
hr :: forall msg. MarkupT msg
hr =
  leaf "hr"

-- | Indictes that its content is preformattet and the format must be preserved.
pre_ :: MarkupT ~> MarkupT
pre_ =
  parent "pre"

-- | Represents content that is quoted from another source
blockquote :: MarkupT ~> MarkupT
blockquote =
  parent "blockquote"

-- | Ordered list of items (li)
ol :: MarkupT ~> MarkupT
ol =
  parent "ol"

-- | Unordered list of items (li)
ul :: MarkupT ~> MarkupT
ul =
  parent "ul"

-- | An item of an enumeration list.
li :: MarkupT ~> MarkupT
li =
  parent "li"

-- | Definition List, a list of terms and their definitions
dl :: MarkupT ~> MarkupT
dl =
  parent "dl"

-- | A term in a definitions list
dt :: MarkupT ~> MarkupT
dt =
  parent "dt"

-- | A definition in a definition list
dd :: MarkupT ~> MarkupT
dd =
  parent "dd"

-- | A figure illustrated as part of the document
figure :: MarkupT ~> MarkupT
figure =
  parent "figure"

-- | Legend for a figure.
figcaption :: MarkupT ~> MarkupT
figcaption =
  parent "figcaption"

-- | Generic container with no special meaning.  Also div.
div_ :: MarkupT ~> MarkupT
div_ =
  parent "div"

-- | Generic container with no special meaning.
-- |
-- | This clashes with a function from the prelude,
-- | so either use div_ or "import Prelude hiding (div)"
div :: MarkupT ~> MarkupT
div = div_



-- TEXT LEVEL ELEMENTS



-- | Represents a link to another resource.
a :: MarkupT ~> MarkupT
a =
  parent "a"

-- | Represents emphasized text, like a stress accent.
em :: MarkupT ~> MarkupT
em =
  parent "em"

-- | Represents especially important text.
strong :: MarkupT ~> MarkupT
strong =
  parent "strong"

-- | Represents a side comment e.g. text like a disclaimer
-- | or copyright.
-- | Not essential to comprehension of the document.
small :: MarkupT ~> MarkupT
small =
  parent "small"

-- | Represents content that is no longer accurate or relevant.
s :: MarkupT ~> MarkupT
s =
  parent "s"

-- | Represents the title of a work.
cite :: MarkupT ~> MarkupT
cite =
  parent "cite"

-- | An inline quotation.
q :: MarkupT ~> MarkupT
q =
  parent "q"

-- | A term whose definition is contained in its nearest ancestore.
dfn :: MarkupT ~> MarkupT
dfn =
  parent "dfn"
-- | An abbreviation or acronym, the expansion can be given in the title attribute.

abbr :: MarkupT ~> MarkupT
abbr =
  parent "abbr"

-- | A data and time value, the machine-readable representation can be give in the datetime attribute.
time :: MarkupT ~> MarkupT
time =
  parent "time"

-- | Represents computer code.
code :: MarkupT ~> MarkupT
code =
  parent "code"

-- | Represents a variable
var :: MarkupT ~> MarkupT
var =
  parent "var"

-- | Represents output of a program or computer.
samp :: MarkupT ~> MarkupT
samp =
  parent "samp"

-- | Represents user input, often from the keyboard.
kbd :: MarkupT ~> MarkupT
kbd =
  parent "kbd"

-- | Subscript
sub :: MarkupT ~> MarkupT
sub =
  parent "sub"

-- | Superscript
sup :: MarkupT ~> MarkupT
sup =
  parent "sup"

-- | Represents text in some alternate voice or mood.
-- |
-- | Used with bootstrap et. al. for icons
i :: MarkupT ~> MarkupT
i =
  parent "i"

-- | Text to which attention should be drawn for utilarian purposes.
-- |
-- | It's bold, the non-semantic strong
b :: MarkupT ~> MarkupT
b =
  parent "b"

-- | Underline with no predefined semantics
u :: MarkupT ~> MarkupT
u =
  parent "u"

-- | Text highlighted for reference purposes, i.e. for relevance in another context.
mark :: MarkupT ~> MarkupT
mark =
  parent "mark"

-- | Ruby annotations, ???
ruby :: MarkupT ~> MarkupT
ruby =
  parent "ruby"

-- | Ruby annotations, ???
rt :: MarkupT ~> MarkupT
rt =
  parent "rt"

-- | Ruby annotations, ???
rp :: MarkupT ~> MarkupT
rp =
  parent "rp"

-- | Content with bidirectional text formatting.
bdi :: MarkupT ~> MarkupT
bdi =
  parent "bdi"

-- | Represents bidirectionality of its children in order to override the unicode bidirectional algorithm.
bdo :: MarkupT ~> MarkupT
bdo =
  parent "bdo"

-- | Text with no specific meaning.
span :: MarkupT ~> MarkupT
span =
  parent "span"

-- | Line break.
br :: forall msg. MarkupT msg
br =
  leaf "br"

-- | Line break opportunity
wbr :: MarkupT ~> MarkupT
wbr =
  parent "wbr"




-- EDITS





-- | Defines an addition to the document.
ins :: MarkupT ~> MarkupT
ins =
  parent "ins"

-- | Defines a deletion from the document.
del :: MarkupT ~> MarkupT
del =
  parent "del"




-- EMBEDDED CONTENT



-- | An image (URL given by src attribute)
img :: MarkupT ~> MarkupT
img =
  parent "img"

-- | Embedded HTML document
iframe :: MarkupT ~> MarkupT
iframe =
  parent "iframe"

-- | Integration point for external application or content.
embed :: MarkupT ~> MarkupT
embed =
  parent "embed"

-- | Represents external resource that may be processed by a plugin
object :: MarkupT ~> MarkupT
object =
  parent "object"

-- | Parameters for plugins invoked by object.
param :: MarkupT ~> MarkupT
param =
  parent "param"

-- | Represents a video
video :: MarkupT ~> MarkupT
video =
  parent "video"

-- | Represents a sound or audio stream.
audio :: MarkupT ~> MarkupT
audio =
  parent "audio"

-- | Allows specification of alternative media resources for video or audio.
source :: MarkupT ~> MarkupT
source =
  parent "source"


-- | Timed text tracks for audio or video content
track :: MarkupT ~> MarkupT
track =
  parent "track"

-- | Bitmap area for graphics rendering.
canvas :: MarkupT ~> MarkupT
canvas =
  parent "canvas"

-- | Mathematical formule
math :: MarkupT ~> MarkupT
math =
  parent "math"

-- | HTML Table
table :: MarkupT ~> MarkupT
table =
  parent "table"

-- | Caption for a table
caption :: MarkupT ~> MarkupT
caption =
  parent "caption"

-- | Colgroup for a table
colgroup :: MarkupT ~> MarkupT
colgroup =
  parent "colgroup"

-- | Represents a column of a table.
col :: MarkupT ~> MarkupT
col =
  parent "col"

-- | Table body - contains the data rows
tbody :: MarkupT ~> MarkupT
tbody =
  parent "tbody"


-- | Table header - contains headers rows explaining the data
thead :: MarkupT ~> MarkupT
thead =
  parent "thead"

-- | Table footer
tfoot :: MarkupT ~> MarkupT
tfoot =
  parent "tfoot"

-- | A table row
tr :: MarkupT ~> MarkupT
tr =
  parent "tr"

-- | A cell in a a table row.
td :: MarkupT ~> MarkupT
td =
  parent "td"

-- | A cell in a table header.
th :: MarkupT ~> MarkupT
th =
  parent "th"

--  | A submittable form
form :: MarkupT ~> MarkupT
form =
  parent "form"

-- | Represents a set of controls in (usually) a form
fieldset :: MarkupT ~> MarkupT
fieldset =
  parent "fieldset"

-- | The caption for a fieldset.
legend :: MarkupT ~> MarkupT
legend =
  parent "legend"

-- | The caption for a form control
label :: MarkupT ~> MarkupT
label =
  parent "label"

-- | Typed data field allowing the user to edit the data.
input :: forall msg. MarkupT msg
input =
  leaf "input"

-- | A button that can be clicked.
button :: MarkupT ~> MarkupT
button =
  parent "button"

-- | Control that allows section among a set of options
select :: MarkupT ~> MarkupT
select =
  parent "select"

-- | A predefined set of options for other controls.
datalist :: MarkupT ~> MarkupT
datalist =
  parent "datalist"

-- | A set of options, logically grouped.
optgroup :: MarkupT ~> MarkupT
optgroup =
  parent "optgroup"

-- | A option in a select tag, or a suggestion in a datalist
option :: MarkupT ~> MarkupT
option =
  parent "option"

-- | A multiline text edit control.
textarea :: forall msg. MarkupT msg
textarea =
  leaf "textarea"

-- | A key-pair generator control.
keygen :: MarkupT ~> MarkupT
keygen =
  parent "keygen"

-- | Represents the result of a computation.
output :: MarkupT ~> MarkupT
output =
  parent "output"

-- | Represents the completion progress of a task.
progress :: MarkupT ~> MarkupT
progress =
  parent "progress"

-- | Represents a scalar measurement with a known range.
meter :: MarkupT ~> MarkupT
meter =
  parent "meter"

-- | Represents a widget from which the user can obtaion additional input or controls.
details :: MarkupT ~> MarkupT
details =
  parent "details"

-- | A summary for a given details.
summary :: MarkupT ~> MarkupT
summary =
  parent "summary"

-- | A command the user can invoke.
menuitem :: MarkupT ~> MarkupT
menuitem =
  parent "menuitem"

-- | A list of invokable commands
menu :: MarkupT ~> MarkupT
menu =
  parent "menu"
