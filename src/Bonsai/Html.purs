-- | Bonsai HTML DSL
-- |
-- | These helper functions are borrowed from Elm,
-- | the actual DSL was "inspired" by Smolder.
module Bonsai.Html
where

import Prelude hiding (div)

import Bonsai.Html.Internal (ContentT, element, emptyElement)



-- BLOCK LEVEL ELEMENTS





body :: ContentT ~> ContentT
body =
  element "body"

section :: ContentT ~> ContentT
section =
  element "section"

nav :: ContentT ~> ContentT
nav =
  element "nav"

article :: ContentT ~> ContentT
article =
  element "article"

aside :: ContentT ~> ContentT
aside =
  element "aside"

h1 :: ContentT ~> ContentT
h1 =
  element "h1"

h2 :: ContentT ~> ContentT
h2 =
  element "h2"

h3 :: ContentT ~> ContentT
h3 =
  element "h3"

h4 :: ContentT ~> ContentT
h4 =
  element "h4"

h5 :: ContentT ~> ContentT
h5 =
  element "h5"

h6 :: ContentT ~> ContentT
h6 =
  element "h6"

header :: ContentT ~> ContentT
header =
  element "header"

footer :: ContentT ~> ContentT
footer =
  element "footer"

address :: ContentT ~> ContentT
address =
  element "address"

main_ :: ContentT ~> ContentT
main_ =
  element "main_"

p :: ContentT ~> ContentT
p =
  element "p"

hr :: forall msg. ContentT msg
hr =
  emptyElement "hr"

pre_ :: ContentT ~> ContentT
pre_ =
  element "pre"

blockquote :: ContentT ~> ContentT
blockquote =
  element "blockquote"

ol :: ContentT ~> ContentT
ol =
  element "ol"

ul :: ContentT ~> ContentT
ul =
  element "ul"

li :: ContentT ~> ContentT
li =
  element "li"

dl :: ContentT ~> ContentT
dl =
  element "dl"

dt :: ContentT ~> ContentT
dt =
  element "dt"

dd :: ContentT ~> ContentT
dd =
  element "dd"

figure :: ContentT ~> ContentT
figure =
  element "figure"

figcaption :: ContentT ~> ContentT
figcaption =
  element "figcaption"

-- | Create a div element.
div_ :: ContentT ~> ContentT
div_ =
  element "div"

-- | Create a div element.
-- | This clashes with a function from the prelude,
-- | so either use div_ or "import Prelude hiding (div)"
div :: ContentT ~> ContentT
div = div_



-- TEXT LEVEL ELEMENTS




a :: ContentT ~> ContentT
a =
  element "a"

em :: ContentT ~> ContentT
em =
  element "em"

strong :: ContentT ~> ContentT
strong =
  element "strong"

small :: ContentT ~> ContentT
small =
  element "small"

s :: ContentT ~> ContentT
s =
  element "s"

cite :: ContentT ~> ContentT
cite =
  element "cite"

q :: ContentT ~> ContentT
q =
  element "q"

dfn :: ContentT ~> ContentT
dfn =
  element "dfn"

abbr :: ContentT ~> ContentT
abbr =
  element "abbr"

time :: ContentT ~> ContentT
time =
  element "time"

code :: ContentT ~> ContentT
code =
  element "code"

var :: ContentT ~> ContentT
var =
  element "var"

samp :: ContentT ~> ContentT
samp =
  element "samp"

kbd :: ContentT ~> ContentT
kbd =
  element "kbd"

sub :: ContentT ~> ContentT
sub =
  element "sub"

sup :: ContentT ~> ContentT
sup =
  element "sup"

i :: ContentT ~> ContentT
i =
  element "i"

b :: ContentT ~> ContentT
b =
  element "b"

u :: ContentT ~> ContentT
u =
  element "u"

mark :: ContentT ~> ContentT
mark =
  element "mark"

ruby :: ContentT ~> ContentT
ruby =
  element "ruby"

rt :: ContentT ~> ContentT
rt =
  element "rt"

rp :: ContentT ~> ContentT
rp =
  element "rp"

bdi :: ContentT ~> ContentT
bdi =
  element "bdi"

bdo :: ContentT ~> ContentT
bdo =
  element "bdo"

span :: ContentT ~> ContentT
span =
  element "span"

br :: forall msg. ContentT msg
br =
  emptyElement "br"

wbr :: ContentT ~> ContentT
wbr =
  element "wbr"




-- EDITS






ins :: ContentT ~> ContentT
ins =
  element "ins"

del :: ContentT ~> ContentT
del =
  element "del"




-- EMBEDDED CONTENT



img :: ContentT ~> ContentT
img =
  element "img"

iframe :: ContentT ~> ContentT
iframe =
  element "iframe"

embed :: ContentT ~> ContentT
embed =
  element "embed"

object :: ContentT ~> ContentT
object =
  element "object"

param :: ContentT ~> ContentT
param =
  element "param"

video :: ContentT ~> ContentT
video =
  element "video"

audio :: ContentT ~> ContentT
audio =
  element "audio"

source :: ContentT ~> ContentT
source =
  element "source"


track :: ContentT ~> ContentT
track =
  element "track"

canvas :: ContentT ~> ContentT
canvas =
  element "canvas"

math :: ContentT ~> ContentT
math =
  element "math"

table :: ContentT ~> ContentT
table =
  element "table"

caption :: ContentT ~> ContentT
caption =
  element "caption"

colgroup :: ContentT ~> ContentT
colgroup =
  element "colgroup"

col :: ContentT ~> ContentT
col =
  element "col"

tbody :: ContentT ~> ContentT
tbody =
  element "tbody"

thead :: ContentT ~> ContentT
thead =
  element "thead"

tfoot :: ContentT ~> ContentT
tfoot =
  element "tfoot"

tr :: ContentT ~> ContentT
tr =
  element "tr"

td :: ContentT ~> ContentT
td =
  element "td"

th :: ContentT ~> ContentT
th =
  element "th"

form :: ContentT ~> ContentT
form =
  element "form"

fieldset :: ContentT ~> ContentT
fieldset =
  element "fieldset"

legend :: ContentT ~> ContentT
legend =
  element "legend"

label :: ContentT ~> ContentT
label =
  element "label"

input :: forall msg. ContentT msg
input =
  emptyElement "input"

button :: ContentT ~> ContentT
button =
  element "button"

select :: ContentT ~> ContentT
select =
  element "select"

datalist :: ContentT ~> ContentT
datalist =
  element "datalist"

optgroup :: ContentT ~> ContentT
optgroup =
  element "optgroup"

option :: ContentT ~> ContentT
option =
  element "option"


textarea :: ContentT ~> ContentT
textarea =
  element "textarea"

keygen :: ContentT ~> ContentT
keygen =
  element "keygen"

output :: ContentT ~> ContentT
output =
  element "output"

progress :: ContentT ~> ContentT
progress =
  element "progress"

meter :: ContentT ~> ContentT
meter =
  element "meter"

details :: ContentT ~> ContentT
details =
  element "details"

summary :: ContentT ~> ContentT
summary =
  element "summary"

menuitem :: ContentT ~> ContentT
menuitem =
  element "menuitem"

menu :: ContentT ~> ContentT
menu =
  element "menu"
