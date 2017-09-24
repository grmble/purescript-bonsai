-- | Bonsai HTML DSL
-- |
-- | These helper functions are borrowed from Elm,
-- | the actual DSL was "inspired" by Smolder.
module Bonsai.Html
where

import Prelude hiding (div)

import Bonsai.Html.Internal (MarkupT, leaf, parent)



-- BLOCK LEVEL ELEMENTS





body :: MarkupT ~> MarkupT
body =
  parent "body"

section :: MarkupT ~> MarkupT
section =
  parent "section"

nav :: MarkupT ~> MarkupT
nav =
  parent "nav"

article :: MarkupT ~> MarkupT
article =
  parent "article"

aside :: MarkupT ~> MarkupT
aside =
  parent "aside"

h1 :: MarkupT ~> MarkupT
h1 =
  parent "h1"

h2 :: MarkupT ~> MarkupT
h2 =
  parent "h2"

h3 :: MarkupT ~> MarkupT
h3 =
  parent "h3"

h4 :: MarkupT ~> MarkupT
h4 =
  parent "h4"

h5 :: MarkupT ~> MarkupT
h5 =
  parent "h5"

h6 :: MarkupT ~> MarkupT
h6 =
  parent "h6"

header :: MarkupT ~> MarkupT
header =
  parent "header"

footer :: MarkupT ~> MarkupT
footer =
  parent "footer"

address :: MarkupT ~> MarkupT
address =
  parent "address"

main_ :: MarkupT ~> MarkupT
main_ =
  parent "main_"

p :: MarkupT ~> MarkupT
p =
  parent "p"

hr :: forall msg. MarkupT msg
hr =
  leaf "hr"

pre_ :: MarkupT ~> MarkupT
pre_ =
  parent "pre"

blockquote :: MarkupT ~> MarkupT
blockquote =
  parent "blockquote"

ol :: MarkupT ~> MarkupT
ol =
  parent "ol"

ul :: MarkupT ~> MarkupT
ul =
  parent "ul"

li :: MarkupT ~> MarkupT
li =
  parent "li"

dl :: MarkupT ~> MarkupT
dl =
  parent "dl"

dt :: MarkupT ~> MarkupT
dt =
  parent "dt"

dd :: MarkupT ~> MarkupT
dd =
  parent "dd"

figure :: MarkupT ~> MarkupT
figure =
  parent "figure"

figcaption :: MarkupT ~> MarkupT
figcaption =
  parent "figcaption"

-- | Create a div element.
div_ :: MarkupT ~> MarkupT
div_ =
  parent "div"

-- | Create a div element.
-- | This clashes with a function from the prelude,
-- | so either use div_ or "import Prelude hiding (div)"
div :: MarkupT ~> MarkupT
div = div_



-- TEXT LEVEL ELEMENTS




a :: MarkupT ~> MarkupT
a =
  parent "a"

em :: MarkupT ~> MarkupT
em =
  parent "em"

strong :: MarkupT ~> MarkupT
strong =
  parent "strong"

small :: MarkupT ~> MarkupT
small =
  parent "small"

s :: MarkupT ~> MarkupT
s =
  parent "s"

cite :: MarkupT ~> MarkupT
cite =
  parent "cite"

q :: MarkupT ~> MarkupT
q =
  parent "q"

dfn :: MarkupT ~> MarkupT
dfn =
  parent "dfn"

abbr :: MarkupT ~> MarkupT
abbr =
  parent "abbr"

time :: MarkupT ~> MarkupT
time =
  parent "time"

code :: MarkupT ~> MarkupT
code =
  parent "code"

var :: MarkupT ~> MarkupT
var =
  parent "var"

samp :: MarkupT ~> MarkupT
samp =
  parent "samp"

kbd :: MarkupT ~> MarkupT
kbd =
  parent "kbd"

sub :: MarkupT ~> MarkupT
sub =
  parent "sub"

sup :: MarkupT ~> MarkupT
sup =
  parent "sup"

i :: MarkupT ~> MarkupT
i =
  parent "i"

b :: MarkupT ~> MarkupT
b =
  parent "b"

u :: MarkupT ~> MarkupT
u =
  parent "u"

mark :: MarkupT ~> MarkupT
mark =
  parent "mark"

ruby :: MarkupT ~> MarkupT
ruby =
  parent "ruby"

rt :: MarkupT ~> MarkupT
rt =
  parent "rt"

rp :: MarkupT ~> MarkupT
rp =
  parent "rp"

bdi :: MarkupT ~> MarkupT
bdi =
  parent "bdi"

bdo :: MarkupT ~> MarkupT
bdo =
  parent "bdo"

span :: MarkupT ~> MarkupT
span =
  parent "span"

br :: forall msg. MarkupT msg
br =
  leaf "br"

wbr :: MarkupT ~> MarkupT
wbr =
  parent "wbr"




-- EDITS






ins :: MarkupT ~> MarkupT
ins =
  parent "ins"

del :: MarkupT ~> MarkupT
del =
  parent "del"




-- EMBEDDED CONTENT



img :: MarkupT ~> MarkupT
img =
  parent "img"

iframe :: MarkupT ~> MarkupT
iframe =
  parent "iframe"

embed :: MarkupT ~> MarkupT
embed =
  parent "embed"

object :: MarkupT ~> MarkupT
object =
  parent "object"

param :: MarkupT ~> MarkupT
param =
  parent "param"

video :: MarkupT ~> MarkupT
video =
  parent "video"

audio :: MarkupT ~> MarkupT
audio =
  parent "audio"

source :: MarkupT ~> MarkupT
source =
  parent "source"


track :: MarkupT ~> MarkupT
track =
  parent "track"

canvas :: MarkupT ~> MarkupT
canvas =
  parent "canvas"

math :: MarkupT ~> MarkupT
math =
  parent "math"

table :: MarkupT ~> MarkupT
table =
  parent "table"

caption :: MarkupT ~> MarkupT
caption =
  parent "caption"

colgroup :: MarkupT ~> MarkupT
colgroup =
  parent "colgroup"

col :: MarkupT ~> MarkupT
col =
  parent "col"

tbody :: MarkupT ~> MarkupT
tbody =
  parent "tbody"

thead :: MarkupT ~> MarkupT
thead =
  parent "thead"

tfoot :: MarkupT ~> MarkupT
tfoot =
  parent "tfoot"

tr :: MarkupT ~> MarkupT
tr =
  parent "tr"

td :: MarkupT ~> MarkupT
td =
  parent "td"

th :: MarkupT ~> MarkupT
th =
  parent "th"

form :: MarkupT ~> MarkupT
form =
  parent "form"

fieldset :: MarkupT ~> MarkupT
fieldset =
  parent "fieldset"

legend :: MarkupT ~> MarkupT
legend =
  parent "legend"

label :: MarkupT ~> MarkupT
label =
  parent "label"

input :: forall msg. MarkupT msg
input =
  leaf "input"

button :: MarkupT ~> MarkupT
button =
  parent "button"

select :: MarkupT ~> MarkupT
select =
  parent "select"

datalist :: MarkupT ~> MarkupT
datalist =
  parent "datalist"

optgroup :: MarkupT ~> MarkupT
optgroup =
  parent "optgroup"

option :: MarkupT ~> MarkupT
option =
  parent "option"


textarea :: forall msg. MarkupT msg
textarea =
  leaf "textarea"

keygen :: MarkupT ~> MarkupT
keygen =
  parent "keygen"

output :: MarkupT ~> MarkupT
output =
  parent "output"

progress :: MarkupT ~> MarkupT
progress =
  parent "progress"

meter :: MarkupT ~> MarkupT
meter =
  parent "meter"

details :: MarkupT ~> MarkupT
details =
  parent "details"

summary :: MarkupT ~> MarkupT
summary =
  parent "summary"

menuitem :: MarkupT ~> MarkupT
menuitem =
  parent "menuitem"

menu :: MarkupT ~> MarkupT
menu =
  parent "menu"
