{-| Shortcuts for common HTML5 attributes and properties
-}
module HtmlT.Wasm.Property where

import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Html
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Types

dynStyles :: Dynamic Utf8 -> Html ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic Utf8 -> Html ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic Utf8 -> Html ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> Html ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> Html ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: Utf8 -> Html ()
title_ = prop "title"
{-# INLINE title_ #-}

selected_ :: Bool -> Html ()
selected_ = prop "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> Html ()
hidden_ = prop "hidden"
{-# INLINE hidden_ #-}

value_ :: Utf8 -> Html ()
value_ = prop "value"
{-# INLINE value_ #-}

defaultValue_ :: Utf8 -> Html ()
defaultValue_ = prop "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: Utf8 -> Html ()
accept_ = prop "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: Utf8 -> Html ()
acceptCharset_ = prop "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: Utf8 -> Html ()
action_ = prop "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> Html ()
autocomplete_ b = prop @Utf8 "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: Utf8 -> Html ()
autosave_ = prop "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> Html ()
disabled_ = prop "disabled"
{-# INLINE disabled_ #-}

enctype_ :: Utf8 -> Html ()
enctype_ = prop "enctype"
{-# INLINE enctype_ #-}

formation_ :: Utf8 -> Html ()
formation_ = prop "formation"
{-# INLINE formation_ #-}

list_ :: Utf8 -> Html ()
list_ = prop "list"
{-# INLINE list_ #-}

maxlength_ :: Utf8 -> Html ()
maxlength_ = prop "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: Utf8 -> Html ()
minlength_ = prop "minlength"
{-# INLINE minlength_ #-}

method_ :: Utf8 -> Html ()
method_ = prop "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> Html ()
multiple_ = prop "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> Html ()
novalidate_ = prop "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: Utf8 -> Html ()
pattern_ = prop "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> Html ()
readonly_ = prop "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> Html ()
required_ = prop "required"
{-# INLINE required_ #-}

size_ :: Utf8 -> Html ()
size_ = prop "size"
{-# INLINE size_ #-}

forProp_ :: Utf8 -> Html ()
forProp_ = prop "for"
{-# INLINE forProp_ #-}

ref_ :: Utf8 -> Html ()
ref_ = prop "ref"
{-# INLINE ref_ #-}

formProp_ :: Utf8 -> Html ()
formProp_ = prop "form"
{-# INLINE formProp_ #-}

max_ :: Utf8 -> Html ()
max_ = prop "max"
{-# INLINE max_ #-}

min_ :: Utf8 -> Html ()
min_ = prop "min"
{-# INLINE min_ #-}

step_ :: Utf8 -> Html ()
step_ = prop "step"
{-# INLINE step_ #-}

cols_ :: Utf8 -> Html ()
cols_ = prop "cols"
{-# INLINE cols_ #-}

rows_ :: Utf8 -> Html ()
rows_ = prop "rows"
{-# INLINE rows_ #-}

wrap_ :: Utf8 -> Html ()
wrap_ = prop "wrap"
{-# INLINE wrap_ #-}

target_ :: Utf8 -> Html ()
target_ = prop "target"
{-# INLINE target_ #-}

download_ :: Utf8 -> Html ()
download_ = prop "download"
{-# INLINE download_ #-}

downloadAs_ :: Utf8 -> Html ()
downloadAs_ = prop "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: Utf8 -> Html ()
hreflang_ = prop "hreflang"
{-# INLINE hreflang_ #-}

media_ :: Utf8 -> Html ()
media_ = prop "media"
{-# INLINE media_ #-}

ping_ :: Utf8 -> Html ()
ping_ = prop "ping"
{-# INLINE ping_ #-}

rel_ :: Utf8 -> Html ()
rel_ = prop "rel"
{-# INLINE rel_ #-}

ismap_ :: Utf8 -> Html ()
ismap_ = prop "ismap"
{-# INLINE ismap_ #-}

usemap_ :: Utf8 -> Html ()
usemap_ = prop "usemap"
{-# INLINE usemap_ #-}

shape_ :: Utf8 -> Html ()
shape_ = prop "shape"
{-# INLINE shape_ #-}

coords_ :: Utf8 -> Html ()
coords_ = prop "coords"
{-# INLINE coords_ #-}

src_ :: Utf8 -> Html ()
src_ = prop "src"
{-# INLINE src_ #-}

height_ :: Utf8 -> Html ()
height_ = prop "height"
{-# INLINE height_ #-}

width_ :: Utf8 -> Html ()
width_ = prop "width"
{-# INLINE width_ #-}

alt_ :: Utf8 -> Html ()
alt_ = prop "alt"
{-# INLINE alt_ #-}

autoplay_ :: Bool -> Html ()
autoplay_ = prop "autoplay"
{-# INLINE autoplay_ #-}

controls_ :: Bool -> Html ()
controls_ = prop "controls"
{-# INLINE controls_ #-}

loop_ :: Bool -> Html ()
loop_ = prop "loop"
{-# INLINE loop_ #-}

preload_ :: Utf8 -> Html ()
preload_ = prop "preload"
{-# INLINE preload_ #-}

poster_ :: Utf8 -> Html ()
poster_ = prop "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> Html ()
default_ = prop "default"
{-# INLINE default_ #-}

kind_ :: Utf8 -> Html ()
kind_ = prop "kind"
{-# INLINE kind_ #-}

srclang_ :: Utf8 -> Html ()
srclang_ = prop "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: Utf8 -> Html ()
sandbox_ = prop "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: Utf8 -> Html ()
seamless_ = prop "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: Utf8 -> Html ()
srcdoc_ = prop "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: Utf8 -> Html ()
reversed_ = prop "reversed"
{-# INLINE reversed_ #-}

start_ :: Utf8 -> Html ()
start_ = prop "start"
{-# INLINE start_ #-}

align_ :: Utf8 -> Html ()
align_ = prop "align"
{-# INLINE align_ #-}

colspan_ :: Utf8 -> Html ()
colspan_ = attr "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: Utf8 -> Html ()
rowspan_ = attr "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: Utf8 -> Html ()
headers_ = prop "headers"
{-# INLINE headers_ #-}

scope_ :: Utf8 -> Html ()
scope_ = prop "scope"
{-# INLINE scope_ #-}

async_ :: Utf8 -> Html ()
async_ = prop "async"
{-# INLINE async_ #-}

charset_ :: Utf8 -> Html ()
charset_ = prop "charset"
{-# INLINE charset_ #-}

content_ :: Utf8 -> Html ()
content_ = prop "content"
{-# INLINE content_ #-}

defer_ :: Utf8 -> Html ()
defer_ = prop "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: Utf8 -> Html ()
httpEquiv_ = prop "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: Utf8 -> Html ()
language_ = prop "language"
{-# INLINE language_ #-}

scoped_ :: Utf8 -> Html ()
scoped_ = prop "scoped"
{-# INLINE scoped_ #-}

type_ :: Utf8 -> Html ()
type_ = prop "type"
{-# INLINE type_ #-}

name_ :: Utf8 -> Html ()
name_ = prop "name"
{-# INLINE name_ #-}

href_ :: Utf8 -> Html ()
href_ = prop "href"
{-# INLINE href_ #-}

id_ :: Utf8 -> Html ()
id_ = prop "id"
{-# INLINE id_ #-}

placeholder_ :: Utf8 -> Html ()
placeholder_ = prop "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> Html ()
checked_ = prop "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> Html ()
autofocus_ = prop "autofocus"
{-# INLINE autofocus_ #-}

class_ :: Utf8 -> Html ()
class_ = prop "className"
{-# INLINE class_ #-}

data_ :: Utf8 -> Utf8 -> Html ()
data_ k v = prop @Utf8 ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: Utf8 -> Html ()
role_ = attr "role"
{-# INLINE role_ #-}

style_ :: Utf8 -> Html ()
style_ = prop "style"
{-# INLINE style_ #-}
