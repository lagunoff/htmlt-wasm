{-| Shortcuts for common HTML5 attributes and properties
-}
module HtmlT.Wasm.Property where

import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Html
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Types

dynStyles :: Dynamic Utf8 -> WA ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic Utf8 -> WA ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic Utf8 -> WA ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> WA ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> WA ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: Utf8 -> WA ()
title_ = prop "title"
{-# INLINE title_ #-}

selected_ :: Bool -> WA ()
selected_ = prop "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> WA ()
hidden_ = prop "hidden"
{-# INLINE hidden_ #-}

value_ :: Utf8 -> WA ()
value_ = prop "value"
{-# INLINE value_ #-}

defaultValue_ :: Utf8 -> WA ()
defaultValue_ = prop "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: Utf8 -> WA ()
accept_ = prop "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: Utf8 -> WA ()
acceptCharset_ = prop "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: Utf8 -> WA ()
action_ = prop "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> WA ()
autocomplete_ b = prop @Utf8 "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: Utf8 -> WA ()
autosave_ = prop "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> WA ()
disabled_ = prop "disabled"
{-# INLINE disabled_ #-}

enctype_ :: Utf8 -> WA ()
enctype_ = prop "enctype"
{-# INLINE enctype_ #-}

formation_ :: Utf8 -> WA ()
formation_ = prop "formation"
{-# INLINE formation_ #-}

list_ :: Utf8 -> WA ()
list_ = prop "list"
{-# INLINE list_ #-}

maxlength_ :: Utf8 -> WA ()
maxlength_ = prop "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: Utf8 -> WA ()
minlength_ = prop "minlength"
{-# INLINE minlength_ #-}

method_ :: Utf8 -> WA ()
method_ = prop "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> WA ()
multiple_ = prop "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> WA ()
novalidate_ = prop "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: Utf8 -> WA ()
pattern_ = prop "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> WA ()
readonly_ = prop "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> WA ()
required_ = prop "required"
{-# INLINE required_ #-}

size_ :: Utf8 -> WA ()
size_ = prop "size"
{-# INLINE size_ #-}

forProp_ :: Utf8 -> WA ()
forProp_ = prop "for"
{-# INLINE forProp_ #-}

ref_ :: Utf8 -> WA ()
ref_ = prop "ref"
{-# INLINE ref_ #-}

formProp_ :: Utf8 -> WA ()
formProp_ = prop "form"
{-# INLINE formProp_ #-}

max_ :: Utf8 -> WA ()
max_ = prop "max"
{-# INLINE max_ #-}

min_ :: Utf8 -> WA ()
min_ = prop "min"
{-# INLINE min_ #-}

step_ :: Utf8 -> WA ()
step_ = prop "step"
{-# INLINE step_ #-}

cols_ :: Utf8 -> WA ()
cols_ = prop "cols"
{-# INLINE cols_ #-}

rows_ :: Utf8 -> WA ()
rows_ = prop "rows"
{-# INLINE rows_ #-}

wrap_ :: Utf8 -> WA ()
wrap_ = prop "wrap"
{-# INLINE wrap_ #-}

target_ :: Utf8 -> WA ()
target_ = prop "target"
{-# INLINE target_ #-}

download_ :: Utf8 -> WA ()
download_ = prop "download"
{-# INLINE download_ #-}

downloadAs_ :: Utf8 -> WA ()
downloadAs_ = prop "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: Utf8 -> WA ()
hreflang_ = prop "hreflang"
{-# INLINE hreflang_ #-}

media_ :: Utf8 -> WA ()
media_ = prop "media"
{-# INLINE media_ #-}

ping_ :: Utf8 -> WA ()
ping_ = prop "ping"
{-# INLINE ping_ #-}

rel_ :: Utf8 -> WA ()
rel_ = prop "rel"
{-# INLINE rel_ #-}

ismap_ :: Utf8 -> WA ()
ismap_ = prop "ismap"
{-# INLINE ismap_ #-}

usemap_ :: Utf8 -> WA ()
usemap_ = prop "usemap"
{-# INLINE usemap_ #-}

shape_ :: Utf8 -> WA ()
shape_ = prop "shape"
{-# INLINE shape_ #-}

coords_ :: Utf8 -> WA ()
coords_ = prop "coords"
{-# INLINE coords_ #-}

src_ :: Utf8 -> WA ()
src_ = prop "src"
{-# INLINE src_ #-}

height_ :: Utf8 -> WA ()
height_ = prop "height"
{-# INLINE height_ #-}

width_ :: Utf8 -> WA ()
width_ = prop "width"
{-# INLINE width_ #-}

alt_ :: Utf8 -> WA ()
alt_ = prop "alt"
{-# INLINE alt_ #-}

autoplay_ :: Bool -> WA ()
autoplay_ = prop "autoplay"
{-# INLINE autoplay_ #-}

controls_ :: Bool -> WA ()
controls_ = prop "controls"
{-# INLINE controls_ #-}

loop_ :: Bool -> WA ()
loop_ = prop "loop"
{-# INLINE loop_ #-}

preload_ :: Utf8 -> WA ()
preload_ = prop "preload"
{-# INLINE preload_ #-}

poster_ :: Utf8 -> WA ()
poster_ = prop "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> WA ()
default_ = prop "default"
{-# INLINE default_ #-}

kind_ :: Utf8 -> WA ()
kind_ = prop "kind"
{-# INLINE kind_ #-}

srclang_ :: Utf8 -> WA ()
srclang_ = prop "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: Utf8 -> WA ()
sandbox_ = prop "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: Utf8 -> WA ()
seamless_ = prop "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: Utf8 -> WA ()
srcdoc_ = prop "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: Utf8 -> WA ()
reversed_ = prop "reversed"
{-# INLINE reversed_ #-}

start_ :: Utf8 -> WA ()
start_ = prop "start"
{-# INLINE start_ #-}

align_ :: Utf8 -> WA ()
align_ = prop "align"
{-# INLINE align_ #-}

colspan_ :: Utf8 -> WA ()
colspan_ = attr "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: Utf8 -> WA ()
rowspan_ = attr "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: Utf8 -> WA ()
headers_ = prop "headers"
{-# INLINE headers_ #-}

scope_ :: Utf8 -> WA ()
scope_ = prop "scope"
{-# INLINE scope_ #-}

async_ :: Utf8 -> WA ()
async_ = prop "async"
{-# INLINE async_ #-}

charset_ :: Utf8 -> WA ()
charset_ = prop "charset"
{-# INLINE charset_ #-}

content_ :: Utf8 -> WA ()
content_ = prop "content"
{-# INLINE content_ #-}

defer_ :: Utf8 -> WA ()
defer_ = prop "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: Utf8 -> WA ()
httpEquiv_ = prop "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: Utf8 -> WA ()
language_ = prop "language"
{-# INLINE language_ #-}

scoped_ :: Utf8 -> WA ()
scoped_ = prop "scoped"
{-# INLINE scoped_ #-}

type_ :: Utf8 -> WA ()
type_ = prop "type"
{-# INLINE type_ #-}

name_ :: Utf8 -> WA ()
name_ = prop "name"
{-# INLINE name_ #-}

href_ :: Utf8 -> WA ()
href_ = prop "href"
{-# INLINE href_ #-}

id_ :: Utf8 -> WA ()
id_ = prop "id"
{-# INLINE id_ #-}

placeholder_ :: Utf8 -> WA ()
placeholder_ = prop "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> WA ()
checked_ = prop "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> WA ()
autofocus_ = prop "autofocus"
{-# INLINE autofocus_ #-}

class_ :: Utf8 -> WA ()
class_ = prop "className"
{-# INLINE class_ #-}

data_ :: Utf8 -> Utf8 -> WA ()
data_ k v = prop @Utf8 ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: Utf8 -> WA ()
role_ = attr "role"
{-# INLINE role_ #-}

style_ :: Utf8 -> WA ()
style_ = prop "style"
{-# INLINE style_ #-}
