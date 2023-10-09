{-| Shortcuts for common HTML5 attributes and properties
-}
module HtmlT.Wasm.Property where

import Data.ByteString (ByteString)

import "this" HtmlT.Wasm.Html
import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Types

dynStyles :: Dynamic ByteString -> WA ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic ByteString -> WA ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic ByteString -> WA ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> WA ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> WA ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}

title_ :: ByteString -> WA ()
title_ = prop "title"
{-# INLINE title_ #-}

selected_ :: Bool -> WA ()
selected_ = prop "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> WA ()
hidden_ = prop "hidden"
{-# INLINE hidden_ #-}

value_ :: ByteString -> WA ()
value_ = prop "value"
{-# INLINE value_ #-}

defaultValue_ :: ByteString -> WA ()
defaultValue_ = prop "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: ByteString -> WA ()
accept_ = prop "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: ByteString -> WA ()
acceptCharset_ = prop "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: ByteString -> WA ()
action_ = prop "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> WA ()
autocomplete_ b = prop @ByteString "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: ByteString -> WA ()
autosave_ = prop "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> WA ()
disabled_ = prop "disabled"
{-# INLINE disabled_ #-}

enctype_ :: ByteString -> WA ()
enctype_ = prop "enctype"
{-# INLINE enctype_ #-}

formation_ :: ByteString -> WA ()
formation_ = prop "formation"
{-# INLINE formation_ #-}

list_ :: ByteString -> WA ()
list_ = prop "list"
{-# INLINE list_ #-}

maxlength_ :: ByteString -> WA ()
maxlength_ = prop "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: ByteString -> WA ()
minlength_ = prop "minlength"
{-# INLINE minlength_ #-}

method_ :: ByteString -> WA ()
method_ = prop "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> WA ()
multiple_ = prop "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> WA ()
novalidate_ = prop "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: ByteString -> WA ()
pattern_ = prop "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> WA ()
readonly_ = prop "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> WA ()
required_ = prop "required"
{-# INLINE required_ #-}

size_ :: ByteString -> WA ()
size_ = prop "size"
{-# INLINE size_ #-}

forProp_ :: ByteString -> WA ()
forProp_ = prop "for"
{-# INLINE forProp_ #-}

ref_ :: ByteString -> WA ()
ref_ = prop "ref"
{-# INLINE ref_ #-}

formProp_ :: ByteString -> WA ()
formProp_ = prop "form"
{-# INLINE formProp_ #-}

max_ :: ByteString -> WA ()
max_ = prop "max"
{-# INLINE max_ #-}

min_ :: ByteString -> WA ()
min_ = prop "min"
{-# INLINE min_ #-}

step_ :: ByteString -> WA ()
step_ = prop "step"
{-# INLINE step_ #-}

cols_ :: ByteString -> WA ()
cols_ = prop "cols"
{-# INLINE cols_ #-}

rows_ :: ByteString -> WA ()
rows_ = prop "rows"
{-# INLINE rows_ #-}

wrap_ :: ByteString -> WA ()
wrap_ = prop "wrap"
{-# INLINE wrap_ #-}

target_ :: ByteString -> WA ()
target_ = prop "target"
{-# INLINE target_ #-}

download_ :: ByteString -> WA ()
download_ = prop "download"
{-# INLINE download_ #-}

downloadAs_ :: ByteString -> WA ()
downloadAs_ = prop "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: ByteString -> WA ()
hreflang_ = prop "hreflang"
{-# INLINE hreflang_ #-}

media_ :: ByteString -> WA ()
media_ = prop "media"
{-# INLINE media_ #-}

ping_ :: ByteString -> WA ()
ping_ = prop "ping"
{-# INLINE ping_ #-}

rel_ :: ByteString -> WA ()
rel_ = prop "rel"
{-# INLINE rel_ #-}

ismap_ :: ByteString -> WA ()
ismap_ = prop "ismap"
{-# INLINE ismap_ #-}

usemap_ :: ByteString -> WA ()
usemap_ = prop "usemap"
{-# INLINE usemap_ #-}

shape_ :: ByteString -> WA ()
shape_ = prop "shape"
{-# INLINE shape_ #-}

coords_ :: ByteString -> WA ()
coords_ = prop "coords"
{-# INLINE coords_ #-}

src_ :: ByteString -> WA ()
src_ = prop "src"
{-# INLINE src_ #-}

height_ :: ByteString -> WA ()
height_ = prop "height"
{-# INLINE height_ #-}

width_ :: ByteString -> WA ()
width_ = prop "width"
{-# INLINE width_ #-}

alt_ :: ByteString -> WA ()
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

preload_ :: ByteString -> WA ()
preload_ = prop "preload"
{-# INLINE preload_ #-}

poster_ :: ByteString -> WA ()
poster_ = prop "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> WA ()
default_ = prop "default"
{-# INLINE default_ #-}

kind_ :: ByteString -> WA ()
kind_ = prop "kind"
{-# INLINE kind_ #-}

srclang_ :: ByteString -> WA ()
srclang_ = prop "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: ByteString -> WA ()
sandbox_ = prop "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: ByteString -> WA ()
seamless_ = prop "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: ByteString -> WA ()
srcdoc_ = prop "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: ByteString -> WA ()
reversed_ = prop "reversed"
{-# INLINE reversed_ #-}

start_ :: ByteString -> WA ()
start_ = prop "start"
{-# INLINE start_ #-}

align_ :: ByteString -> WA ()
align_ = prop "align"
{-# INLINE align_ #-}

colspan_ :: ByteString -> WA ()
colspan_ = attr "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: ByteString -> WA ()
rowspan_ = attr "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: ByteString -> WA ()
headers_ = prop "headers"
{-# INLINE headers_ #-}

scope_ :: ByteString -> WA ()
scope_ = prop "scope"
{-# INLINE scope_ #-}

async_ :: ByteString -> WA ()
async_ = prop "async"
{-# INLINE async_ #-}

charset_ :: ByteString -> WA ()
charset_ = prop "charset"
{-# INLINE charset_ #-}

content_ :: ByteString -> WA ()
content_ = prop "content"
{-# INLINE content_ #-}

defer_ :: ByteString -> WA ()
defer_ = prop "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: ByteString -> WA ()
httpEquiv_ = prop "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: ByteString -> WA ()
language_ = prop "language"
{-# INLINE language_ #-}

scoped_ :: ByteString -> WA ()
scoped_ = prop "scoped"
{-# INLINE scoped_ #-}

type_ :: ByteString -> WA ()
type_ = prop "type"
{-# INLINE type_ #-}

name_ :: ByteString -> WA ()
name_ = prop "name"
{-# INLINE name_ #-}

href_ :: ByteString -> WA ()
href_ = prop "href"
{-# INLINE href_ #-}

id_ :: ByteString -> WA ()
id_ = prop "id"
{-# INLINE id_ #-}

placeholder_ :: ByteString -> WA ()
placeholder_ = prop "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> WA ()
checked_ = prop "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> WA ()
autofocus_ = prop "autofocus"
{-# INLINE autofocus_ #-}

class_ :: ByteString -> WA ()
class_ = prop "className"
{-# INLINE class_ #-}

data_ :: ByteString -> ByteString -> WA ()
data_ k v = prop @ByteString ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: ByteString -> WA ()
role_ = attr "role"
{-# INLINE role_ #-}

style_ :: ByteString -> WA ()
style_ = prop "style"
{-# INLINE style_ #-}
