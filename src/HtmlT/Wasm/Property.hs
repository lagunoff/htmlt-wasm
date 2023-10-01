{-|
Shortcuts for common HTML5 attributes and properties
-}
module HtmlT.Wasm.Property where

import Data.ByteString (ByteString)

import "this" HtmlT.Wasm.Html
import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Types

dynStyles :: Dynamic ByteString -> WASM ()
dynStyles = dynProp "style"
{-# INLINE dynStyles #-}

dynValue :: Dynamic ByteString -> WASM ()
dynValue = dynProp "value"
{-# INLINE dynValue #-}

dynClass :: Dynamic ByteString -> WASM ()
dynClass = dynProp "className"
{-# INLINE dynClass #-}

dynChecked :: Dynamic Bool -> WASM ()
dynChecked = dynProp "checked"
{-# INLINE dynChecked #-}

dynDisabled :: Dynamic Bool -> WASM ()
dynDisabled = dynProp "disabled"
{-# INLINE dynDisabled #-}


title_ :: ByteString -> WASM ()
title_ = prop "title"
{-# INLINE title_ #-}

selected_ :: Bool -> WASM ()
selected_ = prop "selected"
{-# INLINE selected_ #-}

hidden_ :: Bool -> WASM ()
hidden_ = prop "hidden"
{-# INLINE hidden_ #-}

value_ :: ByteString -> WASM ()
value_ = prop "value"
{-# INLINE value_ #-}

defaultValue_ :: ByteString -> WASM ()
defaultValue_ = prop "defaultValue"
{-# INLINE defaultValue_ #-}

accept_ :: ByteString -> WASM ()
accept_ = prop "accept"
{-# INLINE accept_ #-}

acceptCharset_ :: ByteString -> WASM ()
acceptCharset_ = prop "acceptCharset"
{-# INLINE acceptCharset_ #-}

action_ :: ByteString -> WASM ()
action_ = prop "action"
{-# INLINE action_ #-}

autocomplete_ :: Bool -> WASM ()
autocomplete_ b = prop @ByteString "autocomplete" (if b then "on" else "off")
{-# INLINE autocomplete_ #-}

autosave_ :: ByteString -> WASM ()
autosave_ = prop "autosave"
{-# INLINE autosave_ #-}

disabled_ :: Bool -> WASM ()
disabled_ = prop "disabled"
{-# INLINE disabled_ #-}

enctype_ :: ByteString -> WASM ()
enctype_ = prop "enctype"
{-# INLINE enctype_ #-}

formation_ :: ByteString -> WASM ()
formation_ = prop "formation"
{-# INLINE formation_ #-}

list_ :: ByteString -> WASM ()
list_ = prop "list"
{-# INLINE list_ #-}

maxlength_ :: ByteString -> WASM ()
maxlength_ = prop "maxlength"
{-# INLINE maxlength_ #-}

minlength_ :: ByteString -> WASM ()
minlength_ = prop "minlength"
{-# INLINE minlength_ #-}

method_ :: ByteString -> WASM ()
method_ = prop "method"
{-# INLINE method_ #-}

multiple_ :: Bool -> WASM ()
multiple_ = prop "multiple"
{-# INLINE multiple_ #-}

novalidate_ :: Bool -> WASM ()
novalidate_ = prop "noValidate"
{-# INLINE novalidate_ #-}

pattern_ :: ByteString -> WASM ()
pattern_ = prop "pattern"
{-# INLINE pattern_ #-}

readonly_ :: Bool -> WASM ()
readonly_ = prop "readOnly"
{-# INLINE readonly_ #-}

required_ :: Bool -> WASM ()
required_ = prop "required"
{-# INLINE required_ #-}

size_ :: ByteString -> WASM ()
size_ = prop "size"
{-# INLINE size_ #-}

forProp_ :: ByteString -> WASM ()
forProp_ = prop "for"
{-# INLINE forProp_ #-}

ref_ :: ByteString -> WASM ()
ref_ = prop "ref"
{-# INLINE ref_ #-}

formProp_ :: ByteString -> WASM ()
formProp_ = prop "form"
{-# INLINE formProp_ #-}

max_ :: ByteString -> WASM ()
max_ = prop "max"
{-# INLINE max_ #-}

min_ :: ByteString -> WASM ()
min_ = prop "min"
{-# INLINE min_ #-}

step_ :: ByteString -> WASM ()
step_ = prop "step"
{-# INLINE step_ #-}

cols_ :: ByteString -> WASM ()
cols_ = prop "cols"
{-# INLINE cols_ #-}

rows_ :: ByteString -> WASM ()
rows_ = prop "rows"
{-# INLINE rows_ #-}

wrap_ :: ByteString -> WASM ()
wrap_ = prop "wrap"
{-# INLINE wrap_ #-}

target_ :: ByteString -> WASM ()
target_ = prop "target"
{-# INLINE target_ #-}

download_ :: ByteString -> WASM ()
download_ = prop "download"
{-# INLINE download_ #-}

downloadAs_ :: ByteString -> WASM ()
downloadAs_ = prop "downloadAs"
{-# INLINE downloadAs_ #-}

hreflang_ :: ByteString -> WASM ()
hreflang_ = prop "hreflang"
{-# INLINE hreflang_ #-}

media_ :: ByteString -> WASM ()
media_ = prop "media"
{-# INLINE media_ #-}

ping_ :: ByteString -> WASM ()
ping_ = prop "ping"
{-# INLINE ping_ #-}

rel_ :: ByteString -> WASM ()
rel_ = prop "rel"
{-# INLINE rel_ #-}

ismap_ :: ByteString -> WASM ()
ismap_ = prop "ismap"
{-# INLINE ismap_ #-}

usemap_ :: ByteString -> WASM ()
usemap_ = prop "usemap"
{-# INLINE usemap_ #-}

shape_ :: ByteString -> WASM ()
shape_ = prop "shape"
{-# INLINE shape_ #-}

coords_ :: ByteString -> WASM ()
coords_ = prop "coords"
{-# INLINE coords_ #-}

src_ :: ByteString -> WASM ()
src_ = prop "src"
{-# INLINE src_ #-}

height_ :: ByteString -> WASM ()
height_ = prop "height"
{-# INLINE height_ #-}

width_ :: ByteString -> WASM ()
width_ = prop "width"
{-# INLINE width_ #-}

alt_ :: ByteString -> WASM ()
alt_ = prop "alt"
{-# INLINE alt_ #-}

autoplay_ :: Bool -> WASM ()
autoplay_ = prop "autoplay"
{-# INLINE autoplay_ #-}

controls_ :: Bool -> WASM ()
controls_ = prop "controls"
{-# INLINE controls_ #-}

loop_ :: Bool -> WASM ()
loop_ = prop "loop"
{-# INLINE loop_ #-}

preload_ :: ByteString -> WASM ()
preload_ = prop "preload"
{-# INLINE preload_ #-}

poster_ :: ByteString -> WASM ()
poster_ = prop "poster"
{-# INLINE poster_ #-}

default_ :: Bool -> WASM ()
default_ = prop "default"
{-# INLINE default_ #-}

kind_ :: ByteString -> WASM ()
kind_ = prop "kind"
{-# INLINE kind_ #-}

srclang_ :: ByteString -> WASM ()
srclang_ = prop "srclang"
{-# INLINE srclang_ #-}

sandbox_ :: ByteString -> WASM ()
sandbox_ = prop "sandbox"
{-# INLINE sandbox_ #-}

seamless_ :: ByteString -> WASM ()
seamless_ = prop "seamless"
{-# INLINE seamless_ #-}

srcdoc_ :: ByteString -> WASM ()
srcdoc_ = prop "srcdoc"
{-# INLINE srcdoc_ #-}

reversed_ :: ByteString -> WASM ()
reversed_ = prop "reversed"
{-# INLINE reversed_ #-}

start_ :: ByteString -> WASM ()
start_ = prop "start"
{-# INLINE start_ #-}

align_ :: ByteString -> WASM ()
align_ = prop "align"
{-# INLINE align_ #-}

colspan_ :: ByteString -> WASM ()
colspan_ = attr "colspan"
{-# INLINE colspan_ #-}

rowspan_ :: ByteString -> WASM ()
rowspan_ = attr "rowspan"
{-# INLINE rowspan_ #-}

headers_ :: ByteString -> WASM ()
headers_ = prop "headers"
{-# INLINE headers_ #-}

scope_ :: ByteString -> WASM ()
scope_ = prop "scope"
{-# INLINE scope_ #-}

async_ :: ByteString -> WASM ()
async_ = prop "async"
{-# INLINE async_ #-}

charset_ :: ByteString -> WASM ()
charset_ = prop "charset"
{-# INLINE charset_ #-}

content_ :: ByteString -> WASM ()
content_ = prop "content"
{-# INLINE content_ #-}

defer_ :: ByteString -> WASM ()
defer_ = prop "defer"
{-# INLINE defer_ #-}

httpEquiv_ :: ByteString -> WASM ()
httpEquiv_ = prop "httpEquiv"
{-# INLINE httpEquiv_ #-}

language_ :: ByteString -> WASM ()
language_ = prop "language"
{-# INLINE language_ #-}

scoped_ :: ByteString -> WASM ()
scoped_ = prop "scoped"
{-# INLINE scoped_ #-}

type_ :: ByteString -> WASM ()
type_ = prop "type"
{-# INLINE type_ #-}

name_ :: ByteString -> WASM ()
name_ = prop "name"
{-# INLINE name_ #-}

href_ :: ByteString -> WASM ()
href_ = prop "href"
{-# INLINE href_ #-}

id_ :: ByteString -> WASM ()
id_ = prop "id"
{-# INLINE id_ #-}

placeholder_ :: ByteString -> WASM ()
placeholder_ = prop "placeholder"
{-# INLINE placeholder_ #-}

checked_ :: Bool -> WASM ()
checked_ = prop "checked"
{-# INLINE checked_ #-}

autofocus_ :: Bool -> WASM ()
autofocus_ = prop "autofocus"
{-# INLINE autofocus_ #-}

class_ :: ByteString -> WASM ()
class_ = prop "className"
{-# INLINE class_ #-}

data_ :: ByteString -> ByteString -> WASM ()
data_ k v = prop @ByteString ("data-" <> k) v
{-# INLINE data_ #-}

role_ :: ByteString -> WASM ()
role_ = attr "role"
{-# INLINE role_ #-}

style_ :: ByteString -> WASM ()
style_ = prop "style"
{-# INLINE style_ #-}
