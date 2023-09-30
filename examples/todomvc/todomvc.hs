import Data.Binary qualified as Binary
import Data.ByteString.Lazy qualified as BSL
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import Control.Monad.Reader
import Data.Functor

import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Html
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Event

foreign export ccall app :: Ptr Word8 -> IO (Ptr Word8)

app :: Ptr Word8 -> IO (Ptr Word8)
app p = do
  downCmd <- Binary.decode . BSL.fromStrict <$> loadByteString p
  upCmd <- handleCommand wasmMain downCmd
  storeByteString $ BSL.toStrict $ Binary.encode upCmd

foreign export ccall hs_malloc :: Int -> IO (Ptr a)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

main = return ()

data Tab = Foo | Bar | Baz

wasmMain :: WASM ()
wasmMain = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp $ LAssign (unElBuilder domBuilderId) (Var "document" `Dot` "body")
  tabRef <- newRef Foo
  el "div" $ dyn $ fromRef tabRef <&> \case
    Foo -> el "span" (text "Foo")
    Bar -> el "span" (text "Bar")
    Baz -> el "b" (text "Baz")
  el "div" do
    el "button" do
      on_ "click" $ writeRef tabRef Foo
      text "Foo"
    el "button" do
      on_ "click" $ writeRef tabRef Bar
      text "Bar"
    el "button" do
      on_ "click" $ writeRef tabRef Baz
      text "Baz"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button 1 clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button 2 clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  el "div" do
    prop "className" "root"
    el "h1" do prop "className" "h1-wrapper"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Click me!"
    el "button" do
      prop "className" "root-button"
      on_ "click" $ consoleLog (Str "Button clicked!!")
      text "Don't click here!"
  return ()
