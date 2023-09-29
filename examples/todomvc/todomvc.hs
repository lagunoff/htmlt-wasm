import Data.Binary qualified as Binary
import Data.ByteString.Lazy qualified as BSL
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import Control.Monad.Reader

import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Types

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

wasmMain :: Wasm ()
wasmMain = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp $ LAssign (unElBuilder domBuilderId) (Var "document" `Dot` "body")
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
