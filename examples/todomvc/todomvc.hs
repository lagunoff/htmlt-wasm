import Data.Binary qualified as Binary
import Data.ByteString.Lazy qualified as BSL
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import Control.Monad.Reader
import Data.Functor
import Data.ByteString
import Data.ByteString.Char8 qualified as Char8

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

data Employee = Employee
  { employeeName :: ByteString
  , employeeRole :: ByteString
  , employeeSalary :: Int
  }

initialEmployeeList :: [Employee]
initialEmployeeList =
  [ Employee "John Doe" "Manager" 60000
  , Employee "Jane Smith" "Developer" 50000
  , Employee "Jim Brown" "Designer" 55000
  , Employee "Sarah Johnson" "Marketing Specialist" 48000
  , Employee "Michael Davis" "Sales Representative" 52000
  , Employee "Laura Williams" "HR Coordinator" 45000
  ]

wasmMain :: WASM ()
wasmMain = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp $ LAssign (unElBuilder domBuilderId) (Var "document" `Dot` "body")
  el "ul" do
    listRef <- newRef initialEmployeeList
    simpleList listRef.dynref_value \_ix itemRef -> do
      employe <- readRef itemRef
      el "li" do
        el "span" (text employe.employeeName)
        el "b" (text employe.employeeRole)
        el "i" (text (Char8.pack (show employe.employeeSalary)))

  tabRef <- newRef Foo
  tabRef2 <- newRef Foo
  el "div" $ dyn $ fromRef tabRef <&> \case
    Foo -> el "span" (text "Foo")  >> (void $ installFinalizer $ consoleLog (Str "Foo1 finalizeed !!"))
    Bar -> do
      el "div" $ dyn $ fromRef tabRef2 <&> \case
        Foo -> el "span" (text "Foo") >> (void $ installFinalizer $ consoleLog (Str "Foo finalizeed !!"))
        Bar -> el "span" (text "Bar") >> (void $ installFinalizer $ consoleLog (Str "Bar finalizeed !!"))
        Baz -> el "b" (text "Baz") >> (void $ installFinalizer $ consoleLog (Str "Baz finalizeed !!"))
      el "div" do
        el "button" do
          on_ "click" $ writeRef tabRef2 Foo
          text "Foo"
        el "button" do
          on_ "click" $ writeRef tabRef2 Bar
          text "Bar"
        el "button" do
          on_ "click" $ writeRef tabRef2 Baz
          text "Baz"
    Baz -> el "b" (text "Baz") >> (void $ installFinalizer $ consoleLog (Str "Baz1 finalizeed !!"))
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
