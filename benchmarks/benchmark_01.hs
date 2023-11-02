import Control.Monad
import Data.Binary qualified as Binary
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Gauge.Main
import HtmlT

import "this" TodoList qualified as TodoList
import "this" TodoItem qualified as TodoItem


main = defaultMain
  [ bgroup "buildTodoMVC"
    [ bench "10" $ whnfIO $ buildTodoMVC 10
    , bench "100" $ whnfIO $ buildTodoMVC 100
    , bench "1000" $ whnfIO $ buildTodoMVC 1000
    ]
  , bgroup "subscribeAndFire"
    [ bench "100 10 10" $ whnfIO $ subscribeAndFire 100 10 10
    , bench "10 100 10" $ whnfIO $ subscribeAndFire 10 100 10
    , bench "10 10 100" $ whnfIO $ subscribeAndFire 10 10 100
    ]
  ]

subscribeAndFire :: Int -> Int -> Int -> IO ()
subscribeAndFire eventsNum subsNum fireNum = reactive do
  -- Create a bunch of 'DynRef's
  refsList <- forM [1..eventsNum] $ const (newRef (0 :: Int))
  outputRef <- newRef Nothing
  -- Sum all of their values into a single 'Dynamic' using
  -- 'Applicative' instance
  let sumDyn = fmap sum . sequenceA . fmap fromRef $ refsList
  -- Attach subsNum amount of subscriptions
  sequence_ $ take subsNum $ repeat $ subscribeAndWrite sumDyn outputRef
  -- And fire modification event for each 'DynRef' fireNum times
  forM_ [1..fireNum] $ const $
    forM_ refsList $ dynStep . flip modifyRef succ
  where
    subscribeAndWrite from to = void $ subscribe (updates from) $
      writeRef to . Just
    reactive act = do
      (_a, _res) <- unJSM act (JSMEnv (-1)) emptyWAState
      return ()

buildTodoMVC :: Int -> IO ByteString
buildTodoMVC ntasks = do
  let tasks = take ntasks (cycle oneHandredTasks)
  wasmInstance <- do
    wasm_state_ref <- newIORef emptyWAState
    continuations_ref <- newIORef []
    return WasmInstance {wasm_state_ref, continuations_ref}
  haskMessage <- handleMessage wasmInstance (jsmMain tasks) (Start startMessage)
  return $ BSL.toStrict $ Binary.encode haskMessage
  where
    startMessage = StartFlags
      { initial_url = Location
         { protocol = ""
         , hostname = ""
         , port = ""
         , pathname = ""
         , search = ""
         , hash = ""
         }
      }

jsmMain :: [TodoItem.TodoItemState] -> StartFlags -> JSM ()
jsmMain tasks _ = do
  todoListStateRef <- TodoList.eval $ TodoList.InitAction tasks
  attachToBody do
    el "style" $ text TodoList.styles
    TodoList.html TodoList.TodoListConfig
      { state_ref = todoListStateRef
      }

oneHandredTasks :: [TodoItem.TodoItemState]
oneHandredTasks =
  [ TodoItem.TodoItemState "One" False Nothing
  , TodoItem.TodoItemState "Two" False Nothing
  , TodoItem.TodoItemState "Three" False Nothing
  , TodoItem.TodoItemState "Four" False Nothing
  , TodoItem.TodoItemState "Five" False Nothing
  , TodoItem.TodoItemState "Six" False Nothing
  , TodoItem.TodoItemState "Seven" False Nothing
  , TodoItem.TodoItemState "Eight" False Nothing
  , TodoItem.TodoItemState "Nine" False Nothing
  , TodoItem.TodoItemState "Ten" False Nothing
  , TodoItem.TodoItemState "Eleven" False Nothing
  , TodoItem.TodoItemState "Twelve" False Nothing
  , TodoItem.TodoItemState "Thirteen" False Nothing
  , TodoItem.TodoItemState "Fourteen" False Nothing
  , TodoItem.TodoItemState "Fifteen" False Nothing
  , TodoItem.TodoItemState "Sixteen" False Nothing
  , TodoItem.TodoItemState "Seventeen" False Nothing
  , TodoItem.TodoItemState "Eighteen" False Nothing
  , TodoItem.TodoItemState "Nineteen" False Nothing
  , TodoItem.TodoItemState "Twenty" False Nothing
  , TodoItem.TodoItemState "Twenty-One" False Nothing
  , TodoItem.TodoItemState "Twenty-Two" False Nothing
  , TodoItem.TodoItemState "Twenty-Three" False Nothing
  , TodoItem.TodoItemState "Twenty-Four" False Nothing
  , TodoItem.TodoItemState "Twenty-Five" False Nothing
  , TodoItem.TodoItemState "Twenty-Six" False Nothing
  , TodoItem.TodoItemState "Twenty-Seven" False Nothing
  , TodoItem.TodoItemState "Twenty-Eight" False Nothing
  , TodoItem.TodoItemState "Twenty-Nine" False Nothing
  , TodoItem.TodoItemState "Thirty" False Nothing
  , TodoItem.TodoItemState "Thirty-One" False Nothing
  , TodoItem.TodoItemState "Thirty-Two" False Nothing
  , TodoItem.TodoItemState "Thirty-Three" False Nothing
  , TodoItem.TodoItemState "Thirty-Four" False Nothing
  , TodoItem.TodoItemState "Thirty-Five" False Nothing
  , TodoItem.TodoItemState "Thirty-Six" False Nothing
  , TodoItem.TodoItemState "Thirty-Seven" False Nothing
  , TodoItem.TodoItemState "Thirty-Eight" False Nothing
  , TodoItem.TodoItemState "Thirty-Nine" False Nothing
  , TodoItem.TodoItemState "Forty" False Nothing
  , TodoItem.TodoItemState "Forty-One" False Nothing
  , TodoItem.TodoItemState "Forty-Two" False Nothing
  , TodoItem.TodoItemState "Forty-Three" False Nothing
  , TodoItem.TodoItemState "Forty-Four" False Nothing
  , TodoItem.TodoItemState "Forty-Five" False Nothing
  , TodoItem.TodoItemState "Forty-Six" False Nothing
  , TodoItem.TodoItemState "Forty-Seven" False Nothing
  , TodoItem.TodoItemState "Forty-Eight" False Nothing
  , TodoItem.TodoItemState "Forty-Nine" False Nothing
  , TodoItem.TodoItemState "Fifty" False Nothing
  , TodoItem.TodoItemState "Fifty-One" False Nothing
  , TodoItem.TodoItemState "Fifty-Two" False Nothing
  , TodoItem.TodoItemState "Fifty-Three" False Nothing
  , TodoItem.TodoItemState "Fifty-Four" False Nothing
  , TodoItem.TodoItemState "Fifty-Five" False Nothing
  , TodoItem.TodoItemState "Fifty-Six" False Nothing
  , TodoItem.TodoItemState "Fifty-Seven" False Nothing
  , TodoItem.TodoItemState "Fifty-Eight" False Nothing
  , TodoItem.TodoItemState "Fifty-Nine" False Nothing
  , TodoItem.TodoItemState "Sixty" False Nothing
  , TodoItem.TodoItemState "Sixty-One" False Nothing
  , TodoItem.TodoItemState "Sixty-Two" False Nothing
  , TodoItem.TodoItemState "Sixty-Three" False Nothing
  , TodoItem.TodoItemState "Sixty-Four" False Nothing
  , TodoItem.TodoItemState "Sixty-Five" False Nothing
  , TodoItem.TodoItemState "Sixty-Six" False Nothing
  , TodoItem.TodoItemState "Sixty-Seven" False Nothing
  , TodoItem.TodoItemState "Sixty-Eight" False Nothing
  , TodoItem.TodoItemState "Sixty-Nine" False Nothing
  , TodoItem.TodoItemState "Seventy" False Nothing
  , TodoItem.TodoItemState "Seventy-One" False Nothing
  , TodoItem.TodoItemState "Seventy-Two" False Nothing
  , TodoItem.TodoItemState "Seventy-Three" False Nothing
  , TodoItem.TodoItemState "Seventy-Four" False Nothing
  , TodoItem.TodoItemState "Seventy-Five" False Nothing
  , TodoItem.TodoItemState "Seventy-Six" False Nothing
  , TodoItem.TodoItemState "Seventy-Seven" False Nothing
  , TodoItem.TodoItemState "Seventy-Eight" False Nothing
  , TodoItem.TodoItemState "Seventy-Nine" False Nothing
  , TodoItem.TodoItemState "Eighty" False Nothing
  , TodoItem.TodoItemState "Eighty-One" False Nothing
  , TodoItem.TodoItemState "Eighty-Two" False Nothing
  , TodoItem.TodoItemState "Eighty-Three" False Nothing
  , TodoItem.TodoItemState "Eighty-Four" False Nothing
  , TodoItem.TodoItemState "Eighty-Five" False Nothing
  , TodoItem.TodoItemState "Eighty-Six" False Nothing
  , TodoItem.TodoItemState "Eighty-Seven" False Nothing
  , TodoItem.TodoItemState "Eighty-Eight" False Nothing
  , TodoItem.TodoItemState "Eighty-Nine" False Nothing
  , TodoItem.TodoItemState "Ninety" False Nothing
  , TodoItem.TodoItemState "Ninety-One" False Nothing
  , TodoItem.TodoItemState "Ninety-Two" False Nothing
  , TodoItem.TodoItemState "Ninety-Three" False Nothing
  , TodoItem.TodoItemState "Ninety-Four" False Nothing
  , TodoItem.TodoItemState "Ninety-Five" False Nothing
  , TodoItem.TodoItemState "Ninety-Six" False Nothing
  , TodoItem.TodoItemState "Ninety-Seven" False Nothing
  , TodoItem.TodoItemState "Ninety-Eight" False Nothing
  , TodoItem.TodoItemState "Ninety-Nine" False Nothing
  , TodoItem.TodoItemState "One Hundred" False Nothing
  ]
