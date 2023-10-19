module JsmMain where

import Control.Monad
import Control.Monad.Reader
import Data.List qualified as List
import Data.Ord
import GHC.Generics
import HtmlT.WebAssembly
import HtmlT.WebAssembly.Protocol.Utf8 qualified as Utf8

data WidgetState = WidgetState
  { candidates :: [Candidate]
  } deriving (Generic)

data Candidate = Candidate
  { language :: Utf8
  , votes :: Int
  , percentage :: Double
  } deriving (Generic)

data Action = UpVote Utf8 | DownVote Utf8 | Reset

initialState :: WidgetState
initialState = WidgetState $ normalize
  [ Candidate "Python" 100 0
  , Candidate "JavaScript" 80 0
  , Candidate "Java" 70 0
  , Candidate "C++" 60 0
  , Candidate "C#" 50 0
  , Candidate "Go" 40 0
  , Candidate "Rust" 30 0
  , Candidate "Ruby" 20 0
  , Candidate "Swift" 10 0
  , Candidate "Haskell" 423 0
  ]

update :: Action -> WidgetState -> WidgetState
update action old = case action of
  UpVote lang -> old
    {candidates = normalize $ overvote succ lang old.candidates
    }
  DownVote lang -> old
    {candidates = normalize $ overvote pred lang old.candidates
    }
  Reset -> initialState
  where
    overvote :: (Int -> Int) -> Utf8 -> [Candidate] -> [Candidate]
    overvote _ _ [] = []
    overvote f l (x:xs)
      | x.language == l = x {votes = f x.votes} : xs
      | otherwise = x : overvote f l xs

jsmMain :: JSM ()
jsmMain = attachToBody do
  el "style" $ text styles
  votingListRef <- lift $ newRef initialState
  main_ [class_ "wrapper"] do
    h1_ "Programming Languages Rating"
    table_ [class_ "voting-results"] do
      let candidatesDyn = fmap (.candidates) (fromRef votingListRef)
      simpleList candidatesDyn \_ix itemRef -> do
        tr_ do
          td_ do
            b_ $ dynText $ fmap (.language) (fromRef itemRef)
          td_ do
            div_ [class_ "chart-bar"] do
              dynStyles $ chartBarStyles <$> fromRef itemRef
            span_ $ dynText $ nVotesText . (.votes) <$> fromRef itemRef
    p_ "Choose and vote for your favorite programming language (you can also \
      \downvote languages you dislike)"
    div_ [class_ "bottom-toolbar"] do
      choiceRef <- lift $ newRef "Haskell"
      div_ do
        select_ [style_ "margin-right: 4px"] do
          forM_ initialState.candidates \c -> do
            option_ [value_ c.language] $ text c.language
          dynProp "value" $ fromRef choiceRef
          on @"select/change" $ writeRef choiceRef
        votingButtons votingListRef $ fromRef choiceRef
      button_ do
        text "Reset results"
        on @"click" $ modifyRef votingListRef $ update Reset
  where
    votingButtons stateRef languageDyn = div_ [class_ "voting-buttons"] do
      button_ do
        on @"click" $ modifyRef stateRef . update . UpVote =<< readDyn languageDyn
        text "▲"
      button_ do
        on @"click" $ modifyRef stateRef . update . DownVote =<< readDyn languageDyn
        text "▼"
    chartBarStyles c =
      "width:" <> Utf8.utf8Show c.percentage <> "%;"
    nVotesText 1 = "1 vote"
    nVotesText v = Utf8.utf8Show v <> " votes"

normalize :: [Candidate] -> [Candidate]
normalize candidates =
  let
    totalVotes = fromIntegral $ sum $ fmap (.votes) candidates
    updatePercentage c = c
      {percentage = fromIntegral c.votes / totalVotes * 100}
  in
    fmap updatePercentage $ List.sortOn (Down . (.votes)) candidates

styles :: Utf8
styles = "\
  \body, body * {\n\
  \  font-family: Arial, sans-serif;\n\
  \}\n\
  \.wrapper {\n\
  \  max-width: 550px;\n\
  \  margin: 0 auto;\n\
  \}\n\
  \.wrapper h1 {\n\
  \  text-align:center;\n\
  \  margin: 48px 0 36px 0;\n\
  \}\n\
  \.wrapper button {\n\
  \  background: white;\n\
  \  cursor: pointer;\n\
  \  border: solid 1px #aaa;\n\
  \  border-radius: 3px;\n\
  \}\n\
  \.wrapper select {\n\
  \  background: white;\n\
  \  border: solid 1px #aaa;\n\
  \  border-radius: 3px;\n\
  \}\n\
  \.wrapper button:active {\n\
  \  background: #0089ff;\n\
  \  color: white;\n\
  \  border-color: #0089ff;\n\
  \}\n\
  \.voting-results {\n\
  \  width: 100%;\n\
  \  border-collapse: collapse;\n\
  \}\n\
  \.voting-results td {\n\
  \  height: 38px;\n\
  \}\n\
  \.voting-results tr:nth-child(odd) {\n\
  \  background: #f2f2f2;\n\
  \}\n\
  \.voting-results td:nth-child(1) {\n\
  \  text-align: right;\n\
  \  padding: 0 16px 0 16px;\n\
  \}\n\
  \.voting-results td:nth-child(2) {\n\
  \  width: 90%;\n\
  \  position: relative;\n\
  \  text-align: right;\n\
  \}\n\
  \.voting-results td:nth-child(2) > span{\n\
  \  z-index: 10;\n\
  \  position: relative;\n\
  \  text-shadow: 0px 0px 4px white;\n\
  \}\n\
  \.voting-results .voting-buttons {\n\
  \  visibility: hidden;\n\
  \}\n\
  \.voting-results tr:hover .voting-buttons {\n\
  \  visibility: visible;\n\
  \}\n\
  \.voting-buttons {\n\
  \  display: flex;\n\
  \  flex-direction: column;\n\
  \}\n\
  \.voting-buttons > *:nth-child(1) {\n\
  \  border: solid 1px #aaa;\n\
  \  border-bottom-left-radius: 0;\n\
  \  border-bottom-right-radius: 0;\n\
  \  border-top-left-radius: 3px;\n\
  \  border-top-right-radius: 3px;\n\
  \}\n\
  \.voting-buttons > *:nth-child(2) {\n\
  \  border: solid 1px #aaa;\n\
  \  border-top: none;\n\
  \  border-top-left-radius: 0;\n\
  \  border-top-right-radius: 0;\n\
  \  border-bottom-left-radius: 3px;\n\
  \  border-bottom-right-radius: 3px;\n\
  \}\n\
  \.bottom-toolbar {\n\
  \  display: flex;\n\
  \  flex-direction: row;\n\
  \  justify-content: space-between;\n\
  \}\n\
  \.bottom-toolbar > *:nth-child(1) {\n\
  \  display: flex;\n\
  \  flex-direction: row;\n\
  \}\n\
  \.chart-bar {\n\
  \  position: absolute;\n\
  \  height: calc(100% - 16px);\n\
  \  top: 8px;\n\
  \  left: 0px;\n\
  \  background: #0089ff;\n\
  \}\n\
  \ "
