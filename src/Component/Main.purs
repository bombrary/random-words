module Component.Main where


import Prelude

import Affjax as AX
import Affjax.ResponseFormat (string)
import Choose.Distinct (choose)
import Control.Monad.Except as Except
import DOM.HTML.Indexed.InputAcceptType (InputAcceptType(..), InputAcceptTypeAtom(AcceptMediaType))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.String.Utils (lines)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Web.File.Blob (Blob)
import Web.File.File as File
import Web.File.FileReader as FileReader
import Web.HTML.Event.EventTypes as ET

baseURL :: String
baseURL = "https://bombrary.github.io/random-word"


type BasicWords = { nouns :: Array String
                  , adjectives :: Array String
                  , adverbs :: Array String
                  , verbs :: Array String
                  }

type State =
  { words :: Maybe (Array String)
  , randomWords :: Maybe (Array String)
  , basicWords :: Maybe BasicWords
  }


data Action
  = Randomize
  | FileSelected Blob
  | FileLoaded H.SubscriptionId FileReader.FileReader 
  | Initialize
  | SetWords String
  | NoAction


component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
  }


initialState :: forall input. input -> State
initialState _ =
  { words: Nothing
  , randomWords: Nothing
  , basicWords: Nothing
  }


render :: forall m. State -> HH.ComponentHTML Action () m
render { randomWords } =
  HH.main
    []
    [ HH.img
        [ HP.classes [ HH.ClassName "logo" ]
        , HP.src "logo.svg"
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "description" ]
        ]
        [ HH.p_ [ HH.text "生成ボタンを押すと、ランダムに単語が生成されます。" ]
        , HH.p_ [ HH.text "生成する単語の品詞は、生成ボタン右の選択メニューから選べます。またそれ以外にも、「ファイルを選択」ボタンから自分の単語データを読み込むことができます。ただし単語データのファイルは、単語が改行で区切られたテキストファイルとします。"]
        , HH.p_ [ HH.text "品詞ごとの単語データは"
                , HH.a [ HP.href "https://bond-lab.github.io/wnja/" ] [ HH.text "日本語Wordnet" ]
                , HH.text "より抽出しました。"
                ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "random-words" ]] $
        [ renderRandomWords randomWords ]
    , HH.div
        [ HP.classes [ HH.ClassName "control-panel" ] ]
        [ HH.button
            [ HE.onClick (\_ -> Randomize)
            , HP.classes [ HH.ClassName "randomize-btn" ]
            ]
            [ HH.text "単語を生成"
            ]

        , HH.div
            [ HP.classes [ HH.ClassName "select-preset" ]]
            [ HH.select
              [ HP.classes [ HH.ClassName "select-preset" ]
              , HE.onValueChange SetWords
              ]
              [ HH.option [ HP.value "nouns" ] [ HH.text "名詞" ]
              , HH.option [ HP.value "verbs" ] [ HH.text "動詞" ]
              , HH.option [ HP.value "adjectives" ] [ HH.text "形容詞" ]
              , HH.option [ HP.value "adverbs" ] [ HH.text "副詞" ]
              -- , HH.option [ HP.value "all" ] [ HH.text "全て" ]
              ]
            ]
          , HH.label
              [ HP.classes [ HH.ClassName "input-file-wrap" ] ]
              [ HH.text "ファイルを選択"
              , HH.input
                  [ HP.classes [ HH.ClassName "input-file" ]
                  , HP.type_ HP.InputFile
                  , HP.accept (InputAcceptType [AcceptMediaType (MediaType "text/plain")])
                  , HE.onFileUpload $
                      case _ of
                        [file] ->
                          FileSelected (File.toBlob file)

                        _ ->
                          NoAction
                  ]
              ]
        ]
    ]
    

renderRandomWords :: forall w i. Maybe (Array String) -> HH.HTML w i
renderRandomWords randomWords =
  case randomWords of
    Nothing ->
      HH.div_ []

    Just words ->
      HH.ul_ $
        map
          (\word ->
              HH.li_ [ HH.text word ]
          )
          words



getBasicWords :: forall m. MonadAff m => m (Either AX.Error BasicWords)
getBasicWords = do
  adjectives <- liftAff $ AX.get string (baseURL <> "/data/adjectives.txt")
  nouns <- liftAff $ AX.get string (baseURL <> "/data/nouns.txt")
  verbs <- liftAff $ AX.get string (baseURL <> "/data/verbs.txt")
  adverbs <- liftAff $ AX.get string (baseURL <> "/data/adverbs.txt")
  pure $ (\adj n v adv -> { adjectives : lines adj.body
                          , nouns : lines n.body
                          , verbs : lines v.body
                          , adverbs : lines adv.body })
           <$> adjectives
           <*> nouns
           <*> verbs
           <*> adverbs


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    result <- liftAff getBasicWords
    case result of
      Left _ ->
        pure unit

      Right basicWords ->
        H.modify_ _{ basicWords = Just basicWords
                   , words = Just basicWords.nouns
                   }

  Randomize -> do
     { words } <- H.get
     case words of
       Nothing ->
         pure unit
       
       Just wordsJust -> do
         randomWords <- liftEffect $ choose 31 wordsJust
         H.modify_
            _ { randomWords = Just $ randomWords
              }

  FileSelected blob -> do
     reader <- liftEffect $ FileReader.fileReader
     liftEffect $ FileReader.readAsText blob reader

     H.subscribe' \sid -> do
       HQE.eventListener
         ET.load
         (FileReader.toEventTarget reader)
         (\_ -> Just $ FileLoaded sid reader)
        
     pure unit

  FileLoaded sid reader -> do
    H.unsubscribe sid
    result <- liftEffect $ FileReader.result reader
    case Except.runExcept $ Foreign.readString result of
      Left errs ->
        liftEffect $ log $ show $ map Foreign.renderForeignError errs

      Right word -> do
        H.modify_ _ { words = Just $ lines word }
        handleAction Randomize


  SetWords value -> do
     case value of
       "nouns" ->
         H.modify_ $ \s -> s { words = (_.nouns) <$> s.basicWords  }

       "verbs" -> 
         H.modify_ $ \s -> s { words = (_.verbs) <$> s.basicWords  }

       "adjectives" ->
         H.modify_ $ \s -> s { words = (_.adjectives) <$> s.basicWords  }

       "adverbs" ->
         H.modify_ $ \s -> s { words = (_.adverbs) <$> s.basicWords  }

       "all" -> 
         pure unit

       "" ->
         pure unit

       str -> liftEffect $ log ("No value: " <> str)
      

  NoAction -> pure unit
