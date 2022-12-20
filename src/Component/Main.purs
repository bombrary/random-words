module Component.Main where


import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Choose.Distinct (choose)
import Control.Monad.Except as Except
import DOM.HTML.Indexed.InputAcceptType (InputAcceptType(..), InputAcceptTypeAtom(AcceptMediaType))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.String.Utils (lines, toCharArray)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Util (split)
import Web.File.Blob (Blob)
import Web.File.Blob as Blob
import Web.File.File as File
import Web.File.FileReader (FileReader)
import Web.File.FileReader as FileReader
import Web.HTML.Event.EventTypes as ET


type State =
  { words :: Maybe (Array String)
  , randomWords :: Maybe (Array String)
  }


data Action
  = Randomize
  | FileSelected Blob
  | FileLoaded H.SubscriptionId FileReader.FileReader 
  | NoAction



component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            }
  }


initialState :: forall input. input -> State
initialState _ =
  { words: Nothing
  , randomWords: Nothing
  }


render :: forall m. State -> HH.ComponentHTML Action () m
render { randomWords } =
  HH.main
    []
    [ HH.img
        [ HP.classes [ HH.ClassName "logo" ]
        , HP.src "logo.svg"
        ]
        --map
        --  (\c -> HH.span_ [ HH.text c ])
        --  (toCharArray "Random Words")
    , HH.div
        [ HP.classes [ HH.ClassName "random-words" ]] $
        [ renderRandomWordsTable randomWords ]
    , HH.button
        [ HE.onClick (\_ -> Randomize)
        , HP.classes [ HH.ClassName "randomize-btn" ]
        ]
        [ HH.text "ランダムをもう一度。"
        ]
    , HH.input
        [ HP.type_ HP.InputFile
        , HP.accept (InputAcceptType [AcceptMediaType (MediaType "text/plain")])
        , HE.onFileUpload $
            case _ of
              [file] ->
                FileSelected (File.toBlob file)

              _ ->
                NoAction
        ]
    ]
    

renderRandomWordsTable :: forall w i. Maybe (Array String) -> HH.HTML w i
renderRandomWordsTable randomWords =
  case randomWords of
    Nothing ->
      HH.div_ []

    Just words ->
      HH.table_ $
        map
          (\rows ->
              HH.tr_ $
                map
                  (\word ->
                      HH.td_
                        [ HH.text word ]
                  )
                  rows

          )
          (split 5 words)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
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
         (\e -> Just $ FileLoaded sid reader)
        
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

  NoAction -> pure unit
