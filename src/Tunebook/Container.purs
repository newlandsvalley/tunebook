module Tunebook.Container where

import Prelude

import CSS.Display (display, displayNone)
import Data.Abc (AbcTune)
import Data.Abc.Metadata (getTitle)
import Data.Abc.Parser (parse)
import Data.Array (index, range, replicate)
import Data.Either (either)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_, traverseWithIndex_)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (length)
import Data.Traversable (traverse)
import Tunebook.Window (print)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import VexFlow.Abc.Alignment (justifiedScoreConfig, rightJustify)
import VexFlow.Score (Renderer, clearCanvas, createScore, renderTitledScore, initialiseCanvas, resizeCanvas) as Score
import VexFlow.Types (Config)
import Web.Event.Event as Event
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader.Aff as FileReaderAff
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement

type State =
  { titles :: Array String
  , vexRenderers :: Array Score.Renderer
  , vexRendered :: Boolean
  }

data Action =
    Init
  | HandleUploadFiles Event.Event
  | HandlePrint

-- the only reason that we need Query at all is that we need to chain
-- InitDummy followed by InitVex and this is only possible with Queries.
-- Otherwise everything would be encoded as an Action.
-- And the reason for this is that Vex requires a Div element to me rendered
-- before it can be initialised.
--
-- Rendering takes place between the two initialisations.
data Query a =
    InitQuery a
  | InitVex a

maxScores :: Int 
maxScores = 50

scale :: Number
scale = 0.8

canvasWidth :: Int
canvasWidth =
  1300

emptyTune :: AbcTune
emptyTune =
  { headers : Nil, body: Nil }

vexConfig :: Int -> Config
vexConfig index =
  { parentElementId : ("vexflow" <> show index)
  , width : canvasWidth
  , height : 10
  , scale : scale
  , isSVG : true
  , titled : true
  }

type ChildSlots :: ∀ k. Row k
type ChildSlots = ()

component :: ∀ i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialState :: i -> State
  initialState _ =
    { titles: []
    , vexRenderers: []
    , vexRendered: false
    }
 

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      -- defer to the query so we can chain them
      _ <- handleQuery (InitQuery unit)
      pure unit
    HandleUploadFiles ev -> 
      case (HTMLInputElement.fromEventTarget =<< Event.target ev) of 
        Just target -> do
          state <- H.get
          _ <- clearScores state
          titles <- handleRetrieveTitles target 
          _ <- H.modify (\st -> st { titles = titles
                                    , vexRendered = true  
                                    })
          _ <- (handleFileUpload state) target 
          pure unit
        Nothing ->
          pure unit
    HandlePrint -> do
      _ <-  H.liftEffect print
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    InitQuery next -> do
      -- this state change forces our first render
      let 
        titles = replicate maxScores ""
      _ <- H.modify (\st -> st { titles = titles } )
      handleQuery (InitVex next)
    InitVex next -> do
      -- we split initialisation into two because Vex requires a rendering step
      -- before it can be initialised
      let
        rows :: Array Int
        rows = range 0 (maxScores - 1)
      renderers <- H.liftEffect $ traverse (\r -> Score.initialiseCanvas $ vexConfig r) rows
      H.modify_ (\st -> st { vexRenderers = renderers } )
      pure (Just next)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = HH.div
    [ HP.id "tune-book" ]
    [ HH.div_       
       [ HH.h1_ 
         [ HH.text "Tunebook" ]
         , HH.div 
           [ HP.class_ (H.ClassName "instruction") ] 
           [ HH.text "Just put your ABC files in a directory somewhere, select them and print them." ]     
       ]
    , HH.div
      -- left pane
      [ HP.class_ (H.ClassName "leftPane") ]
      [
        -- load
        HH.div
         [ HP.class_ (H.ClassName "leftPanelComponent")  ]
         [ renderInputDir ]
      , HH.div
        -- print
        [ HP.class_ (H.ClassName "leftPanelComponent")]
        [ renderPrintButton state ]
      , HH.div_
        [ HH.ul_ $
          renderScores
        ]
      ]
    ]      

renderInputDir :: ∀ m
  . MonadAff m
  => H.ComponentHTML Action ChildSlots m
renderInputDir = 
  HH.span
    [ HP.class_ $ ClassName "dirInput" ]
    [ -- the label is a hack to allow styling of file input which is
      -- otherwise impossible - see https://stackoverflow.com/questions/572768/styling-an-input-type-file-button
      HH.label
           [ HP.for "file-input"
           , HP.class_ $ ClassName "hoverable fileInputLabel"
           ]
           [ HH.text "choose files" ]
      -- we set the style to display none so that the label acts as a button
    , HH.input
        [ HE.onChange HandleUploadFiles
        , HP.type_ HP.InputFile
        , HP.id  "file-input"
        , HP.enabled true
        , HP.multiple true
        , noDisplayStyle
        ]
    ]            

-- rendering functions
renderPrintButton :: ∀ m
  . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderPrintButton state =
  let
    label = "print scores"
    action =  HandlePrint
    enabled =
      state.vexRendered
    className =
      if enabled then "hoverable" else "unhoverable"
  in
    HH.button
      [ HE.onClick \_ -> action
      , HP.class_ $ ClassName className
      , HP.enabled enabled
      ]
      [ HH.text label ]

renderScores :: ∀ m
  . MonadAff m
  => Array (H.ComponentHTML Action ChildSlots m)
renderScores =
  let
    rows = range 0 (maxScores -1)
  in
    map renderScoreItem rows

renderScoreItem :: ∀ i p. Int -> HH.HTML i p
renderScoreItem idx =
  HH.li
    [ HP.class_ (H.ClassName "scoreItem") ]
    [ HH.div
      [ HP.id ("vexflow" <> show idx)
      , HP.class_ (H.ClassName "canvasDiv")
      ]
      []
    ]      

renderTuneTitle :: ∀ i p. String -> Int -> HH.HTML i p
renderTuneTitle title idx = 
  if (length title > 0) then
    HH.label 
      [ HP.for ("vexflow" <> show idx) ]
      [ HH.h2
        [HP.id "tune-title" ]
        [HH.text (show (idx + 1) <> ". " <> title) ]
      ]
  else
    HH.text ""

noDisplayStyle :: ∀ j r. HP.IProp (style :: String | r) j
noDisplayStyle =
  style do
    display displayNone

handleFileUpload :: ∀ m. MonadAff m  => State -> HTMLInputElement -> m Unit
handleFileUpload state input = do
  mFileList <- H.liftEffect $ HTMLInputElement.files input
  for_ mFileList \fileList -> 
    forWithIndex_ (toFileArray fileList) \n file -> 
      when (n < maxScores) do
        abc <- H.liftAff $ FileReaderAff.readAsText (File.toBlob file)
        let
          renderer = unsafePartial $ fromJust $ index state.vexRenderers n
          eTune = parse (abc <> "\n")
          abcTune = either (\_ -> emptyTune) (identity) eTune
          vexScore = Score.createScore (vexConfig n) abcTune
          -- right justify the score
          justifiedScore = rightJustify canvasWidth scale vexScore 
          config = justifiedScoreConfig justifiedScore (vexConfig n) 
          title = maybe "Untitled" identity $ getTitle abcTune 
        _ <- H.liftEffect $ Score.resizeCanvas renderer config
        _ <- H.liftEffect $ Score.renderTitledScore renderer title justifiedScore
        pure unit

handleRetrieveTitles :: ∀ m. MonadAff m  => HTMLInputElement -> m (Array String)
handleRetrieveTitles input = do
  mFileList <- H.liftEffect $ HTMLInputElement.files input 
  case mFileList of 
    Just fileList ->
      let 
        files = (toFileArray fileList)
      in do 
        titles <- traverse getFileTitle files
        pure titles
    Nothing ->
      pure []

  where

    getFileTitle :: MonadAff m  => File.File -> m String 
    getFileTitle file = do
      abc <- H.liftAff $ FileReaderAff.readAsText (File.toBlob file)
      let
        eTune = parse (abc <> "\n")
        mTitle = either (const Nothing) getTitle eTune
        title = maybe "" identity mTitle
      pure title   



clearScores :: ∀ m. MonadAff m  => State -> m Unit
clearScores state = do
  let
    f :: Int -> Score.Renderer -> Effect Score.Renderer
    f i renderer = Score.resizeCanvas renderer (vexConfig i)
  _ <- H.liftEffect $ traverseWithIndex_ f state.vexRenderers
  _ <- H.liftEffect $ traverse (Score.clearCanvas) state.vexRenderers      
  pure unit

-- specialize FileList.items to produce an Array of File that satisfies the tyoe checker
toFileArray :: FileList.FileList -> Array File.File
toFileArray fileList = 
  FileList.items fileList



