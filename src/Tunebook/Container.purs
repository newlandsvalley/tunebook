module Tunebook.Container where

import Prelude

import CSS.Display (display, displayNone)
import Data.Abc (AbcTune)
import Data.Abc.Parser (parse)
import Data.Abc.Utils (getTitle)
import Abc.EnsembleScore.Renderer (renderPolyphonicVoices) as EnsembleScore
import Data.Abc.Voice (partitionVoices)
import Data.Array (foldM, index, range, sortBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, length) as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_, traverseWithIndex_)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Tunebook.Window (print)
import VexFlow.Score (Renderer, clearCanvas, renderFinalTune, initialiseCanvas, resizeCanvas) as Score
import VexFlow.Types (Config, RenderingError)
import VexFlow.Abc.TickableContext (defaultNoteSeparation)
import Web.Event.Event as Event
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.FileReader.Aff as FileReaderAff
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HTMLInputElement

type State =
  { title :: Maybe String
  , vexRenderers :: Array Score.Renderer
  , vexRendered :: Boolean
  }

data Action
  = Init
  | HandleUploadFiles Event.Event
  | HandlePrint
  | HandleTitleInput String

-- the only reason that we need Query at all is that we need to chain
-- InitQuery followed by InitVex and this is only possible with Queries.
-- Otherwise everything would be encoded as an Action.
-- And the reason for this is that Vex requires a Div element to me rendered
-- before it can be initialised.
--
-- Rendering takes place between the two initialisations.
data Query a
  = InitQuery a
  | InitVex a

maxScores :: Int
maxScores = 50

defaultScale :: Number
defaultScale = 0.8

reducedScale :: Number
reducedScale = 0.6

canvasWidth :: Int
canvasWidth =
  1000

emptyTune :: AbcTune
emptyTune =
  { headers: Nil, body: Nil }

vexConfig :: Int -> Config
vexConfig index =
  { parentElementId: ("vexflow" <> show index)
  , width: canvasWidth
  , height: 10
  , scale: defaultScale
  , isSVG: true
  , titled: true
  , noteSeparation: defaultNoteSeparation
  , showChordSymbols: false
  }

convasWidthExceded :: String 
convasWidthExceded = "Canvas width exceded"

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
    { title: Nothing
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
          _ <- H.modify (\st -> st { vexRendered = true })
          -- this is the meat of getting all the scores
          _ <- (handleFileUpload state) target
          pure unit
        Nothing ->
          pure unit
    HandlePrint -> do
      _ <- H.liftEffect print
      pure unit
    HandleTitleInput title -> do
      _ <- H.modify (\st -> st { title = Just title })
      pure unit

  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    InitQuery next -> do
      -- this state change forces our first render
      _ <- H.modify (\st -> st { vexRendered = false })
      handleQuery (InitVex next)
    InitVex next -> do
      -- we split initialisation into two because Vex requires a rendering step
      -- before it can be initialised
      let
        rows :: Array Int
        rows = range 0 (maxScores - 1)
      renderers <- H.liftEffect $ traverse (\r -> Score.initialiseCanvas $ vexConfig r) rows
      H.modify_ (\st -> st { vexRenderers = renderers })
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
            [ HP.class_ (H.ClassName "leftPanelComponent") ]
            [ renderInputDir ]
        , HH.div
            [ HP.class_ (H.ClassName "leftPanelComponent") ]
            [ renderBookTitleInput state ]
        , HH.div
            -- print
            [ HP.class_ (H.ClassName "leftPanelComponent") ]
            [ renderPrintButton state ]
        , HH.div_
            [ renderBookTitle state
            , HH.ul_ renderScores
            ]
        ]
    ]

-- rendering functions
renderBookTitle
  :: ∀ m
   . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderBookTitle state =
  case state.title of
    Just title ->
      if state.vexRendered then
        HH.h2
          [ HP.id "book-title" ]
          [ HH.text title ]
      else
        HH.text ""
    _ ->
      HH.text ""

renderBookTitleInput
  :: ∀ m
   . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderBookTitleInput state =
  if state.vexRendered then
    HH.div
      [ HP.id "book-title-div" ]
      [ HH.label
          [ HP.class_ (H.ClassName "labelAlignment")
          , HP.for "book-title-edit"
          ]
          [ HH.text "add a title?" ]
      , HH.input
          [ HE.onValueInput HandleTitleInput
          , HP.value (maybe "" identity state.title)
          , HP.type_ HP.InputText
          , HP.id "book-title-edit"
          , HP.class_ $ ClassName "text-input"
          ]
      ]
  else
    HH.text ""

renderInputDir
  :: ∀ m
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
        , HP.id "file-input"
        , HP.enabled true
        , HP.multiple true
        , noDisplayStyle
        ]
    ]

renderPrintButton
  :: ∀ m
   . MonadAff m
  => State
  -> H.ComponentHTML Action ChildSlots m
renderPrintButton state =
  let
    label = "print scores"
    action = HandlePrint
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

renderScores
  :: ∀ m
   . MonadAff m
  => Array (H.ComponentHTML Action ChildSlots m)
renderScores =
  map renderScoreItem $ range 0 (maxScores - 1)

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

noDisplayStyle :: ∀ j r. HP.IProp (style :: String | r) j
noDisplayStyle =
  style do
    display displayNone

-- | process all the chosen files and, wherever possible, convert tha ABC 
-- | to a final score and write to the appropriate canvas Div (by side effect)
handleFileUpload :: ∀ m. MonadAff m => State -> HTMLInputElement -> m Unit
handleFileUpload state input = do
  mFileList <- H.liftEffect $ HTMLInputElement.files input
  for_ mFileList \fileList -> do
    tunes <- collectTunes fileList
    forWithIndex_ (sortTunes tunes) \n tune ->
      when (n < maxScores) do
         mError <- renderTuneAtIndex state n tune         
         _ <-  H.liftEffect $ logError mError tune
         pure unit

  where 
    logError :: Maybe RenderingError -> AbcTune -> Effect Unit 
    logError mError tune = 
      let 
        title = fromMaybe "untitled tune" (getTitle tune)
      in 
        case mError of 
          Just e ->
            log (title <> ": " <> e)
          _ ->  
            pure unit

renderTuneAtIndex :: ∀ m. MonadAff m => State -> Int -> AbcTune -> m (Maybe RenderingError)
renderTuneAtIndex state rendererIndex tune = do
  let 
    voices = partitionVoices tune
  if (NEA.length voices == 1) then     
    renderMonophonicTuneAtIndex state rendererIndex tune        
  else do 
    let 
      title = fromMaybe "untitled" $ getTitle tune
    renderPolyphonicTuneAtIndex state rendererIndex title voices 

-- try to render the tune at the appropriate renderer index.
-- if the canvas width is exceded, reduce the scale and have another go
renderMonophonicTuneAtIndex :: ∀ m. MonadAff m => State -> Int -> AbcTune -> m (Maybe RenderingError)
renderMonophonicTuneAtIndex state rendererIndex tune = do
  let
    renderer = unsafePartial $ fromJust $ index state.vexRenderers rendererIndex
    config = vexConfig rendererIndex
  mError0 <- H.liftEffect $ Score.renderFinalTune config renderer tune
  case mError0 of 
    Just "Canvas width exceded" -> do
      let  
        config' = config { scale = reducedScale }
      H.liftEffect $ Score.renderFinalTune config' renderer tune
    _ ->
      pure mError0

-- try to render the poyphonic tune at the appropriate renderer index.
-- if the rendering fails for any reason, revert to showing just the first voice
renderPolyphonicTuneAtIndex :: ∀ m. MonadAff m => State -> Int -> String -> NonEmptyArray AbcTune -> m (Maybe RenderingError)
renderPolyphonicTuneAtIndex state rendererIndex title voices = do
  let
    renderer = unsafePartial $ fromJust $ index state.vexRenderers rendererIndex
    config = (vexConfig rendererIndex) { scale = reducedScale }
  mError <- H.liftEffect $ EnsembleScore.renderPolyphonicVoices config renderer title voices
  -- fallback to rendering just the first voice
  case mError of 
    Just _ -> do 
      let
        tune = NEA.head voices 
      renderMonophonicTuneAtIndex state rendererIndex tune 
    _ -> 
      pure mError      

clearScores :: ∀ m. MonadAff m => State -> m Unit
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

-- collect all the tunes that parse, splitting any polyphonic tune 
-- into separate tunes for each voice
collectTunes :: ∀ m. MonadAff m => FileList.FileList -> m (Array AbcTune)
collectTunes fileList =
  liftAff $ foldM f [] fileArray

  where

  f :: Array AbcTune -> File.File -> Aff (Array AbcTune)
  f acc file = do
    abc <- FileReaderAff.readAsText (File.toBlob file)
    let
      eTune = parse (abc <> "\n")
    case eTune of
      Left _ ->
        pure acc
      Right tune ->
        pure ([tune] <> acc)

  fileArray :: Array File.File
  fileArray = FileList.items fileList

-- sort the tunes by title
sortTunes :: Array AbcTune -> Array AbcTune
sortTunes tunes = 
  sortBy compareTitles tunes 

  where 
  compareTitles :: AbcTune -> AbcTune -> Ordering 
  compareTitles a b = 
    compare (getTitle a) (getTitle b)

