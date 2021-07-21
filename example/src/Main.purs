module Example.Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

import Editor.Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  let
    initialAbc = Nothing
  traverse_ (runUI Container.component { initialAbc }) =<< HA.selectElement (QuerySelector "#embed-ps-div")
