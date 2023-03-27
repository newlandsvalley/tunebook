module Tunebook.Window (print) where

import Prelude (Unit)
import Effect (Effect)

foreign import print
  :: String -> Effect Unit
