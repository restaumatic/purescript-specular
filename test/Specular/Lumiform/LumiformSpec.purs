module Specular.Lumiform.LumiformSpec where

import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (Unit, discard, ($))
import Specular.Dom.Element (text)
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.Lumiform.Lumiform (bell, done, output, lumiform)

-- DSL expression

main :: Effect Unit
main = do
  runMainWidgetInBody $ text "Hello World"
  log "Hello World"
  lumiform do
    subroutine
    subroutine
    subroutine
    subroutine
    bell
    done
      where
        subroutine = output "E"



