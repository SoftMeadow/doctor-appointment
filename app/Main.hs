module Main (main) where

import Monomer
import qualified Data.Text as T
import UI.App
import UI.Event
import UI.Handler
import UI.Model

main :: IO ()
main =
  startApp initialModel handleEvent buildUI config
  where
    config =
      [ appWindowTitle (T.pack "Запись к врачу")
      , appWindowState (MainWindowNormal (700, 990))
      , appFontDef (T.pack "Regular") (T.pack "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf")
      , appInitEvent AppInit
      ]