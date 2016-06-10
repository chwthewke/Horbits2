module Main where

import Control.Monad.IO.Class
import Graphics.UI.Gtk

import Lib

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  _ <- set window [ windowTitle := mainWindowTitle ]
  widgetShowAll window
  _ <- window `on` objectDestroy $ mainQuit
  mainGUI

