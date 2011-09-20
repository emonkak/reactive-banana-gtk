module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Time
import Reactive.Banana
import Reactive.Banana.Gtk
import System.Environment
import System.Glib.Attributes
import System.Glib.Signals

import qualified Graphics.UI.Gtk as Gtk

infixl 3 <+>
(<+>) = union

timer :: Int -> NetworkDescription (Event ())
timer sec = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ forkIO $ forever $ threadDelay (sec * 10 ^ 6) >> runHandlers ()
  fromAddHandler addHandler

bigLabel :: IO Gtk.Label
bigLabel = do
  label <- Gtk.labelNew $ Nothing
  font  <- Gtk.fontDescriptionNew
  Gtk.fontDescriptionSetSize font 72
  Gtk.widgetModifyFont label $ Just font
  return label

showTime :: Integral a => a -> String
showTime = show . timeToTimeOfDay . secondsToDiffTime . fromIntegral

main :: IO ()
main = do
  Gtk.initGUI

  startTime <- maybe 180 read . listToMaybe <$> getArgs

  window  <- Gtk.windowNew
  label   <- bigLabel
  vbox    <- Gtk.vBoxNew False 0
  hbox    <- Gtk.hButtonBoxNew
  start   <- Gtk.toggleButtonNewWithLabel "Start"
  reset   <- Gtk.buttonNewWithLabel "Reset"

  set hbox [ Gtk.containerChild := start
           , Gtk.containerChild := reset
           , Gtk.buttonBoxLayoutStyle := Gtk.ButtonboxCenter
           ]
  set vbox [ Gtk.containerChild := label
           , Gtk.containerChild := hbox
           ]
  set window [ Gtk.containerChild := vbox
             , Gtk.windowResizable := False
             , Gtk.containerBorderWidth := 8
             ]

  network <- compile $ do
    eDelete <- eventM window Gtk.deleteEvent
    eReset  <- eventM reset Gtk.buttonReleaseEvent
    eStart  <- event0 start Gtk.toggled
    eTick   <- timer 1

    bState <- behavior start Gtk.toggleButtonActive

    let eElapse = filterApply (const . (> 0) <$> value counter) $
                  filterApply (const <$> bState) eTick
        eCount  = subtract 1 <$ eElapse <+>
                  const (initial counter) <$ eReset
        counter = accumD startTime eCount

    sink label [Gtk.labelLabel :== showTime <$> counter]

    reactimate $ Gtk.mainQuit <$ eDelete
    reactimate $ Gtk.buttonReleased reset <$ eReset

  actuate network

  Gtk.widgetShowAll window
  Gtk.mainGUI
