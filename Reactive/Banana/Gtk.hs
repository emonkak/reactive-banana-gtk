{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.Gtk (
  AttrOp'(..), eventM, event0, event1, event2, event3,
  behavior, sink, reactimateEventM
) where

import Control.Monad.Reader
import Foreign.Ptr
import Reactive.Banana
import System.Glib.Attributes
import System.Glib.Signals

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.General.Enums as Gtk

type EventBuilder self callback a =
  self -> Signal self callback -> NetworkDescription (Event a)

eventM :: EventBuilder self (Gtk.EventM a Bool) (Ptr a)
eventM self signal = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ on self signal $ Gtk.tryEvent $ ask >>= liftIO . runHandlers
  fromAddHandler addHandler

eventN :: ((a -> IO ()) -> callback) -> EventBuilder self callback a
eventN f self signal = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ on self signal $ f runHandlers
  fromAddHandler addHandler

event0 :: EventBuilder self (IO ()) ()
event0 = eventN ($ ())

event1 :: (Typeable a) => EventBuilder self (a -> IO ()) a
event1 = eventN id

event2 :: (Typeable a, Typeable b)
       => EventBuilder self (a -> b -> IO ()) (a, b)
event2 = eventN curry

event3 :: (Typeable a, Typeable b, Typeable c)
       => EventBuilder self (a -> b -> c -> IO ()) (a, b, c)
event3 = eventN $ \f a b c -> f (a, b, c)

behavior :: self -> Attr self a -> NetworkDescription (Behavior a)
behavior widget attr = fromPoll $ liftIO $ get widget attr

data AttrOp' o = forall a. (Attr o a) :== Discrete a

infixr 0 :==

sink :: self -> [AttrOp' self] -> NetworkDescription ()
sink self props = mapM_ sink1 props
  where
    sink1 (attr :== x) = do
      liftIOLater $ set self [attr := initial x]
      reactimate $ (\x -> set self [attr := x]) <$> changes x

reactimateEventM :: Event (Ptr a) -> Gtk.EventM a () -> NetworkDescription ()
reactimateEventM event reader = reactimate $ (<$> event) $ runReaderT reader
