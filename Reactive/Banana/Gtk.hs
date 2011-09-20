{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, FlexibleInstances,
             StandaloneDeriving #-}

module Reactive.Banana.Gtk (
  AttrOp'(..), eventM, event0, event1, event2, event3,
  behavior, sink, reactimateEventM
) where

import Control.Monad.Reader
import Data.Typeable
import Foreign.Ptr
import Reactive.Banana
import System.Glib.Attributes
import System.Glib.Signals
import System.IO.Unsafe

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.General.Enums as Gtk

deriving instance Typeable Gtk.EAny
deriving instance Typeable Gtk.EKey
deriving instance Typeable Gtk.EButton
deriving instance Typeable Gtk.EScroll
deriving instance Typeable Gtk.EMotion
deriving instance Typeable Gtk.EExpose
deriving instance Typeable Gtk.EVisibility
deriving instance Typeable Gtk.ECrossing
deriving instance Typeable Gtk.EFocus
deriving instance Typeable Gtk.EConfigure
deriving instance Typeable Gtk.EProperty
deriving instance Typeable Gtk.EProximity
deriving instance Typeable Gtk.EWindowState
deriving instance Typeable Gtk.EOwnerChange
deriving instance Typeable Gtk.EGrabBroken

instance Typeable (Gtk.EventM t a) where
  typeOf event = unsafePerformIO $ runReaderT (typeOf <$> ask) event

type EventBuilder self callback a =
  self -> Signal self callback -> NetworkDescription (Event a)

eventM :: Typeable a => EventBuilder self (Gtk.EventM a Bool) (Ptr a)
eventM self signal = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ on self signal $ Gtk.tryEvent $ ask >>= liftIO . runHandlers
  fromAddHandler addHandler

eventN :: Typeable a
       => ((a -> IO ()) -> callback)
       -> EventBuilder self callback a
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
