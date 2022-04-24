{-
Copyright © Paul Johnson 2019. See LICENSE file for details.
-}

-- |
--
-- Common types for tables and dialogs.
module Reactive.Banana.Common where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Char
import Data.Default
import Data.Maybe
import Data.Time.Calendar
import qualified Data.Colour.SRGB as C
import Data.Time.Format
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Reactive.Banana.Combinators
import Text.Read
import Reactive.Banana.Frameworks


-- | If there is data then it carries a validity flag.
data GadgetData a = GadgetData
   {
      _gdOk :: ! Bool,  -- ^ Is the value considered valid?
      _gdValue :: ! a
   } deriving (Eq, Ord, Show)

instance Functor GadgetData where
   fmap = (gdValue %~)

instance Applicative GadgetData where
   pure = GadgetData True
   (GadgetData ok1 f) <*> (GadgetData ok2 v) = GadgetData (ok1 && ok2) $ f v

instance Foldable GadgetData where
   foldMap f = f . _gdValue

instance Traversable GadgetData where
   traverse f (GadgetData b v) = GadgetData b <$> f v
   sequenceA (GadgetData b v) = GadgetData b <$> v

instance (Semigroup a) => Semigroup (GadgetData a) where
   GadgetData ok1 v1 <> GadgetData ok2 v2 = GadgetData (ok1 && ok2) (v1 <> v2)

instance (Monoid a) => Monoid (GadgetData a) where
   mempty = GadgetData True mempty

-- | Is the data considered valid?
gdOk :: Lens' (GadgetData a) Bool
gdOk = lens _gdOk $ \g ok -> g {_gdOk = ok}

gdValue :: Lens (GadgetData o1) (GadgetData o2) o1 o2
gdValue = lens _gdValue $ \g v -> g {_gdValue = v}



-- | An event and behavior for the same thing travelling together.
--
-- Law: The behavior must be a stepper for the event.
data Changes a = Changes {
      changesE :: Event a,
      changesB :: Behavior a
   }

instance Functor Changes where
   fmap f (Changes e b) = Changes (fmap f e) (fmap f b)

instance Applicative Changes where
   pure v = Changes never $ pure v
   Changes ef bf <*> Changes e b =
      let
         ef1 = (\v f -> Left (f, f v)) <$> b <@> ef
         e1 = (\f v -> Right (v, f v)) <$> bf <@> e
         simultaneous (Left (f, _)) (Right (v, _)) = Left (f, f v)
         simultaneous _ _ = error "Applicative Changes: The impossible happened."
         extract (Left (_, r)) = r
         extract (Right (_, r)) = r
      in Changes (extract <$> unionWith simultaneous ef1 e1) (bf <*> b)

instance (Num a) => Num (Changes a) where
   (+) = liftA2 (+)
   (-) = liftA2 (-)
   (*) = liftA2 (*)
   abs = fmap abs
   signum = fmap signum
   fromInteger = pure . fromInteger
   negate = fmap negate

instance (Fractional a) => Fractional (Changes a) where
   (/) = liftA2 (/)
   fromRational = pure . fromRational
   recip = fmap recip

instance (Floating a) => Floating (Changes a) where
   (**) = liftA2 (**)
   acos = fmap acos
   acosh = fmap acosh
   asin = fmap asin
   asinh = fmap asinh
   atan = fmap atan
   atanh = fmap atanh
   cos = fmap cos
   cosh = fmap cosh
   exp = fmap exp
   log = fmap log
   logBase = liftA2 logBase
   pi = pure pi
   sin = fmap sin
   sinh = fmap sinh
   sqrt = fmap sqrt


-- | Construct a Changes from an initial value and an event.
makeChanges :: (MonadMoment m) =>  a -> Event a -> m (Changes a)
makeChanges initial ev = do
   b <- stepper initial ev
   return $ Changes ev b


-- | When the event occurs the input will be sampled and an event created.
sampledBy :: Changes a -> Event b -> Changes a
sampledBy chng ev = let b = changesB chng in Changes (b <@ ev) b


-- | Combine a list of function changes.
unionsC :: [Changes (a -> a)] -> Changes (a -> a)
unionsC cs = Changes
      (unions $ map changesE cs)
      (foldr (liftA2 (.) . changesB) (pure id) cs)


-- | Filter out events that do not actually change the value.
--
-- This only filters the event: "changes" will still fire for values that are the same.
filterChanges :: (Eq a) => Changes a -> Changes a
filterChanges  = filterChangesWith (/=)


-- | Filter out events unless the old and new values return "True". Note that the behavior will
-- still continue to change even if events are filtered.
filterChangesWith :: (a -> a -> Bool) -> Changes a -> Changes a
filterChangesWith f (Changes e b) =
   let e1 = filterApply (f <$> b) e  -- Event fires before behavior change.
   in Changes e1 b


-- | The difference between the old and new value as an event.
changeDelta ::
   (a -> a -> b)   -- ^ Binary function. First argument is the old value, second is the new.
   -> Changes a
   -> Event b
changeDelta f (Changes ev b) = f <$> b <@> ev

-- | The output is the last "Just" value of the input. Inputs of "Nothing" have no effect.
-- The first argument is used if the value is initially "Nothing".
sticky :: (MonadMoment m) => a -> Changes (Maybe a) -> m (Changes a)
sticky i (Changes e b) = do
   initial <- valueB $ fromMaybe i <$> b
   let
      e2 = filterJust e
   b2 <- stepper initial e2
   return $ Changes e2 b2


-- | Add an extra valid value of \"\" for @Nothing@.
prismToMaybe :: APrism' Text a -> Prism' Text (Maybe a)
prismToMaybe prsm = prism' show1 read1
   where
      show1 Nothing = ""
      show1 (Just x) = x ^. re (clonePrism prsm)
      read1 "" = Just Nothing  -- Valid entry of Nothing.
      read1 str1 = case str1 ^? clonePrism prsm of
         Just v -> Just $ Just v  -- Valid entry of something.
         Nothing -> Nothing  -- Invalid entry.


-- | Simple text prism for data fields.
textPrism :: (Read a, Show a) => Prism' Text a
textPrism = prism' (T.pack . show) (readMaybe . T.unpack)


-- | POSIX-style date formats, as in "Date.Calendar.Format".
type DateFormat = Text

-- | E.g. \"3 January 2018\"
longDate :: DateFormat
longDate = "%-d %B %Y"

-- | E.g. \"3/1/2018\"
shortDate :: DateFormat
shortDate = "%-d/%-m/%Y"


-- | Convenience type for dealing with colours.
newtype Colour = Colour {getColour :: C.Colour Double}
   deriving (Eq, Semigroup, Monoid)

instance Show Colour where
   show = T.unpack . view (re colourPrism)

instance Default Colour where def = Colour $ C.sRGB24 128 128 128  -- Gray


-- | Name of an icon, as text.
type IconName = Text


-- | The icon called \"no-icon\" is a special case. Where the icon is optional this is used
-- for the @Nothing@ value. It contains the text "No Icon".
noIconName :: IconName
noIconName = "no-icon"

-- | The icon called \"blank-icon\" is a similar special case. It contains nothing.
blankIconName :: IconName
blankIconName = "blank-icon"


-- | Format and read the date.
--
-- The format argument is used for date → text conversion. Text → date tries the format argument
-- first. If that fails it then tries a series of built-in formats. These use the default locale
-- (i.e. USA), but assume UK ordering (i.e. dd-mm-yyyy).
--
-- This is going to require proper i18n support in the future.
datePrism :: DateFormat -> Prism' Text Day
datePrism fmt =
      prism' (T.pack . formatTime defaultTimeLocale fmt1) (readDate . T.unpack)
   where
      fmt1 = T.unpack fmt
      readDate str =
         case mapMaybe (\f -> parseTimeM True defaultTimeLocale f str) readFormats of
            (d : _) -> Just d
            [] -> Nothing
      readFormats = [
            fmt1, "%Y-%m-%d", "%-d/%-m/%Y", "%-d/%-m/%c", "%-d/%b/%Y", "%-d/%b/%c",
            "%-d %b %Y", "%-d %b %c", "%-d %B %Y", "%-d %B %c"
         ]


-- | Convert between colour and the 6 digit hex format (e.g. @#ff80ff@).
colourPrism :: Prism' Text Colour
colourPrism = prism' (T.pack . C.sRGB24show . getColour) (fmap Colour <$> readColor)
   where
      readColor str = case C.sRGB24reads $ T.unpack $ T.dropAround isSpace str of
         [(c, "")] -> Just c
         _ -> Nothing


-- | Filter out Nothing values. A node of "Nothing" will have its children pruned.
forestCatMaybe :: Forest (Maybe a) -> Forest a
forestCatMaybe = mapMaybe treeCatMaybe


-- | Filter out Nothing values. If the root contains Nothing then return Nothing.
treeCatMaybe :: Tree (Maybe a) -> Maybe (Tree a)
treeCatMaybe (Node Nothing _ ) = Nothing
treeCatMaybe (Node (Just v) cs) = Just $ Node v $ forestCatMaybe cs

-- | Left-biased utility for the union of events due to signals. Simultaneous signals should
-- never happen because it would imply that the user has clicked on two items simultaneously.
firstEvent :: [Event a] -> Event a
firstEvent [] = never
firstEvent evs = foldr1 (unionWith const) evs


-- | Convenience version of "reactimate_". The returned action is in the @IO@ monad and is
-- wrapped in @forkIO@. This avoids the risk of the application hanging if the returned action
-- is triggered during an "execute", but at the price of non-determinism.
reactimate1 :: Event (IO ()) -> MomentIO (IO ())
reactimate1 evs = do
   net <- getEventNetwork
   stop <- reactimate_ evs
   return $ void $ forkIO $ runMomentIO_ net stop


-- | Convenience version of "reactimate_'"
reactimate1' :: Event (Future (IO ())) -> MomentIO (IO ())
reactimate1' evs = do
   net <- getEventNetwork
   stop <- reactimate_' evs
   return $ void $ forkIO $ runMomentIO_ net stop
