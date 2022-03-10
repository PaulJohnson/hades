{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

This program provides a stress-test for the DSM. It builds a model file containing many
entities with a suitable set of random relationships between them.
-}


module Main where

import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Bits
import qualified  Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.UUID.Generate
import Data.Vector (Vector)
import qualified Data.Vector as V
import DSM.SafetyCase
import Evidence.Model
import GSN.Model
import Model.Abstract.PackageTree
import qualified Model.Reflection.NamedRelation as NR
import Model.Reflection.Types
import QuickCheck.GenT
import Test.QuickCheck (generate)
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe

gsnSize :: Int
gsnSize = 1000


-- | For argument @x>0@, returns @n@ such that @2^n <= x < x^(n+1)@. For @x <= 0@ returns @0@.
intLog2 :: Int -> Int
intLog2 x
   | x > 0  = length $ takeWhile (> 1) $ iterate (`unsafeShiftR` 1) x
   | otherwise = 0


-- | For argument @x>=0@, returns @n@ such that @n^2 <= x < (n+1)^2@.
intSqrt :: Int -> Int
intSqrt n
   | n < 0 = error "intSqrt works for only nonnegative inputs"
   | n < 2 = n
   | otherwise =
      let
         smallCandidate = intSqrt(n `shiftR` 2) `shiftL` 1
         largeCandidate = smallCandidate + 1
      in if largeCandidate*largeCandidate > n then smallCandidate else largeCandidate


-- Select an item from a vector in constant time.
vectorElem :: (Monad m) => Vector a -> GenT m a
vectorElem v = do
   i <- choose (0, V.length v - 1)
   return $ v V.! i


-- | Seemingly constant list of words. Actually uses @unsafePerformIO@ to read
-- @/usr/share/dict/words@
dictionary :: Vector Text
dictionary = unsafePerformIO $
   V.fromList . T.lines <$> T.readFile "/usr/share/dict/words"
{-# NOINLINE dictionary #-}


-- | A string of words starting with a capital letter and ending with a full stop.
arbitrarySentence :: (Monad m) => GenT m Text
arbitrarySentence = do
   n <- choose (2, 8)
   firstWord <- T.toTitle <$> vectorElem dictionary
   T.intercalate " " . (firstWord :) <$> replicateM n (vectorElem dictionary)

-- | A string of gibberish sentences.
arbitraryParagraph :: (Monad m) => GenT m Text
arbitraryParagraph = do
   n <- choose (1,5)
   T.intercalate ". " <$> replicateM n arbitrarySentence


type SafetyEdit = ModelEdit SafetyModel SafetyModel


-- | Generate a list of evidence items in one large package.
generateEvidencePackage :: Text -> GenT SafetyEdit ModelId
generateEvidencePackage nm = sized $ \size -> lift $ do
   pkg <- mkPackage $ Name nm
   goToEntity pkg
   forM_ [1 .. size `div` 5 + 1] $ \n ->
      mkEntity $ Evidence (Name $ "Evidence " <> T.pack (show n)) ^. re _Evidence
   return pkg


-- | Generates a goal tree within a package. Bottom level goals depend on a mixture of solutions
-- and goals taken from the list of subgoals.
generateGsnPackage ::
   Text   -- ^ Base name path for this package. E.g. @\"1.2.3\"@.
   -> [ModelId]   -- ^ Goals on which this model may depend.
   -> ModelId    -- ^ Evidence package. This must contain a flat list of evidence and nothing else.
   -> GenT SafetyEdit ModelId
generateGsnPackage baseName subGoals evidence = sized $ \size -> do
   pkg <- lift $ mkPackage $ Name $ "Goal " <> baseName
   lift $ goToEntity pkg
   arrows <- lift $ mkPackage $ Name "Arrows"
   size2 <- choose (size `div` 2, size)
   goalss <- forM [0 .. intLog2 size2] $ \l ->
      forM [1 :: Int .. 2^l] $ \_ -> do
         desc <- arbitraryParagraph
         newNode <- (gsnNodeText .~ desc) <$> frequency [(5, lift createGoal), (1, lift createStrategy)]
         lift $ entityId <$> mkEntity (newNode ^. re (_Gsn . _GsnNodeItem))
   bottomLevel <- if null subGoals
      then do
         evidenceList <- lift $ fromHere $ do
            goToEntity evidence
            M.elems <$> currentChildren
         forM [0 .. size2 `div` 4] $ \_ -> do
            desc <- arbitrarySentence
            newNode <- (gsnNodeText .~ desc) <$> lift createSolution
            sn <- lift $ mkEntity (newNode ^. re (_Gsn . _GsnNodeItem))
            traceCount <- choose (1,3 :: Int)
            forM_ [1..traceCount] $ \_ -> do
               ev <- elements evidenceList
               lift $ fromHere $ do
                  goToEntity arrows
                  tName <- modelNewName $ Name "Trace "
                  tr <- mkEntity $ Trace tName ^. re _Trace
                  modifyRelations $ NR.insert (entityId tr) (entityId sn) traceTailRelation
                  modifyRelations $ NR.insert (entityId tr) ev traceHeadRelation
            return $ entityId sn
      else return subGoals
   lift $ goToEntity arrows
   let levelList = goalss ++ [bottomLevel]
   forM_ (zip levelList (tail levelList)) $ \(l1, l2) ->
      forM_ [1 .. (length l1 + length l2) * 2] $ \_ -> do
         nm <- lift $ modelNewName $ Name "Support "
         e <- lift $ mkEntity $ GsnEdge SupportArrow nm CardinalityOne ^. re (_Gsn . _GsnEdgeItem)
         g1 <- elements l1
         g2 <- elements l2
         lift $ modifyRelations $ NR.insert (entityId e) g1 NR.edgeFromRelation
         lift $ modifyRelations $ NR.insert (entityId e) g2 NR.edgeToRelation
   return $ head $ head goalss  -- Guaranteed non-empty even if size < 1


-- | Generate a hierarchy of GSN packages.
generateBigGsn ::
   Text  -- ^ Top level package name.
   -> ModelId  -- ^ Evidence package.
   -> GenT SafetyEdit ()
generateBigGsn nm evidence = scale (`div` unitSize) $ sized $ \size -> do
      pkg <- lift $ mkPackage $ Name nm
      let
         size2 = intSqrt size
         size3 = size `div` size2
      level3 <- forM [1..size2] $ \n1 -> forM [1..size3] $ \n2 -> do
         lift $ goToEntity pkg
         generateGsnPackage (T.pack $ "Goal " <> show n1 <> "." <> show n2) [] evidence
      level2 <- forM (zip level3 [2 :: Int ..]) $ \(deps, n) -> do
         lift $ goToEntity pkg
         generateGsnPackage (T.pack $ "Goal " <> show n) deps evidence
      lift $ goToEntity pkg
      void $ generateGsnPackage "Goal 1" level2 evidence
   where
      unitSize = 10


generateModel :: GenT SafetyEdit ()
generateModel = do
   lift goToRoot
   evidence <- generateEvidencePackage "Evidence"
   lift goToRoot
   generateBigGsn "Argument" evidence


main :: IO ()
main = do
   args <- getArgs
   prog <- getProgName
   g <- newGenerator  -- UUIDs are non-deterministic, so repeated runs will not produce collisions.
   script <- generate $ runGenT $ scale (const gsnSize) generateModel
      -- Would prefer deterministic, but QuickCheck does not support that.
   let st = modelStartState g $ emptyModel "Stress Test"
   case execModelEdit id st script of
      Left err -> hPutStrLn stderr $ "Model generation script error: " <> show err
      Right (mdl, _) -> do
         h <- case args of
               [] -> return stdout
               [fname] -> openFile fname WriteMode
               _ -> die $ "Usage: " <> prog <> " [outfile]"
         BL.hPut h $ encode mdl
         hClose h
