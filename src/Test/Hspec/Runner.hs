-- |
-- Stability: provisional
module Test.Hspec.Runner (
-- * Running a spec
  hspec
, hspecResult
, hspecWith
, addSetUp

-- * Types
, Summary (..)
, Config (..)
, ColorMode (..)
, Path
, defaultConfig
, configAddFilter

-- * Internals
, hspecWithFormatter
) where

import           Control.Applicative
import qualified Control.Exception              as E
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import           System.Environment
import           System.Exit
import           System.IO

import           Control.Monad.IO.Class         (liftIO)
import           System.Console.ANSI            (hHideCursor, hShowCursor)
import           System.Random                  (newStdGen)
import qualified Test.QuickCheck                as QC

import           Test.Hspec.Config
import           Test.Hspec.Core.Type
import           Test.Hspec.FailureReport
import           Test.Hspec.Formatters
import           Test.Hspec.Formatters.Internal
import           Test.Hspec.Util

import           Test.Hspec.Options             (ColorMode (..), Options (..),
                                                 defaultOptions)
import           Test.Hspec.Runner.Eval

-- | Filter specs by given predicate.
--
-- The predicate takes a list of "describe" labels and a "requirement".
filterSpecs :: (Path -> Bool) -> [SpecTree] -> [SpecTree]
filterSpecs p = goSpecs []
  where
    goSpecs :: [String] -> [SpecTree] -> [SpecTree]
    goSpecs groups = mapMaybe (goSpec groups)

    goSpec :: [String] -> SpecTree -> Maybe SpecTree
    goSpec groups spec = case spec of
      SpecItem _ requirement _ -> guard (p (groups, requirement)) >> return spec
      SpecGroup group specs     -> case goSpecs (groups ++ [group]) specs of
        [] -> Nothing
        xs -> Just (SpecGroup group xs)

-- | Run given spec and write a report to `stdout`.
-- Exit with `exitFailure` if at least one spec item fails.
hspec :: Spec -> IO ()
hspec = hspecWithOptions defaultOptions

-- | Add custom action before every test runs.
addSetUp :: IO () -> Spec -> Spec
addSetUp action spec = fromSpecList $ (map addActionToSpecItem (runSpecM spec))
  where addActionToSpecItem :: SpecTree -> SpecTree
        addActionToSpecItem (SpecGroup s l) = SpecGroup s (map addActionToSpecItem l)
        addActionToSpecItem (SpecItem b s f) = SpecItem b s (\params -> (action >> (f params)))

-- | This function is used by @hspec-discover@.  It is not part of the public
-- API and may change at any time.
hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWithOptions defaultOptions {optionsFormatter = f} spec

-- Add a StdGen to configQuickCheckArgs if there is none.  That way the same
-- seed is used for all properties.  This helps with --seed and --rerun.
ensureStdGen :: Config -> IO Config
ensureStdGen c = case QC.replay qcArgs of
  Nothing -> do
    stdGen <- newStdGen
    return c {configQuickCheckArgs = qcArgs {QC.replay = Just (stdGen, 0)}}
  _       -> return c
  where
    qcArgs = configQuickCheckArgs c

-- | Run given spec with custom options.
-- This is similar to `hspec`, but more flexible.
hspecWithOptions :: Options -> Spec -> IO ()
hspecWithOptions opts spec = do
  prog <- getProgName
  args <- getArgs
  c <- getConfig opts prog args
  withArgs [] {- do not leak command-line arguments to examples -} $ do
    r <- hspecWith c spec
    unless (summaryFailures r == 0) exitFailure

-- | Run given spec and returns a summary of the test run.
--
-- /Note/: `hspecResult` does not exit with `exitFailure` on failing spec
-- items.  If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecResult :: Spec -> IO Summary
hspecResult = hspecWith defaultConfig

-- | Run given spec with custom options and returns a summary of the test run.
--
-- /Note/: `hspecWith` does not exit with `exitFailure` on failing spec
-- items.  If you need this, you have to check the `Summary` yourself and act
-- accordingly.
hspecWith :: Config -> Spec -> IO Summary
hspecWith c_ spec = withHandle c_ $ \h -> do
  c <- ensureStdGen c_
  let formatter = configFormatter c
      seed = (stdGenToInteger . fst . fromJust . QC.replay . configQuickCheckArgs) c

  useColor <- doesUseColor h c

  withHiddenCursor useColor h $
    runFormatM useColor (configHtmlOutput c) (configPrintCpuTime c) seed h $ do
      runFormatter useColor h c formatter (maybe id filterSpecs (configFilterPredicate c) $ runSpecM spec) `finally_` do
        failedFormatter formatter

      footerFormatter formatter

      -- dump failure report
      xs <- map failureRecordPath <$> getFailMessages
      liftIO $ writeFailureReport FailureReport {
          failureReportSeed = seed
        , failureReportMaxSuccess = QC.maxSuccess (configQuickCheckArgs c)
        , failureReportPaths = xs
        }

      Summary <$> getTotalCount <*> getFailCount
  where
    withHiddenCursor :: Bool -> Handle -> IO a -> IO a
    withHiddenCursor useColor h
      | useColor  = E.bracket_ (hHideCursor h) (hShowCursor h)
      | otherwise = id

    doesUseColor :: Handle -> Config -> IO Bool
    doesUseColor h c = case configColorMode c of
      ColorAuto  -> hIsTerminalDevice h
      ColorNever -> return False
      ColorAlways -> return True

    withHandle :: Config -> (Handle -> IO a) -> IO a
    withHandle c action = case configHandle c of
      Left h -> action h
      Right path -> withFile path WriteMode action

-- | Summary of a test run.
data Summary = Summary {
  summaryExamples :: Int
, summaryFailures :: Int
} deriving (Eq, Show)

instance Monoid Summary where
  mempty = Summary 0 0
  (Summary x1 x2) `mappend` (Summary y1 y2) = Summary (x1 + y1) (x2 + y2)
