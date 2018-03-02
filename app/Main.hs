{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Applicative (pure)
import           Control.Exception
import           Control.Monad (unless)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (asum)
import           Data.Functor ((<$>))
import qualified Data.IntMap.Strict as Map
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Generic as VG
import qualified Options.Applicative as O
import           Profiling.GHC.Aeson
import           System.Exit
import           System.IO

data Opts = Opts
  { profileFile :: !(Maybe FilePath)
  , outputFile :: !(Maybe FilePath)
  , reportType :: !ReportType
  , continueOnMalformed :: !Bool
  }

data ReportType =
    Allocations
  | Entries
  | Ticks

parseOpts :: O.Parser Opts
parseOpts = Opts
  <$> O.optional
        ( O.strOption
          ( O.metavar "PROF_FILE"
         <> O.long "profile-file"
         <> O.help "File containing the JSON-formatted profile. stdin used if not set." ) )
  <*> O.optional
        ( O.strOption
          ( O.metavar "OUTPUT_FILE"
         <> O.long "output-file"
         <> O.help "File to write flamegraph stacks to. stdout used if not set." ))
  <*> parseReportType
  <*> O.switch
        ( O.long "continue-on-malformed"
       <> O.help "Continue if possible even if profile file looks fishy. Quits otherwise" )
  where
    parseReportType :: O.Parser ReportType
    parseReportType = asum
      [ O.flag' Allocations $ O.long "allocations" <> O.help "Uses allocation measurements as the metric."
      , O.flag' Entries $ O.long "entries" <> O.help "Uses number of entries as the metric."
      , O.flag Ticks Ticks $ O.long "ticks" <> O.help "Uses number of ticks (time) as the metric."
      ]

renderProfile :: Opts -> GhcProfile -> Handle -> IO ()
renderProfile opts gProf hOut = processCcs Seq.empty (_gp_profile gProf)
  where
    -- Map containing actual information about the cost centre stacks.
    -- Unlike in regular profiles, this is not duplicated in the
    -- information, the stacks simply refer to the information by ID.
    ccsMap :: Map.IntMap CostCentre
    ccsMap = VG.foldl'
      (\m c -> Map.insert (_cc_id c) c m) Map.empty (_gp_cost_centres gProf)

    -- Renders the actual cost centre stacks.
    processCcs :: Seq.Seq T.Text -> CostCentreStack -> IO ()
    processCcs parentStack ccs = case Map.lookup (_ccs_id ccs) ccsMap of
      Nothing -> do
        hPutStrLn stderr $ unwords
          [ "Cost centre stack found with ID"
          , show (_ccs_id ccs)
          , "not present in cost centre map, check that the profile is valid."
          , "Skipping the stack and its children."
          ]
        unless (continueOnMalformed opts) $ do
          hPutStrLn stderr $  Prelude.unwords
            [ "Quiting due to malformed profile."
            , "Set --continue-on-malfored if you'd rather continue." ]
          exitFailure
      Just cc -> do
        -- Add current level to the stack.
        let currentStack = parentStack Seq.|>
              -- Join module with the label.
              T.concat [_cc_module cc, T.singleton '.',  _cc_label cc]
            outputValue = T.pack . show $ case reportType opts of
              Allocations -> _ccs_alloc ccs
              Entries -> _ccs_entries ccs
              Ticks -> _ccs_ticks ccs
        -- Output each layer of the stack, semi-colon separated.
        case Seq.viewl currentStack of
          -- We have just inserted into this so this case is actually
          -- not reachable. But that's okay.
          Seq.EmptyL -> pure ()
          -- Print the very first entry in the stack as-is then print
          -- the rest separated by a semi-colon.
          front Seq.:< rest -> do
            T.hPutStr hOut front
            mapM_ (\s -> System.IO.hPutChar hOut ';' >> T.hPutStr hOut s) rest
            -- Once we have printed the labels, just output a space
            -- followed by a value the user is interested in
            -- measuring.
            System.IO.hPutChar hOut ' '
            T.hPutStrLn hOut outputValue
        -- Lastly output the data for all the children of this cost
        -- centre stack.
        VG.mapM_ (processCcs currentStack) (_ccs_children ccs)

main :: IO ()
main = O.execParser parser >>= \opts -> do
  !ghcProf <- try (parseProfile opts) >>= \case
    Left (e :: IOException) -> hPutStrLn stderr (show e) >> exitFailure
    Right ghcProf -> pure ghcProf
  -- aeson doesn't stream so we only open output once we have parsed
  -- everything
  let openOutput = case outputFile opts of
        Nothing -> pure stdout
        Just fp -> openFile fp WriteMode
  bracket openOutput hClose (renderProfile opts ghcProf)
  where
    parseProfile :: Opts -> IO GhcProfile
    parseProfile opts = do
      let getBsl = case profileFile opts of
            Nothing -> BSL.hGetContents stdin
            Just fp -> BSL.readFile fp
      A.eitherDecode' <$> getBsl >>= \case
        Left err -> hPutStrLn stderr err >> exitFailure
        Right v -> pure v

    parser = O.info (O.helper <*> parseOpts) O.fullDesc
