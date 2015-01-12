module Development.Duplo.Types.Options where

import           System.Console.GetOpt (ArgDescr (..), OptDescr (..))

data Options = Options { optVerbose :: Bool
                       , optVersion :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { optVerbose = False
                         , optVersion = False
                         }

options :: [ OptDescr (Options -> IO Options)
           ]
options = [ Option "v" ["version"]
                   (NoArg $ \opt -> return opt { optVersion = True })
                   "Display version"
          , Option "V" ["verbose"]
                   (NoArg $ \opt -> return opt { optVerbose = True })
                   "Run chattily"
          ]
