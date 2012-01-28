module Main where

import Data.Maybe (fromJust)
import Data.Map (fromList)
import Control.Monad.State

import Arsenic.Client
import Arsenic.Plugin
import Arsenic.Plugins.System
import Arsenic.Plugins.DAmn
import Arsenic.Plugins.Arsenic
import System.IO

main = do hSetBuffering stdout NoBuffering
          set <- loadSettings
          cli <- case set of
                   Nothing -> do putStrLn "** No settings file found."
                                 putStrLn "** Please configure your bot with\
                                          \ the following information:"
                                 s <- runSetup
                                 saveSettings s
                                 makeClient s
                   Just s  -> makeClient s 
          evalStateT (cLoadPlugins >> startUp) cli
          return ()

cLoadPlugins :: ClientIO Client
cLoadPlugins =
    do -- plugs <- liftIO loadAllPlugins
       let plugs = [ Plugin arsenicPlugin
                   , Plugin systemPlugin
                   , Plugin dAmnPlugin
                   ]
       initAllPlugins plugs
