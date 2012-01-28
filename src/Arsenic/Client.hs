{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}

-- Arsenic's client, and related code
module Arsenic.Client (
    module Control.Monad.State,
    module Arsenic.Misc,
    module Arsenic.Network,
    -- * The client's basic state
    Client(..),
    ClientIO,
    makeClient,
    network,
    -- ** Loading, saving, and modifying client settings
    --Settings(..),
    loadSettings,
    saveSettings,
    cLoadSettings,
    cSaveSettings,
    runSetup,
    getSetting,
    getSettingStr,
    putSetting,
    putSettingStr,
    withSettings,
    setNamespace,
    setNsList,
    -- ** Running the client
    getAuthToken,
    reconnectClient,
    quitClient,
    -- * Printing and logging console messages
    CliPrinter(..),
    putMsg,
    putNotice,
    putErr,
    logMsg,
    -- ** Default implementation
    putMsg',
    putNotice',
    putErr',
    logMsg',
    -- * Formatting and deforming channels
    deformChan,
    formatChan,
    deformList,
    formatList
    ) where

import Control.Monad.State
import Control.Monad.Error
import System.IO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Char (toLower, toUpper)
import Data.Maybe (fromJust)
import Data.Either (either)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Network.HTTP.Base (urlDecode)
import Network.HTTP.Enumerator
import Data.List
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import System.Directory (getDirectoryContents)
import qualified Data.ConfigFile as CF

import Arsenic.Misc
import Arsenic.Types
import Arsenic.Network

instance DAmnNetwork ClientIO where
    netRead = gets cNetwork >>= liftIO . evalStateT netRead
    netSend str = gets cNetwork >>= liftIO . evalStateT (netSend str)

instance Error SettingsError where
    strMsg = SettingsError

-- | Retrieve a setting from the settings file with @String@s.
getSettingStr :: String   -- ^ The name of the setting.
              -> SettingsContainer String -- ^ The value of the setting.
getSettingStr name =
    let (section, option) = settingsVarName name 
    in do cp <- lift (gets sfParser)
          either settingsErrStr return
            $ CF.get cp section option

-- | Add a setting to the settings file with @String@s.
putSettingStr :: String   -- ^ The name of the setting.
              -> String   -- ^ The value of the setting.
              -> SettingsContainer () 
putSettingStr name val =
    let (section, option) = settingsVarName name
    in do cp <- lift (gets sfParser)
          either settingsErrStr (\cp' -> lift (modify $ \s -> s {sfParser=cp'}))
            $ CF.set cp section option val

-- | Add a section to the settings file using a @String@.
addSectionStr :: String  -- ^ The name of the new section.
              -> SettingsContainer ()
addSectionStr section =
    do cp <- lift (gets sfParser)
       either settingsErrStr (\cp' -> lift (modify $ \s -> s {sfParser=cp'}))
         $ CF.add_section cp section

-- | Add a section to the settings file using a @ByteString@.
addSection :: ByteString  -- ^ The name of the new section
              -> SettingsContainer ()
addSection = addSectionStr . L.unpack

-- | Retrieve a setting from the settings file with @ByteString@s.
getSetting :: ByteString   -- ^ The name of the setting.
           -> SettingsContainer ByteString -- ^ The value of the setting.
getSetting = liftM L.pack . getSettingStr . L.unpack

-- | Add a setting to the settings file with @ByeString@s.
putSetting :: ByteString   -- ^ The name of the setting.
           -> ByteString   -- ^ The value of the setting.
           -> SettingsContainer () 
putSetting name val = putSettingStr (L.unpack name) (L.unpack val)

settingsVarName :: String -> (String, String)
settingsVarName name = fmap (drop 1) $ break (=='.') name

settingsErrStr :: CF.CPError -> SettingsContainer a
settingsErrStr (errType, location) = throwError . SettingsError $ show errType ++ ": " ++ location

-- | Perform operations on the @SettingsFile@ in the @Client@ datatype.
withSettings :: SettingsContainer a -> ClientIO a
withSettings sc =
    do (err, s) <- gets cSettings >>= return . runState (runErrorT sc)
       case err of
         Left (SettingsError e) -> do liftIO $ error e
         Right result -> do modify $ \c -> c {cSettings = s}
                            return result

-- | Print a message to the screen and log it.
putMsg', putMsg :: ByteString
                -- ^ The message to print.
                -> ByteString
                -- ^ The namespace of the message. Leave empty for global.
                -> ClientIO ()
putMsg' msg ns = 
     -- We use a timestamp in the messages we print, so we grab the timestamp in
     -- strftime() format from the settings, the timezone, and the epoch time. 
     do ts <- liftM L.unpack $ withSettings (getSetting "bot.timezone") 
        timezone <- liftIO getCurrentTimeZone
        utc <- liftIO getCurrentTime
        let localTime = utcToLocalTime timezone utc
        let output = (L.pack $ formatTime defaultTimeLocale ts localTime)
                 <+> (if L.null ns
                        then " :: "
                        else " :: [" <+> ns <+> "] ") <+> msg
        liftIO $ L.putStrLn output
        logMsg msg $ if L.null ns then "Global" else ns
putMsg msg ns = printer cliPrintMsg >>= \p -> p msg ns

-- | Log a message.
logMsg', logMsg :: ByteString
                -- ^ The message to log.
                -> ByteString
                -- ^ The namespace of the message. Leave empty for global.
                -> ClientIO ()
logMsg' msg ns =
     -- We use a timestamp in the messages we print, so we grab the timestamp in
     -- strftime() format from the settings, the timezone, and the epoch time. 
     do ts <- liftM L.unpack $ withSettings (getSetting "bot.timezone") 
        timezone <- liftIO getCurrentTimeZone
        utc <- liftIO getCurrentTime
        let localTime = utcToLocalTime timezone utc
        let output = (L.pack $ formatTime defaultTimeLocale ts localTime)
                  <+> (if L.null ns
                         then " :: "
                         else " :: [" <+> ns <+> "] ") <+> msg <+> "\n"
            date = formatTime defaultTimeLocale "%b-%d-%y" localTime
            when' cond action = liftIO $ when cond action
        -- logs are saved at ~/.arsenic/logs/<current date>/<value of ns>.txt
        dateDir <- liftIO . confDir' $ "logs/"++date++"/"
        liftIO $ createDirectoryIfMissing True dateDir
        liftIO . reportIOError ("Could not log text for "++(L.unpack ns)++": ")
               $ L.appendFile (dateDir++(L.unpack ns)++".txt") output
logMsg msg ns = printer cliLogMsg >>= \p -> p msg ns

-- | Print a notice to the screen and log it.
putNotice', putNotice :: ByteString -> ByteString -> ClientIO ()
putNotice' msg ns = putMsg ("** "<+>msg) ns
putNotice msg ns = printer cliPrintNotice >>= \p -> p msg ns

-- | Print an error to the screen and log it.
putErr', putErr :: ByteString -> ByteString -> ClientIO ()
putErr' msg ns = putMsg (">> "<+>msg) ns
putErr msg ns = printer cliPrintErr >>= \p -> p msg ns

-- | Ask for someting from the @CliPrinter@ in a "ClientIO" monad.
printer :: (CliPrinter -> a) -> ClientIO a
printer p = gets (p . cPrinter)

-- | Ask for something from the @Network@ in a "ClientIO" monad.
network :: (Network -> a) -- ^ The function to grab a setting.
        -> ClientIO a     -- ^ The setting grabbed.
network n = gets (n . cNetwork)

-- | Edit the namespace list of the client.
setNsList :: NamespaceList -> ClientIO ()
setNsList n = modify (\cli -> cli {cNetwork=(cNetwork cli) {nsList=n}})

-- | Edit an entry in the namespace list of the client.
setNamespace :: ByteString -> Namespace -> ClientIO ()
setNamespace ns namespace =
    do nsList' <- network nsList
       modify $
         \cli -> cli {cNetwork=(cNetwork cli) {nsList=M.insert ns namespace nsList'}}

-- | Load the client's settings.
loadSettings :: IO (Maybe SettingsFile) -- ^ If found, the client's settings.
loadSettings =
    do settingsHs <- confDir' "settings.hs"
       itDoes <- doesFileExist settingsHs
       if itDoes
          then do s <- reportIOError "Failed to read settings: "
                     $ readFile settingsHs
                  either (\_ -> return Nothing) (return . Just)
                     $ do cp <- CF.readstring CF.emptyCP s
                          return $ SettingsFile {sfName="settings.hs", sfParser=cp}
          else return Nothing

-- | Save the client's settings.
saveSettings :: SettingsFile -- ^ The client's settings.
             -> IO ()
saveSettings s = 
    do settingsHs <- confDir' "settings.hs"
       confDir >>= createDirectoryIfMissing True
       reportIOError "Failed to save settings: "
         $ writeFile settingsHs (CF.to_string $ sfParser s)

-- | Load settings in a ClientIO monad.
cLoadSettings :: ClientIO (Maybe SettingsFile)
cLoadSettings = liftIO loadSettings

-- | Save the settings from the environment.
cSaveSettings :: ClientIO ()
cSaveSettings = gets cSettings >>= liftIO . saveSettings

-- | Quit the client.
quitClient :: ClientIO ()
quitClient = modify (\cli -> cli {cQuit = True})

-- | Restart the client.
reconnectClient :: ClientIO ()
reconnectClient = modify (\cli -> cli {cReconnect = True})

-- | Have the user enter settings and return them in a 'Settings' datatype.
runSetup :: IO SettingsFile -- ^ The settings set by the user.
runSetup =
    do username <- getInput "Username: "
       password <- getInput "Password: "
       owner <- getInput "Bot owner (your dA username): "
       trigger  <- getInput "Trigger: "
       autoJoin <- liftM words $ getInput "Channels to join on startup \
                                          \(seperate with spaces): "
       logging <- askPrompt "Do you want the bot to log messages?" DefaultYes
       setTs <- askPrompt "Do you want to set a timestamp in strftime\
                          \ format?\nIf you don't know, pick \"no\". " DefaultNo
       logTs <- if setTs
                   then getInputDef "Timestamp (default %H:%M:%S)" "%H:%M:%S"
                   else return "%H:%M:%S"
       setServerInfo <- askPrompt "Would you like to pick a server to connect\
                                  \ to? If you don't know, pick \"no\"." DefaultNo
       {--let settings = Settings { sUsername=L.pack username
                               , sPassword=L.pack password
                               , sToken=Nothing
                               , sTrigger=L.pack trigger
                               , sAutoJoin=map L.pack autoJoin
                               , sOwner=L.pack owner
                               , sLogTs=logTs
                               , sLogging=logging
                               , sUsers=M.singleton (L.pack owner) 100
                               , sGroups=M.fromList
                                        $ [ group 100 "Owners"
                                          , group 0 "Guests" ]
                               , sAbout=aboutString
                               , sDomain=undefined -- undefined settings will
                               , sPort=undefined   -- be given to us later
                               , sNetName=undefined }
           group level name = (level, PrivGroup level name)--}
       let settings =
             do mapM addSection ["bot", "server", "user"]
                putSettingStr "bot.username" username
                putSettingStr "bot.password" password
                putSettingStr "bot.owner" owner
                putSettingStr "bot.trigger" trigger
                putSettingStr "bot.autojoin" . show $ map L.pack autoJoin
                putSettingStr "bot.logging" $ if logging then "true" else "false"
                putSettingStr "bot.timezone" logTs
                putSettingStr "user.groups" $ show [group 100 "Owners", group 0 "Guests"]
                putSettingStr "user.users" $ show [(L.pack owner, 100)]
                putSettingStr "bot.about" aboutString
                putSettingStr "bot.token" ""
           group level name = (level, PrivGroup level name)
       if setServerInfo
          then do domain <- getInputDef "Domain name (default \
                                        \chat.deviantart.com): "
                                        "chat.deviantart.com"
                  port <- getInputDef "Port (default 3900): " "3900"
                      >>= readNumber
                  name <- getInputDef "Server Name (hit enter for none): " ""
                  return . execState
                    (runErrorT $ settings >>
                       do putSettingStr "server.domain" domain
                          putSettingStr "server.port" $ show port
                          putSettingStr "server.netname" name)
                    $ SettingsFile { sfName="settings.hs"
                                   , sfParser=CF.emptyCP }
          else let (err, s) = runState
                     (runErrorT $ settings >>
                        do putSettingStr "server.domain" "chat.deviantart.com"
                           putSettingStr "server.port" $ show 3900
                           putSettingStr "server.netname" "")
                     $ SettingsFile { sfName="settings.hs"
                                    , sfParser=CF.emptyCP }
               in case err of
                    Right _  -> return s
                    Left err -> error $ settingsError err
       where readNumber :: (Read a, Num a) => String -> IO a
             readNumber port = 
                 case reads port :: (Read a, Num a) => [(a,String)] of
                      [(p,"")] -> return p
                      _ -> do putStrLn ">> Invalid number."
                              port <- getInputDef "Port (default 3900): "
                                                  "3900"
                              readNumber port
             aboutString = "Running Arsenic v. %ver% by :devdeviant-garde:!\
                            \\n<sub>Owned by <b>:dev%owner%:</b>. Running on\
                            \ <b>%os%</b>."

-- | Create a new client using information from a 'Settings' datatype.
makeClient :: SettingsFile -> IO Client
makeClient s =
    let config = evalState (runErrorT $ do
                    domain <- getSettingStr "server.domain"
                    port <- getSettingStr "server.port"
                    netName <- getSetting "server.netname"
                    let netName' = if L.null netName then Nothing else Just netName
                    groups <- getSettingStr "user.groups"
                    users <- getSettingStr "user.users"
                    return (domain, read port, netName', groups, users)) s
    in case config of
         Right (domain, port, netName, groups, users) ->
           do net <- makeNetwork domain port netName
              return $ Client { cVersion="0.3.0"
                              , cSettings=s
                              , cUsers=M.fromList $ read users
                              , cGroups=M.fromList $ read groups
                              , cNetwork=net
                              , cQuit=False
                              , cReconnect=False
                              , cPlugins=M.empty
                              , cPrinter=printer' }
              where printer' = CliPrinter putMsg' putNotice' putErr' logMsg'
         Left err -> error $ settingsError err

-- | Deform a channel using the username from a client.
deformChan :: ByteString -> ClientIO ByteString
deformChan chan = withSettings (getSetting "bot.username") >>= return . deformChanName chan

-- | Format a channel using the username from a client.
formatChan :: ByteString -> ClientIO ByteString
formatChan chan = withSettings (getSetting "bot.username") >>= return . formatChanName chan

-- | Deform a list of channels using the username from a client.
formatList :: [ByteString] -> ClientIO [ByteString]
formatList xs = do name <- withSettings (getSetting "bot.username") 
                   return $ map (flip formatChanName name) xs

-- | Deform a list of channels using the username from a client.
deformList :: [ByteString] -> ClientIO [ByteString]
deformList xs = do name <- withSettings (getSetting "bot.username") 
                   return $ map (flip deformChanName name) xs

-- | Log in on deviantART and return the recieved authtoken using cURL.
getAuthToken :: ByteString -> ByteString -> Bool -> IO (Maybe ByteString)
getAuthToken username pass rememberMe =
       -- this will be using GET by default, so this must be changed later
    do request <- parseUrl "https://www.deviantart.com/users/login"
       manager <- newManager -- apparently "keeps track of open connections"
       let lazyToStrict = B.concat . L.toChunks
       response <- httpLbs (urlEncodedBody [ ("username", lazyToStrict username)
                                           , ("password", lazyToStrict pass)
                                           , ("remember_me", if rememberMe
                                                                then "1"
                                                                else "0")]
                                           $ request) manager
       -- take all the "Set-Cookie" headers and grab the cookie by taking
       -- what's before the semicolon (ie in this=that; expires=sometime
       -- this=that is the actual cookie)
       let cookies = (map (fst . B.break (';'==) . snd)
                   . filter (("Set-Cookie"==) . fst)
                   $ responseHeaders response) :: [B.ByteString]
       request' <- parseUrl "http://chat.deviantart.com/chat/Botdom"
       closeManager manager -- to use a manger again, you must close it
       response' <- httpLbs (request' {requestHeaders=
                               [("Cookie", B.intercalate "; " cookies)]})
                            manager
       -- let subIndex' sub str = findIndex (B.isPrefixOf sub) (B.tails str)
       case subIndexBS "dAmn_Login( " $ responseBody response' of
         Nothing -> return Nothing
         Just pos -> let body = L.drop (fromIntegral $ pos+12) $ responseBody response'
                     in case subIndexBS "\", " body of
                          Nothing -> return Nothing
                          Just pos' -> return . Just . L.take 32
                                     $ L.drop (fromIntegral $ pos'+4) body
