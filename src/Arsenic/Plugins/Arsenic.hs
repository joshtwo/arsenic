{-# LANGUAGE OverloadedStrings #-}

module Arsenic.Plugins.Arsenic (arsenicPlugin, startUp) where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List
import System.Exit (exitSuccess)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime
import Data.Time.Format
import Control.Monad.Error
import System.Locale (defaultTimeLocale)

import Arsenic.Types
import Arsenic.Client
import Arsenic.Plugin

-- | The Arsenic plugin datatype.
data ArsenicPlugin =
    ArsenicPlugin { cliCmds :: CommandList
                  , cliEvts :: EventHookList
                  }

instance PluginData ArsenicPlugin where
    pName _ = "Arsenic Client"
    pVer  _ = "0.2"
    pDesc _ = "This module adds the event hooks for the basic Arsenic client."
    pCmds = cliCmds
    pEvts = cliEvts
    setCmds plug cmds = plug {cliCmds=cmds}
    setEvts plug evts = plug {cliEvts=evts}
    pInit _ = basicHooks

arsenicPlugin = ArsenicPlugin M.empty M.empty

basicHooks :: Plugin -> ClientIO Plugin
basicHooks plug = modifyPlugin_ plug $
    do hookEvent AnyEvent quitRestartHook
       hookEvent RecvMsg commandsHook
       mapM_ (\event -> hookEvent event defaultHook) defaults
       where defaults = [ RecvMsg
                        , RecvJoin
                        , RecvPart
                        , RecvKicked
                        , RecvPrivchg
                        , RecvAction
                        , AdminCreate
                        , AdminUpdate
                        , AdminRename
                        , AdminRemove
                        , AdminMove
                        , Ping
                        , LoginError
                        , JoinError
                        , PartError
                        , GetError
                        , SetError
                        , PropertyTopic
                        , PropertyTitle
                        , PropertyMembers
                        , PropertyPc
                        , DAmnServer
                        , Kicked ]

-- | This is the default event hook that handles basic output and event
-- processing.
defaultHook :: EventHook
defaultHook EventInfo{evtPkt=pkt, evtType=evt, evtNs=ns'} =
    do ns <- deformChan ns'
       nsList' <- network nsList
       timezone <- liftIO getCurrentTimeZone
       let gotHeader =
             do putNotice (headerType <+> " set by " <+> pkt ? "by" <+> " on "
                 <+> (L.pack $ formatTime defaultTimeLocale "%x" ts)) ns
                if M.member ns' nsList'
                   then setNamespace ns' $
                          case evt of      -- change the title or topic for "ns"
                            PropertyTitle -> (nsList' ! ns') {nsTitle=header'}
                            PropertyTopic -> (nsList' ! ns') {nsTopic=header'}
                   else setNamespace ns' $ -- we got a header for a namespace we didn't join
                          case evt of      -- set one header and not the other
                            PropertyTitle -> namespace emptyHeader header'
                            PropertyTopic -> namespace header' emptyHeader
                where namespace = Namespace ns' M.empty M.empty
                      ts = utcToLocalTime timezone . posixSecondsToUTCTime
                         $ fromIntegral ((read . L.unpack $ pkt ? "ts") :: Int)
                      header' = Header (pktBody pkt) (pkt ? "by") (pkt ? "ts")
                      headerType = case evt of
                                     PropertyTitle -> "Title"
                                     PropertyTopic -> "Topic"
           createOrUpdate = putNotice ("privilege class " <+> body?"name"
                        <+> " has been" <+> pktParam body <+> "d with: "
                        <+> body?"privs") ns
           errorPkt = if M.member "r" $ pktArgs pkt
                         then putNotice ("You have been disconnected:"
                          <+> pkt?"e") ""
                         else putNotice ("Error " <+> pktCmd pkt <+> "ing "
                          <+> ns <+> ": " <+> pkt ? "e") ""
           joinOrPart =
               if pkt?"e" == "ok"
                  then do putNotice ("You have "<+>pktCmd pkt<+>"ed " <+> ns) ns
                          setNsList $
                            case evt of
                              JoinError ->
                                  M.insert
                                    ns'
                                    (Namespace
                                      ns'
                                      M.empty
                                      M.empty
                                      emptyHeader
                                      emptyHeader)
                                    nsList'
                              PartError -> M.delete ns nsList'
                  else errorPkt
           getOrSet = do ns <- formatChan $ pktParam pkt
                         putNotice ("Error "<+>pktCmd pkt<+>"ing properties of"
                          <+> ns <+> ": " <+> pkt ? "e") ""
           saveToken :: ByteString -> ClientIO ()
           saveToken t =
                do err <- withSettings $  putSetting "bot.token" t
                   s <- gets cSettings
                   liftIO $ saveSettings s
                   putNotice "Saving authtoken..." ""
                   modify (\cli -> cli { cSettings=s
                                       , cReconnect=True })
       case evt of
         RecvMsg -> putMsg ("<"<+> body?"from" <+>"> "<+>pktBody body) ns
         RecvAction -> putMsg ("* "<+>body?"from"<+>" "<+>pktBody body) ns
         RecvJoin -> putNotice (pktParam body<+>" has joined") ns
         RecvPart -> putNotice (pktParam body<+>" has left"
                 <+> if M.member "r" $ pktArgs body
                        then " [" <+> body?"r" <+> "]"
                        else "") ns
         RecvKicked -> putNotice (pktParam body<+>" has been kicked by "
                   <+> body ? "by" <+> if pktBody body /= ""
                                           then " [" <+> pktBody body <+> "]"
                                           else "") ns
         RecvPrivchg -> putNotice (pktParam body<+>" has been made a member of "
                    <+> body ? "pc" <+> " by " <+> body?"by") ns
         Ping -> dAmnPong 
         SendError -> errorPkt
         SetError -> getOrSet
         GetError -> getOrSet
         KickError -> do ns <- formatChan $ pktParam pkt
                         putNotice ("Cannot kick "<+>pkt?"u"<+>": "<+>pkt?"e") ns
         AdminCreate -> createOrUpdate
         AdminUpdate -> createOrUpdate
         AdminRename -> putNotice ("privilege class " <+> body?"prev"
                    <+> " has been renamed to " <+> body?"name" <+> " by "
                    <+> body?"by") ns
         AdminRemove -> putNotice ("privilege class " <+> body?"name"
                    <+> " has been removed by " <+> body?"by" <+> " -- "
                    <+> body?"n" <+> " members were affected") ns
         AdminMove -> putNotice ("all members of " <+> body?"prev"
                  <+> " have been made " <+> body?"name" <+> " by" <+> body?"by"
                  <+> " -- " <+> body?"n" <+> " members were affected") ns
         DAmnServer ->
             do putNotice ("Server version is "<+>pktParam pkt) ""
                putNotice "Logging in..." ""
                name <- withSettings $ getSetting "bot.username" 
                pass <- withSettings $ getSetting "bot.password" 
                token <- gets cSettings >>= return . evalState (runErrorT 
                       $ do token <- getSetting "bot.token"
                            when (L.null token) . throwError $ SettingsError "null token"
                            return token)
                            
                case token of
                  Right t -> dAmnLogin name t
                  Left (SettingsError e) ->
                      do putNotice (L.pack $ "ERROR: " ++ e) ""
                         putNotice "No authtoken saved. Retrieving new\
                                    \ authtoken..." ""
                         t' <- liftIO $ getAuthToken name pass True
                         case t' of
                           Nothing -> error "Couldn't get an authtoken!"
                           Just t'' -> do saveToken t''
                                          dAmnLogin name t''
         LoginError ->
             do putNotice ("Login for "<+>pktParam pkt <+> ": "
                 <+> pkt ? "e") ""
                case pkt ? "e" of
                  "authentication failed" ->
                      do putErr ("Authentication failed! We better get\
                                 \ another authtoken.") ""
                         name <- withSettings $ getSetting "bot.username"
                         pass <- withSettings $ getSetting "bot.password" 
                         token <- liftIO $ getAuthToken name pass True
                         case token of
                           Just t -> saveToken t
                           Nothing -> do putErr "Failed to retrieve an\
                                                \ authtoken. Aww man!" ""
                                         quitClient
                  "not privileged" -> do putErr "Oh no, you're banned from\
                                                \ dAmn! You better use a\
                                                \ different account!" ""
                                         quitClient
                  "too many connections" -> do putErr "You've connected to\
                                                      \ dAmn too many times\
                                                      \ simultaneously. You\
                                                      \ can't connect right\
                                                      \ now!" ""
                                               quitClient
                  "ok" -> do withSettings (liftM (read :: String -> [ByteString]) $ getSettingStr "bot.autojoin") 
                              >>= formatList
                              >>= mapM_ dAmnJoin
         PropertyTopic -> gotHeader
         PropertyTitle -> gotHeader
         PropertyMembers ->
             let toMember pkt = ( pktParam pkt
                                , Member { memberName=pktParam pkt
                                         , memberSymbol=pkt?"symbol"
                                         , memberPc=pkt?"pc"
                                         , memberIcon=pkt?"usericon"
                                         , realName=pkt?"realname"
                                         , typeName=pkt?"typename"
                                         , memberGpc=pkt?"gpc" })
                 memberList = M.fromList
                            . map (toMember . makePacket) . tokenise "\n\n"
                            $ pktBody pkt
             in do putNotice "Got list of members" ns
                   setNamespace ns' $
                     if M.member ns' nsList'
                        then (nsList' ! ns') {nsMembers=memberList}
                        else Namespace
                               ns'
                               memberList
                               M.empty
                               emptyHeader
                               emptyHeader
         PropertyPc ->
             let pcList = pktArgs . makePacket $ pktBody pkt
             in do putNotice "Got list of privclasses" ns
                   setNamespace ns' $
                     if M.member ns' nsList'
                        then (nsList' ! ns') {nsPcList=pcList}
                        else Namespace
                               ns'
                               M.empty
                               pcList
                               emptyHeader
                               emptyHeader
         JoinError -> joinOrPart
         PartError -> joinOrPart
         Kicked -> do putNotice ("You have been kicked by "<+>pkt ? "by"
                        <+> if not . L.null $ pktBody pkt
                               then " [" <+> pktBody pkt <+> "]"
                               else "") ns
                      dAmnJoin ns'
                      setNsList $
                        if M.member ns' nsList'
                           then M.delete ns' nsList'
                           else nsList'
         _ -> return ()
       where body = subPkt pkt
             emptyHeader = Header "" "" "" -- empty header

-- | This hook allows the user to perform commands.
commandsHook :: EventHook
commandsHook evt@EventInfo{evtPkt=pkt, evtFrom=from, evtNs=ns} =
    do trig <- withSettings $ getSetting "bot.trigger"
       plugs <- gets cPlugins
       users <- gets cUsers 
       name <- withSettings $ getSetting "bot.username" 
       let msg = pktBody $ subPkt pkt
           key = L.drop (L.length trig) . head $ L.words msg
           privs = case M.lookup from users of
                     Nothing -> 0
                     Just priv -> priv
           -- search the CommandLists for the command and run it
           search :: [(Plugin,CommandList)] -> ClientIO ()
           search [] =
               putNotice ("Command \""<+>key<+>"\" does not exist.") ""
           search ((plug,cmds):others) = 
               case M.lookup key cmds of
                 Just cmd ->
                     if privs >= groupOrder (cmdPrivs cmd)
                        then cmdHook cmd (evt {evtPlugin=plug}) args
                        else deformChan ns >>=
                             putNotice
                                (from <+> " attempted to use \""
                                      <+> cmdName cmd
                                      <+> " with insufficient privs")
                 Nothing -> search others
       if L.take (L.length trig) msg == trig
          then search . M.elems $ M.map (\p -> (p,getPlugCmds p)) plugs
          else if msg == (name <+> ": trigcheck")
                  then dAmnSay ns $ from <+> ": My trigger is <code>" <+> trig
                  else return ()
       where args = tail . L.words . pktBody $ subPkt pkt

-- | This hook handles quits and restarts.
quitRestartHook :: EventHook
quitRestartHook _ =
    do quit <- gets cQuit
       reconnect <- gets cReconnect
       when quit $ do putNotice "Bye!" ""
                      cSaveSettings
                      liftIO exitSuccess
       when reconnect $ do putNotice "Reconnecting to dAmn..." ""
                           dAmnQuit
                           net <- gets cNetwork >>= netReconnect
                           modify (\cli -> cli { cNetwork = net
                                               , cReconnect = False })
                           startUp

-- | Used to run the client.
startUp :: ClientIO ()
startUp =
    do name <- withSettings $ getSetting "bot.username" 
       password <- withSettings $ getSetting "bot.password" 
       ver <- gets cVersion
       putMsg ("Arsenic version "<+>ver<+>", by deviant-garde") ""
       -- token <- withSettings $ getSetting "bot.token" 
       putNotice "Performing handshake..." ""
       dAmnClient "Haskell"
       netReadPkts >>= runHooksList
