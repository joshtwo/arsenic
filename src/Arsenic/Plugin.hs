{-# LANGUAGE OverloadedStrings #-}

-- Most of the code that has to do with plugins and events is here
module Arsenic.Plugin (
    module Control.Monad.State,
    module Arsenic.Misc,
    module Arsenic.Network,
    module Arsenic.Documentation,
    -- * Events and event hooks
    Event(..),
    EventHook,
    EventHookList,
    EventInfo(..),
    EventMatch(..),
    runHooks,
    runHooksList,
    -- * Commands
    checkEvent,
    hookEvent,
    unhookEvent,
    addCmd,
    addCmdList,
    addCmdHelp,
    -- * Working with plugins
    PluginData,
    Plugin(..),
    PluginList,
    Command(..),
    CommandList,
    CommandHook,
    initAllPlugins,
    -- ** Accessing plugin data
    getPlugName,
    getPlugVer,
    getPlugCmds,
    setPlugCmds,
    setPlugEvts,
    runPlugInit,
    -- ** The @PluginIO@ monad
    getPlugin,
    getsPlugin,
    getClient,
    putPlugin,
    putClient,
    clientIO,
    savePlugin,
    withPlugin,
    withPlugin_,
    modifyPlugin,
    modifyPlugin_,
    -- ** User privileges
    PrivGroup(..),
    GroupList,
    UserList
) where

import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import System.Random (randomIO)

import Arsenic.Types
import Arsenic.Network
import Arsenic.Misc
import Arsenic.Documentation

-- | Check if a packet matches a certain event.
checkEvent :: Event  -- ^ The event to test for.
           -> Packet -- ^ The packet to test.
           -> Bool   -- ^ Whether or not the packet matches to given event.
checkEvent evt pkt =
    let matches = M.fromList -- (event, main packet, packet body)
          [ (AnyEvent, (Anything, Anything))
          , recv Recv Anything
          , recv RecvJoin $ Cmd "join"
          , recv RecvPart $ Cmd "part"
          , recv RecvPrivchg $ Cmd "privchg"
          , recv RecvKicked $ Cmd "kicked"
          , recv RecvAdmin $ Cmd "admin"
          , recv RecvMsg $ Cmd "msg"
          , recv RecvAction $ Cmd "action"
          , recv AdminShow $ Head "admin" "show"
          , recv AdminShowPc $ HeadArgs "admin" "show" [("p", "privclass")]
          , recv AdminShowUsers $ HeadArgs "admin" "show" [("p", "users")]
          , recv AdminRename $ Head "admin" "rename"
          , recv AdminRemove $ Head "admin" "remove"
          , recv AdminMove $ Head "admin" "move"
          , recv AdminUpdate $ Head "admin" "update"
          , recv AdminCreate $ Head "admin" "create"
          , (Property, (Cmd "property", Anything))
          , prop PropertyPc "privclasses"
          , prop PropertyTitle "title"
          , prop PropertyTopic "topic"
          , prop PropertyMembers "members"
          , prop PropertyInfo "info"
          , (DAmnServer, (Cmd "dAmnServer", Anything))
          , (LoginError, (Cmd "login", Anything))
          , (JoinError, (Cmd "join", Anything))
          , (PartError, (Cmd "part", Anything))
          , (SendError, (Cmd "send", Anything))
          , (GetError, (Cmd "get", Anything))
          , (SetError, (Cmd "set", Anything))
          , (KickError, (Cmd "kick", Anything))
          , (Disconnect, (Cmd "disconnect", Anything))
          , (Ping, (Cmd "ping", Anything))
          , (Kicked, (Cmd "kicked", Anything)) ]
        recv :: Event -> EventMatch -> (Event, (EventMatch, EventMatch))
        recv e match = (e, (Cmd "recv", match))
        prop :: Event -> ByteString -> (Event, (EventMatch, EventMatch))
        prop e arg = (e, (CmdArgs "property" [("p", arg)], Anything))
        pktMatch = matches ! evt
        check pkt match =
            case match of
                 Anything -> True
                 Cmd c -> pktCmd pkt == c
                 Param p -> pktParam pkt == p
                 Head c p -> pktCmd pkt == c && pktParam pkt == p
                 Args a -> M.fromList a `M.isSubmapOf` pktArgs pkt
                 CmdArgs c a ->
                     pktCmd pkt == c
                  && M.fromList a `M.isSubmapOf` pktArgs pkt
                 ParamArgs p a ->
                     pktParam pkt == p
                  && M.fromList a `M.isSubmapOf` pktArgs pkt
                 HeadArgs c p a ->
                     pktCmd pkt == c
                  && pktParam pkt == c
                  && M.fromList a `M.isSubmapOf` pktArgs pkt
    in check pkt (fst pktMatch)
    && check (makePacket $ pktBody pkt) (snd pktMatch)

-- | Run all of the event hooks in the given that match the packet.
runPluginHooks :: Packet -- ^ The packet to test.
               -> Plugin -- ^ The plugin to use.
               -> ClientIO ()
runPluginHooks pkt plugin =
    do cli <- get
       StateT $ \_ -> do cli' <- fst $ M.mapAccum runHooks' (return cli) hooks
                         return ((), cli')
       where hooks = getPlugEvts plugin
             runHooks' :: IO Client -> (Event, EventHook) -> (IO Client, ())
             runHooks' cli hook =
                 if checkEvent (fst hook) pkt
                    -- run the hook with the accumulated client
                    then (cli >>= execStateT (snd hook . eventInfo plugin pkt $ fst hook), ())
                    else (cli, ())

-- | Run all of the event hooks in every plugin that match the packet.
runHooks :: Packet -- ^ The packet the test.
         -> ClientIO ()
runHooks pkt =
    do plugs <- gets cPlugins
       mapM_ (runPluginHooks pkt) . map snd $ M.toList plugs

-- | Run all of the event hooks relevant to each individual packet in a list
-- of packets.
runHooksList :: [Packet] -- ^ The list of packets to test.
             -> ClientIO ()
runHooksList = mapM_ runHooks

-- | Make a new EventInfo.
eventInfo :: Plugin -- ^ The plugin this event is attached to.
          -> Packet -- ^ The packet that triggered the event.
          -> Event  -- ^ The type of event triggered.
          -> EventInfo
eventInfo plugin pkt evt =
    EventInfo { evtPlugin=plugin
              , evtPkt=pkt
              , evtType=evt
              , evtFrom=pktFrom pkt 
              , evtNs=pktNs pkt
              , evtMsg=pktMsg pkt }

-- | Determine who triggered this packet. Returns an empty string if it was
-- caused by the client or no one in particular.
pktFrom :: Packet     -- ^ The packet to check.
        -> ByteString -- ^ The user who triggered the packet, if applicable.
pktFrom pkt =
    case pktCmd pkt of
      "recv" -> case pktCmd sub of
                  "msg" -> sub ? "from"
                  "action" -> sub ? "from"
                  "join" -> pktParam sub
                  "part" -> pktParam sub
                  "kicked" -> sub ? "by"
                  "privchg" -> sub ? "by"
                  "admin" -> if pktCmd sub == "show" then "" else sub ? "by"
                  _ -> ""
      "kicked" -> pkt ? "by"
      _ -> ""
      where sub = subPkt pkt

-- | Determine the channel the packet is relevant to. If it is not relevant to
-- any channel in particular, then this will return an empty string.
pktNs :: Packet     -- ^ The packet to check.
      -> ByteString -- ^ The channel the packet is relevant to, if applicable.
pktNs pkt =
    if pktCmd pkt
         `elem` 
       ["recv", "join", "part", "send", "get", "set", "kicked", "property"]
       then pktParam pkt
       else ""

-- | If the packet has a message to a chatroom, return it.
pktMsg :: Packet -- ^ The packet to check for a message.
       -> ByteString -- ^ The resulting message. If there is none, returns @""@
pktMsg pkt =
    if pktCmd body `elem` ["msg", "npmsg", "action"]
       then pktBody body
       else ""
    where body = subPkt pkt

-- | Get the name of a plugin wrapped in a 'Plugin' container.
getPlugName :: Plugin -> ByteString
getPlugName (Plugin p) = pName p

-- | Get the version of a plugin wrapped in a 'Plugin' container.
getPlugVer :: Plugin -> ByteString
getPlugVer (Plugin p) = pVer p

-- | Get the description of a plugin wrapped in a 'Plugin' container.
getPlugDesc :: Plugin -> ByteString
getPlugDesc (Plugin p) = pDesc p

-- | Get the description of a plugin wrapped in a 'Plugin' container.
getPlugCmds :: Plugin -> CommandList
getPlugCmds (Plugin p) = pCmds p

-- | Get the events from a plugin wrapped in a 'Plugin' container.
getPlugEvts :: Plugin -> EventHookList
getPlugEvts (Plugin p) = pEvts p

-- | Set the commands inside of a plugin wrapped in a 'Plugin' container.
setPlugCmds (Plugin p) cmds = Plugin $ setCmds p cmds

-- | Set the events inside of a plugin wrapped in a 'Plugin' container.
setPlugEvts (Plugin p) evts = Plugin $ setEvts p evts

-- | Run the initializer for a plugin.
runPlugInit :: Plugin -> ClientIO Plugin
runPlugInit plug@(Plugin p) = pInit p plug

-- | Get the plugin from the environment of a 'PluginIO' action.
getPlugin :: PluginIO Plugin
getPlugin = gets snd

-- | Get the client from the environment of a 'PluginIO' action.
getClient :: PluginIO Client
getClient = gets fst

-- | Same as @gets@, except it's being done on the 'Plugin' in the environment.
getsPlugin :: (Plugin -> Plugin) -> PluginIO Plugin
getsPlugin f =
    do plug <- getPlugin
       return $ f plug

-- | Set the plugin in the environment of a 'PluginIO' action.
putPlugin :: Plugin -> PluginIO ()
putPlugin plug =
    do cli <- getClient
       put (cli, plug)

-- | Set the client in the environment of a 'PluginIO' action.
putClient :: Client -> PluginIO ()
putClient cli =
    do plug <- getPlugin
       put (cli, plug)

-- | Lift a 'ClientIO' action to the 'PluginIO' monad.
clientIO :: ClientIO a -> PluginIO a
clientIO action =
    do cli <- getClient
       liftIO $ evalStateT action cli

-- | Run a 'PluginIO' action inside of a 'ClientIO' monad using the
-- given 'Plugin'. Any changes made to the client inside of the 'PluginIO'
-- action will be saved in the 'ClientIO' monad it's being ran in.
-- This returns the 'Plugin' in the state of the 'PluginIO' action and its return
-- value in a pair.
withPlugin :: Plugin -> PluginIO a -> ClientIO (Plugin, a)
withPlugin plug plugIO =
    do cli <- get
       (returnValue, (cli', plug')) <- liftIO $ runStateT plugIO (cli, plug)
       put cli'
       return (plug', returnValue)

-- | Same as 'withPlugin', except the return value of the 'PluginIO' action
-- is thrown away, and only the plugin in the 'PluginIO' environment is
-- returned
withPlugin_ :: Plugin -> PluginIO a -> ClientIO Plugin
withPlugin_ plug plugIO =
    do (plug', _) <- withPlugin plug plugIO
       return plug'

-- | Save the given plugin in the client using its name.
savePlugin :: Plugin -> ClientIO ()
savePlugin plug =
    do plugList <- gets cPlugins
       modify (\cli -> cli {cPlugins=M.insert (getPlugName plug) plug plugList})

-- | This is the same as 'withPlugin' except the plugin is saved after it's
-- used.
modifyPlugin :: Plugin -> PluginIO a -> ClientIO (Plugin, a)
modifyPlugin plug plugIO =
    do returnValue@(plug', _) <- withPlugin plug plugIO
       savePlugin plug'
       return returnValue

-- | Like 'withPlugin_', this is a variation of 'modifyPlugin' with only the
-- 'Plugin' in the 'PluginIO' monad's environment returned.
modifyPlugin_ :: Plugin -> PluginIO a -> ClientIO Plugin
modifyPlugin_ plug plugIO =
    do (plug', _) <- modifyPlugin plug plugIO
       return plug'

-- | Create a new command.
addCmd :: ByteString  -- ^ The name of the command.
       -> Int         -- ^ The minumum privilege level needed to use the command.
       -> CommandHook -- ^ The command hook to use for this command.
       -> PluginIO ()
addCmd name privs hook =
    do groups <- clientIO $ gets cGroups
       plug <- getPlugin
       let cmd = Command { cmdName=name
                         , cmdPrivs=if M.member privs groups
                                       then groups ! privs
                                       else PrivGroup privs ""
                         , cmdHelp=Nothing
                         , cmdHook=hook }
           cmdList = getPlugCmds plug
           plug' = setPlugCmds plug (M.insert name cmd cmdList)
       putPlugin plug'

-- | Create multiple commands.
addCmdList :: [(ByteString, Int, CommandHook)]
           -- ^ A list of (command name, privilege level, command hook) tuples.
           -- These are the same as the name, level, and command hook used with
           -- 'addCmd'.
           -> PluginIO ()
addCmdList cmds =
    do groups <- clientIO $ gets cGroups
       plug <- getPlugin
       let plug' = setPlugCmds plug (getPlugCmds plug `M.union` addCmdList' cmds)
           addCmdList' [] = M.empty
           addCmdList' ((name,privs,hook):xs) =
               let cmd = Command { cmdName=name
                                 , cmdPrivs=if M.member privs groups
                                               then groups ! privs
                                               else PrivGroup privs ""
                                 , cmdHelp=Nothing
                                 , cmdHook=hook }
               in M.singleton name cmd `M.union` addCmdList' xs
       putPlugin plug'

-- | Add documentation to a command. Returns true on success, and false if the
-- command is not found in the plugin.
addCmdHelp :: ByteString -> DocMaker -> PluginIO Bool
addCmdHelp name docs =
    do plug <- getPlugin
       let cmdList = getPlugCmds plug
           docs' = docs
       case M.lookup name cmdList of
         Nothing -> return False
         Just cmd -> do putPlugin . setPlugCmds plug $
                          M.insert name (cmd {cmdHelp=Just docs'}) cmdList
                        return True

-- | Hook an event to a plugin's event hook list.
hookEvent :: Event        -- ^ The event type to hook on.
          -> EventHook    -- ^ The event hook to add.
          -> PluginIO Int -- ^ The ID of the event hook. This is used
                          -- if you want to delete the event hook later.
hookEvent evt hook =
    do id <- liftIO (randomIO :: IO Int)
       plug <- getPlugin
       putPlugin $ setPlugEvts plug (M.insert id (evt, hook) $ getPlugEvts plug)
       return id

-- | Remove an event hook from the client's event hook list.
unhookEvent :: Int -- ^ The ID of the event hook to remove.
            -> PluginIO ()
unhookEvent id =
    do plug <- getPlugin
       putPlugin $ setPlugEvts plug (M.delete id $ getPlugEvts plug)

-- | Run every plugin's startup function given a list of plugins.
initAllPlugins :: [Plugin]        -- ^ The list of plugins
               -> ClientIO Client -- ^ The resulting client.
initAllPlugins [] = get
initAllPlugins (x:xs) =
    do runPlugInit x
       initAllPlugins xs
