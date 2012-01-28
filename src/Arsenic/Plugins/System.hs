{-# LANGUAGE OverloadedStrings #-}

module Arsenic.Plugins.System (systemPlugin) where

import Control.Monad.State
import System.Info (os, arch)
import Data.List
import Data.Ord
import Debug.Trace (trace)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Arsenic.Types
import Arsenic.Client
import Arsenic.Plugin
import Arsenic.Misc (tokenise, replace, andJoin, commaJoin)

-- | The System plugin datatype.
data SystemPlugin =
    SystemPlugin { systemCmds :: CommandList
                 , systemEvts :: EventHookList
                 }

instance PluginData SystemPlugin where
    pName _ = "System Commands"
    pVer  _ = "0.1"
    pDesc _ = "This plugin holds the commands that interact mostly with the\
               \ bot itself rather than dAmn."
    pCmds = systemCmds
    pEvts = systemEvts
    setCmds plug cmds = plug {systemCmds=cmds}
    setEvts plug evts = plug {systemEvts=evts}
    pInit _ = systemInit

-- | The System plugin.
systemPlugin = SystemPlugin M.empty M.empty

-- | Initializer for the System plugin.
systemInit :: Plugin -> ClientIO Plugin
systemInit plug = modifyPlugin_ plug $
    do addCmdList
         [ ("about", 0, aboutCmd)
         , ("commands", 0, cmdsCmd)
         , ("quit", 100, quitCmd)
         , ("autojoin", 100, autoJoinCmd)
         , ("help", 0, helpCmd) ]
       addCmdHelp "autojoin" autoJoinHelp

autoJoinHelp :: DocMaker
autoJoinHelp =
    do addDocLine "Used to view and update the list of rooms the bot\
                  \ joins on startup."
       addAssocList
         [ ("$t$c add <i>room</i>",
            "Adds the room #<i>room</i> to the autojoin list. You may specify\
            \ a space-separated list of rooms to join. For example,\
            \ <code>$t$c add room1 room2 room3</code> will add\
            \ #<i>room1</i>, #<i>room2</i>, and #<i>room3</i> to the autojoin\
            \ list.")
         , ("$t$c del <i>room</i>",
            "Deletes the room #<i>room</i> from the autojoin list. Like with\
            \ the above command, you can specify a space-separated list of\
            \ rooms to delete.")
         , ("$t$c list",
            "Lists all the channels on the autojoin list.") ]

aboutCmd :: CommandHook
aboutCmd EventInfo{evtNs=ns, evtFrom=from} _ =
    do ver <- gets cVersion
       owner <- withSettings $ getSetting "bot.owner"
       about <- withSettings $ getSetting "bot.about"
       dAmnSay ns . replace "%owner%" owner
                  . replace "%ver%" ver
                  $ replace "%os%" operatingSystem about
       where operatingSystem = (case os of
                                  "mingw32" -> "Windows"
                                  "linux" -> "Linux"
                                  "darwin" -> "Mac OS X"
                                  _ -> L.pack os) <+> " " <+> L.pack arch

cmdsCmd :: CommandHook
cmdsCmd EventInfo{evtNs=ns, evtFrom=from} args =
    do plugs <- gets cPlugins
       dAmnSay ns $ "<abbr title=\"" <+> from <+> "\"></abbr>"
        <+> case args of
              "by-plugin":_ -> cmdsByPlugin plugs
              _ -> cmdsByPrivs plugs

-- This is ran when commands are displayed by plugin
cmdsByPlugin :: PluginList -> L.ByteString
cmdsByPlugin plugs =
    let cmdsFor :: CommandList -> L.ByteString
        cmdsFor map' = commaJoin . sort . M.elems $ M.map cmdName map'
    in L.unlines . filter (not . L.null) . M.elems
     $ M.map (\plug ->
         if not . M.null $ getPlugCmds plug
            then "<b><u>" <+> getPlugName plug <+> ":</u></b> "
                          <+> cmdsFor (getPlugCmds plug)
            else "") plugs

-- This is ran when commands are displayed by privileges
cmdsByPrivs :: PluginList -> L.ByteString
cmdsByPrivs plugs =
    let makeCmd :: (L.ByteString, Command) -> L.ByteString
        makeCmd (plug, cmd) =
            "<abbr title=\"" <+> plug <+> "\">" <+> cmdName cmd <+> "</abbr>"
        allCmds :: [Command]
        allCmds = concatMap (M.elems . getPlugCmds) $ M.elems plugs
        listOfPrivs :: [PrivGroup]
        listOfPrivs = nubBy (\a b -> groupOrder a == groupOrder b)
                    $ map cmdPrivs allCmds
        groupCmds :: [PrivGroup]
                  -> [Command]
                  -> [(PrivGroup, [Command])]
        groupCmds [] _ = []
        groupCmds _ [] = []
        groupCmds (p:ps) cmds =
            let (cmds', xs) = partition ((<= groupOrder p) . order) cmds
                order = groupOrder . cmdPrivs
            in (p, cmds') : groupCmds ps xs
        sortCmds = sortBy (comparing cmdName)
        cmdList = reverse . map (\(privGroup, cmds) -> (privGroup, sortCmds cmds))
                $ groupCmds listOfPrivs allCmds
    in L.unlines $ map (\(PrivGroup {groupName=key}, cmds) ->
        "<b><u>" <+> key <+> ":</u></b> "
                 <+> L.intercalate ", " (map cmdName cmds)) cmdList

quitCmd :: CommandHook
quitCmd EventInfo{evtNs=ns, evtFrom=from} _ =
    do dAmnSay ns $ from <+> ": Quitting dAmn."
       quitClient

autoJoinCmd :: CommandHook
autoJoinCmd evt@EventInfo{evtNs=ns, evtFrom=from} args =
    do let saveList list = withSettings . putSetting "bot.autojoin" $ commaJoin list
           autoJoinHelp = helpCmd evt ["autojoin"]
       autoJoin <- withSettings $ liftM (tokenise ", ") $ getSetting "bot.autojoin" 
       if length args > 0
          then case args of
                 "add":chans ->
                     do chans' <- deformList chans
                        saveList $ autoJoin `union` chans'
                        if length chans > 1
                           then dAmnSay ns
                              $ from <+> ": Channels "
                            <+> L.intercalate ", " (init chans') <+> " and "
                            <+> last chans' <+> " were added to the autojoin list."
                           else dAmnSay ns
                              $ from <+> ": Channel " <+> head chans'
                            <+> " was added to the autojoin list."
                 "list":_ ->
                     do list <- deformList autoJoin
                        dAmnSay ns
                            $ "<abbr title=\"" <+> from
                          <+> "\"></abbr><b><u>Autojoin List</u></b>\n<sub>"
                          <+> commaJoin list
                 "del":chans ->
                     do chans' <- formatList chans
                        deformedList <- deformList chans
                        liftIO . L.putStrLn $ "Chans: " <+> L.unwords chans'
                        liftIO . L.putStrLn $ "Autojoin: " <+> L.unwords autoJoin
                        case length chans of
                          1 -> let channel = head chans'
                                   deformed = head deformedList
                               in if channel `elem` autoJoin
                                     then do saveList $ delete channel autoJoin
                                             dAmnSay ns $ from <+> ": Channel "
                                              <+> deformed
                                              <+> " was deleted from the autojoin\
                                                  \ list."
                                     else dAmnSay ns $ from <+> ": Channel "
                                      <+> deformed <+> " is not in the list."
                          0 -> dAmnSay ns $ from <+> ": You did not specify a\
                                                      \ channel to delete."
                          _ -> if and $ map (`elem` autoJoin) chans'
                                  then do saveList $ autoJoin \\ chans'
                                          dAmnSay ns $ from <+> ": Channels "
                                           <+> andJoin deformedList
                                           <+> " were removed from the autojoin\
                                               \ list."
                                  else dAmnSay ns $ from <+> ": The channels "
                                   <+> andJoin deformedList
                                   <+> " are not in the list."
                 _ -> autoJoinHelp 
          else autoJoinHelp

helpCmd :: CommandHook
helpCmd EventInfo{evtNs=ns, evtFrom=from} args =
    if length args > 0
       then do trig <- withSettings $ getSetting "bot.trigger" 
               plugs <- gets (M.elems . cPlugins)
               let name = head args
                   cmdList = foldr1 M.union $ map getPlugCmds plugs
               case M.lookup name cmdList of
                 Nothing -> dAmnSay ns
                          $ from <+> ": The command \"" <+> name <+> "\" does \
                                     \ not exist."
                 Just cmd ->
                     case cmdHelp cmd of
                       Nothing -> dAmnSay ns
                                $ from <+> ": There is no documentation for\
                                           \ the \"" <+> name <+> "\" command."
                       Just docs -> dAmnSay ns . showDocs $
                           do addDocSub "$t" trig
                              addDocSub "$c" name
                              setDocTitle ("Help for \"" <+> name <+>"\"")
                              docs
       else dAmnSay ns $ from <+> ": You must specify a command to get help on."
