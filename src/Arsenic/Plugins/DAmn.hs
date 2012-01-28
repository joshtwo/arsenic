{-# LANGUAGE OverloadedStrings #-}

module Arsenic.Plugins.DAmn (dAmnPlugin) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Arsenic.Types
import Arsenic.Client
import Arsenic.Plugin

-- | The datatype for the dAmn plugin.
data DAmnPlugin =
    DAmnPlugin { dAmnCmds :: CommandList
               , dAmnEvts :: EventHookList
               }

instance PluginData DAmnPlugin where
    pName _ = "dAmn Plugin"
    pVer  _ = "0.1"
    pDesc _ = "This plugin holds the general dAmn-related commands."
    pCmds = dAmnCmds
    pEvts = dAmnEvts
    setCmds plug cmds = plug {dAmnCmds=cmds}
    setEvts plug evts = plug {dAmnEvts=evts}
    pInit _ = dAmnInit


-- | The dAmn plugin.
dAmnPlugin :: DAmnPlugin
dAmnPlugin = DAmnPlugin M.empty M.empty

-- | Initializes the dAmn plugin.
dAmnInit :: Plugin -> ClientIO Plugin
dAmnInit plug = modifyPlugin_ plug $
    do addCmdList
         [ ("join", 100, joinCmd)
         , ("part", 100, partCmd)
         , ("say", 100, sayCmd)
         , ("reconnect", 100, reconnectCmd) ]
       addCmdHelp "say" sayHelp

sayHelp :: DocMaker
sayHelp =
    do addDocLine "Used to make the bot say things in dAmn."
       addAssocList
         [ ("$t$c <i>message</i>",
            "Send the message <i>message</i> to the channel.")
         , ("$t$c #<i>room</i> <i>message</i>",
            "Send the message <i>message</i> to the channel #<i>room</i>.") ]

joinCmd :: CommandHook
joinCmd EventInfo{evtNs=ns, evtFrom=from} args =
    if not $ null args
       then formatChan (head args) >>= dAmnJoin
       else dAmnSay ns
          $ from <+> ": You need to specify a channel to join."

partCmd :: CommandHook
partCmd EventInfo{evtNs=ns} args =
    if not $ null args
       then formatChan (head args) >>= dAmnPart
       else dAmnPart ns

sayCmd :: CommandHook
sayCmd EventInfo{evtPkt=pkt} args =
    if not $ null args
       then let chan = head args
            in if L.head chan == '#' && length args > 1
                  then formatChan chan
                   >>= flip dAmnSay (L.unwords $ tail args)
                  else dAmnSay (pktParam pkt) $ L.unwords args
       else dAmnSay (pktParam pkt)
          $ subPkt pkt ? "from" <+> ": You need to give something to say."

reconnectCmd :: CommandHook
reconnectCmd EventInfo{evtNs=ns, evtFrom=from} _ =
    do dAmnSay ns $ from <+> ": Restarting client. Be right back!"
       reconnectClient
