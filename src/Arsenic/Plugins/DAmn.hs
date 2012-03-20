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
         [ ("join", 50, joinCmd)
         , ("part", 50, partCmd)
         , ("say", 50, sayCmd)
         , ("reconnect", 100, reconnectCmd)
         , ("raw", 100, rawCmd) ]
       mapM_ (uncurry addCmdHelp)
         [ ("say", sayHelp)
         , ("join", joinPartHelp)
         , ("part", joinPartHelp)
         , ("reconnect", reconnectHelp)
         , ("raw", rawHelp) ]

sayHelp :: DocMaker
sayHelp =
    do addDocLine "Used to make the bot say things in dAmn."
       addAssocList
         [ ("$t$c <i>message</i>",
            "Send the message <i>message</i> to the channel.")
         , ("$t$c #<i>room</i> <i>message</i>",
            "Send the message <i>message</i> to the channel #<i>room</i>.") ]

joinPartHelp :: DocMaker
joinPartHelp =
    do addDocLine "Makes the bot $c channels."
       addAssocList
         [ ("$t$c #<i>channel</i>",
            "Make the bot $c a channel. You can also specify more than one channel to\
            \ $c, e.g <code>$t$c #channel1 #channel2 #channel3</code>") ]

reconnectHelp :: DocMaker
reconnectHelp = addDocLine "Disconnects, then reconnects to dAmn."

rawHelp :: DocMaker
rawHelp =
    do addDocLine "Sends a raw packet over dAmn."
       addAssocList [("$t$c <i>packet</i>", "Send the text <i>packet</i>.")]

joinCmd :: CommandHook
joinCmd EventInfo{evtNs=ns, evtFrom=from} args =
    if not $ null args
       then formatList args >>= mapM_ dAmnJoin
       else dAmnSayTo ns from "You need to specify a channel to join."

partCmd :: CommandHook
partCmd EventInfo{evtNs=ns} args =
    if not $ null args
       then formatList args >>= mapM_ dAmnPart
       else dAmnPart ns

sayCmd :: CommandHook
sayCmd EventInfo{evtNs=ns, evtFrom=from} args =
    if not $ null args
       then let chan = head args
            in if L.head chan == '#' && length args > 1
                  then formatChan chan
                   >>= flip dAmnSay (L.unwords $ tail args)
                  else dAmnSay ns $ L.unwords args
       else dAmnSayTo ns from "You need to give something to say."

reconnectCmd :: CommandHook
reconnectCmd EventInfo{evtNs=ns, evtFrom=from} _ =
    do dAmnSayTo ns from "Restarting client. Be right back!"
       reconnectClient

rawCmd :: CommandHook
rawCmd EventInfo{evtNs=ns, evtFrom=from, evtPkt=pkt} args =
    if not $ L.null body
       then netSend $ replace "\\n" "\n" body <+> "\0" 
       else dAmnSayTo ns from "You must give a packet to send."
       where body = pktBody $ subPkt pkt
