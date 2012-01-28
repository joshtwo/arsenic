{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}

module Arsenic.Network (
    -- * Interacting with the network
    Network(..),
    NetworkIO,
    DAmnNetwork,
    makeNetwork,
    netRead,
    netSend,
    netReadPkts,
    netReconnect,
    dAmnClient,
    dAmnClientArgs,
    dAmnLogin,
    dAmnJoin,
    dAmnPart,
    dAmnTopic,
    dAmnTitle,
    dAmnSay,
    dAmnBan,
    dAmnUnban,
    dAmnKick,
    dAmnKill,
    dAmnGet,
    dAmnPromote,
    dAmnQuit,
    dAmnPong,
    -- * Working with packets
    Packet(..),
    makePacket,
    makePacketSep,
    subPkt,
    parseTablumps,
    (?),
    -- * Namespace-related data
    Namespace(..),
    Member(..),
    Header(..),
    MemberList,
    NamespaceList,
    deformChanName,
    formatChanName 
    ) where

import Network
import Text.Printf (hPrintf)
import System.IO
import Data.Map (Map, (!))
import qualified Data.Array as A
import qualified Data.Map as M
import Control.Monad.State
import Data.List
import Text.Regex.PCRE
import Debug.Trace (trace)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B

import Arsenic.Types
import Arsenic.Misc (tokenise, (<+>), (<:>), replace)

instance DAmnNetwork NetworkIO where
    netRead =
        do net <- get
           let fd = netSock net
           liftIO $ hFlush fd
           liftIO $ L.hGetContents fd 
    netSend str =
        do fd <- gets netSock
           liftIO $ L.hPut fd str
           liftIO $ hFlush fd -- this won't send immediately otherwise

-- Create a new connection to a dAmn server.
makeNetwork :: String       -- ^ The domain of the server to connect to.
            -> Int          -- ^ The port number of the server.
            -> Maybe ByteString -- ^ An optional name for the server.
            -> IO Network   -- ^ The new network connection.
makeNetwork domain port name =
    do let port' = PortNumber $ fromIntegral port
       fd <- connectTo domain port'
       return $ Network fd domain port name M.empty

-- | Read a single packet from the dAmn server.
netReadPkts :: DAmnNetwork n => n [Packet]
netReadPkts = do str <- netRead
                 return . map (makePacket . parseTablumps) $ L.split '\0' str

-- | Reconnect to the dAmn server.
netReconnect :: DAmnNetwork n => Network -> n Network
netReconnect net = do liftIO . hClose $ netSock net
                      fd' <- liftIO
                           $ connectTo (netDomain net)
                                       (PortNumber . fromIntegral $ netPort net)
                      return $ net {netSock=fd'}

-- | Convert a packet in string form into a Packet datatype.
-- An example:
--
-- >>> makePacket "send chat:Botdom\ne=not privileged\n"
-- Packet {pktCmd = "send",
--         pktParam = "chat:Botdom",
--         pktArgs = fromList [("e","not privileged")],
--         pktBody = "",
--         pktRaw = "send chat:Botdom\ne=not privileged\n"}
--
makePacket :: ByteString -- ^ The packet to convert.
           -> Packet -- ^ The packet converted into a Packet datatype.
makePacket = makePacketSep '='

-- | Convert a packet in string form into a 'Packet', specifying
-- the argument seperator.
makePacketSep :: Char       -- ^ The argument seperator.
              -> ByteString -- ^ The packet to convert.
              -> Packet     -- ^ The packet converted into a Packet datatype.
makePacketSep sep str =
    let body = case subIndex "\n\n" str of
                    Nothing -> ""
                    Just n  -> L.drop (fromIntegral n+2) str
        lines' = L.lines str
        (cmd,param) = break' ' ' $ if hasHeader then head lines' else L.empty
        args = map (break' sep) . (if hasHeader then drop 1 else id)
             $ takeWhile (not . L.null) lines'
        break' delim = fmap (L.drop 1) . L.break (delim==)
        hasHeader = not (null lines') && not (L.elem sep (head lines'))
        subIndex substr str = findIndex (L.isPrefixOf substr) (L.tails str)
    in Packet cmd param (M.fromList args) body str

-- | Use the given packet's body to create another parsed packet.
subPkt :: Packet -> Packet
subPkt = makePacket . pktBody

-- | Grab an argument from a packet. This is an alternative to the longer
-- @pktArgs pkt ! arg@, allowing you to use @pkt ? arg@.
(?) :: Packet -> ByteString -> ByteString
pkt ? arg = (pktArgs pkt) ! arg

-- | Deform a channel. Because this deforms private chat names into @person,
-- you must supply the client's username so it can be removed.
--
-- For example, if this client is signed in as \"deviant-garde\":
--
-- >>> deformChanName "pchat:deviant-garde:someguy" "deviant-garde"
-- "@someguy"
--
-- This function is what 'deformChanName' is built on top of.
deformChanName :: ByteString -- ^ The formatted channel.
               -> ByteString -- ^ The name of the client.
               -> ByteString -- ^ The deformed channel.
deformChanName "" user = ""
deformChanName chan user =
    case L.head chan of
         '#' -> chan
         '@' -> chan
         _   -> case fst $ L.break (':'==) chan of
                     -- remove "pchat:", our client's name, and attach a @
                     "pchat" -> '@'
                             <:> (head.delete user.L.split ':' $ L.drop 6 chan)
                     "chat" -> '#' <:> L.drop 5 chan
                     _ -> '#' <:> chan

-- | Format a channel. Because this formats private chat names from @person,
-- you must supply the client's username so it can be re-added.
formatChanName :: ByteString -- ^ The deformed channel.
               -> ByteString -- ^ The name of the client.
               -> ByteString -- ^ The formatted channel.
formatChanName "" user = ""
formatChanName chan user =
    case L.head chan of
         '#' -> "chat:" <+> L.drop 1 chan
         '@' -> "pchat:" <+> (L.intercalate ":" $ sort [L.drop 1 chan, user])
         _   -> case fst $ L.break (':'==) chan of
                     "pchat" -> chan
                     "chat" -> chan
                     _ -> "chat:" <+> chan

-- | Join a namespace.
dAmnJoin :: DAmnNetwork n
         => ByteString -- ^ The channel to join.
         -> n ()
dAmnJoin chan = netSend $ "join " <+> chan <+> "\n\0"

-- | Part a namespace.
dAmnPart :: DAmnNetwork n
         => ByteString -- ^ The channel to part.
         -> n ()
dAmnPart chan = netSend $ "part " <+> chan <+> "\n\0"

-- | Say something in a namespace. Prefix a message with @"/me "@
-- to send an action.
dAmnSay :: DAmnNetwork n
        => ByteString -- ^ The channel to send it to.
        -> ByteString -- ^ The message to send.
        -> n ()
dAmnSay chan msg = netSend $ "send " <+> chan <+> "\n\n" <+>
                     (if L.take 4 msg == "/me "
                         then "action main\n\n" <+> L.drop 4 msg
                         else "msg main\n\n" <+> msg) <+> "\0"

-- | Send a non-parsed message. Usage is the same as 'dAmnSay', except you
-- are not able to send an action by prefixing the message with @"/me"@.
dAmnSayNp :: DAmnNetwork n => ByteString -> ByteString -> n ()
dAmnSayNp chan msg = netSend
                   $ "send " <+> chan <+> "\n\nnpmsg main\n\n" <+> msg <+> "\n\0"

-- | Promote someone in a namespace.
dAmnPromote :: DAmnNetwork n
            => ByteString -- ^ The channel to promote them in.
            -> ByteString -- ^ The person to promote.
            -> ByteString -- ^ The privclass to promote them to.
            -> n ()
dAmnPromote chan person pc =
    netSend $ "send "<+>chan<+>"\n\npromote "<+>person<+>"\n\n"<+>pc<+>"\0"

-- | Kick someone in a namespace.
dAmnKick :: DAmnNetwork n
         => ByteString -- ^ The channel to kick them in.
         -> ByteString -- ^ The person to kick
         -> ByteString -- ^ The reason for kicking them.
         -> n ()
dAmnKick chan person reason =
    netSend $ "kick "<+>chan<+>"\nu="<+>person<+>"\n\n"<+>reason<+>"\0"

-- | Kill a user on the dAmn server. This only works if you're an MN@.
dAmnKill :: DAmnNetwork n
         => ByteString -- ^ The person to kill.
         -> ByteString -- ^ The reason for killing them.
         -> n ()
dAmnKill person reason =
    netSend $ "kill login:"<+>person<+>"\n\n"<+>reason<+>"\0"

-- | Ban a person in a namespace.
dAmnBan :: DAmnNetwork n
        => ByteString -- ^ The channel to ban them in.
        -> ByteString -- ^ The person to ban
        -> n ()
dAmnBan chan person = netSend $ "send "<+>chan<+>"\n\nban "<+>person<+>"\n\0"

-- | Unban a person in a namespace. Usage is the same as 'dAmnBan'.
dAmnUnban :: DAmnNetwork n => ByteString -> ByteString -> n ()
dAmnUnban chan person = netSend $ "send "<+>chan<+>"\n\nunban "<+>person<+>"\n\0"

-- | Perform an \"admin\" command.
dAmnAdmin :: DAmnNetwork n
          => ByteString -- ^ The channel to send it to.
          -> ByteString -- ^ The \"admin\" command to send.
          -> n ()
dAmnAdmin chan cmd = netSend $ "send "<+>chan<+>"\n\nadmin\n\n"<+>cmd<+>"\n\0"

-- | Send a \"get\" packet.
dAmnGet :: DAmnNetwork n
        => ByteString -- ^ The channel to get the property from.
        -> ByteString -- ^ The property to get.
        -> n ()
dAmnGet chan prop = netSend $ "get "<+>chan<+>"\np="<+>prop<+>"\n\0"

-- | Set the title of a channel.
dAmnTitle :: DAmnNetwork n
          => ByteString -- ^ The channel to set the title of.
          -> ByteString -- ^ The new title.
          -> n ()
dAmnTitle chan str = netSend $ "set "<+>chan<+>"\np=title\n\n"<+>str<+>"\n\0"

-- | Set the topic of a channel.
dAmnTopic :: DAmnNetwork n
          => ByteString -- ^ The channel to set the topic of.
          -> ByteString -- ^ The new topic.
          -> n ()
dAmnTopic chan str = netSend $ "set "<+>chan<+>"\np=topic\n\n"<+>str<+>"\n\0"

-- | Initiate the client-server handshake, passing an agent string.
dAmnClient :: DAmnNetwork n
           => ByteString -- ^ The agent to use.
           -> n ()
dAmnClient agent = netSend $ "dAmnClient 0.3\nagent=" <+> agent <+> "\n\0"

-- | Initiate the client-server handshake, passing an associative list
-- of arguments to use. Ex:
--
-- @dAmnClientArgs [("agent", "dAmn WebClient 0.7"),
--                  ("browser", "Internet Explorer 6"),
--                  ("url", "http://chat.deviantart.com/chat")]@
dAmnClientArgs :: DAmnNetwork n
               => [(ByteString, ByteString)] -- ^ The list of arguments to use.
               -> n ()
dAmnClientArgs xs = netSend $ "dAmnClient 0.3\n" <+> args <+> "\n\0"
                    where args = L.intercalate "\n"
                               $ map (\(x,y) -> x <+> "=" <+> y) xs

-- | Log in to the dAmn server.
dAmnLogin :: DAmnNetwork n
          => ByteString -- ^ The username of the account to log in to.
          -> ByteString -- ^ The authtoken of the account to log in to.
          -> n ()
dAmnLogin username pk = netSend $ "login "<+>username<+>"\npk="<+>pk<+>"\n\0"

-- | Disconnect from the dAmn server.
dAmnQuit :: DAmnNetwork n => n ()
dAmnQuit = netSend "disconnect\n\0"

-- | Send a \"pong\" packet in reply to a \"ping\" packet.
dAmnPong :: DAmnNetwork n => n ()
dAmnPong = netSend "pong\n\0"

-- | Parse the tablumps in a @ByteString@.
parseTablumps :: ByteString -> ByteString
parseTablumps str =
    let -- substitutes every "\<number>" with match number <number>
        replace' :: ByteString -> [ByteString] -> Int -> ByteString
        replace' str' [] count = if count == 1 then "" else str'
        replace' str' (match:xs) count =
            replace' (intercalate'
                   $ tokenise ('\\' <:> L.pack (show count)) str')
                     xs
                     (count + 1)
            where intercalate' list = L.intercalate match list
        checkForMatches :: ByteString -> ByteString -> Bool
        checkForMatches str regex =
            case str =~ regex :: (ByteString, ByteString, ByteString, [ByteString]) of
              (_,_,_,matches) ->
                  if null matches then False else True
        -- parses a single tablump given the string to parse, the pattern, and
        -- the replacement
        parseLump :: ByteString -> ByteString -> ByteString -> ByteString
        parseLump str' from to =
            case str' =~ from :: (ByteString, ByteString, ByteString, [ByteString]) of
              (before,middle,after,matches) ->
                  if not $ checkForMatches str' from
                     then parsedString
                     else parseLump parsedString from to
                  where replacement = replace' to matches 1
                        parsedString = before <+> replacement <+> after
        -- applies the tablump replacements
        replaceLumps :: ByteString -> [(ByteString, ByteString)] -> ByteString
        replaceLumps str' [] = str'
        replaceLumps str' ((from,to):xs) =
            replaceLumps (parseLump str' from to) xs
        -- list of replacements
        lumps = [("&(b|i|s|u|sub|sup|code|ul|ol|li|p|bcode|br)\t", "<\\1>")
                ,("&/(b|i|s|u|sub|sup|code|ul|ol|li|p|bcode|iframe|embed|abbr)\t",
                  "</\\1>")
                ,("&emote\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t", "\\1")
                ,("&acro\t(.*)\t(.*)&/acro\t",
                  "<acronym title=\"\\1\">\\2</acronym>")
                ,("&abbr\t([^\t]+)\t", "<abbr title=\"\\1\">")
                ,("&link\t([^\t]*)\t([^\t]*)\t&\t", "\\1 (\\2)")
                ,("&link\t([^\t]*)\t&\t", "\\1")
                ,("&a\t(.*)\t(.*)\t(.*)&\\/a\t",
                  "<a href=\"\\1\" title=\"\\2\">\\3</a>")
                ,("&(iframe|embed)\t([^\t]+)\t([^\t]*)\t([^\t]*)\t",
                  "<\\1 src=\"\\2\" width=\"\\3\" height=\"\\4\">")
                ,("&img\t([^\t]+)\t([^\t]*)\t([^\t]*)\t",
                  "<img src=\"\\1\" width=\"\\2\" height=\"\\3\" />")
                ,("&thumb\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t([^\t]+)\t",
                  ":thumb\\1:")
                ,("&dev\t([^\t])\t([^\t]+)\t", ":dev\\2:")
                ,("&avatar\t([^\t]+)\t([^\t]+)\t", ":icon\\1:")]
    in replace "<br>" "\n" $ replaceLumps str lumps
