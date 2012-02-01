{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}

module Arsenic.Types (
    DAmnNetwork(..),
    Network(..),
    NetworkIO,
    ErrorState,
    SettingsStorer(..),
    Settings,
    SettingsFile(..),
    SettingsError(..),
    Client(..),
    ClientIO,
    Event(..),
    EventMatch(..),
    EventHook,
    EventHookList,
    EventInfo(..),
    PluginData(..),
    Plugin(..),
    PluginList,
    PluginIO,
    Command(..),
    CommandHook,
    CommandList,
    PrivGroup(..),
    GroupList,
    UserList,
    Packet(..),
    Namespace(..),
    Member(..),
    Header(..),
    MemberList,
    NamespaceList,
    PcList,
    DocProperties(..),
    DocMaker,
    CliPrinter(..),
    ) where

import Control.Monad.State
import Control.Monad.Error
import System.IO
import Data.Map (Map)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ConfigFile (ConfigParser)
import Control.Monad.Error (Error)
import qualified Data.ByteString.Lazy.Char8 as L

-- | This data type is used to manage the connection to a dAmn server.
data Network = 
    Network { netSock   :: Handle -- ^ Handle connected to the dAmn server.
            , netDomain :: String -- ^ The domain nane to connect to.
            , netPort   :: Int    -- ^ Port the dAmn server is on.
            , netName   :: Maybe ByteString  -- ^ The name of the server.
            , nsList    :: NamespaceList -- ^ The namespaces the client is in.
            }

type NamespaceList = Map ByteString Namespace

-- | Information on a chatroom, referred to as a \"namespace\" by dAmn.
-- Throughout the documentation, \"namespace\" refers to an actual room, while
-- \"channel\" refers to the name of a namespace.
data Namespace =
    Namespace { nsName    :: ByteString -- ^ The name of the namespace.
              , nsMembers :: MemberList -- ^ The people in the namespace.
              , nsPcList  :: PcList     -- ^ The list of privclasses.
              , nsTopic   :: Header     -- ^ Topic of the namespace.
              , nsTitle   :: Header     -- ^ Title of the namespace.
              }

type MemberList = Map ByteString Member

type PcList = Map ByteString ByteString

type AutojoinList = [ByteString]

-- | An account on the dAmn server. This information pertains to one namespace.
data Member =
    Member { memberName   :: ByteString -- ^ The member's username.
           , memberSymbol :: ByteString -- ^ The member's symbol.
           , memberPc     :: ByteString -- ^ The member's privclass.
           , memberIcon   :: ByteString -- ^ The type of icon the member has.
           , realName     :: ByteString -- ^ The member's real name. 
           , typeName     :: ByteString -- ^ The member's account type.
           , memberGpc    :: ByteString -- ^ The member's global privclass.
           }

-- | A title or topic in a namespace.
data Header =
    Header { header   :: ByteString -- ^ The actual text in the header.
           , headerBy :: ByteString -- ^ Who the header was set by.
           , headerTs :: ByteString -- ^ When the header was set.
           }

-- | A packet received from the dAmn server.
data Packet =
    Packet { pktCmd   :: ByteString
           , pktParam :: ByteString
           , pktArgs  :: Map ByteString ByteString
           , pktBody  :: ByteString
           , pktRaw   :: ByteString
           } deriving (Show)

-- | This type synonym is a 'ReaderT' monad that has a 'Network' as its
-- environment and returns something in the @IO@ monad. Used to do any I/O
-- involving the dAmn server.
type NetworkIO = StateT Network IO

-- | This class is for any monad that can read from the network.
class (Monad n, MonadIO n) => DAmnNetwork n where
    -- | Read a string from the dAmn server until arriving at a null
    -- character and return it.
    netRead :: n ByteString
    -- | Send a string to the dAmn server.
    netSend :: ByteString -> n ()

-- | The default settings file datatype.
data SettingsFile =
    SettingsFile { sfParser :: ConfigParser -- ^ The internal config parser.
                 , sfName   :: ByteString   -- ^ The location to save settings.
                 }

-- | This type holds the errors that occur when dealing with settings.
newtype SettingsError = SettingsError { settingsError :: String }

-- | A combination of ErrorT monad using e as the error and @State@ as the
-- innerlying monad, s being its environment.
type ErrorState e s = ErrorT e (State s)

-- | The default settings monad type.
type Settings = ErrorState SettingsError SettingsFile

-- | A class for datatypes that manage settings. A complete implementation
-- requires either @getSettingStr@ or @getSetting@, and either @putSettingStr@
-- or @putSetting@.
class Error e => SettingsStorer e s where
    -- | Retrieve a setting from the settings storer with @String@s.
    getSettingStr :: String                -- ^ The name of the setting.
                  -> ErrorState e s String -- ^ The value of the setting.
    getSettingStr = liftM L.unpack . getSetting . L.pack
    -- | Retrieve a setting from the settings storer with @ByteString@s.
    getSetting :: ByteString                -- ^ The name of the setting.
               -> ErrorState e s ByteString -- ^ The value of the setting.
    getSetting = liftM L.pack . getSettingStr . L.unpack
    -- | Add a setting to the settings storer with @String@s.
    putSettingStr :: String   -- ^ The name of the setting.
                  -> String   -- ^ The value of the setting.
                  -> ErrorState e s ()
    putSettingStr name val = putSetting (L.pack name) (L.pack val)
    -- | Add a setting to the settings storer with @ByteString@s.
    putSetting :: ByteString   -- ^ The name of the setting.
               -> ByteString   -- ^ The value of the setting.
               -> ErrorState e s ()
    putSetting name val = putSettingStr (L.unpack name) (L.unpack val) 

-- | This holds the client's state.
data Client =
    Client { cSettings   :: SettingsFile -- ^ The client's settings.
           , cUsers      :: UserList     -- ^ The user list.
           , cGroups     :: GroupList    -- ^ The group list.
           , cAutojoin   :: AutojoinList -- ^ The bot's autojoin list.
           , cVersion    :: ByteString   -- ^ The client's version.
           , cNetwork    :: Network      -- ^ The network the client is connected to
           , cQuit       :: Bool         -- ^ Whether or not to quit.
           , cReconnect  :: Bool         -- ^ Whether or not to restart.
           , cPlugins    :: PluginList   -- ^ The plugins loaded.
           , cPrinter    :: CliPrinter   -- ^ The methods used for printing.
           }

-- | This type synonym is a 'ReaderT' monad that has a 'Client' as its
-- environment and returns something in the @IO@ monad. Used to do any I/O
-- involving the client.
type ClientIO = StateT Client IO

-- | This type specifies the events you can hook functions to.
data Event = AnyEvent
           | Recv
           | RecvJoin
           | RecvPart
           | RecvMsg
           | RecvAction
           | RecvPrivchg
           | RecvKicked
           | RecvAdmin
           | AdminCreate
           | AdminUpdate
           | AdminRename
           | AdminRemove
           | AdminMove
           | AdminShow
           | AdminShowPc
           | AdminShowUsers
           | Property
           | PropertyTitle
           | PropertyTopic
           | PropertyMembers
           | PropertyPc
           | PropertyInfo
           | DAmnServer
           | LoginError
           | JoinError
           | PartError
           | SendError
           | GetError
           | SetError
           | KickError
           | Kicked
           | Disconnect
           | Ping deriving (Eq, Ord, Show)

-- | The type for event hooks.
type EventHook = EventInfo -> ClientIO ()

-- | The type for commands.
type CommandHook = EventInfo -> [ByteString] -> ClientIO ()

-- | This holds the list of event hooks the client is to process.
-- In the pair, the first value is the ID of the hooked event, while the
-- second value is the event with the aforementioned ID.
type EventHookList = Map Int (Event, EventHook)

-- | This is used to match against different packets and tie them to an event.
data EventMatch = Anything
                | Cmd ByteString
                | Param ByteString
                | Head ByteString ByteString
                | Args [(ByteString, ByteString)]
                | CmdArgs ByteString [(ByteString, ByteString)]
                | ParamArgs ByteString [(ByteString, ByteString)]
                | HeadArgs ByteString ByteString [(ByteString, ByteString)]

-- | A plugin used with the extension system.
class PluginData a where
     -- | The name of the plugin.
     pName :: a -> ByteString
     -- | The version of the plugin.
     pVer  :: a -> ByteString
     -- | A description of the plugin.
     pDesc :: a -> ByteString
     -- | The commands associated with the plugin.
     pCmds :: a -> CommandList
     -- | The events associated with the plugin.
     pEvts :: a -> EventHookList
     -- | Modify the plugin's commands.
     setCmds :: a -> CommandList -> a
     -- | Modify the plugin's events.
     setEvts :: a -> EventHookList -> a
     -- | This is ran when the plugin is initialized.
     pInit :: a -> Plugin -> ClientIO Plugin

-- | A datatype that stores plugins. Anything extending the 'PluginData' class
-- can be stored in this type.
data Plugin = forall a. PluginData a => Plugin { pluginData :: a }

-- | Stores the list of plugins. The first element is the plugin name, and
-- the second element is the 'Plugin' with the aforementioned name.
type PluginList = Map ByteString Plugin

-- | This type synonym is a 'ReaderT' monad that has @(Client, Plugin)@ as its
-- environment and returns something in the @IO@ monad. Used to manage plugins
-- and do I/O involving the plugins and the client.
type PluginIO = StateT (Client, Plugin) IO

-- | This holds the information on a triggered event.
data EventInfo =
    EventInfo { evtPlugin :: Plugin
              , evtPkt    :: Packet
              , evtType   :: Event
              , evtFrom   :: ByteString
              , evtNs     :: ByteString
              , evtMsg    :: ByteString
              }

-- | A command used with the extension system.
data Command =
    Command { cmdName  :: ByteString -- ^ The name of the command
            , cmdPrivs :: PrivGroup  -- ^ The minimum privs needed to use it
            , cmdHelp  :: Maybe DocMaker -- ^ The help for the command
            , cmdHook  :: CommandHook -- ^ This triggers the command
            }

type CommandList = Map ByteString Command

-- | The privileges required to use a command.
data PrivGroup =
    PrivGroup { groupOrder :: Int
                -- ^ What level this group is.
                -- The higher the number, the more privileges.
              , groupName  :: ByteString -- ^ The name of this group.
              } deriving (Read, Show)

type GroupList = Map Int PrivGroup

type UserList = Map ByteString Int

-- | This is used to store the properties of documentation.
data DocProperties =
    DocProperties { docSubs   :: [(ByteString, ByteString)]
                  -- ^ The substitutions made in the documentation string.
                  , docTitle  :: Maybe ByteString
                  -- ^ The title of the documentation.
                  , docString :: ByteString
                  -- ^ The body of the documentation.
                  }

type DocMaker = State DocProperties ()

-- | This holds the functions used to print to the client's interface and
-- log messages.
data CliPrinter =
    CliPrinter { cliPrintMsg    :: ByteString -> ByteString -> ClientIO ()
               -- ^ Print a message to the screen and log it.
               , cliPrintNotice :: ByteString -> ByteString -> ClientIO ()
               -- ^ Print a notice to the screen and log it.
               , cliPrintErr    :: ByteString -> ByteString -> ClientIO ()
               -- ^ Print an error to the screen and log it.
               , cliLogMsg      :: ByteString -> ByteString -> ClientIO ()
               -- ^ Log a message.
               }
