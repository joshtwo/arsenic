# Arsenic v. 0.3
This is a Haskell bot for [dAmn (deviantART Messaging Network)](http://chat.deviantart.com/). To install this, run the following:

    $ git clone git://github.com/joshtwo/arsenic.git  # Get the code
    $ cd arsenic
    $ cabal update             # Update the list of packages cabal knows about

And either...

    # cabal install --global   # Install the "arsenic" program to /usr/local/bin (may need to run as root)

or...

    $ cabal install            # Installs "arsenic" to $HOME/.cabal/bin for use by the current user
                               # if doing this, make sure directory is in your $PATH variable

This will download, compile, and install a fresh copy of Arsenic from source to 

# Running Arsenic
Arsenic should be installed as `arsenic`. On the first run, you'll be prompted to configure the bot. An example configuration:

    $ arsenic
    ** No settings file found.
    ** Please configure your bot with the following information:
    Username: myawesomebot 
    Password: supersecret
    Bot owner (your dA username): deviant-garde
    Trigger: `  
    Channels to join on startup (seperate with spaces): Botdom SomeOtherRoom TheHangout
    Do you want the bot to log messages? [Y/n]: y
    Do you want to set a timestamp in strftime format?
    If you don't know, pick "no".  [y/N]: y
    Timestamp (default %H:%M:%S): %I:%M:%S %p
    Would you like to pick a server to connect to? If you don't know, pick "no". [y/N]: n

A few additional notes:

* Logged messages will be in the $HOME/.arsenic/logs directory.
* You can get more information on the timestamp format (including its Haskell-specific formatting directives) [here](http://www.haskell.org/ghc/docs/latest/html/libraries/time/Data-Time-Format.html).
* If you want your bot to connect to a non-standard dAmn server (such as llAma) hosted off of deviantART, then you can give the domain name and port number of the server.

For example:

    $ arsenic
    ...
    Would you like to pick a server to connect to? If you don't know, pick "no". [y/N]: y
    Domain name (default chat.deviantart.com): llama.fbi.gov             
    Port (default 3900): 12345
    Server Name (hit enter for none): Top Secret FBI dAmn Server

* There may be more information on [Arsenic's Botdom wiki page](http://www.botdom.com/wiki/Arsenic).
* If these resources don't answer your questions, try asking them in the [#Botdom](http://chat.deviantart.com) chatroom on dAmn.
