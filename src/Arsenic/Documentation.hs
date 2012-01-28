{-# LANGUAGE OverloadedStrings #-}

module Arsenic.Documentation (
    DocProperties(..),
    DocMaker,
    docProperties,
    showDocs,
    addDocLine,
    addDocSub,
    setDocTitle,
    addConcatMap,
    addList,
    addOrdList,
    addAssocList
) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (mapAccumL)
import Control.Monad.State

import Arsenic.Types
import Arsenic.Misc

-- | Get the documentation properties from a @DocMaker@. Same as @execState@
-- except more type strict.
docProperties :: DocMaker -> DocProperties
docProperties docs = execState docs $ DocProperties [] Nothing ""

-- | Display the documentation page.
showDocs :: DocMaker -> ByteString
showDocs docs =
    let props = docProperties docs
        body = docString props
        subs = docSubs props
    in (case docTitle props of
          Nothing -> ""
          Just title -> "<b><u>" <+> title <+> "</u></b>\n")
   <+> fst (mapAccumL (\body' (key,val) -> (replace key val body', ())) body subs)

-- | Add a line of text to the documentation body.
addDocLine :: ByteString -> DocMaker
addDocLine line =
    do body <- gets docString
       modify (\props -> props {docString = body <+> line <+> "\n"})

-- | Add a substitution to the documentation properties.
addDocSub :: ByteString -> ByteString -> DocMaker
addDocSub from to =
    do subs <- gets docSubs
       modify (\props -> props {docSubs = (from, to) : subs})

-- | Set the title of a documentation page.
setDocTitle :: ByteString -> DocMaker
setDocTitle title = modify (\props -> props {docTitle = Just title})

-- | Map a function over a list of @ByteStrings@, concatenate them, and add
-- the string to the documentation body.
addConcatMap :: (ByteString -> ByteString) -> [ByteString] -> DocMaker
addConcatMap f list =
    do body <- gets docString
       modify (\props -> props {docString = body <+> L.concat (map f list)})

-- | Put every item in the list in <li> tags and add it to
-- the documentation body.
addListItems :: [ByteString] -> DocMaker
addListItems = addConcatMap (\item -> "<li>" <+> item <+> "</li>")

-- | Add an unordered list of items to the documentation body.
addList :: [ByteString] -> DocMaker
addList list = addDocLine "<ul>" >> addListItems list >> addDocLine "</ul>"

-- | Add an ordered list of items to the documentation body.
addOrdList :: [ByteString] -> DocMaker
addOrdList list = addDocLine "<ol>" >> addListItems list >> addDocLine "</ol>"

-- | Take an associative list, and turn it into a formatted key-value list.
addAssocList :: [(ByteString, ByteString)] -> DocMaker
addAssocList = addList . map (\(key,val) -> "<b>" <+> key <+> "</b> - " <+> val)
