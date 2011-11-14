{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Snap.Snaplet.Gruze (

    -- defines the public interface for the Gruze object store
    
    -- * Snaplet interface
    GruzeSnaplet(..), GrzHandler(..), gruzeInit,
    
    -- * Pure functions for atoms, boxes and objects
    
    -- ** Constructors
    -- (note that the internals of GrzObj are not exported)

    GrzAtomKey(..), GrzObj, GrzBox(..), GrzRel(..), GrzSetters,
    
    -- ** Classes
    
    GrzAtomKeyClass(..),
    
    GrzObjClass(getID,unwrapObj,shrinkObj,isValidObj,getType,
        getTimeCreated,getTimeUpdated,getContainer,getOwner,getSite,
        setEnabled, isEnabled, getMetadata, setMetadata), 
        
    GrzContainerClass(..), GrzOwnerClass(..), GrzSiteClass(..),
    GrzAtomBoxClass(..),
    
    -- ** Types
    GrzAtom, GrzAtomBox, 
    GrzAtomIntKey, GrzAtomTextKey, GrzAtomBoolKey, GrzAtomAtomKey,
    GrzObjBox, GrzInt, GrzString, GrzKey, GrzLogLevel(..),
    GrzDatabaseType(..),

    -- ** Atom converters
    -- *** Strings
    atomToString, maybeAtomToString, safeAtomToString, forceAtomToString, 
    ppAtom, stringToAtom, isStringAtom,
    -- *** Ints
    atomToInt, maybeAtomToInt, safeAtomToInt, intToAtom, isIntAtom,
    -- *** Bools
    atomToBool, maybeAtomToBool, boolToAtom, isBoolAtom,
    -- *** Files
    atomToFileID, maybeAtomToFileID, isFileAtom,
    
    -- ** Empty constructors
    emptyAtomBox, emptyObj, emptyBareObj,
    
    -- ** Type setter   
    setType,
    
    -- ** Attempt to wrap object
    maybeConvert,
    
    -- ** Pretty printers    
    ppAtomBox, ppObj, ppObjFull,    
    
    -- ** Atom boxes
    
    addAtomPair, addAtomPairs, removeFromAtomBox, getKeysFromAtomBox,
    
    -- ** Field lists
    fields, noMetadata, allMetadata,
    
    -- ** Object boxes
    setObj, getObj, maybeGetObj, removeFromObjBox, getKeysFromObjBox, 
        
    -- * Query combinators
    
    -- ** Types
    GrzQueryDef, GrzRelDir(..), GrzRef(..), GrzOrderBy(..),
    
    -- ** Classes
    GrzQueryTypeClass(hasIn,hasBetween,hasOp),
    
    -- ** Booleans
    hasTrue, hasFalse,
    
    -- ** Filter by specific objects
    withObj, withObjs,
    
    -- ** Select by type 
    hasType, hasTypes,
    
    -- ** Select by enabled/disabled
    hasEnabled, hasDisabled,
    
    -- ** Select by the fixed relationships
    withOwner, withContainer, withSite,
    withOwners, withContainers, withSites,
    
    -- ** Select by general relationships
    hasRel,
    
    -- ** Select by searchable fields
    hasSearchable,
    
    -- ** Has the specified metadata names defined
    hasData,
    
    -- return objects with the given metadata in the results
    -- withData,

    -- * Object IO
    
    -- ** Data handle
    GrzHandle(..), setDefaultSite, setThumbDefs, setLogLevel,
    
    -- ** File atom handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- ** Manage individual objects
    createObj, saveObj, delObj, delObjByID, disableObj, enableObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,
    
    -- ** Query retrieval functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjSumCount, getObjsAggByObjCount, getObjsAggByObjSumCount, 
    setSearchable,    
    
    -- ** Relationship IO
    addRel, delRel, checkRel, hasContainer, hasOwner, hasSite,
    
    -- ** Utility functions    
    grzLog, grzCommit, grzRollback, grzQuery, grzRunSql, grzConfigToHandle,
    
    -- rexport some basic modules
    
    module Data.Maybe,
    module Data.Typeable,
    module Data.List.Split 
    
) where

import Snap.Snaplet.Gruze.Box
import Snap.Snaplet.Gruze.IO
import Snap.Snaplet.Gruze.QueryDef
import Snap.Snaplet.Gruze.Utility
import Snap.Snaplet.Gruze.Types

import Data.Maybe
import Data.Typeable
import Data.List.Split
import Snap.Snaplet
-- import Snap.Snaplet.Hdbc
import Database.HDBC
import Data.Lens.Template
import Control.Monad.Trans (liftIO)

gruzeInit :: IConnection c => IO c -> GrzHandle -> SnapletInit b (GruzeSnaplet c)
gruzeInit conn grzC = makeSnaplet "gruze" "Gruze object store" Nothing 
    $ do
        conn' <- liftIO conn
        return $ GruzeSnaplet conn' grzC
