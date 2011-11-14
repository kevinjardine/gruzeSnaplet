{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Gruze.IO (
    GrzHandle(..), 
    
    grzLog, grzCommit, setDefaultSite, setThumbDefs, setLogLevel,
    
    -- query functions
    getObjs, getUnwrappedObjs, getBareObjs, getUnwrappedBareObjs, getObjIDs,
    getObjCount, getObjSumCount, getObjsAggByObjCount, getObjsAggByObjSumCount, 
    setSearchable,
    
    -- object IO
    createObj, saveObj, delObj, delObjByID, disableObj, enableObj,
    loadObj, maybeLoadObj, maybeLoadContainer, maybeLoadOwner, maybeLoadSite,

    -- file handler
    createFileAtom, maybeGetFileMetadata, maybeGetFileContent, maybeGetFileThumb,
    
    -- relationship IO
    addRel, delRel, checkRel, hasContainer, hasOwner, hasSite
) where

-- functions to manage objects, string handles and file handles

import Snap.Snaplet.Gruze.Box
import Snap.Snaplet.Gruze.Types
import Snap.Snaplet.Gruze.QueryDef
import Snap.Snaplet.Gruze.Handles
import Snap.Snaplet.Gruze.Query
import Snap.Snaplet.Gruze.Utility

import Snap.Snaplet.Hdbc
import Database.HDBC(IConnection)
--import Database.HDBC.Sqlite3
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import qualified Data.Map as Map
--import Control.Monad (forM, when)
--import Control.Monad.Reader(liftIO,ask)
import Data.Maybe
import Data.Typeable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

import Control.Monad (forM)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as S
import qualified Data.Text as T

-- TODO: put constraints in object table to make sure that the owner, container and site IDs
-- can only be existing objects or 0

-- database API

-- some useful pure functions

atomToStorageInt :: GrzAtom -> Int
atomToStorageInt (GrzAtomInt i) = 0
atomToStorageInt (GrzAtomBool b) = 1
atomToStorageInt (GrzAtomString s) = 2
atomToStorageInt (GrzAtomFile s) = 3

getHandleDict :: Maybe GrzQuery -> [(GrzString,Int)]
getHandleDict (Just (((query,queryType),handleDict),(needs,values))) = handleDict
getHandleDict _ = []

-- the Object IO functions

-- handle functions
-- | Set default site in handle.    
setDefaultSite :: GrzSiteClass os => 
    os              -- ^ site
    -> GrzHandle    -- ^ current handle
    -> GrzHandle    -- ^ new handle
setDefaultSite site grzH  =               
    grzH {grzDefaultSite = unwrapObj site}

-- | Configure image thumbnail settings.    
setThumbDefs :: 
    [(GrzString,GrzString)]   -- ^ list of (size,param) pairs
    -> GrzHandle        -- ^ current handle
    -> GrzHandle        -- ^ new handle
setThumbDefs td grzH =               
    grzH {grzThumbDefs = td}

-- | Set Gruze logging level.      
setLogLevel ::
    GrzLogLevel     -- ^ log level
    -> GrzHandle    -- ^ current handle
    -> GrzHandle    -- ^ new handle
setLogLevel level grzH =               
    grzH {grzLogLevel = level}
        
-- metadata handler functions
        
-- TODO: the next function seems redundant - just replace it with grzInsertMetadata
grzAddMetadata :: IConnection c => Int -> Int -> (GrzString,[GrzAtom]) -> GrzHandler b c Integer
grzAddMetadata guid h (s,atoms) = grzInsertMetadata guid h atoms
            
grzInsertMetadata :: IConnection c => Int -> Int -> [GrzAtom] -> GrzHandler b c Integer
grzInsertMetadata guid h (a:as) =
    do
        grzRunSql query $ (map toSql [atomToStorageInt a,guid,h,safeAtomToInt a]) ++ [toSql $ safeAtomToString a]
        grzInsertMetadata guid h as
    where
        query = "INSERT INTO metadata (metadataType,objectGuid,nameId,integerValue,stringValue) values(?,?,?,?,?)" 
grzInsertMetadata _ _ [] = return 0

grzDeleteMetadata :: IConnection c => Int -> Int -> GrzHandler b c ()
grzDeleteMetadata guid h =
    do
        grzRunSql query $ (map toSql [guid,h])
    where
        query = "DELETE FROM metadata WHERE objectGuid = ? AND nameId = ?"

-- TODO: rewrite this using mapM or forM
grzAddMetadataArray :: IConnection c => Int -> [(GrzString,[GrzAtom])] -> GrzHandler b c Integer 
grzAddMetadataArray guid (d:ds) =
    do
        h <- getStringHandle (fst d)
        grzAddMetadata guid h d
        grzAddMetadataArray guid ds
grzAddMetadataArray _ [] = return 0

-- TODO: rewrite this using mapM or forM
grzSetMetadataArray :: IConnection c => Int -> [(GrzString,[GrzAtom])] -> GrzHandler b c Integer 
grzSetMetadataArray guid (d:ds) =
    do
        h <- getStringHandle (fst d)
        grzDeleteMetadata guid h
        grzAddMetadata guid h d
        grzSetMetadataArray guid ds
grzSetMetadataArray _ [] = return 0

-- higher level object functions

{-| Takes a type constructor and a setter function
    and saves the object to the datebase, returning the new wrapped object.
-} 
createObj :: (Typeable o, GrzObjClass o, IConnection c) => 
       (GrzObj -> o)        -- ^ type wrapper
    -> (GrzObj -> GrzObj)   -- ^ setter functions
    -> GrzHandler b c o                 -- ^ new object
createObj w p =
    if T.null t then 
        return (w emptyObj)
    else do
        ptime <- liftIO getPOSIXTime
        let time = floor ptime
        h <- getStringHandle t
        grzRunSql query $ map toSql [h,ownerID,containerID,siteID,time,time,enabled]
        guid <- getLastInsertId
        let theObj = obj {
            objID = guid,
            objType = t,
            objTimeCreated = time, 
            objTimeUpdated = time,
            objOwner = shrinkObj $ getOwner obj,
            objContainer = shrinkObj $ getContainer obj,
            objSite = shrinkObj $ getSite obj
        }
        grzLog DebugLogLevel $ "in createObj, created the object " `T.append` (ppObjFull theObj)

        -- TODO: perhaps f could return an error condition triggering a rollback?
        f <- grzAddMetadataArray guid (Map.toList $ objMetadata obj)
        grzCommit
        return $ w theObj
    where
        obj = p emptyObj
        enabled = if isEnabled obj then 1 else 0
        siteID = getID $ getSite obj
        ownerID = getID $ getOwner obj
        containerID = getID $ getContainer obj
        t = objWrapperToString (w emptyObj) 
        query = "INSERT INTO objects(objectType,ownerGuid,containerGuid,siteGuid,timeCreated,timeUpdated,enabled) values(?,?,?,?,?,?,?)"         

{-|
  Loads the current data for an object from the database,
  including the specified metadata.
-}        
loadObj :: (GrzObjClass o, IConnection c) => 
    o           -- ^ object to refresh
    -> [GrzAtomKey GrzString]     -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> GrzHandler b c o         -- ^ The returned object
loadObj obj needs =
    do
        objs <- getUnwrappedObjs qd needs [] 1 0
        return $ case objs of
                    [] -> replaceObj obj emptyObj
                    a  -> replaceObj obj (head a)
    where
        qd = (withObjs [obj])

-- | Reloads an unwrapped object as a wrapped one.     
maybeLoadObj :: (GrzObjClass o, IConnection c) => 
    (GrzObj -> o)    -- ^ type wrapper
    -> GrzObj           -- ^ The unwrapped object to load
    -> [GrzAtomKey GrzString]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> GrzHandler b c (Maybe o)     -- ^ The returned object
maybeLoadObj w obj needs =
    if isValidObj obj
        then do
            o2 <- loadObj (w obj) needs
            if isValidObj o2
                then
                    return $ maybeConvert w (unwrapObj o2)
                else
                    return Nothing           
        else
            return Nothing

-- | Loads the container for the given object if it exists.
maybeLoadContainer :: (GrzObjClass o, GrzObjClass oc, GrzContainerClass oc, IConnection c) => 
    (GrzObj -> oc)   -- ^ expected type wrapper for container
    -> o                -- ^ object which we are returning the container for
    -> [GrzAtomKey GrzString]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> GrzHandler b c (Maybe oc)    -- ^ maybe returns the container
    
maybeLoadContainer w obj needs = maybeLoadObj w (getContainer obj) needs

-- | Loads the owner for the given object if it exists.
maybeLoadOwner :: (GrzObjClass o, GrzOwnerClass oo, IConnection c) => 
    (GrzObj -> oo)   -- ^ expected type wrapper for owner
    -> o                -- ^ object which we are returning the owner for
    -> [GrzAtomKey GrzString]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> GrzHandler b c (Maybe oo)    -- ^ maybe returns the owner
    
maybeLoadOwner w obj needs = maybeLoadObj w (getOwner obj) needs

-- | Loads the site for the given object if it exists.
maybeLoadSite :: (GrzObjClass o, GrzSiteClass os, IConnection c) => 
    (GrzObj -> os)   -- ^ expected type wrapper for site
    -> o                -- ^ object which we are returning the site for
    -> [GrzAtomKey GrzString]         -- ^ metadata names needed (use fields function to provide this in a typesafe way)
    -> GrzHandler b c (Maybe os)    -- ^ maybe returns the site
    
maybeLoadSite w obj needs = maybeLoadObj w (getSite obj) needs

-- TODO: need some exception handling here
-- | Saves the given object data to the database and resets the timeUpdated field.   
saveObj :: (GrzObjClass o, IConnection c) => 
    o        -- ^ object to save   
    -> GrzHandler b c o     -- ^ updated object
saveObj o =
    do 
        ptime <- liftIO getPOSIXTime
        let time = floor ptime
        grzRunSql query $ map toSql [ownerID,containerID,siteID,time,guid]
        let theObj = obj {objTimeUpdated = time}
        -- TODO: perhaps f could return an error condition triggering a rollback?
        f <- grzSetMetadataArray guid (Map.toList $ getMetadata theObj)
        grzCommit
        return $ replaceObj o theObj
    where
        obj = unwrapObj o
        guid = getID obj
        enabled = if isEnabled obj then 1 else 0
        siteID = getID $ getSite obj
        ownerID = getID $ getOwner obj
        containerID = getID $ getContainer obj
        query = "UPDATE objects SET ownerGuid = ?, containerGuid = ?, siteGuid = ?, timeUpdated = ? WHERE guid = ?"

{-|
  Deletes all the object data including metadata and relationships and recursively
  deletes all the objects that have this object as a container, owner or site.
-}
delObj :: (IConnection c, GrzObjClass o) => 
    o        -- ^ object to delete
    -> GrzHandler b c Bool  -- ^ success status
delObj obj = do
    r <- delObjByID (getID obj)
    if r
        then do
            grzCommit
            return r
        else do
            grzRollback
            return r

-- delObjByID :: (MonadState (GruzeSnaplet c) m, IConnection c) => Int -> m Bool
delObjByID :: IConnection c =>  Int ->  GrzHandler b c Bool 
delObjByID 0 = return True
delObjByID guid =
    do    
        -- delete the metadata 
        grzRunSql metadata_query $ [toSql guid]
        
        -- delete the relationships
        grzRunSql relationship_query $ [toSql guid,toSql guid]
        
        -- delete the owned objects
        owned <- getObjIDs (withOwners [GrzObjID guid]) [] 0 0
        mapM_ (delObjByID) owned
        
        -- delete the contained objects
        contained <- getObjIDs (withContainers [GrzObjID guid]) [] 0 0
        mapM_ (delObjByID) contained
        
        -- delete the objects that have this object as a site
        sited <- getObjIDs (withSites [GrzObjID guid]) [] 0 0
        mapM_ (delObjByID) sited
        
        -- delete the object from the object table
        grzRunSql object_query $ [toSql guid]
        
        return True
    where
        metadata_query = "DELETE FROM metadata WHERE objectGuid = ?"
        relationship_query = "DELETE FROM relationships WHERE guid1 = ? OR guid2 = ?"
        object_query = "DELETE FROM objects WHERE guid = ?"
        
setEnableObj :: IConnection c => GrzObjClass o => Bool -> o -> GrzHandler b c Bool
setEnableObj state obj  = do
    r <- setEnableObjByID (if state then 1 else 0) (getID obj) 
    if r
        then do
            grzCommit
            return r
        else
            return r

setEnableObjByID :: IConnection c => Int -> Int -> GrzHandler b c Bool
setEnableObjByID _ 0 = return True
setEnableObjByID state guid  =
    do
        
        -- set the enable state on the owned objects
        owned <- getObjIDs (withOwners [GrzObjID guid]) [] 0 0
        mapM_ (setEnableObjByID state) owned
        
        -- set the enable state on the contained objects
        contained <- getObjIDs (withContainers [GrzObjID guid]) [] 0 0
        mapM_ (setEnableObjByID state) contained
        
        -- set the enable state on the objects that have this object as a site
        sited <- getObjIDs (withSites [GrzObjID guid]) [] 0 0
        mapM_ (setEnableObjByID state) sited
        
        -- set the enable state on the object itself
        grzQuery object_query $ [toSql state, toSql guid]       
        
        return True
    where
        object_query = "UPDATE objects SET enabled = ? WHERE guid = ?"

{-|
  Disables the object and recursively disables all the objects that
  have this object as a container, owner or site.
-}
disableObj :: IConnection c => GrzObjClass o => 
    o        -- ^ object to disable
    -> GrzHandler b c Bool  -- ^ success status   
disableObj obj = setEnableObj False obj

{-|
  Enables the object and recursively enables all the objects that
  have this object as a container, owner or site.
-}
enableObj :: IConnection c => GrzObjClass o => 
    o        -- ^ object to enable
    -> GrzHandler b c Bool  -- ^ success status     
enableObj obj = setEnableObj True obj
        
-- relationship functions                  

-- | Adds the relationship to the given objects (if it does not exist).
addRel :: (GrzObjClass o1, GrzObjClass o2, IConnection c) => 
    GrzRel   -- ^ relationship
    -> o1       -- ^ first object
    -> o2       -- ^ second object
    -> GrzHandler b c ()
addRel (GrzRel rel) obj1 obj2 =
    do
        b <- checkRel (GrzRel rel) obj1 obj2
        if b
            then
                return ()
            else do
                ptime <- liftIO getPOSIXTime
                let time = floor ptime
                h <- getStringHandle rel
                grzRunSql query $ map toSql [getID obj1,getID obj2,h,time]
                grzCommit
             
    where
        query = "INSERT INTO relationships(guid1,guid2,relationshipType,timeCreated) VALUES(?,?,?,?)"
        
-- | Removes the relationship from the given objects (if it exists).        
delRel :: (GrzObjClass o1, GrzObjClass o2, IConnection c) => 
    GrzRel   -- ^ relationship 
    -> o1       -- ^ first object
    -> o2       -- ^ second object 
    -> GrzHandler b c ()
delRel (GrzRel rel) obj1 obj2 =
    do
        maybeH <- maybeGetStringHandle rel
        case maybeH of
            Nothing -> return ()
            Just h -> do
                grzQuery query $ map toSql [getID obj1, getID obj2, snd h]
                grzCommit
    where
        query = "DELETE FROM relationships WHERE guid1 = ? AND guid2 = ? AND relationshipType = ?"   

-- | Returns True if the given relationship exists, otherwise False.   
checkRel :: (GrzObjClass o1, GrzObjClass o2, IConnection c) => 
    GrzRel   -- ^ relationship
    -> o1       -- ^ first object
    -> o2       -- ^ second object
    -> GrzHandler b c Bool  -- ^ True or False
checkRel (GrzRel rel) obj1 obj2 =
    do
        maybeH <- maybeGetStringHandle rel
        case maybeH of
            Nothing -> return False
            Just h -> do            
                val <- grzQuery query $ map toSql [getID obj1,getID obj2, snd h]
                return $ not $ null val
    where
        query = "SELECT * FROM relationships WHERE guid1 = ? AND guid2 = ? AND relationshipType = ?" 
        
-- | The relationship between an object and its container
hasContainer :: GrzRel
hasContainer = GrzRel "hasContainer"

-- | The relationship between an object and its owner
hasOwner :: GrzRel
hasOwner = GrzRel "hasOwner"

-- | The relationship between an object and its site
hasSite :: GrzRel
hasSite = GrzRel "hasSite"
        
{-|
  Tells the object store which fields are searchable.
-}        
setSearchable :: (GrzObjClass o, IConnection c) => 
    (GrzObj -> o)    -- ^ type wrapper 
    -> [GrzString]         -- ^ list of names for metadata to be searchable for this type wrapper
    -> GrzHandler b c ()
setSearchable w ns = do
    oti <- getStringHandle ot
    nsi <- mapM (getStringHandle) ns
    grzRunSql deleteQuery [toSql oti]
    mapM_ (grzRunSql insertQuery) (map (\x -> [toSql oti, toSql x]) nsi)
    
    where
        ot = objWrapperToString (w emptyObj)
        deleteQuery = "DELETE FROM searchable WHERE typeID = ?"
        insertQuery = "INSERT INTO searchable(typeID, nameID) values (?,?)"
                    
{-|
  Runs a query definition and retrieves a list of 
  unwrapped objects from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getUnwrappedObjs :: IConnection c =>
    (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [GrzAtomKey GrzString]                      -- ^ required metadata
    -> [GrzOrderBy]                  -- ^ order by
    -> Int                           -- ^ limit (number of objects to return)
    -> Int                           -- ^ offset
    -> GrzHandler b c [GrzObj]         -- ^ list of objects
getUnwrappedObjs queryDefs needs orderBy limit offset =
    do
        grzH <- S.get
        let limitBit = getLimitBit (grzConfig grzH) offset limit 
        query <- grzCreateQuery ((withData (map atomKey needs)) . (setOrderBy orderBy) . queryDefs)
        result <- runQuery $ addToQuery limitBit query
        return $ queryResultToObjs result
          
{-|
  Runs a query definition and retrieves a list of objects with the
  given type from the database. Provide a limit of 0 to get all objects for
  the query.
-}        
getObjs :: (IConnection c,  GrzObjClass o) => 
    (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [GrzAtomKey GrzString]                         -- ^ required metadata
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> GrzHandler b c [o]                           -- ^ list of objects
getObjs w queryDefs needs orderBy limit offset =
    do
        grzH <- S.get
        let limitBit = getLimitBit (grzConfig grzH) offset limit
        query <- grzCreateQuery ((withData (map atomKey needs)) . (setOrderBy orderBy).(hasType w) . queryDefs)
        result <- runQuery $ addToQuery limitBit query 
        return $ map w (queryResultToObjs result)
                
{-|
  Runs a query definiton and retrieves a list of object ids from the
  database. Provide a limit of 0 to get all object ids for the query.
-}        
getObjIDs :: IConnection c =>
    (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> GrzHandler b c [Int]                         -- ^ list of object IDs
getObjIDs queryDefs orderBy limit offset =
    do
        grzH <- S.get
        let limitBit = getLimitBit (grzConfig grzH) offset limit
        query <- grzCreateQuery ((setOrderBy orderBy) . queryDefs . (setQueryType GrzQTID))
        result <- runQuery $ addToQuery limitBit query
        return $ map (\x -> fromSql $ head x) (fst result)
        
{-|
  Runs a query definition and retrieves a list of bare objects
  (w (GrzObjID id) ) from the database. Provide a limit of 0 to get all objects
  for the query.
-}        
getBareObjs :: (GrzObjClass o, IConnection c) =>
    (GrzObj -> o)                    -- ^ wrapper
    -> (GrzQueryDef -> GrzQueryDef)     -- ^ Query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> GrzHandler b c [o]                           -- ^ list of objects
getBareObjs w queryDefs orderBy limit offset = 
    fmap (map (w . GrzObjID) ) (getObjIDs ((hasType w) . queryDefs) orderBy offset limit)

{-|
  Runs a query definition and retrieves a list of unwrapped
  bare objects (GrzObjID id) from the database. Provide a limit of 0 to get all
  objects for the query.
-}        
getUnwrappedBareObjs :: IConnection c =>
    (GrzQueryDef -> GrzQueryDef)     -- ^ query definition
    -> [GrzOrderBy]                     -- ^ order by
    -> Int                              -- ^ limit (number of objects to return)
    -> Int                              -- ^ offset
    -> GrzHandler b c [GrzObj]                           -- ^ list of objects
getUnwrappedBareObjs queryDefs orderBy limit offset = fmap (map GrzObjID) (getObjIDs queryDefs orderBy offset limit)

        
{-|
  Runs a query definition and retrieves a count of objects
  from the database.
-}        
getObjCount :: IConnection c =>
    (GrzQueryDef -> GrzQueryDef)     -- ^ query definition 
    -> GrzHandler b c Int                           -- ^ count of objects
getObjCount queryDefs =
    do
        query <- grzCreateQuery (queryDefs . (setQueryType GrzQTCount))
        result <- runQuery query
        return $ queryResultToCount result
                        
{-|
  Takes a query definition and two types, and retrieves a list of objects with 
  an aggregated count associated with each.
-}        
getObjsAggByObjCount :: (GrzObjClass o1, GrzObjClass o2, IConnection c) =>
    (GrzObj -> o1)                    -- ^ wrapper for type to be aggregated
    -> (GrzObj -> o2)                    -- ^ wrapper for result type
    -> (GrzQueryDef -> GrzQueryDef)      -- ^ query definition
    -> [GrzString]                          -- ^ required metadata
    -> [GrzOrderBy]                      -- ^ order by
    -> Int                               -- ^ limit (number of objects to return)
    -> Int                               -- ^ offset
    -> GrzHandler b c [(o2, Int)]                    -- ^ (object, count) pair
getObjsAggByObjCount w1 w2 queryDefs needs orderBy limit offset =
    do
        grzH <- S.get
        let limitBit = getLimitBit (grzConfig grzH) offset limit
        t2 <- maybeGetStringHandle (objWrapperToString (w2 emptyObj))
        case t2 of
            Nothing -> return []
            Just (_,i2) -> do
                query <- grzCreateQuery ((withData needs) . (setAggOrderBy orderBy) . (hasType w1) . queryDefs . (setQueryType (GrzQTAggByObjCount i2)))
                result <- runQuery $ addToQuery limitBit query 
                return $ queryResultToAggByObjCount w2 result
        
{-|
  Takes a query definition, two types and a metadata 
  name; retrieves a list of objects with an aggregated sum 
  (of the metadata value) and the count associated with each.
-}        
getObjsAggByObjSumCount :: (GrzObjClass o1, GrzObjClass o2, GrzAtomKeyClass k, IConnection c) =>
    (GrzObj -> o1)                    -- ^ wrapper for type to be aggregated
    -> (GrzObj -> o2)                    -- ^ wrapper for result type
    -> (GrzQueryDef -> GrzQueryDef)      -- ^ query definition
    -> GrzAtomKey k                      -- ^ metadata name attached to values being aggregated
    -> [GrzString]                          -- ^ required metadata
    -> [GrzOrderBy]                      -- ^ order by
    -> Int                               -- ^ limit (number of objects to return)
    -> Int                               -- ^ offset
    -> GrzHandler b c [(o2, (Int,Int))]              -- ^ (object, count) pair
getObjsAggByObjSumCount w1 w2 queryDefs name needs orderBy limit offset =
    do
        grzH <- S.get
        let limitBit = getLimitBit (grzConfig grzH) offset limit
        t2 <- maybeGetStringHandle (objWrapperToString (w2 emptyObj))
        case t2 of
            Nothing -> return []
            Just (_,i2) -> do
                mn <- maybeGetStringHandle (atomKey name)
                case mn of
                    Nothing -> return []
                    Just (_,n) -> do
                        query <- grzCreateQuery ((withData needs) . (setAggOrderBy orderBy) . (hasType w1) . queryDefs . (setQueryType (GrzQTAggByObjSumCount i2 n)))
                        result <- runQuery $ addToQuery limitBit query 
                        return $ queryResultToAggByObjSumCount w2 result        
{-|
  Takes a query definition and a metadata name; 
  retrieves a tuple (sum, count) of objects from the database that have 
  metadata integer values with that name.
-}        
getObjSumCount :: (GrzAtomKeyClass k, IConnection c) =>
    (GrzQueryDef -> GrzQueryDef)    -- ^ query definition
    -> GrzAtomKey k                    -- ^ metadata name 
    -> GrzHandler b c (Int, Int)                   -- ^ sum and count of objects
getObjSumCount queryDefs name =
    do
        query <- grzCreateQuery (queryDefs . (setQueryType GrzQTAggSumCount) . (hasData name))
        result <- runQuery query
        return $ queryResultToSumCount result
                               
-- utilities

trimWhiteSpace :: GrzString -> GrzString
trimWhiteSpace = T.pack . dropWhile isSpace . reverse . dropWhile isSpace . reverse . T.unpack        
