{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Gruze.Handles

where

import Snap.Snaplet.Gruze.Utility
import Snap.Snaplet.Gruze.Box
import Snap.Snaplet.Gruze.Types

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe
import Data.HashTable (hashString)
import System.Random
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import System.Process
import System.Exit (ExitCode)
import System.Directory
import Snap.Snaplet.Hdbc
import Database.HDBC(IConnection)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as S
import qualified Data.Text as T

-- string handler functions

-- currently all string handles are stored only in the database
-- the string handle system is only likely to pay off when there is
-- some kind of value cache like memcached so that common string handles
-- can be looked up in memory without accessing the database

-- tries to extract valid string handle from query result     
getStringHandleFromQueryResult :: GrzString -> [SqlValue] -> Maybe Int
getStringHandleFromQueryResult s [sqlId, sqlString] = 
    case fromSql sqlString of
            Just s2 -> case s2 == s of
                        True -> Just ((fromSql sqlId)::Int)
                        False -> Nothing
            Nothing -> Nothing

-- gets existing string handle or creates one if it doesn't exist
getStringHandle :: IConnection c => GrzString -> GrzHandler b c Int
getStringHandle s = do
    h <- maybeGetStringHandle s
    case h of
        Nothing -> do
                    let hash_s = fromIntegral (hashString (T.unpack s))
                    makeStringHandle s hash_s
        Just i -> return $ snd i
        
-- returns Just existing (string, string handle) pair or Nothing        
maybeGetStringHandle :: IConnection c => GrzString -> GrzHandler b c (Maybe (GrzString, Int))
maybeGetStringHandle s =
    do
        qs <- grzQuery ("SELECT id, string FROM names WHERE hash = " `T.append` (T.pack $ show hash_s)) []
        let ids = catMaybes $ map (getStringHandleFromQueryResult s) qs
        if null ids 
            then do
                grzLog NotificationLogLevel ("Unable to find handle for string: " `T.append` s)
                return Nothing 
            else 
                return $ Just (s, head ids)
    where
        hash_s = fromIntegral (hashString $ T.unpack s)

-- returns a dictionary of string handles, or nothing 
maybeGetStringHandles :: IConnection c => [GrzString] -> GrzHandler b c (Maybe [(GrzString,Int)])    
maybeGetStringHandles ss = do
    r <- mapM (maybeGetStringHandle) ss
    if and $ map isJust r
        then
            return $ Just (map fromJust r)
        else
            return Nothing

-- makes a new string handle and inserts it in the database   
makeStringHandle :: IConnection c => GrzString -> Int -> GrzHandler b c Int
makeStringHandle s hash_s =
    do
        grzRunSql query [toSql s, toSql hash_s]
        getLastInsertId
    where
        query = "INSERT INTO names(string,hash) values(?,?)"

-- functions to manage files

-- TODO: getOrphanFiles

getFileHandle :: IConnection c => GrzString -> GrzString -> GrzString -> GrzString -> Int -> GrzHandler b c Int
getFileHandle ofn ct locd locf time = do
        grzRunSql query [toSql ofn, toSql ct, toSql locd, toSql locf, toSql time]
        getLastInsertId
    where
        query = "INSERT INTO files(originalName,contentType,locationDir,locationFile,timeCreated) values(?,?,?,?,?)"

-- | Attempts to get the original name, content type, location directory, location file and 
-- time created for a file atom
maybeGetFileMetadata :: IConnection c =>
    GrzAtom                      -- ^ file atom
    -> GrzHandler b c (Maybe ([GrzString],Int))    -- ^ Maybe results
maybeGetFileMetadata (GrzAtomFile i) = do
        val <- grzQuery query [toSql i]
        return $ getFileMetadataResult val
    where
        query = "SELECT originalName,contentType,locationDir,locationFile,timeCreated FROM files WHERE id = ?"
        
maybeGetFileMetadata _ = return Nothing

maybeGetFileContent' :: IConnection c => GrzAtom -> GrzString -> GrzHandler b c (Maybe B.ByteString)
maybeGetFileContent' a@(GrzAtomFile i) prefix = do
        grzH <- S.get
        md <- maybeGetFileMetadata a
        case md of
                    Just ([_,_,locd,locf],_) -> do
                                                    v <- (liftIO $ BS.readFile (T.unpack $ 
                                                        (T.pack (grzDataDirectory $ grzConfig grzH))
                                                        `T.append` "/" 
                                                        `T.append` locd
                                                        `T.append` "/" 
                                                        `T.append` prefix
                                                        `T.append` locf))
                                                    return $ Just v
                    otherwise -> return Nothing
maybeGetFileContent' _ _ = return Nothing

-- | attempts to get the file content                    
maybeGetFileContent :: IConnection c =>
    GrzAtom                  -- ^ file atom
    -> GrzHandler b c (Maybe B.ByteString)  -- ^ returns a Maybe Bytestring value
maybeGetFileContent a = maybeGetFileContent' a ""

-- | Attempts to get a file thumbnail for a given size
maybeGetFileThumb :: IConnection c =>
    GrzString                   -- ^ thumbnail size
    -> GrzAtom                  -- ^ file atom
    -> GrzHandler b c (Maybe B.ByteString)  -- ^ returns a Maybe Bytestring value
maybeGetFileThumb s a = maybeGetFileContent' a s

delFile :: IConnection c => GrzAtom -> GrzHandler b c Bool
delFile a@(GrzAtomFile i) = do
        grzH <- S.get
        let dataDir = grzDataDirectory (grzConfig grzH)
        fd <- maybeGetFileMetadata a
        case fd of
            Just ([_,_,locd,_],_) -> do 
                                        liftIO $ removeDirectoryRecursive (dataDir ++ "/" ++ (T.unpack locd))
                                        grzRunSql queryMetadata [toSql i]
                                        grzRunSql queryFiles [toSql i]
                                        grzCommit
                                        return True
            otherwise -> return False  
    where
        queryFiles = "DELETE FROM files WHERE id = ?"
        queryMetadata = "DELETE FROM metadata WHERE metadataType = 3 AND integerValue = ?"
        
delFile _ = return False

getFileMetadataResult :: [[SqlValue]] -> Maybe ([GrzString],Int)
getFileMetadataResult [[SqlNull,_,_,_]] = Nothing
getFileMetadataResult [[ofn, ct, locd, locf, time]] = Just $ (map fromSql [ofn, ct, locd, locf], fromSql time)

-- TODO: fix this function to make sure it cannot generate
-- a name that already exists
generateFileLocationDirectory dataDir d = do
    rn <- getStdRandom (randomR (0::Int,1000))
    let loc = "uploads/" ++ (show rn) ++ "/" ++ d
    let dir = dataDir ++ "/" ++ loc
    createDirectoryIfMissing True dir
    return loc

-- | Create a file atom.      
createFileAtom :: IConnection c =>
    GrzString           -- ^ original file name
    -> GrzString           -- ^ content (MIME) type
    -> B.ByteString     -- ^ file content as a bytestring 
    -> GrzHandler b c GrzAtom       -- ^ returns a file atom
createFileAtom ofn ct ubs =  do
        ptime <- liftIO getPOSIXTime
        let time = floor ptime
        loc <- saveFile ofn ct ubs time
        h <- getFileHandle ofn ct (fst loc) (snd loc) time
        return $ GrzAtomFile h
    
saveFile :: IConnection c => GrzString -> GrzString -> B.ByteString -> Int -> GrzHandler b c (GrzString, GrzString)
saveFile ofn ct ubs time = do
    grzH <- S.get
    let dataDir = grzDataDirectory (grzConfig grzH) 
    locd <- liftIO $ generateFileLocationDirectory dataDir (show time)
    let ffn = dataDir ++ "/" ++ locd ++ "/" ++ (T.unpack ofn)
       
    liftIO $ BS.writeFile ffn ubs
    
    -- create thumbnails if any defined
    if (ct `elem` ["image/jpeg","image/jpg","image/gif","image/png","image/pjpeg","image/x-png"]) 
        then do
            mapM_ (resizeImage (dataDir ++ "/" ++ locd) ofn) (grzThumbDefs (grzConfig grzH))
            -- TODO: check the exit codes?
            return ()
        else return ()
    return (T.pack $ locd, ofn)
    
resizeImage :: IConnection c => FilePath -> GrzString -> (GrzString,GrzString) -> GrzHandler b c ExitCode
resizeImage dir ofn (tn,ts) = do
    -- use rawSystem to run an imagemagick command
    grzH <- S.get
    let gcl = grzConvertLocation (grzConfig grzH)
    let args = [dir ++ "/" ++ (T.unpack ofn),
                "-scale", (T.unpack ts),
                dir ++ "/" ++ (T.unpack tn) ++ (T.unpack ofn)
               ]
    grzLog DebugLogLevel (T.pack $ show (gcl,args))
    code <- liftIO $ rawSystem gcl args
    grzLog DebugLogLevel $ "Exit code: " `T.append` (T.pack $ show code)
    return code
