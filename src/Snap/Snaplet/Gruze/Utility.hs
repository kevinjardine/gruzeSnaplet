{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Gruze.Utility

where

import Snap.Snaplet.Gruze.Box
import Snap.Snaplet.Gruze.Types

-- import Snap.Snaplet.Hdbc
import Database.HDBC
import Data.Time.Clock.POSIX
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.Maybe
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import Data.List as DL
import Data.HashMap.Strict as HM
import Control.Monad.State (gets)
import qualified Data.Text as T

-- the slightly strange record handling here is due to the fact that the first
-- value of the GrzHandle record is a polymorphic type and apparently standard
-- record accessor functions cannot be used to store it. Moreover only
-- pattern matching can be used to extract the value. Hence the several
-- grzH@( GrzHandle {grzDatabaseHandle = dbc} ) references below.

getLastInsertId :: IConnection c => GrzHandler b c Int   
getLastInsertId = do
    grzC <- gets grzConfig
    let fc = T.pack $ case grzDatabaseType grzC of
                    GrzMySQLDB -> "LAST_INSERT_ID()"
                    GrzSqlite3DB -> "last_insert_rowid()"
                    GrzPostgreSQLDB -> "lastval()"
    qs <- grzQuery ("SELECT " `T.append` fc `T.append` " AS id") []
    return ((fromSql (head (head qs)))::Int)
    
getLimitBit :: GrzHandle -> Int -> Int -> GrzString 
getLimitBit grzC offset limit = do                 
    if limit == 0 
        then
            "" 
        else 
            case grzDatabaseType grzC of
                GrzPostgreSQLDB -> " OFFSET " `T.append` (T.pack $ show offset) `T.append` " LIMIT " `T.append` (T.pack $ show limit)
                otherwise -> " LIMIT " `T.append` (T.pack $ show offset) `T.append` "," `T.append` (T.pack $ show limit)

-- for now this just replaces LIKE with ILIKE for PostgreSQL
transformStringClause :: GrzHandle -> GrzString -> GrzString               
transformStringClause grzC sc = do
    case grzDatabaseType grzC of
        GrzPostgreSQLDB -> T.replace sc "LIKE" "ILIKE"
        otherwise -> sc

-- | Runs a SELECT (something that returns values)
grzQuery :: IConnection c => GrzString -> [SqlValue] -> GrzHandler b c [[SqlValue]] 
grzQuery query values =
    do
        conn <- gets hdbcConn
        grzLog DebugLogLevel $ "query: " `T.append` query `T.append` " values: " `T.append` (T.pack $ show values)
        liftIO $ quickQuery' conn (T.unpack query) values
        
-- | Runs an INSERT, UPDATE or other SQL command that does not return values        
grzRunSql :: IConnection c => GrzString -> [SqlValue] -> GrzHandler b c ()  
grzRunSql query values =
    do
        conn <- gets hdbcConn
        grzLog DebugLogLevel $ "run sql: " `T.append` query `T.append` " values: " `T.append` (T.pack $ show values)
        liftIO $ run conn (T.unpack query) values
        return ()

-- | Commits a database update.      
grzCommit :: IConnection c => GrzHandler b c ()
grzCommit = do
    conn <- gets hdbcConn
    liftIO $ commit conn

-- | Rolls back a database update. 
grzRollback :: IConnection c => GrzHandler b c ()
grzRollback = do
    conn <- gets hdbcConn
    liftIO $ rollback conn
  
logLevelToString = [    (DebugLogLevel,"DEBUG"),
                        (NotificationLogLevel,"NOTIFICATION"),
                        (WarningLogLevel,"WARNING"),
                        (FatalLogLevel,"FATAL")
                   ]
                   
-- | Logs a message.
grzLog :: IConnection c =>
    GrzLogLevel      -- ^ log level 
    -> GrzString           -- ^ log message
    -> GrzHandler b c ()                 
grzLog level s = do
    grzC <- gets grzConfig
    if level >= (grzLogLevel grzC)
        then do
            ptime <- liftIO getCurrentTime
            let ftime = formatTime defaultTimeLocale "%c" ptime
            liftIO $ appendFile (grzLogFile grzC) 
                (ftime 
                    ++ ": " 
                    ++ (fromMaybe "" $ Prelude.lookup level logLevelToString)
                    ++ ": " 
                    ++ (T.unpack s)
                    ++ "\n")
        else
            return ()
            
-- TODO: remove the database type part
-- | Converts a Snap configuration structure to a Gruze handle.
-- grzConfigToHandle :: Config  -- ^ configuration structure
--    -> GrzHandle                -- ^ data handle
grzConfigToHandle dataDirectory convertLocation logFile dbType = 
    GrzHandle dataDirectory convertLocation logFile defaultSite thumbDefs logLevel dbType
        where
            defaultSite = emptyBareObj
            thumbDefs = []
            logLevel = WarningLogLevel       
 
            
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
 