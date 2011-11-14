{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Snap.Snaplet.Gruze.Types where

import qualified Data.Map as Map
import Data.Typeable
import Snap.Snaplet
--import Snap.Snaplet.Hdbc
import Database.HDBC
import Data.Lens.Template
import qualified Control.Monad.State as S
import Control.Monad.IO.Control
import Control.Monad.Trans(liftIO)
import Data.Text (Text)

-- * Basic types

type GrzKey = Text
type GrzString = Text
type GrzInt = Int

type GrzQuery = (((GrzString,GrzQueryType),[(GrzString,Int)]), ([GrzString],GrzQueryValues))

data GrzQueryType = GrzQTFull | GrzQTCount | GrzQTID | GrzQTAggCount 
    | GrzQTAggSumCount | GrzQTAggByObjCount Int | GrzQTAggByObjSumCount Int Int
    deriving (Show, Eq)

type GrzQueryDef = ((Int,Int), [GrzQueryDefItem])

data GrzQueryDefItem = 
        GrzQDIsCount Bool 
        | GrzQDWhere Int GrzString 
        | GrzQDJoin Int GrzString 
        | GrzQDWhereFrags Int [GrzQDWFItem]
        | GrzQDOrderBy Int GrzString
        | GrzQDGroupBy Int GrzString
        | GrzQDNeeds GrzString Int
        | GrzQDSelect GrzString
        | GrzQDGroup GrzString
        | GrzQDType GrzQueryType
        | GrzQDAgg GrzString

data GrzQDWFItem =
        GrzQDName GrzString
        | GrzQDNameList [GrzString]
        | GrzQDString GrzString
        | GrzQDAtomClause Bool GrzString GrzString GrzString [Int] [Int] [GrzString]
        
type GrzQueryValues = ([Int], [GrzString])

-- TODO: need to rethink the basic type approach - perhaps standardise on ByteString and Int for now?
-- another option might be to use a class, especially for strings

-- toGrzKey :: String -> GrzKey
-- toGrzKey = id

-- toGrzString :: String -> GrzString
-- toGrzString = id

-- toGrzInt :: Int -> GrzInt
-- toGrzInt = id

-- * Gruze atom definition

data GrzAtom = 
    GrzAtomInt GrzInt 
    | GrzAtomBool Bool 
    | GrzAtomString GrzString 
    | GrzAtomFile GrzInt 
    deriving (Show, Ord, Eq, Read)
       
-- * Gruze atom box definition
    
type GrzAtomBox = Map.Map GrzKey [GrzAtom]

emptyAtomBox = Map.empty

class GrzAtomBoxClass c where
    getAtomBox :: c -> GrzAtomBox
    putAtomBox :: GrzAtomBox -> c -> c
    
instance GrzAtomBoxClass GrzAtomBox where
    getAtomBox c = c
    putAtomBox b _ = b
    
-- * Gruze atom box key definitions

data GrzAtomKey a = GrzAtomKey { 
    atomKey :: GrzString
}
    deriving (Eq, Show)

class GrzAtomKeyClass t where
    set :: GrzAtomBoxClass b => GrzAtomKey t -> t -> b -> b                 -- ^ replaces the current value with a single value (or creates a new one)
    add :: GrzAtomBoxClass b => GrzAtomKey t -> t -> b -> b                 -- ^ appends to the current values (or creates a new one)
    get :: GrzAtomBoxClass b => GrzAtomKey t -> t -> b -> t                 -- ^ gets the current value (the first one if there are several defined for this field) or returns the default value supplied
    maybeGet :: GrzAtomBoxClass b => GrzAtomKey t -> b -> Maybe t           -- ^ gets the current value (the first one if there are several defined for this field) or returns Nothing
    setList :: GrzAtomBoxClass b => GrzAtomKey t -> [t] -> b -> b           -- ^ replaces the current value with a list (or creates a new one)
    addList :: GrzAtomBoxClass b => GrzAtomKey t -> [t] -> b -> b           -- ^ appends the list to the current values (or creates a new one)
    getList :: GrzAtomBoxClass b => GrzAtomKey t -> [t] -> b -> [t]         -- ^ gets the current list of values for this field or returns the default list supplied
    maybeGetList :: GrzAtomBoxClass b => GrzAtomKey t -> b -> Maybe [t]     -- ^ gets the current list of values for this field or returns Nothing

type GrzAtomIntKey = GrzAtomKey Int
type GrzAtomTextKey = GrzAtomKey GrzString
type GrzAtomBoolKey = GrzAtomKey Bool
type GrzAtomAtomKey = GrzAtomKey GrzAtom
   
-- * Gruze object definition
    
data GrzObj = GrzObjID GrzInt |
    GrzObjFull {
        objID :: GrzInt,
        objType :: GrzString,
        objTimeCreated :: GrzInt,
        objTimeUpdated :: GrzInt,
        objOwner :: GrzObj,
        objContainer :: GrzObj,
        objSite :: GrzObj,
        objEnabled :: Bool,
        objMetadata :: GrzAtomBox
    }
    deriving (Read, Show, Typeable)
    
type GrzSetters = GrzObj -> GrzObj
       
instance GrzAtomBoxClass GrzObj where
    getAtomBox c = objMetadata c
    putAtomBox b c = c { objMetadata = b } 
    
-- * Gruze object box definition
    
type GrzObjBox = Map.Map GrzKey GrzObj

class GrzObjBoxClass c where
    getObjBox :: c -> GrzObjBox
    putObjBox :: GrzObjBox -> c -> c
    
instance GrzObjBoxClass GrzObjBox where
    getObjBox c = c
    putObjBox b _ = b
    
-- * Gruze box definition
    
data GrzBox = GrzBox {
    atomBox :: GrzAtomBox,
    objBox :: GrzObjBox
}

instance GrzAtomBoxClass GrzBox where
    getAtomBox c = atomBox c
    putAtomBox b c = c { atomBox = b }
    
instance GrzObjBoxClass GrzBox where
    getObjBox c = objBox c
    putObjBox b c = c { objBox = b }
    
-- Gruze relationships

data GrzRel = GrzRel { getGrzRelString :: GrzString }
    
data GrzRelDir = FwdRel | InvRel
    deriving Eq
    
data GrzLogLevel = DebugLogLevel | NotificationLogLevel | WarningLogLevel | FatalLogLevel
    deriving (Eq, Ord)
    
data GrzRef =  ObjRef | ContainerRef | OwnerRef | SiteRef
    deriving Eq
    
data GrzOrderBy = GuidAsc | GuidDesc | TimeCreatedAsc | TimeCreatedDesc
        | TimeUpdatedAsc | TimeUpdatedDesc | StringAsc (GrzAtomKey String) | StringDesc (GrzAtomKey String)
        | IntAsc (GrzAtomKey Int) | IntDesc (GrzAtomKey Int)
        | CountAsc | CountDesc | SumAsc | SumDesc        
-- TODO: add | AvgAsc | AvgDesc
    deriving (Eq,Show)

data GrzDatabaseType = GrzSqlite3DB | GrzMySQLDB | GrzPostgreSQLDB

data GrzHandle = GrzHandle {
    grzDataDirectory :: FilePath,
    grzConvertLocation :: FilePath,
    grzLogFile :: FilePath,
    grzDefaultSite :: GrzObj,
    grzThumbDefs :: [(GrzString,GrzString)],
    grzLogLevel :: GrzLogLevel,
    grzDatabaseType :: GrzDatabaseType
}

data GruzeSnaplet c = (IConnection c) => GruzeSnaplet {
        hdbcConn    :: c
       ,grzConfig   :: GrzHandle
}
-- makeLenses [''GruzeSnaplet]

--class (IConnection c, MonadControlIO m) => HasGruze m c | m -> c where
--  getGruzeConfig :: m GrzHandle
  
 -- TODO: replace Handler with a simpler state monad as access to Snap functions is not required
-- eg. GruzeState
-- type IOState s = S.StateT s IO
-- type GrzHandler c = IOState (GruzeSnaplet c)
type GrzHandler b c = Handler b (GruzeSnaplet c)

--instance (IConnection c) => HasHdbc (GrzHandler b c) c IO where
--    getConnSrc = with hdbc $ S.gets connSrc     

--instance MonadControlIO (GrzHandler c) where
--  liftControlIO f = liftIO (f return)
--
--instance (IConnection c) => HasHdbc (GrzHandler c) c IO where
--  getConnSrc = with hdbc $ S.gets connSrc

data GrzQueryLocation = Exterior | Interior
