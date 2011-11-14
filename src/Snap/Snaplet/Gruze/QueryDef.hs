{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Gruze.QueryDef (

-- classes
GrzQueryTypeClass(hasIn,hasBetween,hasOp),

-- booleans
hasTrue, hasFalse,

-- filter by specific objects
withObj, withObjs,

-- by type 
hasType, hasTypes,

-- by enabled/disabled
hasEnabled, hasDisabled,

-- by the fixed relationships
withOwner, withContainer, withSite,
withOwners, withContainers, withSites,

-- by general relationships
hasRel,

-- by searchable fields
hasSearchable,

-- has the specified names defined
hasData,

-- return objects with the given metadata in the results
withData,

-- internal functions needed by the IO module
setQueryType, setOrderBy, setAggOrderBy

) where

import Snap.Snaplet.Gruze.Types
import Snap.Snaplet.Gruze.Box

import Data.List (intercalate, foldl')
import Data.Maybe
import Data.Typeable
import qualified Data.Text as T

grzMakeQueryDefName :: GrzString -> GrzQDWFItem
grzMakeQueryDefName s = GrzQDName s

-- some private query functions

setQueryType :: GrzQueryType -> GrzQueryDef -> GrzQueryDef
setQueryType t ((m,n),x) =
    ((m,n), (GrzQDType t) : x)
    
setOrderBy :: [GrzOrderBy] -> GrzQueryDef -> GrzQueryDef
setOrderBy (ob:obs) = (setOrderBy obs) . (setOrderByItem ob False)
setOrderBy [] = id

setAggOrderBy :: [GrzOrderBy] -> GrzQueryDef -> GrzQueryDef
setAggOrderBy (ob:obs) = (setAggOrderBy obs) . (setOrderByItem ob True)
setAggOrderBy [] = id

setOrderByItem ob isAgg =
    case ob of
        StringAsc s -> setOrderByMetadataItem ob (atomKey s) isAgg
        StringDesc s -> setOrderByMetadataItem ob (atomKey s) isAgg
        IntAsc s -> setOrderByMetadataItem ob (atomKey s) isAgg
        IntDesc s -> setOrderByMetadataItem ob (atomKey s) isAgg
        otherwise -> setOrderByOrdinaryItem ob
        
setOrderByOrdinaryItem ob ((m,n),x) =
    ((m,n),(GrzQDOrderBy (-1) (getOrderBit ob n)) :x)       

setOrderByMetadataItem :: GrzOrderBy -> GrzString -> Bool -> GrzQueryDef -> GrzQueryDef
setOrderByMetadataItem ob s isAgg ((m,n),x) =
    ((m,n+1),(GrzQDOrderBy realM (getOrderBit ob n))
    : (GrzQDSelect (getSelectBit ob n))
    : (GrzQDGroupBy realM ("mob" `T.append` (T.pack $ show n) `T.append` ".objectGuid"))
    : (GrzQDJoin realM qdjoin) 
    : (GrzQDWhereFrags realM
            (
                [GrzQDString 
                    ("mob" `T.append` (T.pack $ show n) `T.append`  ".nameId = "
                    )
                 ] 
                ++ [GrzQDName s] 
            )
        )
      : x)
    where    
        realM = if isAgg then m-1 else m
        qdjoin1 = "metadata mob" `T.append` (T.pack $ show n) `T.append` " ON (mob" `T.append` (T.pack $ show n) `T.append` ".objectGuid = obj"
        qdjoin = qdjoin1 `T.append` (T.pack $ show realM) `T.append` ".guid)"
        
getSelectBit :: GrzOrderBy -> Int -> GrzString
getSelectBit ob n =
    case ob of
        StringAsc _ -> "max(mob" `T.append` sn `T.append` ".stringValue) AS mob" `T.append` sn `T.append` "_stringValue"
        StringDesc _ -> "max(mob" `T.append` sn `T.append` ".stringValue) AS mob" `T.append` sn `T.append` "_stringValue"
        IntAsc _ -> "max(mob" `T.append` sn `T.append` ".integerValue) AS mob" `T.append` sn `T.append` "_integerValue"
        IntDesc _ -> "max(mob" `T.append` sn `T.append` ".integerValue) AS mob" `T.append` sn `T.append` "_integerValue"
    where
        sn = T.pack $ show n

getOrderBit :: GrzOrderBy -> Int -> GrzString
getOrderBit ob n =
    case ob of
        GuidAsc -> "objGuid ASC"
        GuidDesc -> "objGuid DESC"
        TimeCreatedAsc -> "timeCreated ASC"
        TimeCreatedDesc -> "timeCreated DESC"
        TimeUpdatedAsc -> "timeUpdated ASC"
        TimeUpdatedDesc -> "timeUpdated DESC"
        StringAsc _ -> "q1.mob" `T.append` sn `T.append` "_stringValue ASC"
        StringDesc _ -> "q1.mob" `T.append` sn `T.append` "_stringValue DESC"
        IntAsc _ -> "q1.mob" `T.append` sn `T.append` "_integerValue ASC"
        IntDesc _ -> "q1.mob" `T.append` sn `T.append` "_integerValue DESC"
        CountAsc -> "grzCount ASC"
        CountDesc -> "grzCount DESC"
        SumAsc -> "grzSum ASC"
        SumDesc -> "grzSum DESC"
        -- TODO: AvgAsc and AvgDesc
    where
        sn = T.pack $ show n

-- the public query functions

withObjs :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef  
withObjs objList ((m,n), x) =
   ((m,n), (GrzQDWhere m ("obj" `T.append` (T.pack $ show m) `T.append` ".guid IN (" `T.append` (objListToString objList) `T.append` ")")) : x)
   
withObj o = withObjs [o]

hasEnabled :: GrzQueryDef -> GrzQueryDef
hasEnabled ((m,n), x) =
    ((m,n), (GrzQDWhere m ("obj" `T.append` (T.pack $ show m) `T.append` ".enabled = 1")) : x)

hasDisabled :: GrzQueryDef -> GrzQueryDef 
hasDisabled ((m,n), x) =
    ((m,n), (GrzQDWhere m ("obj" `T.append` (T.pack $ show m) `T.append` ".enabled = 0")) : x)

hasTypes :: (Typeable o, GrzObjClass o) => [GrzObj -> o] -> GrzQueryDef -> GrzQueryDef  
hasTypes tcList ((m,n), x) =
   ((m,n), (GrzQDWhereFrags m ([GrzQDString ("obj" `T.append` (T.pack $ show m) `T.append` ".objectType IN (")] 
        ++ [GrzQDNameList (map (\y -> objWrapperToString $ (y emptyObj)) tcList)] 
        ++ [GrzQDString ")"]))  : x)
        
hasType ot = hasTypes [ot]

refDict :: [(GrzRef,GrzString)]
refDict = [(ObjRef,"guid"),(ContainerRef,"containerGuid"), (OwnerRef,"ownerGuid"),(SiteRef,"siteGuid")]

-- a non-public utility function to avoid writing the same code many times
hasFixed :: GrzObjClass o => GrzRef -> [o] -> GrzQueryDef -> GrzQueryDef
hasFixed ref objList ((m,n), x) =
    ((m,n), (GrzQDWhere m ("obj" `T.append` (T.pack $ show m) `T.append` "." `T.append` field `T.append` " IN (" `T.append` (objListToString objList) `T.append` ")")) : x)
    where
        field = fromJust $ lookup ref refDict

-- the public functions
withOwners :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef   
withOwners = hasFixed OwnerRef
    
withOwner o = withOwners [o]

withContainers :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef
withContainers = hasFixed ContainerRef
    
withContainer o = withContainers [o]

withSites :: GrzObjClass o => [o] -> GrzQueryDef -> GrzQueryDef
withSites = hasFixed SiteRef
   
withSite o = withSites [o]

-- relationships

hasRel :: GrzRel
    -> GrzRelDir
    -> GrzQueryDef
    -> GrzQueryDef
hasRel (GrzRel rel) dir =
    if isSpecial rel
        then handleRelSpecial rel dir
        else handleRel rel dir
    where
        isSpecial rel = rel `elem` ["hasContainer","hasOwner","hasSite"]

-- the three special relationships: hasContainer, hasOwner and hasSite
-- can be implemented without the relationship table and so
-- can avoid a join
handleRelSpecial :: GrzString
    -> GrzRelDir    
    -> GrzQueryDef
    -> GrzQueryDef
handleRelSpecial rel dir ((m,n), x) =
    ((m+1,n),
        (GrzQDJoin (m+1) ("objects obj" `T.append` (T.pack $ show (m+1)) `T.append` " ON (obj"
            `T.append` (T.pack $ show m) `T.append` guidA `T.append` " = obj" `T.append` (T.pack $ show (m+1)) `T.append` guidB `T.append` ")"))
         : x)
    where
        field = case rel of
                    "hasContainer" -> ".containerGuid"
                    "hasOwner" -> ".ownerGuid"
                    "hasSite" -> ".siteGuid"
        guidA = case dir of 
                    FwdRel -> field
                    InvRel -> ".guid"
        guidB = case dir of
                    FwdRel -> ".guid"
                    InvRel -> field

-- the usual case with a join to the relationship table   
handleRel :: GrzString
    -> GrzRelDir    
    -> GrzQueryDef
    -> GrzQueryDef
handleRel rel dir ((m,n), x) =
    ((m+1,n),
        (GrzQDJoin (m+1) ("objects obj" `T.append` (T.pack $ show (m+1)) `T.append` " ON (r"
            `T.append` sm `T.append` guidB `T.append` " = obj" `T.append` (T.pack $ show (m+1)) `T.append` ".guid)"))
        : (GrzQDJoin m ("relationships r" `T.append` sm `T.append` " ON (r"
            `T.append` sm `T.append` guidA `T.append` " = obj" `T.append` sm `T.append` ".guid)"))
        : (GrzQDWhereFrags m
            (
                [GrzQDString 
                    ("r" `T.append` sm `T.append`  ".relationshipType = "
                    )
                 ] 
                ++ [GrzQDName rel] 
            )
        ) : x)
     where
        sm = T.pack $ show m
        guidA = case dir of 
                    FwdRel -> ".guid1"
                    InvRel -> ".guid2"
        guidB = case dir of
                    FwdRel -> ".guid2"
                    InvRel -> ".guid1"
       
hasAtomOp :: GrzString -> GrzString -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomOp name op values ((m,n), x) =
    ((m,n+1),(GrzQDJoin m ("metadata m" `T.append` mbit 
        `T.append` " ON (obj" `T.append` (T.pack $ show m) `T.append` ".guid = m" `T.append` mbit `T.append` ".objectGuid)")) 
    : (GrzQDWhereFrags m ([GrzQDString ("m" `T.append` mbit `T.append` ".nameId = ")] 
        ++ ([GrzQDName name]) 
        ++ [GrzQDString (" AND ")]
        ++ [getAtomClause values (m,n) op]))
    : x)
    where
        mbit = (T.pack $ show m) `T.append` "_" `T.append` (T.pack $ show n)
    
hasData :: GrzAtomKeyClass k => GrzAtomKey k -> GrzQueryDef -> GrzQueryDef   
hasData name ((m,n), x) =
    ((m,n+1),(GrzQDJoin m ("metadata m" `T.append` mbit 
        `T.append` " ON (obj" `T.append` (T.pack $ show m) `T.append` ".guid = m" `T.append` mbit `T.append` ".objectGuid)")) 
    : (GrzQDWhereFrags m ([GrzQDString ("m" `T.append` mbit `T.append` ".nameId = ")] 
        ++ [GrzQDName (atomKey name)] 
        ))
    : x)
    where
        mbit = (T.pack $ show m) `T.append` "_" `T.append` (T.pack $ show n)
    
hasSearchable :: GrzString -> GrzQueryDef -> GrzQueryDef   
hasSearchable content ((m,n), x) =
    ((m,n+1),(GrzQDJoin m ("metadata m" `T.append` sm `T.append` "_" `T.append` sn 
        `T.append` " ON (m" `T.append` sm `T.append` "_" `T.append` sn `T.append` ".nameID = s" `T.append` sn `T.append` ".nameID)"))
    : (GrzQDJoin m ("searchable s" `T.append` sn
        `T.append` " ON (obj" `T.append` sm `T.append` ".objectType = s" `T.append` sn `T.append` ".typeID)"))
    : (GrzQDWhereFrags m ([getAtomClause [stringToAtom content] (m,n) "match"]))
    : (GrzQDWhere m ("m" `T.append` sm `T.append` "_" `T.append` sn `T.append` ".objectGuid = obj" `T.append` sm `T.append` ".guid"))
    : x)
    where
        sn = T.pack $ show n
        sm = T.pack $ show m

hasAtomIn :: GrzString -> [GrzAtom] -> GrzQueryDef -> GrzQueryDef   
hasAtomIn name values = hasAtomOp name "IN" values

class GrzQueryTypeClass qt where
    hasIn :: GrzAtomKey qt -> [qt] -> GrzQueryDef -> GrzQueryDef
    hasBetween :: GrzAtomKey qt -> (qt,qt) -> GrzQueryDef -> GrzQueryDef
    hasOp :: GrzAtomKey qt -> GrzString -> qt -> GrzQueryDef -> GrzQueryDef
    
instance GrzQueryTypeClass GrzString where
    hasIn name values = hasAtomIn (atomKey name) (map stringToAtom values)
    hasBetween name (v0,v1) = hasAtomOp (atomKey name) "=><=" (map stringToAtom [v0,v1])
    hasOp name op value = hasAtomOp (atomKey name) op (map stringToAtom [value])
    
instance GrzQueryTypeClass GrzInt where
    hasIn name values = hasAtomIn (atomKey name) (map intToAtom values)
    hasBetween name (v0,v1) = hasAtomOp (atomKey name) "=><=" (map intToAtom [v0,v1]) 
    hasOp name op value = hasAtomOp (atomKey name) op (map intToAtom [value])
    
hasTrue :: GrzAtomKey Bool -> GrzQueryDef -> GrzQueryDef
hasTrue name = hasAtomIn (atomKey name) (map boolToAtom [True])

hasFalse :: GrzAtomKey Bool -> GrzQueryDef -> GrzQueryDef
hasFalse name = hasAtomIn (atomKey name) (map boolToAtom [False])

withData :: [GrzString] -> GrzQueryDef -> GrzQueryDef
withData vs ((m,n), x) = foldl' (\((m,n),x) v -> ((m,n+1),(GrzQDNeeds v n):x)) ((m,n),x) vs

-- various simple utility functions

objListToString :: GrzObjClass o => [o] -> GrzString
-- a bit of a kludge
objListToString [] = "-1"
objListToString x =
    T.intercalate ", " (map (T.pack . show . getID . unwrapObj) x)

grzSafeAtomListToIntString :: [GrzAtom] -> GrzString
grzSafeAtomListToIntString x =
    T.intercalate ", " (map (T.pack . show . safeAtomToInt) x)

-- given a list of atoms, a join number and an operation, generates appropriate where value clauses

getAtomClause :: [GrzAtom] -> (Int,Int) -> GrzString -> GrzQDWFItem   
getAtomClause atoms (m,n) op = 
        GrzQDAtomClause valid intClause boolClause stringClause ints bools strings
    where
        mname = "m" `T.append` (T.pack $ show m) `T.append` "_" `T.append` (T.pack $ show n)
        intAtoms = filter isIntAtom atoms
        intMarks = T.intercalate "," $ map toMark intAtoms
        ints = map atomToInt intAtoms
        boolAtoms = filter isBoolAtom atoms
        boolMarks = T.intercalate "," $ map toMark boolAtoms
        bools = map safeAtomToInt boolAtoms
        stringAtoms = filter isStringAtom atoms
        stringMarks = T.intercalate "," $ map toMark stringAtoms
        strings = if (op == "match") -- add % on either side
                    then map (\x -> ("%" `T.append` (atomToString x) `T.append` "%")) stringAtoms
                    else map atomToString stringAtoms
        maybeOp = lookup op normaliseOp

        valid = ((isJust maybeOp)
                    && case maybeOp of
                        Just "><" -> ((length atoms) == 2) 
                                        && (((length intAtoms) == 2) 
                                              || ((length boolAtoms) == 2) 
                                              || ((length stringAtoms) == 2))
                        otherwise -> True
                )
        vop = if valid then fromJust maybeOp else ""

        intClause = if null ints 
                        then "" 
                        else "(" `T.append` mname `T.append` ".metadataType = 0 AND " 
                            `T.append` (getOpBit op (mname `T.append` ".integerValue") intAtoms) `T.append` ")"
        boolClause = if null bools 
                        then "" 
                        else "(" `T.append` mname `T.append` ".metadataType = 1 AND " 
                            `T.append` (getOpBit op (mname `T.append` ".integerValue") boolAtoms)  `T.append` ")"
        stringClause = if null strings 
                        then "" 
                        else "(" `T.append` mname `T.append` ".metadataType = 2 AND " 
                            `T.append` (getOpBit op (mname `T.append` ".stringValue") stringAtoms)  `T.append` ")"

-- normalises op
normaliseOp = [("in", "IN"),("In","IN"),("IN","IN"),("iN","IN"),("><","><") , ("=><=","=><="),
    ("<","<"),("<=","<="),("=","="),(">",">"),(">=",">="),("match","match")]
                        
-- generate the condition bit
-- TODO: handle other conditions?
-- eg. exclusive between
getOpBit :: GrzString -> GrzString -> [GrzAtom] -> GrzString
getOpBit "IN" var atoms = " ( " `T.append` var `T.append` " IN (" `T.append` (T.intercalate "," $ map toMark atoms) `T.append` ")) "
getOpBit "><" var _ = " ( " `T.append` var `T.append` " > ? AND " `T.append` var `T.append` " < ? ) "
getOpBit "=><=" var _ = " ( " `T.append` var `T.append` " >= ? AND " `T.append` var `T.append` " <= ? ) "
getOpBit "match" var _ = " ( " `T.append` var `T.append` " LIKE ? ) "
getOpBit op var _ = " ( " `T.append` var `T.append` " " `T.append` op `T.append` " ? ) "

toMark _ = "?"                        
        
grzAtomListToStrings :: [GrzAtom] -> GrzString -> GrzString -> [GrzString]
grzAtomListToStrings values start end =
    reverse $ (grzAtomListToStrings2 values [start] end)
    where
        grzAtomListToStrings2 :: [GrzAtom] -> [GrzString] -> GrzString -> [GrzString]
        grzAtomListToStrings2 ((GrzAtomString s) : vs) xs end = 
            grzAtomListToStrings2 vs("," : s : xs) end
        grzAtomListToStrings2 (other : vs) (x:xs) end = 
            grzAtomListToStrings2 vs ((x `T.append` (T.pack $ show $ safeAtomToInt other) `T.append` ",") : xs) end
        grzAtomListToStrings2 (other : vs) [] end = 
            grzAtomListToStrings2 vs [(T.pack $ show $ safeAtomToInt other) `T.append` ","] end
        grzAtomListToStrings2 [] (x:xs) end = ((grzTrimComma x `T.append` end): xs)
        grzAtomListToStrings2 [] [] _ = []
        
grzMakeQueryStrings :: [GrzString] -> GrzString -> GrzString -> [GrzString]
grzMakeQueryStrings values start end =
    reverse $ (grzMakeQueryStrings2 values [start] end)
    where
        grzMakeQueryStrings2 :: [GrzString] -> [GrzString] -> GrzString -> [GrzString]
        grzMakeQueryStrings2 (s : vs) xs end = grzMakeQueryStrings2 vs ("," : s : xs) end
        grzMakeQueryStrings2 [] (x:xs) end = ((grzTrimComma x `T.append` end): xs)
        grzMakeQueryStrings2 [] [] _ = []

grzTrimComma x =
    if (T.last x == ',') then T.init x else x