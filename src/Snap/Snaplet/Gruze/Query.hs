{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Gruze.Query

where

-- internal functions to convert and run query definitions

import Snap.Snaplet.Gruze.Types
import Snap.Snaplet.Gruze.Box
import Snap.Snaplet.Gruze.Handles
import Snap.Snaplet.Gruze.Utility

import Snap.Snaplet.Hdbc
import Database.HDBC (IConnection)
import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import Data.Maybe
import qualified Control.Monad.State as S
import qualified Data.Text as T

-- some useful pure query-related functions

valuesToSql :: GrzQueryValues -> [SqlValue]
valuesToSql values = ((map toSql (fst values)) ++ (map toSql (snd values)))

-- adds an extra bit to the query string
addToQuery :: GrzString 
    -> Maybe GrzQuery    
    -> Maybe GrzQuery
addToQuery extra q = 
    case q of
        Just (((qs,qt),hd),v) -> Just (((qs `T.append` extra,qt),hd),v) 
        Nothing -> Nothing

queryResultToCount :: ([[SqlValue]],([(GrzString,Int)],[[SqlValue]]))  -- ^ the query result
    -> Int
queryResultToCount ([[v]],_) = (fromSql v)::Int
queryResultToCount _ = 0

queryResultToSumCount :: ([[SqlValue]],([(GrzString,Int)],[[SqlValue]]))  -- ^ the query result
    -> (Int, Int)
    
queryResultToSumCount ([[_,SqlNull]],_) = (0,0)
queryResultToSumCount ([[SqlNull,_]],_) = (0,0)
queryResultToSumCount ([[s,c]],_) = ((fromSql s)::Int,(fromSql c)::Int)
queryResultToSumCount _ = (0,0)

populateObj m [oid,ot,otc,otu,oo,oc,os,e] =
    GrzObjFull {
        objID = guid,
        objType = (fromSql ot) :: GrzString,
        objTimeCreated = (fromSql otc) :: Int,
        objTimeUpdated = (fromSql otu) :: Int,
        objOwner = GrzObjID ((fromSql oo) :: Int),
        objContainer = GrzObjID ((fromSql oc) :: Int),
        objSite = GrzObjID ((fromSql os) :: Int),
        objEnabled = enabled,
        objMetadata = case Map.lookup guid m of
                        Just b -> b
                        Nothing -> emptyAtomBox
    }
    where 
        guid = (fromSql oid) :: Int
        enabled = if ((fromSql e) :: Int) == 1 then True else False
           
queryRowToObj :: Map.Map Int GrzAtomBox -> [SqlValue] -> GrzObj
queryRowToObj m r = populateObj m r    

queryResultToAggByObjCount :: GrzObjClass o =>
     (GrzObj -> o)                                      -- ^ type wrapper
    -> ([[SqlValue]],([(GrzString,Int)],[[SqlValue]]))     -- ^ query result
    -> [(o,Int)]                                        -- returns list of (obj, count) pairs
queryResultToAggByObjCount w (r, (n,mr)) = map (queryRowToAggByObjCount w mq) r
    where
        mq =  metadataQueryToAtomBoxDict n mr
        
queryResultToAggByObjSumCount :: GrzObjClass o =>
     (GrzObj -> o)                                      -- ^ type wrapper
    -> ([[SqlValue]],([(GrzString,Int)],[[SqlValue]]))     -- ^ query result
    -> [(o,(Int,Int))]                                    -- returns list of (obj, count) pairs
queryResultToAggByObjSumCount w (r, (n,mr)) = map (queryRowToAggByObjSumCount w mq) r
    where
        mq =  metadataQueryToAtomBoxDict n mr
                        
queryRowToAggByObjCount :: GrzObjClass o =>
    (GrzObj -> o)               -- ^ type wrapper
    -> Map.Map Int GrzAtomBox   -- ^ metadata 
    -> [SqlValue]               -- row from database
    -> (o,Int)                  -- resulting (obj,count) pair
queryRowToAggByObjCount w m r =
    (obj, count)
    where 
        count = (fromSql (r !! 8)) :: Int
        obj = w $ populateObj m (take 8 r)

queryRowToAggByObjSumCount :: GrzObjClass o =>
    (GrzObj -> o)               -- ^ type wrapper
    -> Map.Map Int GrzAtomBox   -- ^ metadata 
    -> [SqlValue]               -- row from database
    -> (o,(Int,Int))            -- resulting (obj,(sum,count)) pair
queryRowToAggByObjSumCount w m r =
    (obj, (qsum,count))
    where 
        count = (fromSql (r !! 8)) :: Int
        qs = r !! 9 
        qsum = if qs == SqlNull then 0 else ((fromSql qs) :: Int)
        obj = w $ populateObj m (take 8 r)
            
queryResultToObjs :: ([[SqlValue]],([(GrzString,Int)],[[SqlValue]]))
    -> [GrzObj]
queryResultToObjs (r, (n,mr)) = map (queryRowToObj mq) r
    where
        mq =  metadataQueryToAtomBoxDict n mr
        
orderByToString :: GrzQueryDefItem -> [GrzString]
orderByToString (GrzQDOrderBy _ s) = [s]
orderByToString _ = []

getOrderBy :: [GrzQueryDefItem] -> GrzString
getOrderBy q =
    " ORDER BY " `T.append` if T.null ob then "objGuid " else ob `T.append` " "
    where ob = T.intercalate ", " $ reverse $ concatMap orderByToString q
    
groupByToString :: GrzQueryDefItem -> [GrzString]
groupByToString (GrzQDGroupBy _ s) = [s]
groupByToString _ = []

getGroupBy :: [GrzQueryDefItem] -> GrzString
getGroupBy q =
    " GROUP BY objGuid" `T.append` if T.null gb then " " else ", " `T.append` gb `T.append` " "
    where
        gb = T.intercalate ", " $ concatMap groupByToString q 

needToString :: GrzQueryDefItem -> [GrzString]
needToString (GrzQDNeeds x _) = [x]
needToString _ = []

getQueryNeeds :: [GrzQueryDefItem] -> [GrzString]
getQueryNeeds q = concatMap needToString q

joinToString :: Int -> Maybe Int -> GrzQueryLocation -> GrzQueryDefItem -> [GrzString]
joinToString _ _ Exterior (GrzQDJoin (-1) x) = [" INNER JOIN " `T.append` x `T.append` " "]
joinToString _ _ Exterior (GrzQDJoin _ _) = []
joinToString _ _ Interior (GrzQDJoin (-1) x) = []
joinToString m (Just _) _ (GrzQDJoin n x) = 
    if n == m 
        then 
            [" LEFT JOIN " `T.append` x `T.append` " "] 
        else [" INNER JOIN " `T.append` x `T.append` " "]
joinToString m Nothing _ (GrzQDJoin n x) = [" INNER JOIN " `T.append` x `T.append` " "]
joinToString _ _ _ _ = []

getQueryJoins :: [GrzQueryDefItem] -> Int -> Maybe Int -> GrzQueryLocation -> [GrzString]
getQueryJoins q m maybeAggByObjQueryTypeID ql = concatMap (joinToString m maybeAggByObjQueryTypeID ql) q

whereToString :: Int -> Maybe Int -> GrzQueryDefItem -> [GrzString]
whereToString m (Just _) (GrzQDWhere n s) = 
    if n >= m-1
        then [" ((obj" `T.append` (T.pack $ show n) `T.append` ".guid IS NULL) OR (" `T.append` s `T.append` ")) "]
        else [s]
whereToString m Nothing (GrzQDWhere n s) = [s]
whereToString _ _ _ = []

getQueryWheres :: [GrzQueryDefItem] -> Int -> Maybe Int -> [GrzString]
getQueryWheres q m maybeAggByObjQueryTypeID = concatMap (whereToString m maybeAggByObjQueryTypeID) q

getQueryType :: [GrzQueryDefItem] -> GrzQueryType
getQueryType q = case concatMap isQueryType q of
                        [] -> GrzQTFull
                        (x:xs) -> x
isQueryType :: GrzQueryDefItem -> [GrzQueryType]
isQueryType (GrzQDType t) = [t]
isQueryType _ = []

getSelects :: [GrzQueryDefItem] -> [GrzString]
getSelects q = concatMap isSelect q

isSelect :: GrzQueryDefItem -> [GrzString]
isSelect (GrzQDSelect s) = [s]
isSelect _ = []

getQueryAgg :: [GrzQueryDefItem] -> GrzString
getQueryAgg q = case concatMap isQueryAgg q of
                        [] -> "obj0.guid"
                        (x:xs) -> x
isQueryAgg :: GrzQueryDefItem -> [GrzString]
isQueryAgg (GrzQDAgg s) = [s]
isQueryAgg _ = []


-- Query IO functions
       
grzCreateQuery :: IConnection c => (GrzQueryDef -> GrzQueryDef) -> GrzHandler b c (Maybe GrzQuery)
grzCreateQuery q = grzGetQuery (q ((0,0), []))

-- Derives the query string, looking up any string handles in the database.

-- If any string handle lookups fail, this function returns Nothing;
-- otherwise it returns the list of need names and the actual query string.

-- Much of the complexity of this function comes from dealing with aggregation,
-- which in some cases requires LEFT JOINS and special null value handling
-- as the objects being aggregated do not necessarily exist (and
-- so in that case should return a count and sum of zero).

grzGetQuery :: IConnection c => GrzQueryDef -> GrzHandler b c (Maybe GrzQuery)
grzGetQuery ((m,n), q) =
    do
        maybeFrags <- grzQueryWhereFragments q m maybeAggByObjQueryTypeID Interior
        maybeFrags2 <- grzQueryWhereFragments q m maybeAggByObjQueryTypeID Exterior
        if isNothing maybeFrags
            then
                return Nothing
            else do
                let justFrags = fromJust maybeFrags
                let qv = map snd justFrags
                let d = concatMap (snd . fst) justFrags
                let frags = filter (not . T.null) (map (fst . fst) justFrags)
                let nullWhereBit = T.null $ T.concat whereBit
                let nullFrags = T.null $ T.concat frags
                return $ Just (
                    (((prefix
                    `T.append` " "
                    `T.append` (T.intercalate " " (reverse $ getQueryJoins q m maybeAggByObjQueryTypeID Interior ))
                    `T.append` aggNameJoin
                    `T.append` (if nullWhereBit && nullFrags
                            then ""
                            else (" WHERE " 
                                `T.append` (if nullWhereBit then "" else (T.intercalate " AND " whereBit))
                                `T.append` (if nullFrags || nullWhereBit then "" else " AND ")
                                `T.append` (if nullFrags then "" else T.intercalate " AND " frags)
                            )
                       )
                    `T.append` aggTypeWhere
                    `T.append` aggNameWhere
                    `T.append` " " 
                    `T.append` (if queryType `elem` [GrzQTCount,GrzQTAggSumCount]
                            then
                                ""
                            else
                                getGroupBy q 
                    )
                    `T.append` suffix
                    `T.append` (T.intercalate " " (reverse $ getQueryJoins q m maybeAggByObjQueryTypeID Exterior ))
                    `T.append` typeStringJoin
                    `T.append` (if isNothing maybeFrags2 
                            then 
                                "" 
                            else
                                (let s = (T.intercalate " AND " (
                                        filter (not . T.null) (map (fst . fst) (fromJust maybeFrags2))))
                                    in
                                        if T.null s then "" else " WHERE " `T.append` s
                                )
                       )
                    `T.append` (if queryType `elem` [GrzQTCount, GrzQTAggSumCount] 
                            then 
                                "" 
                            else 
                                --  (" GROUP BY objGuid " ++ (getOrderBy q) )
                                ((getOrderBy q) )
                       )
                    ), queryType), d),
                   (needs, (concatMap fst qv, concatMap snd qv)) )
    where
        whereBit = getQueryWheres q m maybeAggByObjQueryTypeID
        needs = getQueryNeeds q
        queryType = getQueryType q
        maybeAggByObjQueryTypeID = case queryType of
                                GrzQTAggByObjCount i -> Just i
                                GrzQTAggByObjSumCount i _ -> Just i
                                otherwise -> Nothing
        maybeAggByObjQueryNameID = case queryType of
                                GrzQTAggByObjSumCount _ j -> Just j
                                otherwise -> Nothing
        -- need to add some sanity checking in here in case m is zero?
        agg = "obj" `T.append` (T.pack $ show $ if isJust maybeAggByObjQueryTypeID 
                                then m-1 
                                else m)
        typeStringJoin = if queryType `elem` [GrzQTCount,GrzQTAggSumCount,GrzQTID]
                            then
                                ""
                            else
                                (" INNER JOIN objects obje ON (obje.guid = q1.objGuid) " 
                                    `T.append` " INNER JOIN names n ON (n.id = obje.objectType) "
                                )
        -- possibly add type and metadata constraints for the objects being aggregated
        aggTypeWhere = case maybeAggByObjQueryTypeID of
                        Just i -> " AND obj" `T.append` (T.pack $ show (m-1)) 
                            `T.append` ".objectType = " `T.append` (T.pack $ show i)
                        Nothing -> ""
        aggNameJoin = case maybeAggByObjQueryNameID of 
                        Just _ -> " LEFT JOIN metadata ma ON (obj" 
                            `T.append` (T.pack $ show m) `T.append` ".guid = ma.objectGuid) "
                        Nothing -> ""
        aggNameWhere = case maybeAggByObjQueryNameID of 
                        Just j -> " AND ((ma.nameId IS NULL) OR (ma.nameId = " `T.append` (T.pack $ show j) 
                            `T.append` " AND ma.metadataType = 0)) " 
                        Nothing -> ""
        sqlFrags = fromJust $ lookup (sqlIndex queryType) sqlDict
        selectBit = getSelects q    
        prefix = (fst sqlFrags) 
                    `T.append` if queryType `elem` [GrzQTAggCount,GrzQTAggSumCount] 
                        then "" 
                        else if isJust maybeAggByObjQueryNameID 
                                then
                                    "SELECT " `T.append` agg `T.append` ".guid AS objGuid, count(DISTINCT ma.id) AS grzCount, "
                                        `T.append` "sum(DISTINCT ma.integerValue) AS grzSum"
                                        `T.append` (if null selectBit then "" else ", " `T.append` (T.intercalate ", " selectBit))
                                        `T.append` " FROM objects obj0 "
                                else if queryType == GrzQTCount
                                        then
                                            "SELECT " `T.append` agg `T.append` ".guid AS objGuid FROM objects obj0 "
                                        else
                                            "SELECT " `T.append` agg `T.append` ".guid AS objGuid, count(DISTINCT obj" `T.append` (T.pack $ show m) 
                                                `T.append` ".guid) AS grzCount"
                                                `T.append` (if null selectBit then "" else ", " `T.append` (T.intercalate ", " selectBit))
                                                `T.append` " FROM objects obj0 "

        suffix = snd sqlFrags
                
sqlIndex queryType = case queryType of
                    GrzQTCount                  -> "count"
                    GrzQTID                     -> "guid"
                    GrzQTFull                   -> "full"
                    GrzQTAggSumCount            -> "agg_sumcount"
                    GrzQTAggByObjCount _        -> "aggbyobj_count"
                    GrzQTAggByObjSumCount _ _   -> "aggbyobj_sumcount"                    
sqlDict = [
    ("full",(
        "SELECT q1.objGuid, n.string, timeCreated, timeUpdated, ownerGuid, containerGuid, siteGuid, enabled FROM (",
        ") as q1"
        )
    ),
    ("guid", (
        "SELECT q1.objGuid as objGuid FROM (",
        ") as q1 "
        )
    ),
    ("count", (
        "SELECT count(DISTINCT q1.objGuid) as total FROM (",
        ") as q1 GROUP BY objGuid"
        )
    ),
    ("agg_count", (
        "SELECT count(DISTINCT obj0.guid) as total FROM objects obj0",
        ""
        )
    ),
    ("agg_sumcount", (
        "SELECT sum(m0_0.integerValue) as total, count(DISTINCT obj0.guid) as count FROM objects obj0",
        ""
        )
     ),
     ("aggbyobj_count", (
        "SELECT q1.objGuid, n.string, timeCreated, timeUpdated, ownerGuid, "
            `T.append` "containerGuid, siteGuid, enabled, q1.grzCount FROM (",
        ") as q1"
        )
     ),
     ("aggbyobj_sumcount", (
        "SELECT q1.objGuid, n.string, timeCreated, timeUpdated, ownerGuid, "
            `T.append` "containerGuid, siteGuid, enabled, q1.grzCount, "
            `T.append` "CASE WHEN q1.grzSum IS NULL THEN 0 ELSE q1.grzSum END AS grzSum FROM (",
        ") as q1"
        )
     )
  ]   
                   
grzQueryWhereFragments :: IConnection c =>
    [GrzQueryDefItem]
    -> Int
    -> Maybe Int
    -> GrzQueryLocation
    -> GrzHandler b c (Maybe [((GrzString,[(GrzString,Int)]), GrzQueryValues)])
grzQueryWhereFragments q m maybeAggByObjQueryTypeID ql = do
    r <- mapM (grzHandleWhereQueryFragment m maybeAggByObjQueryTypeID ql) frags
    if and $ map isJust r
        then
            return $ Just (map fromJust r)
        else
            return Nothing
    where
        frags = filter (\x -> case x of
                                GrzQDWhereFrags _ _ -> True
                                otherwise -> False
                       ) q

grzHandleWhereQueryFragment :: IConnection c =>
    Int
    -> Maybe Int
    -> GrzQueryLocation 
    -> GrzQueryDefItem
    -> GrzHandler b c (Maybe ((GrzString,[(GrzString,Int)]), GrzQueryValues))
grzHandleWhereQueryFragment m maybeAggByObjQueryTypeID Exterior (GrzQDWhereFrags (-1) items) =
    grzProcessWhereQueryFragment m maybeAggByObjQueryTypeID (-1) items
grzHandleWhereQueryFragment _ _ Exterior (GrzQDWhereFrags _ _) =
    return $ Just (("",[]),([],[]))
grzHandleWhereQueryFragment _ _ Interior (GrzQDWhereFrags (-1) _) =
    return $ Just (("",[]),([],[]))
grzHandleWhereQueryFragment m maybeAggByObjQueryTypeID ql (GrzQDWhereFrags n items) =
    grzProcessWhereQueryFragment m maybeAggByObjQueryTypeID n items    
grzHandleWhereQueryFragment _ _ _ _ =
    return $ Just (("",[]),([],[])) 

grzProcessWhereQueryFragment m maybeAggByObjQueryTypeID n items = do
    r <- mapM qwfItemToMaybeString items
    if and $ map isJust r
        then do
            let j = map fromJust r
            let s = T.concat $ map (fst . fst) j
            let s' = case maybeAggByObjQueryTypeID of
                        (Just _) -> if m == n
                                        then
                                            " ((obj" `T.append` (T.pack $ show n) `T.append` ".guid IS NULL) OR (" `T.append` s `T.append` ")) "
                                        else
                                            s
                        Nothing -> s 
            let d = concatMap (snd . fst) j
            let v = map snd j            
            return $ Just ((s',d), (concatMap fst v, concatMap snd v))
        else
            return Nothing

qwfItemToMaybeString :: IConnection c => 
    GrzQDWFItem 
    -> GrzHandler b c (Maybe ((GrzString,[(GrzString,Int)]), GrzQueryValues))  
qwfItemToMaybeString item = do
    grzH <- S.get
    case item of
        GrzQDString s -> return $ Just ((s,[]),([],[]))
        GrzQDName s -> do
                        r <- maybeGetStringHandle s
                        case r of
                            Just h -> return $ Just ((T.pack $ show $ snd h, [h]),([],[]))
                            Nothing -> return $ Nothing
        GrzQDNameList ss -> do
                                r <- maybeGetStringHandles ss
                                case r of
                                    Just hs -> return $ Just ((T.intercalate "," (map (T.pack . show . snd) hs),hs),([],[]))
                                    Nothing -> return $ Nothing
        GrzQDAtomClause valid ic bc sc i b s -> do
                                            if valid
                                                then
                                                    if null i && null b && null s
                                                        then
                                                            return $ Just (("",[]),([],[]))
                                                        else do
                                                            let ic2 = if T.null ic then [] else [ic]
                                                            let bc2 = if T.null bc then [] else [bc]
                                                            let sc2 = if T.null sc then [] else [transformStringClause (grzConfig grzH) sc]
                                                            let c = T.intercalate " OR " $ concat [ic2, bc2, sc2]
                                                            return $ Just ((" (" `T.append` c `T.append` ")",[]), (i ++ b, s))
                                                else
                                                    return $ Nothing
                                                    
-- runQuery converts a query request into object and metdata results
runQuery :: IConnection c =>
    Maybe GrzQuery 
    -> GrzHandler b c ([[SqlValue]],([(GrzString,Int)],[[SqlValue]]))
runQuery (Just (((query,queryType),handleDict),(needs,values))) = 
    do
        -- get the objects
        r <- grzQuery query (valuesToSql values)

        -- get the metadata if it was requested, this is a full query, 
        -- and some objects were retrieved
        if (not ((sqlIndex queryType) `elem` ["full","aggbyobj_count","aggbyobj_sumcount"])) || (null needs) || (null r)
            then
                return (r, (handleDict,[]))
            else do
                if head needs == "*"
                    then do
                        -- this is asking for all the metadata
                        let startBit = "SELECT m.* FROM metadata m WHERE"
                        let objBit = " m.objectGuid IN (" `T.append` (T.intercalate "," (map (T.pack . show) $ getObjIDListFromQueryResult r)) `T.append` ") "
                        let needsQuery = startBit `T.append` objBit
                        r2 <- grzQuery needsQuery []
                        -- need an extra query to return all metadata keys
                        let keysQuery = "SELECT m.nameId, n.string FROM metadata m "
                                `T.append` "INNER JOIN names n ON (m.nameId = n.id) WHERE " `T.append` objBit 
                                `T.append` " GROUP BY m.nameId, n.string "
                        r3 <- grzQuery keysQuery []
                        return (r, ((map getHandleFromKeysQuery r3), r2))
                   else do
                        -- first step - get the extra string handles in needs but not in dict
                        let stillNeeded = filter (\x -> x `notElem` (map fst handleDict)) needs
                        hs <- mapM (maybeGetStringHandle) stillNeeded
                        let hd = handleDict ++ (map fromJust (filter isJust hs))
                        
                        -- newNeeds contains names with existing string handles
                        let newNeeds = filter (\x -> x `elem` (map fst hd)) needs
                        
                        -- nhd contains all the available string handles for the requested data names
                        let nhd = filter (\x -> (fst x) `elem` needs) hd
                        if null nhd
                            then
                                -- oops, all the requested data names were invalid, so return no metadata
                                return (r, (handleDict,[]))
                            else do
                                -- ok, we have string handles for the names of the metadata to retrieve
                                -- so build the query to get the data
                                let startBit = "SELECT m.* FROM metadata m WHERE"
                                let needsBit = " m.nameId IN (" `T.append` (T.intercalate "," (map (T.pack . show . snd) nhd)) `T.append` ") AND "
                                let objBit = " m.objectGuid IN (" `T.append` (T.intercalate "," (map (T.pack . show) $ getObjIDListFromQueryResult r)) `T.append` ") "
                                let needsQuery = startBit `T.append` needsBit `T.append` objBit
                                r2 <- grzQuery needsQuery []
                                return (r, (hd, r2))
                                
runQuery Nothing = return ([],([],[]))

getHandleFromKeysQuery [id, s] = (((fromSql s) :: GrzString),((fromSql id) :: Int))

getObjIDListFromQueryResult r = map (\x -> ((fromSql (head x)) :: Int)) r

metadataQueryToAtomBoxDict :: [(GrzString,Int)] -> [[SqlValue]] -> Map.Map Int GrzAtomBox
metadataQueryToAtomBoxDict hd rs = foldl' accumBoxes Map.empty ps
    where
        rhd = map (\x -> (snd x, fst x)) hd
        ps = catMaybes (map (metadataQueryRowToPair rhd) rs)
        
accumBoxes :: Map.Map Int GrzAtomBox -> (Int, (GrzString, [GrzAtom])) -> Map.Map Int GrzAtomBox
accumBoxes m (i,p) =
    case Map.lookup i m of
        Just bs -> Map.insert i (addAtomPair p bs) m
        Nothing -> Map.insert i (addAtomPair p emptyAtomBox) m

metadataQueryRowToPair :: [(Int,GrzString)] -> [SqlValue] -> Maybe (Int, (GrzString, [GrzAtom]))
metadataQueryRowToPair rhd [_, objectGuid, metadataType, nameId, integerValue, stringValue] =
    case lookup ((fromSql nameId) :: Int) rhd of
        Just s -> case (fromSql metadataType) :: Int of
                    0 -> Just (guid, (s, [intToAtom iv]))
                    1 -> Just (guid, (s, [boolToAtom (if iv == 1 then True else False)]))
                    2 -> Just (guid, (s, [stringToAtom sv]))
                    3 -> Just (guid, (s, [GrzAtomFile iv]))
        Nothing -> Nothing
    where
        guid = (fromSql objectGuid) :: Int
        sv = (fromSql stringValue) :: GrzString
        iv = (fromSql integerValue) :: Int
