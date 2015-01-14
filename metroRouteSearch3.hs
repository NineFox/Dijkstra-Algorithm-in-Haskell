import Data.List
import Data.Ord
import Debug.Trace
import Control.Applicative
import Control.Monad



newtype StationName a = StationName a deriving(Show,Read,Eq,Ord)

data Edge a= Edge {getEdgeCost :: Double, getFrom :: StationName a, getTo :: StationName a} deriving(Show,Eq)
type LineSystem a= [Edge a]

type Cost = Maybe Double
type StationUsed = Bool
data PrevStationName a = UnKnown | StartNode | PrevStation (StationName a) deriving(Show,Eq)
data Node a = Node {getStationName :: StationName a, getStationCost :: Cost, getStationUsed :: StationUsed, getPrevStationName :: PrevStationName a} deriving(Show,Eq)
type EachStationCost a = [Node a]



instance (Eq a) => Ord (Edge a) where
	compare (Edge x1 x2 x3) (Edge y1 y2 y3)
		| x1 == y1 = EQ
		| x1 < y1 = LT
		| otherwise = GT


instance (Eq a) => Ord (Node a) where
	compare (Node x1 x2 x3 x4) (Node y1 y2 y3 y4)
		| x2 == y2 = EQ
		| x2 < y2 = if (x2 == Nothing)
			then GT
			else LT
		| x2 > y2 = if (y2 == Nothing)
			then LT
			else GT


type ListZipper a = ([a],[a])

goHead :: ListZipper a -> Maybe (ListZipper a)
goHead (xs,[]) = Just (xs,[])
goHead zl = goBack zl >>= goHead

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward ([],_) = Nothing
goForward (x:xs,bs) = Just (xs, x:bs)

goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (_,[]) = Nothing
goBack (xs,b:bs) = Just (b:xs,bs)

modifyList :: (a -> a) -> ListZipper a -> ListZipper a
modifyList func ([],bs) = ([],bs)
modifyList func (x:xs,bs) = ((func x) : xs,bs)

getZipperListHead :: ListZipper a -> a
getZipperListHead ((x:_),_) = x

lengthZipperList :: ListZipper a -> Int
lengthZipperList (xs,bs) = (length xs) + (length bs)


moveZipperList :: (Eq a, Eq b ) => (a -> b) -> ListZipper a -> b -> Maybe (ListZipper a)
moveZipperList nodeFunc allList@(xs,bs) nodeElemnt
	|bs == [] = moveZipperListHelper nodeFunc allList nodeElemnt
	|otherwise = do
		initList <- goHead allList
		moveZipperListHelper nodeFunc initList nodeElemnt
	where
		moveZipperListHelper :: (Eq a, Eq b ) => (a -> b) -> ListZipper a -> b -> Maybe (ListZipper a)
		moveZipperListHelper _ ([],_) _ = Nothing
		moveZipperListHelper nodeFunc allList  nodeElemnt = do
			if nodeElemnt == nodeFunc (getZipperListHead allList)
				then return allList
				else do
					newList <- goForward allList
					moveZipperListHelper nodeFunc newList nodeElemnt


costPlus :: (Applicative f, Num b) => f b -> f b -> f b
costPlus iCost yCost = (+) <$> iCost <*> yCost

{-
	駅までの最短経路が確定した場合、使用しないようにする
	Trueになった駅は最短経路確定
-}
stationUsed :: Node a -> Node a
stationUsed (Node a b c d) = Node a b True d

-- 最短経路が確定したノードを記録する
prevStation :: StationName a -> Node a -> Node a
prevStation _ node@(Node _ _ _ StartNode) = node
prevStation prevNode (Node a b c _)  = (Node a b c (PrevStation prevNode))

-- 駅のコストの更新と前のノードを記録する
-- stationCostUpdate (StationName 1) (StationName 2)
stationCostUpdate :: (Eq a)=> Cost -> StationName a -> LineSystem a ->  Node a -> Node a
stationCostUpdate updateCost iSt lSys yNode@(Node ySt yCost yTF yPre) =
	if (yCost /= Nothing) && (yTF || (nowCost > yCost))
		then yNode
		else prevStation iSt (Node ySt nowCost yTF yPre)
	where
		nowCost = costPlus updateCost (iAndYouCost iSt ySt lSys)



-- 自分と相手の駅間のコストを返す
iAndYouCost ::(Eq a)=> StationName a -> StationName a -> LineSystem a -> Cost
iAndYouCost iSt ySt lSys
	|iAndYouEdge == [] = Nothing
	|otherwise = Just (getEdgeCost . head $ iAndYouEdge)
	where
		iAndYouEdge = filter yStPredicate $ connectEdge iSt lSys
		yStPredicate x = getTo x == ySt || getFrom x == ySt

-- 自分とつながっているエッジを洗い出す
connectEdge :: (Eq a) => StationName a -> LineSystem a -> LineSystem a
connectEdge stName lSys = filter (stfilter stName) lSys
	where
		stfilter :: (Eq a) => StationName a -> Edge a -> Bool
		stfilter stName stEdge = stName == getFrom stEdge || stName == getTo stEdge

{- 自分とつながっている駅を洗い出す
connectStationHelper stName = connectStation stName (connectEdge stName (routeToEdge routeRecord))
-}
connectStation:: Eq a => StationName a -> LineSystem a -> [StationName a]
connectStation stName lSys = map (otherStName stName) (connectEdge stName lSys)

-- エッジのレコード構文から自分とつながっている駅を1つ返す
otherStName :: (Eq a) => StationName a -> Edge a-> StationName a
otherStName name edge = if name == getTo edge
	then getFrom edge
	else getTo edge


-- 自分と繋がっている駅のコストを更新する
connectStationMapHelper iSt nowCost zipLs edges = connectStationMap iSt nowCost zipLs edges (connectStation iSt edges)

connectStationMap :: (Eq a) => StationName a -> Cost -> ListZipper (Node a) -> LineSystem a -> [StationName a] -> Maybe (ListZipper (Node a))
connectStationMap _ _ zipLs _ [] = Just zipLs
connectStationMap iSt nowCost zipLs lSys (x:xs) = do
	newList <- applyZipperList (stationCostUpdate nowCost iSt lSys) zipLs x
	connectStationMap iSt nowCost newList lSys xs


{-
	Edgeの情報[コスト,ノード、接続先のノード]
-}
-- 駅間のコスト
route :: [(Double,(Int,Int))]
route = [
	(4,(2,3)),
	(10,(1,5)),
	(5,(1,2)),
	(6,(3,4)),
	(5,(2,5)),
	(3,(4,5))
	]

route' = [
	Edge {getEdgeCost = 4, getFrom = StationName 2, getTo = StationName 3},
	Edge {getEdgeCost = 10, getFrom = StationName 1, getTo = StationName 5},
	Edge {getEdgeCost = 5, getFrom = StationName 1, getTo = StationName 2},
	Edge {getEdgeCost = 6, getFrom = StationName 3, getTo = StationName 4},
	Edge {getEdgeCost = 5, getFrom = StationName 2, getTo = StationName 5},
	Edge {getEdgeCost = 3, getFrom = StationName 4, getTo = StationName 5}
	]

-- 駅間のコストの情報のリストをレコード構文に変換
-- routeToEdge route
routeToEdge :: [(Double,(a,a))] -> LineSystem a
routeToEdge = map routeRecord
	where
		routeRecord (a,(b,c)) = Edge {getEdgeCost=a,getFrom=StationName b,getTo=StationName c}


-- edgeの情報からNodeの初期状態のリストを返す
routeToStationInitState :: (Ord a) => StationName a -> LineSystem a -> Maybe (ListZipper (Node a))
routeToStationInitState stName costInfo = applyZipperList (\_ -> Node stName (Just 0) True StartNode)  (sortBy (comparing getStationName) . nub . concat $ map (\ x -> [Node (getFrom x) Nothing False UnKnown, Node (getTo x) Nothing False UnKnown]) costInfo, []) stName

-- Zipperから駅の情報を取得する
-- stNameGetState routeToStationInitState (StationName 1) ->
-- 		Just (Node {getStationName = StationName 1, getStationCost = Nothing, getStationUsed = False})
stNameGetState :: (Eq a) => ListZipper (Node a) -> StationName a  -> Maybe (Node a)
stNameGetState allList stName = do
	newList <- moveZipperList getStationName allList stName
	return (getZipperListHead newList)


-- 関数をListZipperの要素stNameに適用する
applyZipperList :: Eq a => (Node a -> Node a) -> ListZipper (Node a) -> StationName a  -> Maybe (ListZipper (Node a))
applyZipperList func allList stName = do
	newList <- moveZipperList getStationName allList stName
	goHead (modifyList func newList)


-- 最小となるコストのノードを見つけコストを確定したノードを返す
costDone :: (Eq a) => ListZipper (Node a) -> Node a
costDone allList = (stationUsed . minimum) $ filter filterPredicate nodes
	where
		(Just (nodes,_) ) = goHead allList
		filterPredicate x = (getStationCost x /= Nothing) && (getStationUsed x == False)


{-
	example use
	dijkstraMain (StationName 1) (routeToEdge route)
-}
dijkstraMain :: (Show a, Ord a) => StationName a -> LineSystem a -> Maybe (ListZipper (Node a))
dijkstraMain startStation allList = do
	initList <- (routeToStationInitState startStation allList)
	let
		(Just startStationNode) = stNameGetState initList startStation
	dijkstraHelper startStationNode initList allList (lengthZipperList initList)

dijkstraHelper :: (Show a,Eq a) => Node a -> ListZipper (Node a) -> LineSystem a -> Int -> Maybe (ListZipper (Node a))
dijkstraHelper _ allList _ 1 = Just allList
dijkstraHelper startStation allList　edges cnt = do
	updatedCostList <- connectStationMapHelper (getStationName startStation) (getStationCost startStation) allList edges
	let
		nextMinimumNode = (costDone updatedCostList)
	minimumNodeConfirmedList <- applyZipperList (\_ -> nextMinimumNode) updatedCostList (getStationName nextMinimumNode)
	dijkstraHelper nextMinimumNode minimumNodeConfirmedList edges  (cnt - 1)

-- 経路復元
transferGuide :: (Eq a) => StationName a -> Maybe (ListZipper (Node a)) -> (EachStationCost a)
transferGuide  _  Nothing = []
transferGuide goalStation (Just costConfirmedList)
	| goalStationState == Nothing = []
	| otherwise = let
		(Just goalNode) = goalStationState
		in (reverse (transferGuideHelper goalNode costConfirmedList))
	where
		goalStationState = stNameGetState costConfirmedList goalStation

transferGuideHelper :: (Eq a) => Node a -> ListZipper (Node a) -> EachStationCost a
transferGuideHelper goalNode allList
	| UnKnown == prevStationNameState = []
	| StartNode == prevStationNameState = [goalNode]
	| otherwise = let
			(Just prevGoalNode) = stNameGetState allList (prevStationName goalNode)
			in goalNode : (transferGuideHelper prevGoalNode allList)
	where
		prevStationNameState = getPrevStationName goalNode
		prevStationName :: Node a -> (StationName a)
		prevStationName (Node _ _ _ (PrevStation prevStName)) = prevStName



transferRoute :: (Show a, Ord a) => StationName a -> StationName a -> [(Double, (a, a))] -> EachStationCost a
transferRoute startStation goalStation routeRecord = transferGuide goalStation (dijkstraMain (startStation) (routeToEdge routeRecord))


main :: IO ()
main = do
		putStrLn "StartNode Input"
		startStation <- getLine
		if null startStation
			then return ()
			else do
				putStrLn "GoalNode Input"
				goalStation <- getLine
				let
					stringToStationName stringStationName = (read ("StationName " ++ stringStationName)) :: (StationName Int)
				mapM_ (putStrLn . show) (transferRoute (stringToStationName startStation) (stringToStationName goalStation) route)
				main
