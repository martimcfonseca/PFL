
type Match = ((String, String), (Int, Int))
type MatchDay = [Match]
type League = [MatchDay]


winner :: Match -> String
winner ((h,a),(hScore,aScore))
    | hScore > aScore = h
    | aScore > hScore = a
    | otherwise = "draw"

--matchDayScore :: String -> MatchDay -> Int
--matchDayScore _ [] = 0
--matchDayScore t (m@((h,a),_):ms)
--    | t==h || t==a = aux t m
--    | otherwise = matchDayScore t ms
--    where aux
--        | result == t = 3
--        | result == "draw" = 1
--        | otherwise = 0
--        where result = winner m

-- ranking :: League -> [(String,Int)]
-- ranking [] = []
-- ranking l@(d:_) = sortByCond [(t, leagueScore t l) | t <- concat [[h,a] | ((h,a),_) <- d]] (\(t1,s1) (t2,s2) -> s1 > s2 || (s1 == s2 && t1 < s2))


--numMatchDaysWithDraws :: League -> Int 
--numMatchDaysWithDraws l = length $ filter (\d -> any (=="draw") (map winner d)) 1

type City = String
type Distance = Int
type RoadMap = [(City,City,Distance)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent graph c = [(if x == c then y else x,d) | (x,y,d) <- graph, c==x || c==y]

areConnected :: RoadMap -> City -> City -> Bool
areConnected graph x y = aux graph [x] y
    where aux graph (n:ns) y
        | n == y = True
        | otherwise = or [aux graph (a:n:ns) y | (a,_) <-
        adjacent graph n, not (elem a (n:ns))]