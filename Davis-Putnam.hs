module Main where
import System.IO
import CNFTokens
import CNFGrammar
import Data.List as L
import Data.Maybe

type Variables = Int
type Clauses = [Int]
type Literal = Int

data Status = SAT | UNSAT deriving Show

remove :: Eq a => a -> [a] -> [a]
remove element list = filter (\e -> e/=element) list

literal :: Int -> CNFFormula -> Int
literal v (Literal a) = if v < a then error "Parsing Error" else a
literal v (Negative (Literal a)) = if v < a then error "Parsing Error" else -a

getclause :: Int -> CNFFormula -> [Int]
getclause v (Clause a) = getclause v a
getclause v (UnitClause a) = [literal v a]
getclause v (PartialClause a b) = [literal v a, literal v b]
getclause v (PartialClauseJoined a b) = literal v a : getclause v b

getclauses :: Int -> Int -> CNFFormula -> [Clauses]
getclauses v 0 _ = error "Parsing Error"
getclauses v 1 (PartialFormula a b) = error "Parsing Error"
getclauses v 2 (PartialFormula a b) = [getclause v a, getclause v b]
getclauses v n (PartialFormulaJoined a b) = getclause v a:(getclauses v (n-1) b)

generate :: CNFFormula -> ([Clauses] , [Variables])
generate (Formula v c t) = ( getclauses v c t, [1..v])


-- UNIT PROPOGATION
remove_unit l []    =  []
remove_unit l (c:cs) |  (elem l c)  =  (remove_unit l cs) 
                     | otherwise    =  ((filter (\li -> li /= (-l)) c): (remove_unit l cs))

unit :: [Clauses] -> Maybe Literal
unit []     =   Nothing
unit (c:cs) |   length c == 1   =  Just (head c)
            |   otherwise       = unit cs



assignproper value v | elem value v     =  v
                     | otherwise        =  [value] ++ (remove (-value) v)

unit_propogation (cs,v) = case up of 
                            Nothing  -> Nothing
                            Just l -> Just (remove_unit l cs, assignproper l v)
                            where up = unit cs


--Pure Literal Elemination

check_ple_on_l cs v = case ((or (map (elem v) cs)), (or (map (elem (-v)) cs))) of 
                            (True,False) -> (True,v)
                            (True,True)   -> (False,1)
                            (False,True)  -> (True,(-v))
                            (False,False)  -> (False,1) 
                            
check_ple []  v      =  (False,1)
check_ple _ []       =  (False,1)
check_ple cs (v:vs)  = case (check_ple_on_l cs  v) of 
                                (True,v') -> (True,v')
                                (False,_) ->  check_ple cs vs

pure_literal_elemination (cs,v)   = case (check_ple cs v ) of 
                                    (True,v')        ->       Just ((remove_unit v' cs),assignproper v' v)
                                    (False,_)           ->       Nothing

--Resolution Propogation


checkleinclause c = or [and [(elem (-l) c),(elem l c)]  | l <- c] 

--check_le []     = False
--check_le (c:cs) | checkleinclause c  = True
--                | otherwise          = check_le cs

dole []      = []
dole (c:cs)  | checkleinclause c    =   dole cs
             | otherwise            =  c:(dole cs)

literal_elemination cs  =  dole cs

countlit cs l = 
    let m = length (filter (elem l) cs ) in
    let n = length (filter (elem (-l)) cs ) in 
    abs (m*n - m - n)

most_efficient_literal _ [x] = x
most_efficient_literal cs (x:y:xs) | (countlit cs x) < (countlit cs y)  = most_efficient_literal cs (y:xs)
                                   | otherwise                          = most_efficient_literal cs (x:xs)
 
allpairsunion s1 s2 = [sort (union a b) | a <- s1 , b <- s2]

resolve l cs =
            let (pos, others) = L.partition (elem l) cs in
            let (neg, others') = L.partition (elem (-l)) others in
            let set1 = nub (map (filter (\t -> t /= l)) (nub pos) ) in
            let set2 = nub (map (filter (\t -> t /= (-l))) (nub neg) ) in 
            union (nub (allpairsunion set1 set2)) cs


resolution_propogation (cs,v) = 
    let p = most_efficient_literal cs v in
        resolve p cs


--Main Algorithm 

dp ([],v) = (SAT, v)
dp (cs,v) | elem [] cs      =  (UNSAT,error "No assignment possible")
          | otherwise = case up of 
                            Just (cs', v') -> dp (literal_elemination cs',v')
                            Nothing        -> case le of 
                                                Just (cs'',v'') -> dp (literal_elemination cs'',v'')
                                                Nothing   -> dp (literal_elemination (resolution_propogation (cs,v)),v)
                            where up = unit_propogation (cs,v)
                                  le = pure_literal_elemination (cs,v)


main :: IO()
main = do
    cnftext <- getContents
    print $ dp (generate $ parseCNF (scanTokens cnftext))
