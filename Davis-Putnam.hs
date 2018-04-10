module Main where
import System.IO
import CNFTokens
import CNFGrammar
import Data.List as L
import Data.Maybe

data Bintree a = Node a (Bintree a) (Bintree a) | SAT | UNSAT deriving Show

type Variables = Int
type Clauses = [Int]
type Literal = Int



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

checkleinclause c = or [and [(elem (-l) c),(elem l c)]  | l <- c] 

check_le []     = False
check_le (c:cs) | checkleinclause c  = True
                | otherwise          = check_le cs
doleinclause []     =   []
doleinclause (c:cs)   |  (elem (-c) cs) = doleinclause (remove (-c) cs)
                    |  otherwise      = c:(doleinclause cs)
dole []      = []
dole (c:cs)  | checkleinclause c    =  (doleinclause c : dole cs)
             | otherwise            =  c:(dole cs)

literal_elemination cs |  check_le cs   =  Just (dole cs)
                       |  otherwise     =  Nothing
    
--dp ([],v) = (SAT, v)
--dp (cs,v) | elem [] cs      =  (UNSAT,v)
--          | otherwise = case up of 
--                            Just (cs', v') -> dp (cs',v')
--                            Nothing        -> case le of 
--                                                Just cs'' -> dp (cs'',v)
--                                                Nothing          -> dp ((resolution_propogation cs),v)
--                            where up = unit_propogation (cs,v)
--                                  le = literal_elemination cs


--main :: IO()
--main = do
--    cnftext <- getContents
--    print $ dp $ generate $ parseCNF (scanTokens cnftext)
