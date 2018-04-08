module Main where
import System.IO
import CNFTokens
import CNFGrammar
import Data.List as L

data Bintree a = Node a (Bintree a) (Bintree a) | SAT | UNSAT deriving Show

type Variables = Int
type Clauses = [Int]

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

-- removes clauses containing dec var and removes neg dec var from clauses
clauseElem :: (Eq a, Num a) => [[a]] -> a -> [[a]]
clauseElem cs v = [(remove (-1*v) k) | k <- cs , not (elem v k)]

decision_up (cs,v)  | elem [] cs    =   UNSAT
                    | v == []       =   SAT
                    | otherwise     =   unitPropogate (cs,v) (findsinglevar cs)
                    -- | (findsinglevar cs) == 0 =   dpll (cs,v)
                    -- | (findsinglevar cs) > 0  =   Node (abs (findsinglevar cs)) (decision_up (clauseElem cs (findsinglevar cs), L.delete (abs (findsinglevar cs)) v)) UNSAT
                    -- | otherwise =   Node (abs (findsinglevar cs)) UNSAT (decision_up (clauseElem cs (findsinglevar cs), L.delete (abs (findsinglevar cs)) v))
                    

-- finds var present in single length clause from clauses if any
findsinglevar [] = 0
findsinglevar (c:cs) | (length c) == 1  =   (head c)
                     | otherwise        =   findsinglevar cs

-- check if a single variable clause is present, and further call decision on that variable decV
unitPropogate (cs,v) decV   | decV == 0 =   dpll (cs,v)
                            | decV > 0  =   Node (abs decV) (decision_up (clauseElem cs decV, L.delete (abs decV) v)) UNSAT
                            | otherwise =   Node (abs decV) UNSAT (decision_up (clauseElem cs decV, L.delete (abs decV) v))



-- decisionneg_up :: (Eq a, Num a) => ([[a]], [a]) -> ([[a]], [a])
-- decisionneg_up (cs,v)         =      (clauseElem cs (-1*(head v)), drop 1 v)


-- dpll (cs,v)  | elem [] cs    =   UNSAT
--             | v == []        =   SAT
--             | otherwise      =   Node (head v) (decision_up (clauseElem cs decV, L.delete (abs decV) v) (head v)) (decision_up (clauseElem cs decV, L.delete (abs decV) v) (-1*(head v)))

dpll (cs,v)  | elem [] cs    =   UNSAT
            | v == []        =   SAT
            | otherwise      =   Node (head v) (decision_up (clauseElem cs (head v), tail v)) (decision_up (clauseElem cs (-1*(head v)), tail v))


main :: IO()
main = do
    cnftext <- getContents
    print $ dpll $ generate $ parseCNF (scanTokens cnftext)
    -- print $ parseCNF (scanTokens cnftext)
