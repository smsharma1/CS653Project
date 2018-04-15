module Main where
import System.IO
import CNFTokens
import CNFGrammar
import Data.List as L

type Variables = Int
type Clauses = [Int]

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

-- removes clauses containing dec var and removes neg dec var from clauses
clauseElem :: (Eq a, Num a) => [[a]] -> a -> [[a]]
clauseElem cs v = [(remove (-1*v) k) | k <- cs , not (elem v k)]

generateforvo :: CNFFormula -> [Variables] -> ([Clauses], [Variables])
generateforvo (Formula v c t) v1 = (getclauses v c t, v1)

isVPureLit (cs,v) = (or (map (elem v) cs)) /= (or (map (elem (-1 * v)) cs))

findFirstPureLit (cs,[]) = 0
findFirstPureLit (cs,(v:vs)) | (isVPureLit (cs,v)) = v
                        | otherwise = findFirstPureLit (cs,vs)

pureLitElem (cs,vs) = takeDecision (cs,vs) (findFirstPureLit (cs,vs))

takeDecision (cs, vs) decV | decV==0 = dpll (cs,vs)
                        | otherwise = pureLitElem ((clauseElem cs decV), L.delete (abs decV) vs)


decision_up (cs,vs)  | elem [] cs    =   False
                    | vs == []       =   True
                    | otherwise     =   unitPropogate (cs,vs) (findsinglevar cs)

-- finds var present in single length clause from clauses if any
findsinglevar [] = 0
findsinglevar (c:cs) | (length c) == 1  =   (head c)
                     | otherwise        =   findsinglevar cs

-- check if a single variable clause is present, and further call decision on that variable decV
unitPropogate (cs,vs) decV   | decV == 0 =   pureLitElem (cs,vs)
                            | otherwise =   decision_up (clauseElem cs decV, L.delete (abs decV) vs)

-- returns at first True due to lazy evaluation
dpll (cs,vs)  | elem [] cs    =   False
            | vs == []        =   True
            | otherwise      =   (decision_up (clauseElem cs (head vs), tail vs)) || (decision_up (clauseElem cs (-1*(head vs)), tail vs))

dpllStatus (cs,vs) | (dpll (cs,vs)) = SAT
                   | otherwise = UNSAT

main :: IO()
main = do
    cnftext <- getContents
    print $ dpllStatus $ generate $ parseCNF (scanTokens cnftext)
