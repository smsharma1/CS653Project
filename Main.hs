module Main where
import System.IO
import CNFTokens
import CNFGrammar
import Data.List as L
import System.Environment


data Bintree a = Node a (Bintree a) (Bintree a) | SAT | UNSAT deriving Show

type Variables = Int
type Clauses = [Int]

getmodels :: Num a => Bintree t -> a
getmodels SAT = 1
getmodels UNSAT = 0
getmodels (Node a t1 t2) = (getmodels t1) + (getmodels t2)

getmodelsconfig v SAT = [v]
getmodelsconfig v UNSAT = [[]]
getmodelsconfig v (Node a t1 t2) = (getmodelsconfig (v ++ [a]) t1 ) ++ (getmodelsconfig (v ++ [(-a)]) t2)


checkmodel _ SAT = True
checkmodel _ UNSAT = False
checkmodel v (Node a t1 t2) |  elem a v     =   checkmodel v t1
                            |  elem (-a) v  =   checkmodel v t2
                            |  otherwise    =   False


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

decision_up (cs,v)  | elem [] cs    =   UNSAT
                    | v == []       =   SAT
                    | otherwise     =   unitPropogate (cs,v) (findsinglevar cs)

-- finds var present in single length clause from clauses if any
findsinglevar [] = 0
findsinglevar (c:cs) | (length c) == 1  =   (head c)
                     | otherwise        =   findsinglevar cs

-- check if a single variable clause is present, and further call decision on that variable decV
unitPropogate (cs,v) decV   | decV == 0 =   dpll (cs,v)
                            | decV > 0  =   Node (abs decV) (decision_up (clauseElem cs decV, L.delete (abs decV) v)) UNSAT
                            | otherwise =   Node (abs decV) UNSAT (decision_up (clauseElem cs decV, L.delete (abs decV) v))

dpll (cs,v)  | elem [] cs    =   UNSAT
            | v == []        =   SAT
            | otherwise      =   Node (head v) (decision_up (clauseElem cs (head v), tail v)) (decision_up (clauseElem cs (-1*(head v)), tail v))

generatevo 0 b c = b
generatevo a b c = generatevo (a-1) (b ++ [(read (head c) :: Int)]) (drop 1 c)

getmodel a [] = a
getmodel a b  = getmodel (a ++ [(read (head b) :: Int)]) (drop 1 b)


main :: IO()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    cnftext <- hGetContents handle
    if ((args !! 1) == "getmodels") then (print $ getmodels $ dpll $ generate $ parseCNF (scanTokens cnftext))
    else (if ((args !! 1) == "tree") then  (print $ dpll $ generate $ parseCNF (scanTokens cnftext))
    else (if ((args !! 1) == "vo") then  (print $ dpll $ generateforvo (parseCNF (scanTokens cnftext)) (generatevo (read (args !! 2)::Int) [] (drop 3 args)))
    else (if ((args !! 1) == "checkmodel") then  (print $ checkmodel (getmodel [] (drop 2 args)) (dpll $ generate $ parseCNF (scanTokens cnftext)))        
    else (if ((args !! 1) == "getmodelsconfig") then  (print $ filter (not . null) (nub ( getmodelsconfig [] (dpll $ generate $ parseCNF (scanTokens cnftext)))))
            else ( putStrLn "Insufficient commands")))))
