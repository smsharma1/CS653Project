module Main where
import System.IO
import CNFTokens
import CNFGrammar

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

poselem :: (Eq a, Foldable t) => [t a] -> a -> [t a]
poselem c v = [k | k <- c , not (elem v k)]

decisionpos_up :: (Eq a, Foldable t) => ([t a], [a]) -> ([t a], [a])
decisionpos_up (c,v)         =      (poselem c (head v), drop 1 v)

negelem :: Eq a => [[a]] -> a -> [[a]]
negelem c v = map (remove v) c

decisionneg_up (c,v)         =      (negelem c (head v), drop 1 v)


dpll (c,v)  | elem [] c      =   UNSAT
            | v == []        =   SAT
            | otherwise      =   Node (head v) (dpll (decisionpos_up (c,v))) (dpll (decisionneg_up (c,v)))

main :: IO()
main = do
    cnftext <- getContents
    print $ dpll $ generate $ parseCNF (scanTokens cnftext)
