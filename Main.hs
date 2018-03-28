module Main where
import System.IO
import CNFTokens
import CNFGrammar

type Variables = Int
type Clauses = [Int]


literal v (Literal a) = if v < a then error "Parsing Error" else a
literal v (Negative (Literal a)) = if v < a then error "Parsing Error" else -a


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

main :: IO()
main = do
    cnftext <- getContents
    print $ generate $ parseCNF (scanTokens cnftext)
