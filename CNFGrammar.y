{
module CNFGrammar where
import CNFTokens
}

%name parseCNF
%tokentype { CNFToken }
%error { parseError }

%token
    'p'  { TokenStartOfFormula }
    int  { TokenInt $$ }
    '-'  { TokenNeg }
    '0'  { TokenEndOfClause }

%left '-'
%%

Formula : 'p' int int PartialFormula { Formula $2 $3 $4 }

PartialFormula : Clause Clause { PartialFormula $1 $2 }
               | Clause PartialFormula { PartialFormulaJoined $1 $2 }

Clause : PartialClause '0' { Clause $1  }
       | Literal '0'       { UnitClause $1 }

PartialClause : Literal Literal { PartialClause $1 $2 }
              | Literal PartialClause   { PartialClauseJoined $1 $2 }

Literal : '-' Literal   { Negative $2 }
        |  int          { Literal $1 }

{

data CNFFormula = Literal Int
                | Negative CNFFormula
                | PartialClause CNFFormula CNFFormula
                | PartialClauseJoined CNFFormula CNFFormula
                | Clause CNFFormula
                | UnitClause CNFFormula
                | PartialFormula CNFFormula CNFFormula
                | PartialFormulaJoined CNFFormula CNFFormula
                | Formula Int Int CNFFormula
                deriving Show

parseError :: [ CNFToken ] -> a
parseError a = error "Error in Parsing"

}
