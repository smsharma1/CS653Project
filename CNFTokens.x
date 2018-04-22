{
module CNFTokens where    
}

%wrapper "basic"

$digit  = 0-9
$eol    = [\n]

tokens :-

    $eol    ;
    $white+ ;
    p       ;
    cnf     { \s -> TokenStartOfFormula }
    0       { \s -> TokenEndOfClause }
    $digit+ { \s -> TokenInt ( read s ) }
    \-      { \s -> TokenNeg }


{
    
data CNFToken = TokenStartOfFormula
              | TokenNeg
              | TokenEndOfClause
              | TokenInt Int
              deriving ( Eq,Show )

scanTokens = alexScanTokens

}
