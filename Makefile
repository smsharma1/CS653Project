main : clean lex parse compile 

lex:
	alex CNFTokens.x

parse:
	happy CNFGrammar.y

compile:
	ghc Main.hs
	ghc Davis-Putnam.hs
	ghc dpll.hs

clean :
	rm *.o *.hi dpll Davis-Putnam Main CNFGrammar.hs CNFTokens.hs