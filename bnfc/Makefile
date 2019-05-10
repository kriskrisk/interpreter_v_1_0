all:
	happy -gca ParCH.y
	alex -g LexCH.x
	ghc --make TestCH.hs -o TestCH

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCH.* LexCH.* ParCH.* LayoutCH.* SkelCH.* PrintCH.* TestCH.* AbsCH.* TestCH ErrM.* SharedString.* ComposOp.* c-h.dtd XMLCH.* Makefile*
	
