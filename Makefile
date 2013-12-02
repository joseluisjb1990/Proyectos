all: Main.hs
	 ghc -o music --make Main.hs

clean:
	$(RM) music *.hs~ *.o *.hi
