EXE = AM trees dict

%: %.fut
	futhark c $<

clean:
	rm -f $(EXE) $(EXE:=.c)