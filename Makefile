EXE = AM

default:
	futhark c $(EXE:=.fut) 
# futhark compilation in this way only works on one input anyway

clean:
	rm $(EXE) $(EXE:=.c)