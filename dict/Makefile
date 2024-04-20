EXE = arraydict sorteddict
TESTS = $(EXE:=_tests)
COMP = $(EXE) $(TESTS)

%: %.fut
	futhark c $<

test_%: %_tests.fut
	futhark test $<

clean:
	rm -f $(COMP) *.c *.fut.*