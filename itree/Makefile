EXE = interval_tree helper
TESTS = $(EXE:=_tests)
COMP = $(EXE) $(TESTS)

%: %.fut
	futhark c $<

test_%: %_tests.fut
	futhark test $<

gen_data:
	futhark dataset -g [66536]f64 -g [66536]f64 -g f64 |> 16
	futhark dataset -g [131072]f64 -g [131072]f64 -g f64 |> 17
	futhark dataset -g [262144]f64 -g [262144]f64 -g f64 |> 18
	futhark dataset -g [524288]f64 -g [524288]f64 -g f64 |> 19
	futhark dataset -g [1048576]f64 -g [1048576]f64 -g f64 |> 20

gen_itrees:
	futhark cuda 

bench_itree: gen_data
	

clean:
	rm -f $(COMP) *.c *.fut.*
	rm -f 16 17 18 19 20