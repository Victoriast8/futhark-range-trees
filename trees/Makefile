EXE = interval_tree range_tree helper
TESTS = $(EXE:=_tests)
COMP = $(EXE) $(TESTS)
DATA_DIR = data


%: %.fut
	futhark c $<

test_%: %_tests.fut
	futhark test $<

gen_intervals:
	futhark c interval_tree_tests.fut
	mkdir -p $(DATA_DIR)
	futhark dataset -g [66536]f64 -g [66536]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/16
	futhark dataset -g [131072]f64 -g [131072]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/17
	futhark dataset -g [262144]f64 -g [262144]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/18
	futhark dataset -g [524288]f64 -g [524288]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/19
	futhark dataset -g [1048576]f64 -g [1048576]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/20

# |- validation can be performed using 'make test_interval_tree',
#    although to a slightly less convincing degree
validate_1D_count: gen_intervals
	$(info )
	$(info *** VALIDATION ***)
	$(info )
	cat data/16 | ./interval_tree_tests -e validate_itree1D_count
	cat data/17 | ./interval_tree_tests -e validate_itree1D_count
	cat data/18 | ./interval_tree_tests -e validate_itree1D_count
	cat data/19 | ./interval_tree_tests -e validate_itree1D_count
	cat data/20 | ./interval_tree_tests -e validate_itree1D_count

bench_1D_count: gen_intervals
	futhark bench interval_tree_tests.fut --entry-point=create_tree
	futhark bench interval_tree_tests.fut --entry-point=bench_itree1D_count
	futhark bench interval_tree_tests.fut --entry-point=bench_brute_count

clean:
	rm -f $(COMP) *.c *.fut.*
	rm -rf $(DATA_DIR)
