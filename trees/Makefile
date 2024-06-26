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
	futhark dataset -g [1024]f64 -g [1024]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i10
	futhark dataset -g [2048]f64 -g [2048]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i11
	futhark dataset -g [4096]f64 -g [4096]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i12
	futhark dataset -g [8292]f64 -g [8292]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i13
	futhark dataset -g [16384]f64 -g [16384]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i14
	futhark dataset -g [32786]f64 -g [32786]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i15
	futhark dataset -g [66536]f64 -g [66536]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i16
	futhark dataset -g [131072]f64 -g [131072]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i17
	futhark dataset -g [262144]f64 -g [262144]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i18
	futhark dataset -g [524288]f64 -g [524288]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i19
	futhark dataset -g [1048576]f64 -g [1048576]f64 | ./interval_tree_tests -e fix_intervals > $(DATA_DIR)/i20

# |- validation can be performed using 'make test_interval_tree',
#    although to a slightly less convincing degree
validate_1D_count: gen_intervals
	$(info )
	$(info *** VALIDATION ***)
	$(info )
	cat data/i16 | ./interval_tree_tests -e validate_itree1D_count
	cat data/i17 | ./interval_tree_tests -e validate_itree1D_count
	cat data/i18 | ./interval_tree_tests -e validate_itree1D_count
	cat data/i19 | ./interval_tree_tests -e validate_itree1D_count
	cat data/i20 | ./interval_tree_tests -e validate_itree1D_count

bench_1D_count: gen_intervals
	futhark bench interval_tree_tests.fut --backend=c
	futhark bench interval_tree_tests.fut --entry-point=create_tree --backend=multicore
	futhark bench interval_tree_tests.fut --entry-point=create_tree --backend=cuda

gen_points:
	futhark c range_tree_tests.fut
	mkdir -p $(DATA_DIR)
	futhark dataset -g [2]f64 -g [2]f64 | ./range_tree_tests -e fix_box > $(DATA_DIR)/pb2
	futhark dataset -g [3]f64 -g [3]f64 | ./range_tree_tests -e fix_box > $(DATA_DIR)/pb3
	futhark dataset -g [4]f64 -g [4]f64 | ./range_tree_tests -e fix_box > $(DATA_DIR)/pb4
	futhark dataset -g [1024][2]f64 > $(DATA_DIR)/p2_10
	futhark dataset -g [2048][2]f64 > $(DATA_DIR)/p2_11
	futhark dataset -g [4096][2]f64 > $(DATA_DIR)/p2_12
	futhark dataset -g [8292][2]f64 > $(DATA_DIR)/p2_13
	futhark dataset -g [16384][2]f64 > $(DATA_DIR)/p2_14
	futhark dataset -g [32786][2]f64 > $(DATA_DIR)/p2_15
	futhark dataset -g [66536][2]f64 > $(DATA_DIR)/p2_16
	futhark dataset -g [131072][2]f64 > $(DATA_DIR)/p2_17
	futhark dataset -g [262144][2]f64 > $(DATA_DIR)/p2_18
	futhark dataset -g [1024][3]f64 > $(DATA_DIR)/p3_10
	futhark dataset -g [1024][4]f64 > $(DATA_DIR)/p4_10

validate_KD_query: gen_points
	$(info )
	$(info *** VALIDATION ***)
	$(info )
	{ cat data/pb2 & cat data/p2_16; } | ./range_tree_tests -e validate_rtree2D_query
	{ cat data/pb2 & cat data/p2_17; } | ./range_tree_tests -e validate_rtree2D_query
	{ cat data/pb2 & cat data/p2_10; } | ./range_tree_tests -e validate_rtree2D_query
	{ cat data/pb3 & cat data/p3_10; } | ./range_tree_tests -e validate_rtree2D_query
	{ cat data/pb4 & cat data/p4_10; } | ./range_tree_tests -e validate_rtree2D_query

bench_range_tree: gen_points
	futhark bench range_tree_tests.fut --backend=c
	futhark bench range_tree_tests.fut --entry-point=create_tree --backend=multicore
	futhark bench range_tree_tests.fut --entry-point=create_tree --backend=cuda


clean:
	rm -f $(COMP) *.c *.fut.*
	rm -rf $(DATA_DIR)
