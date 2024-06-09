import "interval_tree"

entry fix_intervals [n] (iv1 : [n]f64) (iv2 : [n]f64) : ([n]f64,[n]f64) =
    map2 (\f s -> if f < s then (f,s) else (s,f)) iv1 iv2 |> unzip

def brute_count [n] (p : point) (iv : [n]interval) : i64 =
    map (\(l,h) -> if p >= l && h >= p then 1 else 0) iv |> reduce (+) 0

-- perhaps also compare to a loop, that uses binary search on sorted intervals?
def loop_count [n] (p : point) (iv : [n]interval) : i64 =
    loop acc = 0 for x in iv do
        if p >= x.0 && p <= x.1 then acc + 1 else acc

-- Validation testing

-- validates itree1D.count (and indirectly itree1D.many), by comparing
-- the tree-count result with the result of a trivial brute-count
-- ==
-- entry: validate_itree1D_count
-- nobench random input { [100000]f64 [100000]f64 } output { true }
-- nobench input 
-- { [0f64,100f64,100f64,42f64]
--   [93f64,100f64,10000f64,73f64]
-- } 
-- output { true }
entry validate_itree1D_count [n] (iv1 : [n]f64) (iv2 : [n]f64) : bool =
    let iv = zip iv1 iv2
    let p = ((last iv).0 + (last iv).1)*0.5 -- mostly targeted at random datasets
    let t = itree1D.many iv
    let traversal = itree1D.count p t
    let brute = brute_count p iv
    in brute == traversal

-- Benchmarking

-- ==
-- entry: create_tree
-- compiled input @data/i16
-- compiled input @data/i17
-- compiled input @data/i18
-- compiled input @data/i19
-- compiled input @data/i20
entry create_tree [n] (iv1 : [n]f64) (iv2 : [n]f64) : itree1D.tree =
    itree1D.many (zip iv1 iv2)

-- ==
-- entry: bench_itree1D_count
-- script input { create_tree ($loaddata "data/i10") }
-- script input { create_tree ($loaddata "data/i11") }
-- script input { create_tree ($loaddata "data/i12") }
-- script input { create_tree ($loaddata "data/i13") }
-- script input { create_tree ($loaddata "data/i14") }
-- script input { create_tree ($loaddata "data/i15") }
-- script input { create_tree ($loaddata "data/i16") }
-- script input { create_tree ($loaddata "data/i17") }
-- script input { create_tree ($loaddata "data/i18") }
-- script input { create_tree ($loaddata "data/i19") }
-- script input { create_tree ($loaddata "data/i20") }
entry bench_itree1D_count (t : itree1D.tree) =
    itree1D.count 0.5 t

-- ==
-- entry: bench_brute_count
-- compiled input @data/i10
-- compiled input @data/i11
-- compiled input @data/i12
-- compiled input @data/i13
-- compiled input @data/i14
-- compiled input @data/i15
-- compiled input @data/i16
-- compiled input @data/i17
-- compiled input @data/i18
-- compiled input @data/i19
-- compiled input @data/i20
entry bench_brute_count [n] (iv1 : [n]f64) (iv2 : [n]f64) =
    brute_count 0.5 (zip iv1 iv2)