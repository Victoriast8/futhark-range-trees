import "interval_tree"

entry fix_intervals [n] (iv1 : [n]f64) (iv2 : [n]f64) : ([n]f64,[n]f64) =
    map2 (\f s -> if f < s then (f,s) else (s,f)) iv1 iv2 |> unzip

def brute_count [n] (p : point) (iv : [n]interval) : i64 =
    map (\(l,h) -> if p >= l && h >= p then 1 else 0) iv |> reduce (+) 0

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
-- compiled input @data/16
-- compiled input @data/17
-- compiled input @data/18
-- compiled input @data/19
-- compiled input @data/20
entry create_tree [n] (iv1 : [n]f64) (iv2 : [n]f64) : itree1D.tree =
    itree1D.many (zip iv1 iv2)

-- ==
-- entry: bench_itree1D_count
-- script input { create_tree ($loaddata "data/16") }
-- script input { create_tree ($loaddata "data/17") }
-- script input { create_tree ($loaddata "data/18") }
-- script input { create_tree ($loaddata "data/19") }
-- script input { create_tree ($loaddata "data/20") }
entry bench_itree1D_count (t : itree1D.tree) =
    itree1D.count 0.5 t

-- entry prep_ivs [n] (iv1 : [n]f64) (iv2 : [n]f64) : [n]interval = zip iv1 iv2

-- ==
-- entry: bench_brute_count
-- compiled input @data/16
-- compiled input @data/17
-- compiled input @data/18
-- compiled input @data/19
-- compiled input @data/20
entry bench_brute_count [n] (iv1 : [n]f64) (iv2 : [n]f64) =
    brute_count 0.5 (zip iv1 iv2)