import "range_tree"

entry fix_box [d] (b1 : point [d]) (b2 : point [d]) : (point [d], point [d]) =
    map2 (\f s -> if f <= s then (f,s) else (s,f)) b1 b2 |> unzip

def brute_query [n][d] (b : box [d]) (ps : [n]point [d]) : i64 =
    map (\p -> 
            if (map3 (\p lo hi ->
                p >= lo && p <= hi
            ) p b.0 b.1
            |> all (\t -> t)) then 1 else 0
        ) ps |> reduce (+) 0

def loop_query [n][d] (b : box [d]) (ps : [n]point [d]) : i64 =
    loop acc = 0 for x in ps do
        if (loop alld = true for i in 0...d-1 do
            if x[i] >= b.0[i] && x[i] <= b.1[i] then alld && true else false)
            then acc + 1 else acc

-- Validation testing
-- ==
-- entry: validate_rtree2D_query
-- nobench input { [1.0,1.0] [5.7,5.8] [[1.1,2.2],[3.3,4.4],[5.5,6.6],[7.7,8.8]] }
-- output { true }
-- nobench random input { [2]f64 [2]f64 [64][2]f64 } output { true }
entry validate_rtree2D_query [n][d] (b1 : point [d]) (b2 : point [d]) (ps : [n]point [d]) : bool =
    let b = fix_box b1 b2
    let brute = brute_query b ps
    let t = k_range_tree.build ps
    let traversal = k_range_tree.count b t
    in (brute == traversal)


-- Benchmarking

-- ==
-- entry: create_tree
-- compiled input @data/p2_16
-- compiled input @data/p2_17
-- compiled input @data/p2_18
-- compiled input @data/p2_10
-- compiled input @data/p3_10
-- compiled input @data/p4_10
entry create_tree [n][d] (ps : [n]point [d]) : k_range_tree.tree [d] =
    k_range_tree.build ps


-- ==
-- entry: bench_rtree2D_count
-- script input { create_tree ($loaddata "data/p2_16") }
-- script input { create_tree ($loaddata "data/p2_17") }
-- script input { create_tree ($loaddata "data/p2_18") }
-- script input { create_tree ($loaddata "data/p2_10") }
-- script input { create_tree ($loaddata "data/p3_10") }
-- script input { create_tree ($loaddata "data/p4_10") }
entry bench_rtree2D_count [d] (t : k_range_tree.tree [d]) =
    let point = iota d |> map f64.i64
    in k_range_tree.count (point, (map (+1f64) point)) t

-- ==
-- entry: bench_brute_query
-- compiled input @data/p2_16
-- compiled input @data/p2_17
-- compiled input @data/p2_18
-- compiled input @data/p2_10
-- compiled input @data/p3_10
-- compiled input @data/p4_10
entry bench_brute_query [n][d] (ps : [n]point [d]) =
    let point = iota d |> map f64.i64
    in loop_query (point, (map (+1f64) point)) ps

