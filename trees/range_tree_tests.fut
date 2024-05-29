import "range_tree"

entry fix_box [d] (b1 : point [d]) (b2 : point [d]) : (point [d], point [d]) =
    map2 (\f s -> if f <= s then (f,s) else (s,f)) b1 b2 |> unzip

entry brute_query [n][d] (b : box [d]) (ps : [n]point [d]) : i64 =
    map (\p -> 
            if (map3 (\p lo hi ->
                p >= lo && p <= hi
            ) p b.0 b.1
            |> all (\t -> t)) then 1 else 0
        ) ps |> reduce (+) 0

-- entry loop_query [n][d] (b : box [d]) (ps : [n]point [d]) =
--     loop acc = 0 for x in ps do
--         if (loop alld = true for (k,i,j) in (x,b.0,b.1) do
--             if k >= i && k <= j then alld && true else false)
--             then acc + 1 else acc

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
-- compiled input @data/p2_10
-- compiled input @data/p3_10
-- compiled input @data/p4_10
entry create_tree [n][d] (ps : [n]point [d]) : k_range_tree.tree [d] =
    k_range_tree.build ps