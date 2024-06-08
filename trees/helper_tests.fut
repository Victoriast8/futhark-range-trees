import "helper"
-- |- This file serves the purpose of debugging helper functions.
--    A function contained here, has posed a problem or has caused
--    some doubt of correctness.
--    Some functions *have not* been tested here.


-- ==
-- entry: test_intertwine
-- input { [1,2,3] [4,5,6] }
-- output { [1,4,2,5,3,6] }
entry test_intertwine [n] (as : [n]i32) (bs : [n]i32) : [n*2]i32 =
    intertwine as bs

-- ==
-- entry: test_partitionL
-- input 
-- { 
--   [true,false,true,false,true,false,true,false,true,false,true,false,true,false,true,false,true,false,true,false] 
--   0
--   [5,2,7,6]
--   [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
-- }
-- output 
-- { 
--   [3,1,3,3] 
--   [0,2,4,1,3,6,5,8,10,12,7,9,11,13,14,16,18,15,17,19]
-- }
entry test_partitionL [n] [m]
                  (condsL: [n]bool) (dummy: i32)
                  (shp: [m]i32) (arr: [n]i32) :
                  ([m]i32, [n]i32) =
    let (splits,(_,partitioned)) = partitionL condsL dummy (shp,arr)
    in (splits, partitioned)

-- ==
-- entry: test_flat_replicate_bools
-- input { [2i64,5i64,3i64,1i64] [true,false,true,false] }
-- output { [true,true,false,false,false,false,false,true,true,true,false] }
-- input { [1i64,4i64,0i64,0i64,1i64,0i64] [true,false,true,true,false,true] }
-- output { [true,false,false,false,false,false] }
entry test_flat_replicate_bools [n] (ns : [n]i64) (ms : [n]bool) : []bool =
    flat_replicate_bools ns ms
