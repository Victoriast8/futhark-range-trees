import "interval_tree"

-- Validation testing
def brute_count [n] (iv : [n]interval) (p : point) : i64 =
    map (\(l,h) -> if p >= l && h >= p then 1 else 0) iv |> reduce (+) 0

-- ==
-- entry: validate_itree1D_count
-- nobench random input { [100000]f64 [100000]f64 } output { 0i64 }
-- nobench input 
-- { [0f64,100f64,10000f64,42f64]
--   [93f64,100f64,100f64,73f64]
-- } 
-- output { 0i64 }
entry validate_itree1D_count [n] (iv1 : [n]f64) (iv2 : [n]f64) : bool =
    let iv = fix_random_intervals <| zip iv1 iv2
    let p = ((last iv).0 + (last iv).1)*0.5 -- mostly targeted at random datasets
    let t = itree1D.many iv
    let traversal = itree1D.count p t
    let brute = brute_count iv p
    in brute == traversal

entry debug_itree1D_count [n] (iv1 : [n]f64) (iv2 : [n]f64) : (i64,i64) =
    let iv = fix_random_intervals <| zip iv1 iv2
    let p = ((last iv).0 + (last iv).1)*0.5
    let t = itree1D.many iv
    let traversal = itree1D.count p t
    let brute = brute_count iv p
    in (brute,traversal)

-- Benchmarking