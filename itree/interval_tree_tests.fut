import "interval_tree"

-- Validation testing
def brute_count [n] (iv : [n]interval) (p : point) : i64 =
    map (\(l,h) -> if p >= l && h >= p then 1 else 0) iv |> reduce (+) 0

def fix_random_intervals [n] (iv : [n]interval) : [n]interval =
    let (fst,snd) = (map (.0) iv, map (.1) iv)
    in map2 (\f s -> if f < s then (f,s) else (s,f)) fst snd

-- ==
-- entry: validate_itree1D_count
-- random input { [100000]i64 [100000]i64 f64 } auto output
entry validate_itree1D_count [n] (iv1 : [n]i64) (iv2 : [n]i64) (p : point) : bool =
    let iv = map2 (\i1 i2 -> ((f64.i64 i1), (f64.i64 i2))) iv1 iv2
    let iv' = fix_random_intervals iv
    let t = itree1D.many iv'
    let traversal = itree1D.count p t
    let brute = brute_count iv' p
    in traversal == brute

-- Benchmarking