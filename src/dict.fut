type opt 'v = #some v
              | #none

module type dict = {
    type~ dict 'v
    type key = i32

    val many [n] 'v : [n]key -> [n]v -> dict v
    val single 'v : key -> v -> dict v
    val union 'v : dict v -> dict v -> dict v
    val lookup 'v: key -> dict v -> opt v
}


-- A simple implementation of module type dict
module arraydict : dict = {
    type key = i32
    type~ dict 'v = [](key, v)

    -- Stolen from Futhark examples: https://futhark-lang.org/examples/searching.html
    def find_index 'a [n] (p: a -> bool) (as: [n]a): i64 =
        let op (x, i) (y, j) =
            if x && y then if i < j
                then (x, i)
                else (y, j)
            else if y then (y, j)
            else (x, i)
        in (reduce_comm op (false, -1) (zip (map p as) (iota n))).1
    
    def many [n] 'v (ns : [n]key) (ts : [n]v) : dict v =
        let zs = map2 (\n t -> (n,t)) ns ts
        in remove_dup zs

    def single 'v (n : key) (t : v) : dict v =
        [(n, t)]

    -- sorting and comparing neighbors may be more efficient
    -- find a more stable way to remove duplicates
    def remove_dup 'v (d : dict v) : dict v =
        let n = length d
        let dflg = map (\i  -> if i == n-1 then false else any (\x -> x == d[i]) d[i+1:n]) (iota n)
        in (zip dflg d |> filter (\(i,_) -> i) |> unzip).1
        -- let n = length d
        -- let bs = tabulate_2d n n (\i j -> if i <= j then false else xs[i] == xs[j]) |> map (reduce (||) false)
        -- in (zip (iota n) xs |> filter (\(i,_) -> not bs[i]) |> unzip).1

    def union 'v (d : dict v) (d' : dict v) : dict v =
        let new = d ++ d'
        in remove_dup new

    def lookup 'v (n : key) (d : dict v) : opt v =
        let p (x: key, y: v) : bool = x == n
        let m = find_index p d
        in if m != -1 then #some d[m].1 else #none 
}

-- module sorteddict : dict = {

-- }

-- tænk over at lave bedre arraydict, hvor dictionarien er sorteret.
def test_arraydict =
    let arrd = arraydict.single 0 true
    let look = arraydict.lookup 1 arrd
    in look