import "../lib/github.com/diku-dk/sorts/merge_sort"

type opt 'v = #some v 
            | #none

module type dict = {
    type key = i32
    type~ dict 'v

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
    
    -- please note: The following is neither benchmarked nor asymptotically optimized. A naive approach.
    -- sorting and comparing neighbors may be more efficient.
    -- here are some other (perhaps somewhat similar) ways to remove duplicates: https://futhark-lang.org/examples/removing-duplicates.html
    -- nub removes leading duplicates - could easily be changed to trailing duplicates.
    def nub 'v (d : dict v) : dict v =
        let n = length d
        let dflg = map (\i  -> if i == n-1 then false else any (\x -> x.0 == d[i].0) d[i+1:n]) (iota n)
        in (zip dflg (d :> [n](key, v)) |> filter (\(flg,_) -> not flg) |> unzip).1

    def many [n] 'v (ns : [n]key) (ts : [n]v) : dict v =
        nub (map2 (\n t -> (n,t)) ns ts)

    def single 'v (n : key) (t : v) : dict v =
        [(n, t)]
    
    def union 'v (d : dict v) (d' : dict v) : dict v =
        nub (d ++ d')

    def lookup 'v (n : key) (d : dict v) : opt v =
        let p (x: key, y: v) : bool = x == n
        let m = find_index p d
        in if m != -1 then #some d[m].1 else #none 
}

module sorteddict : dict = {
    type key = i32
    type~ dict 'v = [](key, v)

    def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
        let (l, _) =
            loop (l, r) = (0, n-1) while l < r do
            let t = l + (r - l) / 2
            in if x `lte` xs[t]
            then (l, t)
            else (t+1, r)
        in l
    
    -- let's define a sort function that use some predefined sort
    def sort 'v (d : dict v) : dict v =
        merge_sort_by_key (\i -> i.0) (i32.<=) d

    -- if we sort before removing duplicates, we can remove duplicates by checking neighbors 
    def nub_sorted 'v (d : dict v) : dict v =
        let dflg = map2 (\(x,_) (y,_) -> x == y) d (rotate (-1) d)
        in (zip dflg d |> filter (\(flg,_) -> not flg) |> unzip).1

    def many [n] 'v (ns : [n]key) (ts : [n]v) : dict v =
        map2 (\n t -> (n,t)) ns ts |> sort |> nub_sorted

    def single 'v (n : key) (t : v) : dict v =
        [(n, t)]
    
    def union 'v (d : dict v) (d' : dict v) : dict v =
        d ++ d' |> sort |> nub_sorted

    def lookup 'v (n : key) (d : dict v) : opt v =
        let bs = binary_search (<=) (unzip d).0 n
        in if length d > 0 && d[bs].0 == n then #some d[bs].1 else #none
}