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
    
    -- sorting and comparing neighbors may be more efficient
    -- question for troels - how does this relate to an optimal solution? Am I way off?
    def remove_dup 'v (d : dict v) : dict v =
        let n = length d
        let dflg = map (\i  -> if i == n-1 then false else any (\x -> x.0 == d[i].0) d[i+1:n]) (iota n)
        in (zip dflg (d :> [n](key, v)) |> filter (\(flg,_) -> not flg) |> unzip).1

    def many [n] 'v (ns : [n]key) (ts : [n]v) : dict v =
        let zs = map2 (\n t -> (n,t)) ns ts
        in remove_dup zs

    def single 'v (n : key) (t : v) : dict v =
        [(n, t)]
    
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


def test_arraydict =
    let arrd = arraydict.single 0 true
    let look = arraydict.lookup 1 arrd
    in look

def test_duplicate_keys =
    arraydict.many [1,1,1,1] [true,true,false,false]

