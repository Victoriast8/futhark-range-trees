import "dict"

-- A simple implementation of module type dict
module arraydict : dict = {
    type key = i32
    type~ dict 'v = [](key, v)

    -- Stolen from Futhark examples: https://futhark-lang.org/examples/searching.html
    local def find_index 'a [n] (p: a -> bool) (as: [n]a): i64 =
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
    local def nub 'v (d : dict v) : dict v =
        let n = length d
        let dflg = map (\i  -> if i == n-1 then false else any (\x -> x.0 == d[i].0) d[i+1:n]) (iota n)
        in (zip dflg (d :> [n](key, v)) |> filter (\(flg,_) -> not flg) |> unzip).1

    

    def map 'a 'b (f: a -> b) (d : dict a) : dict b =
        map (\(k, v) -> (k, (f v))) d

    def reduce 'a (f : a -> a -> a) (ne : a) (d : dict a) : a =
        (unzip d).1 |> reduce f ne

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

-- module mk_arraydict (P:{type t 
--                         val == : t -> t -> bool}) =
--  {type key = P.t
--  }
-- module m = mk_arraydict f32
-- læs futhark bogen (specielt kapitel 4 om moduler)
-- parametricering af modul funktioner som ovenstående, kan bruges som yderligere abstraktion.
-- https://futhark-book.readthedocs.io/en/latest/modules.html