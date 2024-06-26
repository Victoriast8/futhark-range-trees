import "dict"
import "../lib/github.com/diku-dk/sorts/merge_sort"

module sorteddict : dict = {
    type k = i32
    type~ dict 'v = [](k, v)

    -- Stolen from: https://futhark-lang.org/examples/binary-search.html
    -- Eytzinger may be considered, for specific sizes
    local def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
        let (l, _) =
            loop (l, r) = (0, n-1) while l < r do
            let t = l + (r - l) / 2
            in if x `lte` xs[t]
            then (l, t)
            else (t+1, r)
        in l
    
    -- let's define a sort function that use some predefined sort
    local def sort 'v (d : dict v) : dict v =
        merge_sort_by_key (.0) (i32.<=) d

    -- if we sort before removing duplicates, we can remove duplicates by checking neighbors
    local def nub_sorted 'v (d : dict v) : dict v =
        (zip3 (indices d) d (rotate (-1) d)
        |> filter (\(i,x,y) -> i == 0 || x.0 > y.0) |> unzip3).1

    local def ilog2 (x: i64) = 63 - i64.i32 (i64.clz x)

    def size 'v (d : dict v) : i64 =
        length d

    def d_map 'a 'b (f: a -> b) (d : dict a) : dict b =
        map (\(k, v) -> (k, (f v))) d

    def d_reduce 'a (f : a -> a -> a) (ne : a) (d : dict a) : a =
        (unzip d).1 |> reduce f ne
    
    def d_filter 'v (f: k -> v -> bool) (d : dict v) : dict v =
        filter (\(x,y) -> f x y) d

    -- Consider using insertion as the method of sorting.
    -- Building should have nlgn work anyways 
    def many [n] 'v (ns : [n]k) (ts : [n]v) : dict v =
        map2 (\n t -> (n,t)) ns ts |> sort |> nub_sorted

    def single 'v (key : k) (value : v) : dict v =
        [(key, value)]

    def insert 'v (key : k) (value : v) (d : dict v) : dict v =
        let s = size d
        in if s == 0 then single key value -- if dict is empty, return single
            else let i = binary_search (<=) (unzip d).0 key
            in if (i+1) == s && d[i].0 < key
                then d ++ [(key,value)] -- if it is the biggest value
                else d[:i] ++ [(key,value)] ++ d[i:] -- otherwise, insert before index i
    
    -- Consider using insertion as the method of sorting.
    -- Building should have nlgn work anyways 
    def union 'v (d : dict v) (d' : dict v) : dict v =
        let dedup_time (d1: dict v) (d2: dict v) : bool =
            let (s1, s2) = (size d1, size d2)
            in ilog2 s1 <= ilog2 (s1+s2) && ilog2 s2 <= ilog2 (s1+s2)
        in if dedup_time d d'
        then
            (d ++ d' |> sort |> nub_sorted)
        else 
            (d ++ d' |> sort)

    def lookup 'v (n : k) (d : dict v) : opt v =
        let bs = binary_search (<=) (unzip d).0 n
        in if length d > 0 && d[bs].0 == n then #some d[bs].1 else #none

    -- Filter is slow. Removing a single element is not easy...
    def delete 'v (d : dict v) (key : k) : dict v =
        filter (\(i,_) -> i != key) d
}

-- module mk_arraydict (P:{type t val ==: t -> t -> bool}) =
--  {type k = P.t
--  }
-- module m = mk_arraydict f32
-- læs futhark bogen (specielt kapitel 4 om moduler)
-- parametricering af modul funktioner som ovenstående, kan bruges som yderligere abstraktion.
-- https://futhark-book.readthedocs.io/en/latest/modules.html