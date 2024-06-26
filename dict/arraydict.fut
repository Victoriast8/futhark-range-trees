import "dict"
import "../lib/github.com/diku-dk/sorts/merge_sort"

-- A simple implementation of module type dict
module mk_arraydict (P:{type t val == : t -> t -> bool}) : dict with k = P.t = {
    type k = P.t
    type~ dict 'v = [](k, v)

    -- Stolen from Futhark examples: https://futhark-lang.org/examples/searching.html
    local def find_index 'a [n] (p: a -> bool) (as: [n]a): i64 =
        let op (x, i) (y, j) =
            if x && y then if i < j
                then (x, i)
                else (y, j)
            else if y then (y, j)
            else (x, i)
        in (reduce_comm op (false, -1) (zip (map p as) (iota n))).1
    
    ------
    -- As a note for this deduping:
    -- The Futhark documentation may have a more efficient way of removing duplicates,
    -- while keeping the order of elements intact...
    -- We keep two versions for now, as the naive nub implementation may have some merit.
    -- ALSO: consider sorting and removing neighbors - preserving the order of elements
    -- may also be ignored! We could consider the arraydict, to be the greedy or naive dict.
    ------
    local def nub 'v (d : dict v) : dict v =
        let n = length d
        let dflg = map (\i  -> if i == n-1 then false else any (\x -> x.0 P.== d[i].0) d[i+1:n]) (iota n)
        in (zip dflg (d :> [n](k, v)) |> filter (\(flg,_) -> not flg) |> unzip).1

    -- Still somewhat inefficient due to the double sorting, as explained in the docs:
    -- https://futhark-lang.org/examples/removing-duplicates.html
    -- local def doc_nub 'v (d : dict v) : dict v =
    --     let sorted = zip d (indices d)
    --                  |> merge_sort_by_key (\i -> i.0.0) (i32.<=)
    --     let (ds, is) = unzip sorted
    --     let pack = zip4 (indices ds) ds (rotate (-1) ds) is
    --                |> filter (\(i,x,y,_) -> i == 0 || x.0 > y.0)
    --     in merge_sort_by_key (\i -> i.3) (i64.<=) pack |> map (.1)

    local def ilog2 (x: i64) = 63 - i64.i32 (i64.clz x)

    def size 'v (d : dict v) : i64 =
        length d

    def d_map 'a 'b (f: a -> b) (d : dict a) : dict b =
        map (\(k, v) -> (k, (f v))) d

    def d_reduce 'a (f : a -> a -> a) (ne : a) (d : dict a) : a =
        (unzip d).1 |> reduce f ne
    
    def d_filter 'v (f: k -> v -> bool) (d : dict v) : dict v =
        filter (\(x,y) -> f x y) d

    -- creates many values at one time. Always removes duplicates.
    def many [n] 'v (ns : [n]k) (ts : [n]v) : dict v =
        map2 (\n t -> (n,t)) ns ts |> nub

    def single 'v (key : k) (value : v) : dict v =
        [(key, value)]

    def insert 'v (key : k) (value : v) (d : dict v) : dict v =
        d ++ [(key, value)]
    
    def union 'v (d : dict v) (d' : dict v) : dict v =
         -- decides if its time to dedup.
         -- if the united array is a power of two or more bigger than both
        let dedup_time (d1: dict v) (d2: dict v) : bool =
            let (s1, s2) = (size d1, size d2)
            in ilog2 s1 <= ilog2 (s1+s2) && ilog2 s2 <= ilog2 (s1+s2)
        in if dedup_time d d' then nub (d ++ d') else d ++ d'

    def lookup 'v (n : k) (d : dict v) : opt v =
        let p (x: k, y: v) : bool = x P.== n
        let m = find_index p d
        in if m != -1 then #some d[m].1 else #none

    -- As this is unsorted, filtering mimics worst case runtime.
    def delete 'v (d : dict v) (key : k) : dict v =
        filter (\(i,_) -> !(i P.== key)) d
}

module arraydict = mk_arraydict i32
-- module m = mk_arraydict f32