import "../lib/github.com/diku-dk/sorts/merge_sort"
import "helper"

-- interval trees

module type itree = {
    type interval
    type point
    type~ tree

    -- count må gerne være sekventiel - vi kan kalde den med maps: parallelismen ligger i many
    val count : point -> tree -> i64 -- skal tælle hvor mange intervaller indeholder et punkt
    val many [n] : [n]interval -> tree
    -- ...
}
-- start med at skrive en rekursiv many
-- derefter lav en futhark lad som om det er et rekusivt sprog
-- lav den rekursive om til en lifted/flad version
-- læs op intervaltræer igen

module itree1D : itree with interval = (f64, f64)
                       with point = f64
                       = {
    type interval = (f64, f64)
    type point = (f64)
    -- conventions for child (L,R) pointers: if negative, child doesn't exist.
    -- out of bounds is not taken into account, but may later be discarded in a similar fashion to negative numbers if necessary.
    type~ tree = [](point,(i32,i32),[]interval,[]interval)

    local def sort_by_key [n] 't (key : t -> f64) (xs : [n]t) : [n]t =
        merge_sort_by_key key (f64.<=) xs

    def count (p : point) (t : tree) : i64 =
        if length t == 0 then 0 else
            let (_,cnt) = loop (i,acc) = (0,0) while i > 0 do
                let c = t[i]
                in if !(p == c.0) then
                    let dir = p < c.0
                    -- 'h' below could probably be calculated with a loop instead, with less work (but with a bigger span!)
                    -- (due to these being sorted by interval startpoint/endpoint)
                    -- with a map, you can actually avoid the whole sorting process and only keep track of single overlapping intervals
                    let (h,new_i) = if dir then ((map (\(j,_) -> if p > j then 1 else 0) c.2), c.1.0) 
                                           else ((map (\(_,j) -> if p < j then 1 else 0) c.3), c.1.1)
                    let sum = reduce (+) 0 h
                    in (new_i,(acc + sum))
                else
                    (-1, acc + (length c.2))
            in cnt
    

    def many [n] (iv : [n]interval) : tree =
        let avg_intvls (x,y) =
            (x/2.0) + (y/2.0)
        let x_cent = reduce (+) 0.0 (map avg_intvls iv)
        let shp = [(i32.i64 n)]
        let res = []
        let (_,_,res) = loop (iv,shp,res) while (length iv > 0) do
            let fst_i = scanExcl (+) 0 shp |> map i64.i32
            let begs  = scan (+) 0 shp
            let flags = mkFlagArray shp 0i32 <| map (+1) <| map i32.i64 (iota (length shp))
            --let II1 = sgmSumInt flags <| map (\f -> if f==0 then 0 else f-1) flags

            -- again, a "qualified guess" may be be just as good as the actual average x_center
            -- ... it now dawns on me: What happens to x_cents, if there are only two non-overlapping intervals left in that segment?
            -- actually, this won't be a problem. The problem will be subdivided once again, making an "empty" (no overlapping intervals) node
            let avgs    = sgmScan (+) 0.0 flags <| map avg_intvls iv
            let x_cents = map2 (\b s -> if s == 0 then 0 else avgs[b-1]/(f64.i32 s)) begs shp -- check in map, only if shp contains sizes of 0
            let scx     = sgmScan (+) 0.0 (flags :> [length iv]i32)
                                    <| map f64.i64 
                                    <| scatter (replicate (length iv) 0) fst_i (map i64.f64 x_cents)
 
            -- please see "helper.fut" as to why two partitions are necessary
            let conds1        = map2 (\center (_,y) -> y < center) scx (iv :> [length iv]interval)
            let (ps, (_,iv')) = partition2L conds1 (0.0,0.0) ((shp :> [length shp]i32),(iv :> [length iv]interval))

            let conds2          = map2 (\center (x,_) -> x > center) scx iv'
            let (ps', (_,iv'')) = partition2L conds1 (0.0,0.0) (shp,iv')
            
            let shp' = intertwine ps' ps
            let ps'' = map2 (\x y -> x+y) ps ps'

            -- use partition, to partition split points
            -- first build false/true
            let (ns, ms)   = map2 (\p'' s -> [(p'',true),((s-p''),false)]) ps'' shp |> flatten |> unzip
            let condsSpl   = flat_replicate_bools (map i64.i32 ns) ms
            -- now we simply partition by the built boolean array
            let (p1,p2)    = partition (\(_,cond) -> cond) (zip iv'' (condsSpl :> [length iv]bool))
            let (iv''',rs) = ((unzip p1).0, (unzip p2).0)

            -- all that remains, is to put the "hit" intervals into res (the accumulated tree)
            let rs_shp = map2 (\x y -> y-x) ps'' shp

            -- NOTE: before I write this code, I know it won't be the right way to do it.
            -- We make this now, to hopefully have something that works in practice
            let srs  = scanExcl (+) 0 rs_shp
            let res' = map3 (\p s r -> (p,(-1,-1),(rs[s:(s+r)]),(rs[s:(s+r)]))) x_cents srs rs_shp

            in (iv''',shp',res')
        in res

    -- def recursive_many [n] (iv : [n]interval) : tree =
    --     -- compute x_center
    --     let fsize = f64.i64 n
    --     let avg_intvls (x,y) =
    --         (x/(2.0*fsize)) + (y/(2.0*fsize))
    --     let x_cent = reduce (+) 0.0 (map avg_intvls iv)
    --     -- partition intervals
    --     let (p1,p2,p3) = partition2 (\(x,_) -> x > x_cent) (\(_,y) -> x_cent > y) iv
    --     -- run recursively on side intervals
    --     let (s1,s2) = (sort_by_key (.0) p2, sort_by_key (.1) p2)
    --     in (x_cent,(many p1, many p3),s1,s2)
}

module itree2D : itree with interval = (f64, f64)
                        with point = f64
                        = {
    type interval = (f64, f64)
    type point = (f64)
    type~ tree = [](point,(i32,i32),interval,interval)

    def count (p : point) (t : tree) : i64 =
        ??? 
    
    def many [n] (iv : [n]interval) : tree =
        ???
}

-- module interval_tree : dict = {
--     type k = i32
--     -- Let's start with something simple:
--     -- k & v: key-value pair
--     -- in order: L(eft), R(ight), P(arent). Pointers to children/parent of the given key-value pair.
--     type~ dict 'v = [](k,v,i32,i32,i32)

--     -- as to make it easier to change which sort this definition of ''dict'' uses
--     local def sort 'v (d : dict v) : dict v =
--         radix_sort_int_by_key (.0) i32.num_bits i32.get_bit d

--     local def ilog2 (x: i64) = 63 - i64.i32 (i64.clz x)

--     def size 'v (d : dict v) : i64 =
--         length d

--     -- Perhaps a depth function? udregning af depth i hvert element, height af træet eller begge?

--     -- Overvej om f også skal påvirker key'en
--     def d_map 'a 'b (f : a -> b) (d : dict a) : dict b =
--         ???

--     -- Overvej om f også skal påvirker key'en. I PAM artiklen tager mapReduce funktionen f key-value parret som input
--     def d_reduce 'a (f : a -> a -> a) (ne : a) (d : dict a) : a =
--         ???
    
--     def d_filter 'v (f: k -> v -> bool) (d : dict v) : dict v =
--         ???

--     -- TODO
--     def many [n] 'v (keys : [n]k) (values : [n]v) : dict v =
--         let m = map2 (\k v -> (k,v,-1,-1,0)) keys values
--         let s = sort m
--         let s' = 

--     def single 'v (key : k) (value : v) : dict v = [(key,value,-1,-1,0)]

--     -- TODO
--     def insert 'v (key : k) (value : v) (d : dict v) : dict v =
--         ???
    
--     -- TODO
--     def union 'v (d : dict v) (d' : dict v) : dict v =
--         ???

--     -- TODO
--     def lookup 'v (n : k) (d : dict v) : opt v =
--         ???

--     -- TODO
--     def delete 'v (d : dict v) (key : k) : dict v =
--         ???


--     -- -- Tree type. An array of tuples of:
--     -- -- 1+2. A dictionary / array-like structure key-value pair,
--     -- -- 3. A parent reference,
--     -- -- 4. A depth (maybe not necessary, perhaps omittet later...)
--     -- type~ tree 'k 'v = [](k,v,u32,(i32,i32))

--     -- -- represents an empty tree. To avoid type ambiguities, small dummy types are passed to the tree constructor.
--     -- def empty : tree u8 u8 =
--     --     []

--     -- def single 'k 'v (key: k) (value: v) : tree k v =
--     --     [(key,value,0,(-1,-1))]
    
--     -- -- simply the size of the tree, we can take the length of any of the representing arrays.
--     -- -- the parent array should be the most ''stable'' array (least likely to be changed),
--     -- -- so we'll make use of that.
--     -- def size 'k 'v (t: tree k v) : i64 =
--     --     length t

--     -- def many [n] 'k 'v (init: [n](k,v)) (lte: v -> v -> v) : tree k v =
--     --     map (\(x,y) -> (x,y,0,(-1,-1))) init

--     -- --def find 'k 'v (t: tree k v) (key: k) : v =

-- }
