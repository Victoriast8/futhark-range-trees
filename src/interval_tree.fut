import "dict"
import "../lib/github.com/diku-dk/sorts/radix_sort"

module type itree = {
    type interval
    type point
    type tree

    -- count må gerne være sekventiel - vi kan kalde den med maps: parallelismen ligger i many
    val count : point -> tree -> i64 -- skal tælle hvor mange intervaller indeholder et punkt
    val many : []interval -> tree
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

    -- assumes 't' contains at least 1 element
    def count : (p : point) (t : tree) : i64 =
        if length t == 0 then 0 else
            -- check p against point (first element)
            -- then compare against sorted array in appropriate direction
            -- then run on child in same direction
    
    def many [n] (iv : [n]interval) : tree =
        -- compute x_center
        let fsize = f64.i64 n
        let avr_intvls (x,y) =
            (x/(2.0*fsize)) + (y/(2.0*fsize))
        let x_cent = reduce (+) 0.0 (map avr_intvls iv)
        -- partition intervals
        let (p1,p2,p3) = partition2 (\(x,_) -> x > x_cent) (\(_,y) -> x_cent > y) iv
        -- run recursively on side intervals
        let (s1,s2) = (sort_by_key (.0) p2, sort_by_key (.1) p2)
        in (x_cent,(many p1, many p3),s1,s2)
}

module itree2D : itree with interval = (f64, f64)
                        with point = f64
                        = {
    type interval = (f64, f64)
    type point = (f64)
    type~ tree = [](point,(i32,i32),interval,interval)

    def count : (p : point) (t : tree) : i64 =
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
