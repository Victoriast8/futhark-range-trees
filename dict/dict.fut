-- module file for dicts
type opt 'v = #some v 
            | #none

module type dict = {
    type k
    type~ dict 'v

    val size 'v : dict v -> i64
    val d_map 'a 'b: (a -> b) -> dict a -> dict b
    val d_reduce 'a: (a -> a -> a) -> a -> dict a -> a
    val d_filter 'v : (k -> v -> bool) -> dict v -> dict v
    val many [n] 'v : [n]k -> [n]v -> dict v
    val single 'v : k -> v -> dict v
    val insert 'v : k -> v -> dict v -> dict v
    val union 'v : dict v -> dict v -> dict v
    val lookup 'v : k -> dict v -> opt v
    val delete 'v : dict v -> k -> dict v
}

-- module type tree_functions = {
--     type K
--     type V
--     type~ M = [](K, V)

--     -- Simple operations
--     val empty: M
--     val size: M -> i64
--     val insert: M -> K -> V -> (V -> V -> V) -> M
--     val delete: M -> K -> M
--     val find: M -> K -> V
--     val first: M -> V
--     val last: M -> V
--     -- val previous: -- ??
--     -- val next: -- ??
--     -- val rank: -- ??
--     -- val select: -- ??
--     val upTo: M -> K -> M
--     val downTo: M -> K -> M

--     -- Bulk operations
--     val join: M -> M -> M
--     val union: M -> M -> M
--     val intersect: M -> M -> M
--     val difference: M -> M -> M
--     val mapReduce: (K -> V -> B) -> (B -> B -> B) -> B -> M -> B
--     val filter: (K -> V -> bool) -> M -> M
--     val range: M -> K -> K -> M
--     -- val split: -- ??
--     -- val join2: -- ??
--     val build: [](K, V) -> (V -> V -> V) -> M
-- }

-- module type PAM = {
--     type K
--     type V
--     type A
--     type M
--     val <: K -> K -> bool
--     val g: K -> V -> A
--     val f: A -> A -> A
--     val I: A
--     -- val M: tree
--     -- Should the tree be part of the AugMap, or just input to its functions?
    
--     val augVal: M -> A
--     val augLeft: M -> K -> A
--     val augRange: M -> K -> K -> A
--     val augFilter: (A -> bool) -> M -> M
--     val augProject: (A -> B) -> (B -> B -> B) -> M -> K -> K -> B
-- }