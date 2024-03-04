
-- abstractions can come later
-- module type node = {
--     type K
-- }


module type tree = {
    type K
    type V
    type~ M = [](K, V)

    -- Simple operations
    val empty: M
    val size: M -> i64
    val insert: M -> K -> V -> M
    val delete: M -> K -> M
    val find: M -> K -> 
    val first:
    val last:
    val previous:
    val next:
    val rank:
    val select: 
    val upTo:
    val downTo:

    -- Bulk operations
    val join: M -> M -> M
    val union: M -> M -> M
    val intersect: M -> M -> M
    val difference: M -> M -> M
    val mapReduce: (K -> B) 
    val filter:
    val range:
    val split:
    val join2: -- ??
    val build:
}


-- module interval_tree : tree = {

-- }