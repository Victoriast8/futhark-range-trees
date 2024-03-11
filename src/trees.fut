

module type tree = {
    type K
    type V
    type~ M = [](K, V)

    -- Simple operations
    val empty: M
    val size: M -> i64
    val insert: M -> K -> V -> (V -> V -> V) -> M
    val delete: M -> K -> M
    val find: M -> K -> V
    val first: M -> V
    val last: M -> V
    -- val previous: -- ??
    -- val next: -- ??
    -- val rank: -- ??
    -- val select: -- ??
    val upTo: M -> K -> M
    val downTo: M -> K -> M

    -- Bulk operations
    val join: M -> M -> M
    val union: M -> M -> M
    val intersect: M -> M -> M
    val difference: M -> M -> M
    val mapReduce: (K -> V -> B) -> (B -> B -> B) -> B -> M -> B
    val filter: (K -> V -> bool) -> M -> M
    val range: M -> K -> K -> M
    -- val split: -- ??
    -- val join2: -- ??
    val build: [](K, V) -> (V -> V -> V) -> M
}


module interval_tree : tree = {
    type K = i64
    type V = i64
    type~ M = [](K, V)

    def empty : M = [](K, V)

    def size (t: M) : i64 = length M -- Perhaps do not count "dead" nodes?
                                     -- ... Do not count leaves ...

    def insert (t: M) (k: K) (v: V) (f: V -> V -> V) : M =
        if M == empty 
            then [(k, v)]
            else let (L, k', v', R) = t 

    def delete

    def join (t1: M) (t2: M) : M =

}