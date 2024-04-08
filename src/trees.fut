--import "mk_dict"
--import "sorteddict"

module type tree = {
    type A
    type~ tree 'k 'v

    val single 'k 'v : k -> v -> tree k v
    val empty : tree u8 u8
    val size 'k 'v : tree k v -> i64
    --val find 'k 'v : tree k v -> k -> v
    --val first 'k 'v : tree k v -> v
}

module BST = {
    -- For now, an augmented value will simply be an integer.
    -- Parameterize this.
    type A = i64

    -- Tree type. A tuple consisting of:
    -- 1. A dictionary / array-like structure key-value pair,
    -- 2. A parent reference,
    -- 3. A depth (maybe not necessary, perhaps omittet later...)
    type~ tree 'k 'v = ([](k,v), []u32, []u32)

    def single 'k 'v (key: k) (value: v) : tree k v =
        ([(key,value)],[0],[0])
    
    -- represents an empty tree. To avoid type ambiguities, small dummy types are passed to the tree constructor.
    def empty : tree u8 u8 =
        ([],[],[])
    
    -- simply the size of the tree, we can take the length of any of the representing arrays.
    -- the parent array should be the most ''stable'' array (least likely to be changed),
    -- so we'll make use of that.
    def size 'k 'v (t: tree k v) : i64 =
        length t.1

    
}

-------------
-- Something like the below may be possible, using dicts of dicts to keep track of parents and depth.
-- It is a funny idea, however (probably) not very useful, as a 'treedict.dict' would probably create double nested arrays.
-- type~ tree 'k 'v = treedict.dict (treedict.dict k v) (treedict.dict u32 u32)

-- A better idea may be to just simply keep track of two dicts - one for the actual key-value pair, and one for the parents/depth:
-- type~ tree 'k 'v = ((treedict.dict k v), (treedict.dict u32 u32))
-- And run every operation twice - this may be too costly, but will make it so that handling parents and depth is trivial.
-------------

-- module i_tree = {
--     type A = i64
-- }


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
