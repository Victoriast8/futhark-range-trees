import "dict"

module interval_tree : dict = {
    type k = i32
    -- Let's start with something simple:
    -- k & v: key-value pair
    -- in order: L(eft), R(ight), P(arent). Pointers to children/parent of the given key-value pair.
    type dict 'v = [](k,v,i32,i32,i32)

    


    -- -- Tree type. An array of tuples of:
    -- -- 1+2. A dictionary / array-like structure key-value pair,
    -- -- 3. A parent reference,
    -- -- 4. A depth (maybe not necessary, perhaps omittet later...)
    -- type~ tree 'k 'v = [](k,v,u32,(i32,i32))

    -- -- represents an empty tree. To avoid type ambiguities, small dummy types are passed to the tree constructor.
    -- def empty : tree u8 u8 =
    --     []

    -- def single 'k 'v (key: k) (value: v) : tree k v =
    --     [(key,value,0,(-1,-1))]
    
    -- -- simply the size of the tree, we can take the length of any of the representing arrays.
    -- -- the parent array should be the most ''stable'' array (least likely to be changed),
    -- -- so we'll make use of that.
    -- def size 'k 'v (t: tree k v) : i64 =
    --     length t

    -- def many [n] 'k 'v (init: [n](k,v)) (lte: v -> v -> v) : tree k v =
    --     map (\(x,y) -> (x,y,0,(-1,-1))) init

    -- --def find 'k 'v (t: tree k v) (key: k) : v =

}
