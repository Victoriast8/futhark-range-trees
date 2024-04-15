import "dict"

module interval_tree : dict = {
    type k = i32
    -- Let's start with something simple:
    -- k & v: key-value pair
    -- in order: L(eft), R(ight), P(arent). Pointers to children/parent of the given key-value pair.
    type dict 'v = [](k,v,i32,i32,i32)

    local def ilog2 (x: i64) = 63 - i64.i32 (i64.clz x)

    def size 'v (d : dict v) : i64 =
        length d

    -- Perhaps a depth function? udregning af depth i hvert element, current max depth eller begge?

    -- Overvej om f også skal påvirker key'en
    def d_map 'a 'b (f: a -> b) (d : dict a) : dict b =
        -- Siden dette er et BST, er dette ikke nok - ellers skal der restrictions på d_map funktionen.
        -- Ellers skal træet checkes for reordering, hvilket bliver dyrt
        map (\(k,v,l,r,p) -> (k,(f v),l,r,p)) d

    -- Overvej om f også skal påvirker key'en. I PAM artiklen tager mapReduce funktionen f key-value parret som input
    def d_reduce 'a (f : a -> a -> a) (ne : a) (d : dict a) : a =
        (unzip d).1 |> reduce f ne
    
    def d_filter 'v (f: k -> v -> bool) (d : dict v) : dict v =
        filter (\(x,y,_,_,_) -> f x y) d

    -- TODO
    def many [n] 'v (keys : [n]k) (values : [n]v) : dict v =
        ???

    def single 'v (key : k) (value : v) : dict v =
        [(key, value,-1,-1,0)]

    -- TODO
    def insert 'v (key : k) (value : v) (d : dict v) : dict v =
        ???
    
    -- TODO
    def union 'v (d : dict v) (d' : dict v) : dict v =
        ???

    -- TODO
    def lookup 'v (n : k) (d : dict v) : opt v =
        ???

    -- TODO
    def delete 'v (d : dict v) (key : k) : dict v =
        ???


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
