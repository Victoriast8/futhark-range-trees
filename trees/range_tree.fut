import "helper"

type opt 'v = #some v
            | #none
type child = opt i64
type point [d] = [d]f64
type box   [d] = (point [d],point [d])
type node  [d] = {m : point [d], slice : (i64,i64), subtree : child, left : child, right : child}

def create_node [d] (p : point [d]) (start : i64, len : i64) (sub : child) (l : child) (r : child) : node [d] =
    {m = p, slice = (start,len), subtree = sub, left = l, right = r}

module type range_tree = {
    type~ treeNodes [d]
    type~ treePoints [d]
    type~ tree [d]

    val count [d] : box [d] -> tree [d] -> i64 -- arguably should return the actual points
    val query [d] : box [d] -> tree [d] -> []point [d] -- now return the actual points!
    val build [d] [n] : [n]point [d] -> tree [d]
}

module k_range_tree : range_tree = {
    type~ treeNodes [d] = []node [d]
    type~ treePoints [d]= []point [d]
    type~ tree [d]      = {tNodes : treeNodes [d], tCanonical : treePoints [d]}

    -- | A fully sequential, queue-based query.
    -- A data-parallel query may be constructed, if time permits
    def query [d] (bs : box [d]) (t : tree [d]) : []point [d] =
        let in_box (b : box [d]) (p : point [d]) : bool =
            map3 (\lo hi p' -> lo <= p' && p' <= hi) b.0 b.1 p
                |> all (\t -> t)
        let node_idx (n : child) : i64 =
            match n
            case #some idx -> idx
            case #none     -> -1
        let (_,_,_,res) =
            loop (queue,typ,dim,acc) =
                ([t.tNodes[0]],[0],[1],[])
            while !(null queue) do

            let (nd, tp, pd) = (head queue, head typ, head dim)
            let ipd = pd - 1
            let (nd' : treeNodes [d],tp',pd',acc') =

                -- find v_split
                if tp == 0 then
                    let split = loop node = nd
                        while !(node.m[ipd] >= bs.0[ipd]
                             && node.m[ipd] <= bs.1[ipd])
                             && node.left != #none do
                            if node.m[ipd] > bs.0[ipd] then
                                 t.tNodes[(node_idx node.left)]
                            else t.tNodes[(node_idx node.right)]
                    let is_leaf = split.left == #none
                    let nn = if is_leaf then [] else
                             [t.tNodes[(node_idx split.left)],
                              t.tNodes[(node_idx split.right)]]
                    let (tp',pd') = if is_leaf then
                                        ([],[])
                                    else
                                        ([1,2],[pd,pd])
                    let acc' = if is_leaf && (in_box bs (split.m :> point [d]))
                               then [split.m] else []
                    in (nn :> treeNodes [d],tp',pd',acc')

                -- do search and accumulation on left tree
                else if tp == 1 then
                    let (acc'',leaf,nodes) = loop (acc''',node,nn) = ([],nd,[])
                        while node.left != #none do
                            let dir = node.m[ipd] > bs.0[ipd]
                            let rc = t.tNodes[(node_idx node.right)]
                            let nacc = if pd == d && dir
                                then t.tCanonical[rc.slice.0:rc.slice.0+rc.slice.1]
                                else []
                            let nn' = if pd < d && dir
                                then [t.tNodes[(node_idx rc.subtree)]]
                                else []
                            let nd' = if dir then
                                 t.tNodes[(node_idx node.left)]
                            else rc
                            in (acc'''++ nacc,nd',nn++nn')
                    let acc'' = if (in_box bs (leaf.m :> point [d]))
                                then acc''++([leaf.m] :> [1]point [d]) else acc''
                    let (tp',pd') = map (\_ -> (0,pd+1)) nodes |> unzip
                    in (nodes :> treeNodes [d],tp',pd',acc'')

                -- do search and accumulation on right tree
                else if tp == 2 then
                    let (acc'',leaf,nodes) = loop (acc''',node,nn) = ([],nd,[])
                        while node.left != #none do
                            let dir = node.m[ipd] < bs.1[ipd]
                            let lc = t.tNodes[(node_idx node.left)]
                            let nacc = if pd == d && dir
                                then t.tCanonical[lc.slice.0:lc.slice.0+lc.slice.1]
                                else []
                            let nn' = if pd < d && dir
                                then [t.tNodes[(node_idx lc.subtree)]]
                                else []
                            let nd' = if dir then
                                 t.tNodes[(node_idx node.right)]
                            else lc
                            in (acc'''++ nacc,nd',nn++nn')
                    let acc'' = if (in_box bs (leaf.m :> point [d]))
                                then acc''++([leaf.m] :> [1]point [d]) else acc''
                    let (tp',pd') = map (\_ -> (0,pd+1)) nodes |> unzip
                    in (nodes :> treeNodes [d],tp',pd',acc'')
                else ([],[],[],[])
            in ((drop 1 queue) ++ nd',
                (drop 1 typ) ++ tp',
                (drop 1 dim) ++ pd',
                acc ++ (acc' :> []point [d]))
        in res

    -- | Same as 'query' but return number of hits instead of actual points
    def count [d] (bs : box [d]) (t : tree [d]) : i64 =
        let in_box (b : box [d]) (p : point [d]) : bool =
            map3 (\lo hi p' -> lo <= p' && p' <= hi) b.0 b.1 p
                |> all (\t -> t)
        let node_idx (n : child) : i64 =
            match n
            case #some idx -> idx
            case #none     -> -1
        let (_,_,_,res) =
            loop (queue,typ,dim,acc) =
                ([t.tNodes[0]],[0],[1],0)
            while !(null queue) do

            let (nd, tp, pd) = (head queue, head typ, head dim)
            let ipd = pd - 1
            let (nd' : treeNodes [d],tp',pd',acc') =

                -- find v_split
                if tp == 0 then
                    let split = loop node = nd
                        while !(node.m[ipd] >= bs.0[ipd]
                             && node.m[ipd] <= bs.1[ipd])
                             && node.left != #none do

                            if node.m[ipd] > bs.0[ipd] then
                                 t.tNodes[(node_idx node.left)]
                            else t.tNodes[(node_idx node.right)]

                    let is_leaf = split.left == #none
                    let nn = if is_leaf then [] else
                             [t.tNodes[(node_idx split.left)],
                              t.tNodes[(node_idx split.right)]]
                    let (tp',pd') = if is_leaf then
                                        ([],[])
                                    else
                                        ([1,2],[pd,pd])
                    let acc' = if is_leaf && (in_box bs (split.m :> point [d]))
                               then 1 else 0
                    in (nn :> treeNodes [d],tp',pd',acc')

                -- do search and accumulation on left tree
                else if tp == 1 then
                    let (acc'',leaf,nodes) = loop (acc''',node,nn) = (0,nd,[])
                        while node.left != #none do

                            let dir = node.m[ipd] > bs.0[ipd]
                            let rc = t.tNodes[(node_idx node.right)]
                            let nacc = if pd == d && dir 
                                then rc.slice.1
                                else 0
                            let nn' = if pd < d && dir
                                then [t.tNodes[(node_idx rc.subtree)]]
                                else []
                            let nd' = if dir then
                                 t.tNodes[(node_idx node.left)]
                            else rc
                            in (acc'''+ nacc,nd',nn++nn')

                    let acc'' = if (in_box bs (leaf.m :> point [d]))
                                then acc''+1 else acc''
                    let (tp',pd') = map (\_ -> (0,pd+1)) nodes |> unzip
                    in (nodes :> treeNodes [d],tp',pd',acc'')

                -- do search and accumulation on right tree
                else if tp == 2 then
                    let (acc'',leaf,nodes) = loop (acc''',node,nn) = (0,nd,[])
                        while node.left != #none do

                            let dir = node.m[ipd] < bs.1[ipd]
                            let lc = t.tNodes[(node_idx node.left)]
                            let nacc = if pd == d && dir
                                then lc.slice.1
                                else 0
                            let nn' = if pd < d && dir
                                then [t.tNodes[(node_idx lc.subtree)]]
                                else []
                            let nd' = if dir then
                                 t.tNodes[(node_idx node.right)]
                            else lc
                            in (acc'''+ nacc,nd',nn++nn')

                    let acc'' = if (in_box bs (leaf.m :> point [d]))
                                then acc''+1 else acc''
                    let (tp',pd') = map (\_ -> (0,pd+1)) nodes |> unzip
                    in (nodes :> treeNodes [d],tp',pd',acc'')
                else ([],[],[],0)
            in ((drop 1 queue) ++ nd',
                (drop 1 typ) ++ tp',
                (drop 1 dim) ++ pd',
                acc + acc')
        in res

    -- The easiest, but asymptotically worst, implementation of kd range trees
    def build [d] [n] (ps : [n]point [d]) : tree [d] =
        let accChilds (i : i32) : i32 = if i <= 1 then 0 else 1
        let ps' = sort_by_key (\e -> e[0]) (f64.<=) ps

        let (_,_,_,res) =
            loop (wrk, wrk_shp, wrk_dim, acc) = 
                 (ps', [(i32.i64 n)], [1i32], ([],[]))
            while !(null wrk) do

            let begs  = scanExcl (+) 0 wrk_shp -- shp = [2,3,2] -> begs = [0,2,5], aka. start indexes

            let [n] (seg: [n]i32) =
                flat_replicate_i32
                    (map i64.i32 wrk_shp)
                    (map i32.i64 (indices wrk_shp))
            let wrk = sized n wrk
            let seg_dim =
                flat_replicate_i32
                    (map i64.i32 wrk_shp)
                    wrk_dim :> [n]i32
            let new_sub_wrk =
                zip3 wrk seg seg_dim
                |> filter (\(_,_,dim) -> (i64.i32 dim) < d)
            let (sub_wrk,_,_) = sort_by_key (\e -> e)
                                (\x y -> if x.1 == y.1 then x.0[x.2] <= y.0[y.2]
                                         else x.1 < y.1
                                ) new_sub_wrk |> unzip3
            
            let (sub_shp,tmp) = zip wrk_shp wrk_dim
                |> filter (\(_,dim) -> (i64.i32 dim) < d)
                |> unzip
            let sub_dim = map (+1) tmp
                        
            let non = length acc.0 + length wrk_shp
            let can_off = length acc.1

            -- next iteration
            let medians = map (\shp ->
                    if shp < 2 then 1 else shp - (shp)/2
                ) wrk_shp
            let (new_shp,new_dim) = zip3 wrk_shp medians wrk_dim
                |> filter (\(shp,_,_) -> shp > 1)
                |> map (\(s,i,d) -> [(i,d),(s-i,d)])
                |> flatten
                |> unzip

            let points = map2 (\b m -> b+m-1) begs medians
            -- NOTE: These offsets may cause a problem. Keep an eye out here if validation fails
            let ioffs = zip wrk_shp wrk_dim
                        |> scan (\(s1,_)(s2,d2) ->
                                    if (i64.i32 d2) == d
                                    then (s1 + s2,d2)
                                    else (0,0)
                                ) (0,0)
                        |> unzip |> (.0)
            let ioffs' = map (\i -> if i > 0 then ioffs[i-1] else 0) (indices ioffs)
            let islice = map3 (\b s dim -> 
                    if (i64.i32 dim) < d then (0,0)
                    else (i64.i32 b + can_off, i64.i32 s)
                ) ioffs' wrk_shp wrk_dim

            let valid_chl= map accChilds wrk_shp
            let nons = scanExcl (+) 0 valid_chl
            -- foreach non-single element, predict children
            let children =
                map2 (\v i -> if v > 0
                    then let i = i64.i32 i
                         in (#some (2*i+non), #some (2*i+1+non))
                    else (#none,#none)
                ) valid_chl nons

            let child_off = (last nons) * 2 + 2

            -- foreach valid subtree, predict its child
            let subs = 
                map (\dim -> 
                    if (i64.i32 dim) < d
                    then 1 else 0
                ) wrk_dim
            let subs'= scanExcl (+) 0 subs
            let subtrees = 
                map2 (\v i -> if v > 0
                    then #some (i+non+(i64.i32 child_off))
                    else #none
                ) subs subs'

            let nodes = map4 (\p i s (l,r) -> 
                    create_node wrk[p] i s l r
                ) points islice subtrees children

            -- by allowing leafs to have a canonical element, we effectively double the size of canonicals.
            -- this does however simplify and speed up both construction and query slightly.
            let canonicals = zip wrk seg_dim
                |> filter (\(_,dim) -> (i64.i32 dim) == d)
                |> unzip |> (.0)

            let new_wrk =
                flat_replicate_i32
                    (map i64.i32 wrk_shp)
                    wrk_shp
                |> sized n
                |> zip wrk
                |> filter (\(_,size) -> size > 1)
                |> unzip |> (.0)
            
            in (new_wrk ++ sub_wrk, 
                new_shp ++ sub_shp,
                new_dim ++ sub_dim,
                (acc.0 ++ nodes, acc.1 ++ canonicals))
        in {tNodes = res.0, tCanonical = res.1}
}