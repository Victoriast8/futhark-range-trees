import "helper"

-- interval trees
-- the keyword 'Maybe' in Haskell
type opt 'v = #some v
            | #none

-- representing tree nodes with the following types
type point = f64
type child = opt i64
type interval = (point,point)
type node = {m: point, slice: (i64,i64), left: child, right: child}

def create_node (p : point) (start : i64, len : i64) (l : child) (r : child) : node =
    {m = p, slice = (start,len), left = l, right = r}

module type itree = {
    type~ treeIntervals
    type~ treeNodes
    type~ tree

    val count : point -> tree -> i64
    val many [n] : [n]interval -> tree
}

module itree1D : itree = {
    type~ treeIntervals = []interval
    type~ treeNodes     = []node
    type~ tree          = {tNodes: treeNodes, 
                           tStartSortedIntervals: treeIntervals,
                           tEndSortedIntervals: treeIntervals}

    def count (p : point) (t : tree) : i64 =
        let new_child_idx (n : child) : i64 =
            match n
            case #some idx -> idx
            case #none     -> -1
        -- loop traverses tree
        let (_,cnt) = loop (i,acc) = (0,0) while i >= 0 do
            let current = t.tNodes[i]
            let (istart, ilen) = (current.slice.0, current.slice.1)
            let dir = p <= current.m
            let (new_i, ivs, startidx, ldir) =
                if dir then (new_child_idx current.left,
                             t.tStartSortedIntervals, istart, 1)
                       else (new_child_idx current.right,
                             t.tEndSortedIntervals, istart+ilen-1, (-1))
            in if ilen <= 0 then (new_i,acc) else
                -- loop counts # of hit intervals
                let (_,sum) = loop (idx,iacc) = (startidx,0)
                    while ilen > iacc
                        && (if idx >= 0 && idx < istart+ilen then
                            p >= ivs[idx].0 && p <= ivs[idx].1
                                else false) do
                                    (idx+ldir,iacc+1)
                in (new_i,acc+sum)
        in cnt

    def many[n] (iv : [n]interval) : tree =
        let accChilds (i : i32) : i32 = if i <= 0 then 0 else 1
        -- parameters for loop:
        -- 1. work; dictates what data needs processing
        -- 2. shape of work; dictates the sizes of each subarray in 'work'
        -- 3. accumulator; accumulates the resulting nodes and sorted intervals
        -- 4. number of nodes; simply a constant. Could be replaced with 'length acc.0' where 'non' is used
        -- 5. initial offsets; keeps track of each subarray's offset from previous iterations
        let (_,_,res,_,_) = loop (wrk, wrk_shp,       acc,         non, iv_off)
                               = (iv,  [(i32.i64 n)], ([],[],[]),  0,   0     )
            while !(null wrk) do
            -- let begs  = scanExcl (+) 0 wrk_shp -- shp = [2,3,2] -> begs = [0,2,5], aka. start indexes
            -- let ends  = map2 (\b i -> b+i-1) begs wrk_shp -- marks the ends of each segement: ends = [1,4,6]
            let ends  = scan (+) 0 wrk_shp |> map (+(-1)) -- same ends computation, but independent from 'begs' calculation
            let [n] (flags: [n]i32) = 
                mkFlagArray wrk_shp 0i32 (map (+1) (map i32.i64 (indices wrk_shp)))
            let wrk = sized n wrk

            -- step 1. create mid values for partition
            let min = sgmScan (\x y -> if x < y then x else y) 
                              f64.highest flags (map (.0) wrk)
            let max = sgmScan (\x y -> if x > y then x else y) 
                              f64.lowest  flags (map (.1) wrk)
            let mid = map (\i -> min[i] + 0.5 * (max[i] - min[i])) ends -- the "x_centers" of each segment, so to speak

            -- step 2. do the partition2L
            let flg_scn = map (+(-1)) (sgmScan (+) 0 flags flags) -- in other words, the index of the segment
            let ((split1,split2),(_,pwrk)) = flat_res_partition2L
                    (\t -> t.1.1 >= mid[t.0] && mid[t.0] >= t.1.0)
                    (\t -> mid[t.0] > t.1.1)
                    (0,(0.0,0.0)) (wrk_shp,(zip flg_scn wrk))
            let (_,pwrk) = unzip pwrk

            -- step 3. create child predictions
            let left_length = split2
            let cent_length = split1
            let cent_offsets = map2 (+) left_length split1
            let right_length = map2 (-) wrk_shp cent_offsets
            let childL  = map accChilds split2
            let childR  = map accChilds right_length
            let nons    = scanExcl (+) 0 (map2 (+) childL childR)
            let new_non = (last childL) + (last childR) + (last nons) + non
            let children= map3 (\l r n -> 
                            let left = if l>0 then 
                                #some (i64.i32 (l + non + n)) else #none
                            let right = if r>0 then 
                                #some (i64.i32 (r + l + non + n)) else #none
                            in (left:child,right:child)
                          ) childL childR nons
            
            -- generate work and shape for next iteration
            let (ns,ms) = map3 (\l c r -> [(l,true),(c,false),(r,true)]) 
                        left_length cent_length right_length
                        |> flatten
                        |> unzip
            let wrk_bools = zip
                (pwrk :> [n]interval)
                ((flat_replicate_bools (map i64.i32 ns) ms) :> [n]bool)
            let (tmp1,tmp2) = partition (\(_,cond) -> cond) wrk_bools
            let (new_wrk,done) = ((unzip tmp1).0, (unzip tmp2).0)
            let new_shp = intertwine left_length right_length
                          |> filter (\i -> !(i == 0))

            -- accumulate results
            let sbs_acc = sort_by_key (.0) (f64.<=) done -- sorted by start
            let sbe_acc = sort_by_key (.1) (f64.<=) done -- sorted by end
            let islice = map2 (\off len ->
                                ((i64.i32 (off + iv_off)), i64.i32 len)
                              ) (scanExcl (+) 0 cent_length) cent_length
            let new_acc = 
                (concat acc.0 
                    (map3 (\p i (l,r) -> create_node p i l r) mid islice children),
                 concat acc.1 sbs_acc,
                 concat acc.2 sbe_acc)
            let new_off = iv_off + (reduce (+) 0 cent_length)

            -- TODO: remove new_non and new_off to avoid calculations
            in (new_wrk, new_shp, new_acc, new_non, new_off)
        in {tNodes = res.0,
            tStartSortedIntervals = res.1,
            tEndSortedIntervals = res.2}
}
