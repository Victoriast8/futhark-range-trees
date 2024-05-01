import "../lib/github.com/diku-dk/sorts/merge_sort"
import "helper"

-- interval trees
-- predefining some types. Makes the code more readable for onlookers
type opt 'v = #some v
            | #none
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

    -- count må gerne være sekventiel - vi kan kalde den med maps: parallelismen ligger i many
    val count : point -> tree -> i64 -- skal tælle hvor mange intervaller indeholder et punkt
    val many [n] : [n]interval -> tree
    -- ...
}
-- start med at skrive en rekursiv many
-- derefter lav en futhark lad som om det er et rekusivt sprog
-- lav den rekursive om til en lifted/flad version
-- læs op intervaltræer igen

module itree1D : itree = {
    type~ treeIntervals = []interval
    type~ treeNodes     = []node
    type~ tree          = {tNodes: treeNodes, 
                           tStartSortedIntervals: treeIntervals,
                           tEndSortedIntervals: treeIntervals}

    local def sort_by_key [n] 't 'k (key : t -> k) (dir : k -> k -> bool) (xs : [n]t) : [n]t =
        merge_sort_by_key key dir xs

    def count (p : point) (t : tree) : i64 =
        let new_child_idx (n : child) : i64 =
            match n
            case #some idx -> idx
            case #none     -> -1
        let (_,cnt) = loop (i,acc) = (0,0) while i >= 0 do
            let current = t.tNodes[i]
            let (istart, ilen) = (current.slice.0, current.slice.1)
            in if !(p == current.m) then
                let dir = p < current.m
                let (new_i, ivs, start, ldir) = 
                    if dir then (new_child_idx current.left, t.tStartSortedIntervals, istart, 1)
                           else (new_child_idx current.right, t.tEndSortedIntervals, istart+ilen-1, (-1))
                let (_,sum) = loop (e,iacc) = (ivs[start],0)
                    while ilen > iacc && (if dir then p >= e.0 else p <= e.1) do
                        let upd_i = iacc+1
                        in (ivs[start+(upd_i*ldir)],upd_i)
                in (new_i,(acc + sum))
            else
                (-1, acc + ilen)
        in cnt

    def many [n] (iv : [n]interval) : tree =
        let sortedByStart = sort_by_key (.0) (f64.<=) iv
        let sortedByEnd   = sort_by_key (.1) (f64.<=) iv
        let accChilds (i : i32) : i32 = if i <= 0 then 0 else 1
        -- parameters for loop:
        -- 1. work; dictates what data needs processing
        -- 2. shape of work; dictates the sizes of each subarray in 'work'
        -- 3. accumulator; accumulates the resulting nodes
        -- 4. number of nodes; simply a constant. Could be replaced with 'length acc' where 'non' is used
        -- 5. initial offsets; keeps track of each subarray's offset from previous iterations
        let (_,_,res,_,_) = loop (wrk, wrk_shp,       acc, non, init_offs)
                               = (iv,  [(i32.i64 n)], [],  0,   [0]      )
            while !(null wrk) do
            --let begs  = scanExcl (+) 0 wrk_shp -- shp = [2,3,2] -> begs = [0,2,5], aka. start indexes
            --let ends  = map2 (\b i -> b+i-1) begs wrk_shp -- marks the ends of each segement: ends = [1,4,6]
            let ends  = scan (+) 0 wrk_shp |> map (+(-1))
            let flags = mkFlagArray wrk_shp 0i32 (map (+1) (map i32.i64 (indices wrk_shp)))

            let min = sgmScan (\x y -> if x < y then x else y) f64.highest flags (map (.0) wrk)
            let max = sgmScan (\x y -> if x > y then x else y) f64.lowest  flags (map (.1) wrk)
            let mid = map (\i -> min[i] + 0.5 * (max[i] - min[i])) ends -- the "x_centers" of each segment, so to speak
            
            let flg_scn = sgmScan (+) 0 flags flags |> map (+(-1)) -- in other words, the index of the segment
            let ((split1,split2),(_,pwrk)) = flat_res_partition2L
                                                (\t -> t.1.1 >= mid[t.0] && mid[t.0] >= t.1.0)
                                                (\t -> mid[t.0] > t.1.1)
                                                (0,(0.0,0.0)) (wrk_shp,(zip flg_scn wrk))
            let (_,pwrk) = unzip pwrk

            -- create child predictions and calculate intervals
            let left_offsets = split2
            let cent_offsets = map2 (+) left_offsets split1
            let right_length = map2 (-) wrk_shp cent_offsets
            -- around here, could do a check if any work is left next iteration
            let childL  = map accChilds split2
            let childR  = map accChilds right_length
            let nons    = scanExcl (+) 0 (map2 (+) childL childR)
            let new_non = (last childL) + (last childR) + (last nons) + non
            let children= map3 (\l r n -> let left = if l>0 then #some (i64.i32 (l + non + n)) else #none
                                          let right = if r>0 then #some (i64.i32 (r + l + non + n)) else #none
                                          in (left,right)
                               ) childL childR nons

            let islice = map3 (\left len init -> ((i64.i32 (left + init)), i64.i32 len)
                              ) left_offsets split1 init_offs
            let new_acc = acc ++ map3 (\p i (l,r) -> create_node p i l r) mid islice children
            
            -- generate work and shape for next iteration
            let (ns,ms) = map3 (\l current r -> [(l,true),(current,false),(r,true)]) left_offsets split1 right_length
                            |> flatten
                            |> unzip
            let wrk_bools = zip (pwrk :> [length wrk]interval) ((flat_replicate_bools (map i64.i32 ns) ms) :> [length wrk]bool)
            let (new_wrk,_) = unzip (filter (\(_,cond) -> cond) wrk_bools)
            let (new_shp, new_offs) = zip (intertwine left_offsets right_length)
                                          (intertwine init_offs    cent_offsets)
                                      |> filter (\i -> !(i.0 == 0)) |> unzip
            in (new_wrk, new_shp, new_acc, new_non, new_offs)
        in {tNodes = res,
            tStartSortedIntervals = sortedByStart,
            tEndSortedIntervals = sortedByEnd}
}

