import "../lib/github.com/diku-dk/sorts/merge_sort"
import "helper"

-- interval trees
-- predefining some types. Makes the code more readable for onlookers
type opt 'v = #some v
            | #none
type point = f64
type interval = (f64,f64)
type child = opt i64
type left = child
type right = child
type node = (point,(i64,i64),left,right)

def create_node (p : point) (start : i64, len : i64) (l : left) (r : right) : node = (p,(start,len),l,r)

module type itree = {
    type~ treeIntervals
    type~ treeNodes
    type~ tree

    -- count må gerne være sekventiel - vi kan kalde den med maps: parallelismen ligger i many
    val count : point -> tree -> i64 -- skal tælle hvor mange intervaller indeholder et punkt
    val many [n] : [n]interval -> tree
    val query : tree -> interval -> i64
    -- ...
}
-- start med at skrive en rekursiv many
-- derefter lav en futhark lad som om det er et rekusivt sprog
-- lav den rekursive om til en lifted/flad version
-- læs op intervaltræer igen

module itree1D : itree = {
    type~ treeIntervals = []interval
    type~ treeNodes     = []node
    type~ tree          = opt (treeNodes,treeIntervals,treeIntervals)

    local def sort_by_key [n] 't 'k (key : t -> k) (dir : k -> k -> bool) (xs : [n]t) : [n]t =
        merge_sort_by_key key dir xs

    def count (p : point) (t : tree) : i64 =
        let new_child_idx (n : child) : i64 =
            match n
            case #some idx -> idx
            case #none     -> -1
        in match t
           case #none -> 0
           case #some t ->
                let (_,cnt) = loop (i,acc) = (0,0) while i >= 0 do
                    let c = t.0[i]
                    let (istart, ilen) = (c.1.0, c.1.1)
                    in if !(p == c.0) then
                        let dir = p < c.0
                        let (new_i, ivs, start, ldir) = 
                            if dir then (new_child_idx c.2,t.1,istart,1)
                                   else (new_child_idx c.3,t.2,istart+ilen-1,(-1))
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
            let (ns,ms) = map3 (\l c r -> [(l,true),(c,false),(r,true)]) left_offsets split1 right_length
                            |> flatten
                            |> unzip
            let wrk_bools = zip (pwrk :> [length wrk]interval) ((flat_replicate_bools (map i64.i32 ns) ms) :> [length wrk]bool)
            let (new_wrk,_) = unzip (filter (\(_,cond) -> cond) wrk_bools)
            let (new_shp, new_offs) = zip (intertwine left_offsets right_length) 
                                          (intertwine init_offs    cent_offsets)
                                      |> filter (\i -> !(i.0 == 0)) |> unzip
            in (new_wrk, new_shp, new_acc, new_non, new_offs)
        in #some (res,sortedByStart,sortedByEnd)
    
    def query (iv : interval) (t : tree) : i64 =
        ???
}