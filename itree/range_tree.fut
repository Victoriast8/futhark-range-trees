import "../lib/github.com/diku-dk/sorts/merge_sort"
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

    val query [d] : box [d] -> tree [d] -> i64 -- argueably should return the actual hits
    val build [d] [n] : [n]point [d] -> tree [d]
}

module k_range_tree : range_tree = {
    type~ treeNodes [d] = []node [d]
    type~ treePoints [d]= []point [d]
    type~ tree [d]      = {tNodes : treeNodes [d], tCanonical : treePoints [d]}

    local def sort_by_key [n] 't 'k (key : t -> k) (dir : k -> k -> bool) (xs : [n]t) : [n]t =
        merge_sort_by_key key dir xs        

    def query [d] (bs : box [d]) (t : tree [d]) : i64 =
        ???

    -- The easiest, but asymptotically worst, implementation of range trees
    def build [d] [n] (ps : [n]point [d]) : tree [d] =
        let accChilds (i : i32) : i32 = if i <= 1 then 0 else 1
        let ps' = sort_by_key (\e -> e[0]) (f64.<=) ps

        let (_,_,_,res) =
            loop (wrk, wrk_shp, wrk_dim, acc) = 
                 (ps', [(i32.i64 n)], [1i32], ([],[]))
            while !(null wrk) do

            let begs  = scanExcl (+) 0 wrk_shp -- shp = [2,3,2] -> begs = [0,2,5], aka. start indexes

            -- a 'segmented sort,' if you will.
            -- we do a segmented sort by coordinate for subtrees
            -- this is the part where a bottom-up approach could do better
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
            let sort_by_coord = sort_by_key (\e -> e.0[e.2]) (f64.<=) new_sub_wrk
            let (sub_wrk,_,_) = 
                sort_by_key (\e -> e.1) (i32.<=) sort_by_coord
                |> unzip3
            
            let non = length acc.0
            let can_off = length acc.1
            let tnon = length wrk_shp

            -- next iteration
            let medians = map (\shp -> shp - (shp+1)/2) wrk_shp
            let (new_shp,new_dim) = zip3 wrk_shp medians wrk_dim
                |> filter (\(shp,_,_) -> shp > 1)
                |> map (\(s,i,d) -> [(i,d),(s-i,d)])
                |> flatten
                |> unzip
            
            let (sub_shp,tmp) = zip wrk_shp wrk_dim
                |> filter (\(_,dim) -> (i64.i32 dim) < d)
                |> unzip
            let sub_dim = map (+1) tmp

            let points = map2 (+) medians begs
            let islice = map2 (\b s -> (i64.i32 b + can_off, i64.i32 s)) begs wrk_shp

            let valid_chl= map accChilds wrk_shp
            let nons = scanExcl (+) 0 valid_chl
            -- foreach non-single element, predict children
            let children = map2 (\v i -> if v > 0
                                then 
                                    let i = i64.i32 i
                                    in (#some (2*i+non+tnon), #some (2*i+1+non+tnon))
                                else (#none,#none)
                           ) valid_chl nons

            let child_off = (last nons) * 2 + 2

            -- foreach valid subtree, predict its child
            let subs = map (\dim -> if (i64.i32 dim) < d then 1 else 0) wrk_dim
            let subs'= scanExcl (+) 0 subs
            let subtrees = map2 (\v i -> if v > 0
                                then #some (i+non+(i64.i32 child_off)+tnon)
                                else #none
                           ) subs subs'

            let nodes = map4 (\p i s (l,r) -> create_node wrk[p] i s l r) points islice subtrees children

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
                (acc.0 ++ nodes, acc.1 ++ wrk))
            
        in {tNodes = res.0, tCanonical = res.1}
}