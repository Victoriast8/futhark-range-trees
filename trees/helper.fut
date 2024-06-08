import "../lib/github.com/diku-dk/sorts/merge_sort"
-- helper functions

-- intertwines two arrays - as = [1,2,3], bs = [4,5,6]; intertwine as bs = [1,4,2,5,3,6]
def intertwine [n] 't (as : [n]t) (bs : [n]t) : [n*2]t =
    map2 (\x y -> [x,y]) as bs |> flatten

def sort_by_key [n] 't 'k (key : t -> k) (dir : k -> k -> bool) (xs : [n]t) : [n]t =
    merge_sort_by_key key dir xs

-- Typical exclusive scan
def scanExcl [n] 't (op : t -> t -> t) (ne: t) (arr : [n]t) : [n]t =
    scan op ne (map (\i -> if i > 0 then arr[i-1] else ne) (iota n))

-- Typical (inclusive) segmented scan
def sgmScan [n] 't (op : t -> t -> t) 
                   (ne : t) (flg : [n]i32) (arr : [n]t)
                   : [n]t =
    let flgs_vals = 
        scan (\(f1,x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 `op` x2)
        ) (0,ne) (zip flg arr)
    let (_, vals) = unzip flgs_vals
    in vals

-- Segmented (inclusive) scan on type i32 with operator '+'
def sgmSumInt [n] (flg : [n]i32) (arr : [n]i32) : [n]i32 =
    let flgs_vals = 
        scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2))
            (0,0) (zip flg arr)
    let (_, vals) = unzip flgs_vals
    in vals

-- Makes a flag array: Given a shape and values to insert as the flag-value.
def mkFlagArray 't [m] 
            (aoa_shp: [m]i32) (zero: t)       --aoa_shp=[0,3,1,0,4,2,0]
            (aoa_val: [m]t  ) : []t =         --aoa_val=[1,1,1,1,1,1,1]
    let shp_rot = map (\i->if i==0 then 0       --shp_rot=[0,0,3,1,0,4,2]
                            else aoa_shp[i-1]
                      ) (map i32.i64 (iota m))
    let shp_scn = scan (+) 0 shp_rot            --shp_scn=[0,0,3,4,4,8,10]
    let aoa_len = shp_scn[m-1]+aoa_shp[m-1]     --aoa_len= 10
    let shp_ind = map2 (\shp ind ->             --shp_ind= 
                            if shp==0 then -1      --  [-1,0,3,-1,4,8,-1]
                            else ind               --scatter
                       ) aoa_shp shp_scn        --   [0,0,0,0,0,0,0,0,0,0]
    in scatter(replicate (i64.i32 aoa_len) zero)--   [-1,0,3,-1,4,8,-1]
            (map i64.i32 shp_ind) aoa_val     --   [1,1,1,1,1,1,1]
    -- result: [1,0,0,1,1,0,0,0,1,0]

-- flat_scatter; deprecated.
-- def flat_scatter 't [m] [n] [g]
--                     (xs: [m]t) (is: [n]i32) (as: [n]t) 
--                     (xs_shp: [g]i32) (is_shp: [g]i32) 
--                     : [m]t =
--     let shpscan_i = scanExcl (+) 0 is_shp
--     let x_incl = scan (+) 0 xs_shp
--     let x_excl = scanExcl (+) 0 xs_shp
--     let sc1 = scatter 
--         (replicate n 0)
--         (map i64.i32 shpscan_i)
--         x_incl
--     let upper = sgmSumInt sc1 sc1
--     let sc2 = scatter 
--         (replicate n 0)
--         (map i64.i32 shpscan_i)
--         x_excl
--     let base = sgmSumInt sc2 sc2
--     let upd_i = map3 (\i b u -> 
--             let ind = i+b
--             in if ind >= u || i < 0 then -1 else ind
--         ) is base upper
--         |> map i64.i32
--     in scatter (copy xs) upd_i as

-- Lifted partition
def partitionL 't [n] [m]
                  (condsL: [n]bool) (dummy: t)
                  (shp: [m]i32, arr: [n]t) :
                  ([m]i32, ([m]i32, [n]t)) =
    let begs   = scan (+) 0 shp
    let flags  = mkFlagArray shp 0i32 (map (+1) (map i32.i64 (iota m)))
    let outinds= map (\f -> if f==0 then 0 else f-1) flags |> sgmSumInt flags :> [n]i32

    let tflgs = map (\ c -> if c then 1 else 0) condsL
    let fflgs = map (\ b -> 1 - b) tflgs

    let indsT = sgmSumInt (flags :> [n]i32) tflgs
    let tmp   = sgmSumInt (flags :> [n]i32) fflgs
    let lst   = map2 (\b s -> if s > 0 then indsT[b-1] else -1i32) begs shp
    let indsF = map2 (\i v -> lst[i]+v) outinds tmp

    let inds  =  map4(\c indT indF sgmind->
                       let offs = if sgmind > 0 then (i64.i32 begs[sgmind-1]) else 0i64
                       in if c then offs + (i64.i32 indT) - 1i64
                               else offs + (i64.i32 indF) - 1i64
                     ) condsL indsT indsF outinds

    let fltarr = scatter (replicate n dummy) inds arr

    in  (lst, (shp,fltarr))

-- Lifted partition2. There are some shortcomings, namely the repeated partition on an aldready partitioned segment
def flat_res_partition2L 't [n] [m]
                   (p1 : t -> bool) (p2 : t -> bool)
                   (dummy : t) (shp : [m]i32, arr : [n]t)
                   : (([m]i32,[m]i32), ([m]i32, [n]t)) =
    let (split1, (_,ps))  = partitionL (map p1 arr) dummy (shp, arr)
    let (split2, (_,ps')) = partitionL (map p2 ps)  dummy (shp, ps)
    in ((split1,split2),(shp,ps'))

-- Flat/segmented replicate of bools. Never use this function, unless 'ns' reduces to >=1i64:
-- ns - is a shape array. Convention: foreach segment: replicate ns[i] ms[i]
def flat_replicate_bools [n] (ns : [n]i64) (ms : [n]bool) : []bool =
    let scn = scanExcl (+) 0 ns
    let inds = map2 (\n i -> if n>0 then i else -1) ns scn
    let size = (last scn) + (last ns) -- DPP slides use '(last inds)'. This is wrong. Consider the last element of 'ns' is 0.
    let vals = scatter (replicate size false) inds ms
    let flgs = scatter (replicate size 0) inds ns
    in sgmScan (||) false (map i32.i64 flgs) vals

-- Flat/segmented replicate for i32
def flat_replicate_i32 [n] (ns : [n]i64) (ms : [n]i32) : []i32 =
    let scn = scanExcl (+) 0 ns
    let inds = map2 (\n i -> if n>0 then i else -1) ns scn
    let size = (last scn) + (last ns)
    let vals = scatter (replicate size 0) inds ms
    let flgs = scatter (replicate size 0) inds ns
    in sgmScan (+) 0 (map i32.i64 flgs) vals
    