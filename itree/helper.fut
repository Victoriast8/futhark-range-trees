-- helper functions

-- intertwines two arrays - as = [1,2,3], bs = [4,5,6]; intertwine as bs = [1,4,2,5,3,6]
def intertwine [n] 't (as : [n]t) (bs : [n]t) : [n*2]t =
    map2 (\x y -> [x,y]) as bs |> flatten

-- Typical exclusive scan
def scanExcl [n] 't (op : t -> t -> t) (ne: t) (arr : [n]t) : [n]t =
    scan op ne <| map (\i -> if i > 0 then arr[i-1] else ne) (iota n)

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

-- Stolen from DPP
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

-- Stolen from DPP
-- Creates a flag array from a shape by scattering values into an array of the shape's simulated size
-- In case you were wondering - yes, this is just a more convenient way to create a flag array in most cases
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

-- note - this is my own DPP A3 assignment handin of flat_scatter + partition2L.
-- The implementation *is* working. It should, however, probably be looked through once more.
def flat_scatter 't [m] [n] [g]
                    (xs: [m]t) (is: [n]i32) (as: [n]t) 
                    (xs_shp: [g]i32) (is_shp: [g]i32) 
                    : [m]t =
    let shpscan_i = scanExcl (+) 0 is_shp
    let x_incl = scan (+) 0 xs_shp
    let x_excl = scanExcl (+) 0 xs_shp
    let sc1 = scatter 
        (replicate n 0)
        (map i64.i32 shpscan_i)
        x_incl
    let upper = sgmSumInt sc1 sc1
    let sc2 = scatter 
        (replicate n 0)
        (map i64.i32 shpscan_i)
        x_excl
    let base = sgmSumInt sc2 sc2
    let upd_i = map3 (\i b u -> 
            let ind = i+b
            in if ind >= u || i < 0 then -1 else ind
        ) is base upper
        |> map i64.i32
    in scatter (copy xs) upd_i as

-- to see the effects of a partition2L, either:
-- 1. make a new function that doesn't impose partition2L, or
-- 2. use partitionL two times in succession (true elements will appear first)
def partitionL 't [n] [m]
                  (condsL: [n]bool) (dummy: t)
                  (shp: [m]i32, arr: [n]t) :
                  ([m]i32, ([m]i32, [n]t)) =
    let begs   = scan (+) 0 shp
    let flags  = mkFlagArray shp 0i32 (map (+1) (map i32.i64 (iota m)))
    let outinds= sgmSumInt flags <| map (\f -> if f==0 then 0 else f-1) flags

    let tflgs = map (\ c -> if c then 1 else 0) condsL
    let fflgs = map (\ b -> 1 - b) tflgs

    let indsT = sgmSumInt (flags :> [n]i32) tflgs
    let tmp   = sgmSumInt (flags :> [n]i32) fflgs
    let lst   = map2 (\b s -> if s > 0 then indsT[b-1] else -1i32) begs shp
    let indsF = map2 (\i v -> lst[i]+v) (outinds :> [n]i32) tmp

    let inds  = map3 (\ c indT indF -> if c then indT-1i32 else indF-1) condsL indsT indsF

    let fltarr = flat_scatter (replicate n dummy) inds arr shp shp

    in  (lst, (shp,fltarr))

-- It is important to note, that the two partitions happen on the same segment twice, without removing successes.
-- In some cases, this won't be an issue, for instance when no element succeeding p1 can succeed p2 (e.g. p1='>' and p2='<')
-- This may or may not be fixed, but for now the partition2L implementation works for what is needed,
-- downprioritizing a refactor, which would probably take some time.
def flat_res_partition2L 't [n] [m]
                   (p1 : t -> bool) (p2 : t -> bool)
                   (dummy : t) (shp : [m]i32, arr : [n]t)
                   : (([m]i32,[m]i32), ([m]i32, [n]t)) =
    let (split1, (_,ps))  = partitionL (map p1 arr) dummy (shp, arr)
    let (split2, (_,ps')) = partitionL (map p2 ps) dummy (shp, ps)
    in ((split1,split2),(shp,ps'))

-- Was going to generalize this case, but making use of a segmented scan requires too many obscure parameters;
-- we should instead strive to create the flat implementation for each type, as the necessity arises
def flat_replicate_bools [n] (ns : [n]i64) (ms : [n]bool) : []bool =
    let inds = scanExcl (+) 0 ns
               |> map2 (\n i -> if n>0 then i else -1) ns
    let size = (last inds) + (last ns)
    let vals = scatter (replicate size false) inds ms
    let flgs = scatter (replicate size 0) inds ns
    in sgmScan (||) false (map i32.i64 flgs) vals