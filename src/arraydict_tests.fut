import "arraydict"

-- this basetest assumes that 'arraydict.single' creates a dict containing a single (key, value) pair
-- ==
-- entry: arraydict_lookup
-- input { 0i32 0i32 }
-- output { [0i32] true }
-- input { 0i32 1i32 }
-- output { [0i32] false }
entry arraydict_lookup (key : i32) (lookup : i32) : 
                       ((arraydict.dict bool), bool) =
    let d = arraydict.single key true
    let v = match (arraydict.lookup lookup d)
            case #some s -> true
            case #none -> false
    in (d, v)


-- ==
-- entry: arraydict_many
-- input { [1,2,3,4] }
-- output { [1i32, 2i32, 3i32, 4i32] }
-- input { [1,1,1,1] }
-- output { [1i32] }
entry arraydict_many [n] (keys : [n]i32) : arraydict.dict bool =
    arraydict.many keys (replicate n true)


-- ==
-- entry: arraydict_union
-- input { [1,2,3,4] [5,6,7,8] }
-- output { [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32] }
-- input { [1,1,1,1] [1,1,1,1] }
-- output { [1i32] }
entry arraydict_union [n] (k1 : [n]i32) (k2 : [n]i32) : arraydict.dict bool =
    let values = replicate n true
    in arraydict.union (arraydict.many k1 values) (arraydict.many k2 values)


-- ==
-- entry: arraydict_map
-- input { [1,2,3,4] [0,0,0,0] }
-- output { [1i32, 2i32, 3i32, 4i32] }
entry arraydict_map [n] (keys : [n]i32) (values : [n]i32) : arraydict.dict bool =
    let d = arraydict.many keys values
    in arraydict.d_map (bool.i32) d


-- ==
-- entry: arraydict_reduce
-- input { [1,2,3,4] [1,1,1,1] }
-- output { 4i32 }
entry arraydict_reduce [n] (keys : [n]i32) (values : [n]i32) : i32 =
    let d = arraydict.many keys values
    in arraydict.d_reduce (+) 0 d


-- ==
-- entry: arraydict_delete
-- input { [1,2,3,4] 2 }
-- output { [1i32, 3i32, 4i32] }
entry arraydict_delete [n] (keys : [n]i32) (rm : i32) : i32 =

    