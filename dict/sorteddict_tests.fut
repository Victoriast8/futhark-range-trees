import "sorteddict"

-- this basetest assumes that 'sorteddict.single' creates a dict containing a single (key, value) pair
-- ==
-- entry: sorteddict_lookup
-- input { 0i32 0i32 }
-- output { [0i32] true }
-- input { 0i32 1i32 }
-- output { [0i32] false }
entry sorteddict_lookup (key : i32) (lookup : i32) : 
                       ((sorteddict.dict bool), bool) =
    let d = sorteddict.single key true
    let v = match (sorteddict.lookup lookup d)
            case #some s -> true
            case #none -> false
    in (d, v)


-- ==
-- entry: sorteddict_many
-- input { [1,2,3,4] }
-- output { [1i32, 2i32, 3i32, 4i32] }
-- input { [1,1,1,1] }
-- output { [1i32] }
entry sorteddict_many [n] (keys : [n]i32) : sorteddict.dict bool =
    sorteddict.many keys (replicate n true)


-- ==
-- entry: sorteddict_size
-- input { 100i64 }
-- output { 100i64 }
entry sorteddict_size (size : i64) : i64 =
    sorteddict.size (sorteddict.many (map i32.i64 (iota size)) (iota size))


-- ==
-- entry: sorteddict_union
-- input { [1,2,3,4] [5,6,7,8] }
-- output { [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32] }
-- input { [1,1,1,1] [1,1,1,1] }
-- output { [1i32] }
entry sorteddict_union [n] (k1 : [n]i32) (k2 : [n]i32) : sorteddict.dict bool =
    let values = replicate n true
    in sorteddict.union (sorteddict.many k1 values) (sorteddict.many k2 values)


-- ==
-- entry: sorteddict_map
-- input { [1,2,3,4] [0,0,0,0] }
-- output { [1i32, 2i32, 3i32, 4i32] }
entry sorteddict_map [n] (keys : [n]i32) (values : [n]i32) : sorteddict.dict bool =
    let d = sorteddict.many keys values
    in sorteddict.d_map (bool.i32) d


-- ==
-- entry: sorteddict_reduce
-- input { [1,2,3,4] [1,1,1,1] }
-- output { 4i32 }
-- input { [1,2,3,4] [1,2,3,4] }
-- output { 10i32 }
entry sorteddict_reduce [n] (keys : [n]i32) (values : [n]i32) : i32 =
    let d = sorteddict.many keys values
    in sorteddict.d_reduce (+) 0 d


-- ==
-- entry: sorteddict_delete
-- input { [1,2,3,4] 2 }
-- output { [1i32, 3i32, 4i32] }
entry sorteddict_delete [n] (keys : [n]i32) (rm : i32) : sorteddict.dict bool =
    let d = sorteddict.many keys (replicate n true)
    in sorteddict.delete d rm


-- ==
-- entry: sorteddict_insert
-- input { [1,2,4,5] 3 }
-- output { [1i32, 2i32, 3i32, 4i32, 5i32] }
entry sorteddict_insert [n] (keys : [n]i32) (ins : i32) : sorteddict.dict bool =
    let d = sorteddict.many keys (replicate n true)
    in sorteddict.insert ins true d