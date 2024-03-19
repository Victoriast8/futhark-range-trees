import "dict"

-- ==
-- entry: arraydict_single
-- input { 0i32 true }
-- output { [0i32] }
entry arraydict_single (key : i32) (value : bool) : arraydict.dict bool =
    arraydict.single key value

-- ==
-- entry: arraydict_many
-- input { [1,2,3,4] [true,true,false,false] }
-- output { [1i32, 2i32, 3i32, 4i32] }
-- input { [1,1,1,1] [true,true,false,false] }
-- output { [1i32] }
entry arraydict_many [n] (keys : [n]i32) (values : [n]bool) : arraydict.dict bool =
    arraydict.many keys values

-- Funny compiler error - if you delete the below entrypoint, the tests will pass:
-- Otherwise, the compiler will complain about 'arraydict.dict bool' being an opaque type:
-- One would think the compiler would complain, when just a single entrypoint makes this error?
-- TODO: refactor; use lookup and better input to generate predictable (testable) and meaningful output.
-- ==
-- entry: arraydict_union
-- input { [1,2,3,4] [5,6,7,8] [true,true,false,false] }
-- output { [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32] }
entry arraydict_union [n] (k1 : [n]i32) (k2 : [n]i32) 
                          (values : [n]bool) : arraydict.dict bool =
    arraydict_union (arraydict.many k1 values) (arraydict.many k2 values)
