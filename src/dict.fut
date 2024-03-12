type opt 'v = #some v
              | #none

module type dict = {
    type~ dict 'v
    type key = i32

    val single 'v : key -> v -> dict v
    val union 'v : dict v -> dict v -> dict v
    val lookup 'v: key -> dict v -> opt v
}

module arraydict : dict = {
    type key = i32
    type~ dict 'v = [](key, v)

    def find_index 'a [n] (p: a -> bool) (as: [n]a): i64 =
        let op (x, i) (y, j) =
            if x && y then if i < j
                then (x, i)
                else (y, j)
            else if y then (y, j)
            else (x, i)
        in (reduce_comm op (false, -1) (zip (map p as) (iota n))).1
    
    -- val many [n] 'v : [n]key -> [n]t -> dict v

    def single 'v (ke : key) (va : v) : dict v =
        [(ke, va)]

    def union 'v (d : dict v) (d' : dict v) : dict v =
        d ++ d'

    -- Tænk over hvad sker der hvis der er dubletter i keys?
    -- lookup der mindre sekventielt
    def lookup 'v (ke : key) (d : dict v) : opt v =
        let p (x: key, y: v) : bool = x == ke
        let m = find_index p d
        in if m != -1 then #some d[m].1 else #none 
}

-- tænk over at lave bedre arraydict, hvor dictionarien er sorteret.
def test_arraydict =
    let arrd = arraydict.single 0 true
    let look = arraydict.lookup 1 arrd
    in look