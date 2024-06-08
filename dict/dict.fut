-- module file for dicts
type opt 'v = #some v 
            | #none

module type dict = {
    type k
    type~ dict 'v

    val size 'v : dict v -> i64
    val d_map 'a 'b: (a -> b) -> dict a -> dict b
    val d_reduce 'a: (a -> a -> a) -> a -> dict a -> a
    val d_filter 'v : (k -> v -> bool) -> dict v -> dict v
    val many [n] 'v : [n]k -> [n]v -> dict v
    val single 'v : k -> v -> dict v
    val insert 'v : k -> v -> dict v -> dict v
    val union 'v : dict v -> dict v -> dict v
    val lookup 'v : k -> dict v -> opt v
    val delete 'v : dict v -> k -> dict v
}