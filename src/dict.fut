-- module file for dicts
type opt 'v = #some v 
            | #none

module type dict = {
    type key = i32
    type~ dict 'v

    val map 'a 'b: (a -> b) -> dict a -> dict b
    val reduce 'a: (a -> a -> a) -> a -> dict a -> a
    val many [n] 'v : [n]key -> [n]v -> dict v
    val single 'v : key -> v -> dict v
    val union 'v : dict v -> dict v -> dict v
    val lookup 'v : key -> dict v -> opt v
}