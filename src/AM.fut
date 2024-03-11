import "trees"

module type AM = {
    type K
    type V
    type A
    type M
    val <: K -> K -> bool
    val g: K -> V -> A
    val f: A -> A -> A
    val I: A
    -- val M: tree
    -- Should the tree be part of the AugMap, or just input to its functions?
    
    val augVal: M -> A
    val augLeft: M -> K -> A
    val augRange: M -> K -> K -> A
    val augFilter: (A -> bool) -> M -> M
    val augProject: (A -> B) -> (B -> B -> B) -> M -> K -> K -> B
}
