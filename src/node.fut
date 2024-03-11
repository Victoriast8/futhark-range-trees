module type node = {
    type K
    type V
    val k : (K, V)
    val Tr : node
    val Tl : node
}
-- https://github.com/athas/raytracingthenextweekinfuthark/blob/master/radixtree.fut