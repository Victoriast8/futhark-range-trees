-- interface
module type AM = {
    type K
    type V
    type A
    val <: K -> K -> bool
    val g: K -> V -> A
    val f: A -> A -> A
    val I: A
    val M: 

    val size:
}

-- 
-- module itree: AM with K = i64 = {
    
-- }