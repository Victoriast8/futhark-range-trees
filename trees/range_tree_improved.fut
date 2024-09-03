-- A new implementation, to represent the improved (and parallelized queries) of range trees!
-- Instead of increasing the compilation time of (or replacing) the original `range_tree.fut`,
-- this version serves as a contrast to the original implementation.
import "helper"

type opt 'v = #some v
            | #none
type child = opt i64
type point [d] = [d]f64
type box   [d] = (point [d],point [d])
type node  [d] = {m : point [d], slice : (i64,i64), subtree : child, left : child, right : child}

def create_node [d] (p : point [d]) (start : i64, len : i64) (sub : child) (l : child) (r : child) : node [d] =
    {m = p, slice = (start,len), subtree = sub, left = l, right = r}

module type range_tree = {
    type~ treeNodes [d]
    type~ treePoints [d]
    type~ tree [d]

    -- *split-parallel* means parallelization of splitting nodes.
    --   More effective with more splits, which generally applies to more dimensions.

    val count_par [d] : box [d] -> tree [d] -> i64 -- split-parallel count
    val query_par [d] : box [d] -> tree [d] -> treePoints [d] -- split-parallel query
    val multi_q [d] [n] : [n]box [d] -> tree [d] -> treePoints [d] -- many split-parallel queries
    val build_imp [d] [n] : [n]point [d] -> tree [d] -- improved build

    -- | Fractional cascading: a faster query method.
    -- Ups the runtime of a query with a logarithmic factor.
    -- Build is equally fast, but saves canonicals differently(?),
    --   which the fractional query then uses for faster queries.
    -- OF ALL OPTIMIZATIONS these *should* be the most significant ones.
    val build_frac [d] [n] : [n]point [d] -> tree [d] -- improved build, but uses fractional cascading
    val query_frac [d] [n] : [n]box [d] -> tree [d] -> treePoints [d] -- query with fractional cascading
}

module k_range_tree_imp : range_tree = {
    type~ treeNodes [d] = []node [d]
    type~ treePoints [d]= []point [d]
    type~ tree [d]      = {tNodes : treeNodes [d], tCanonical : treePoints [d]}

    def count_par [d] (b : box [d]) (t : tree [d]) : i64 =
        ???
    
    def query_par [d] (b : box [d]) (t : tree [d]) : treePoints [d] =

        -- checks if a single point is contained within a single box.
        let in_box (b : box [d]) (p : point [d]) : bool =
            map3 (\lo hi p' -> lo <= p' && p' <= hi) b.0 b.1 p
                |> all (\t -> t)

        -- unpacks a child to an integer/index (no child yields `(-1)`)
        let node_idx (n : child) : i64 =
            match n
            case #some idx -> idx
            case #none     -> -1
            
        -- find first v_split?

        -- traverse tree and report points
        let (_,_,_,res) =
            loop (queue,typ,dim,acc) =
                ([t.tNodes[0]],[0],[1],[])
            while !(null queue) do

            

        in res

    
    -- as a collection query, we need to return a shape of the result as well?
    def multi_q [d] [n] (b : [n]box [d]) (t : tree [d]) : treePoints [d] =
        ???

    def build_imp [d] [n] (ps : [n]point [d]) : tree [d] =
        ??? -- finish this last - should be independent from the other implementations
            -- ... may also be impossible

        -- let accChilds (i : i32) : i32 = if i <= 1 then 0 else 1
        -- let ps' = map (\i -> sort_by_key (\e -> e[i]) (f64.<=) ps) (iota d)
        --     |> flatten -- is there a more efficient way? look for a flat function perhaps...
        
        -- let (_,_,_,res) =
        --     loop (wrk, wrk_shp, wrk_dim, acc) = 
        --          (ps', [(i32.i64 n)], [1i32], ([],[]))
        --     while !(null wrk) do
            
            
    
    def build_frac [d] [n] (ps : [n]point [d]) : tree [d] =
        ???
    
    -- as a collection query, we need to return a shape of the result as well?
    def query_frac [d] [n] (bs : [n]box [d]) (t : tree [d]) : treePoints [d] =
        ???
}