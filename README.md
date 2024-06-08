## Interval Trees and Range Trees in Futhark
This project is about implementing interval trees in futhark using PAMs (Parallel Augmented Maps).

The project's original purpose is to seek an answer to the question: "Are PAMs suited for data-parallel construction of interval trees?", however we may find another road along the way.

Also, the project extends as far as to implement the PBBS benchmark for 2D range queries, mentioned [here](https://cmuparlay.github.io/pbbsbench/benchmarks/rangeQuery2d.html)

## What are PAMs?
Here's an article about PAMs: https://dl.acm.org/doi/pdf/10.1145/3200691.3178509 \
This project sought to use PAMs as mentioned in the article, but the fork-join strategy is not suited for Futhark, due to unsupported manual thread manipulation(?). And in collateral, recursion, as well as pointer structures.

### Who are we and why this project?
I am a bachelor student writing this project as part of my bachelor project, supervised by Troels Henriksen. \
The project idea came from Troels and is interesting, because range trees have some real world applications, for instance window queries (2-dimensional range queries) in SQL.
