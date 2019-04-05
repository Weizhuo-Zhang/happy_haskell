#但是我怎么知道每个参数是传给 stack 还是 ghc 的呢？方法是，用 -- 分割，前面的给 stack，后面的给 ghc

#stack ghc -- -o TraversalHS.out Traversal.hs
stack ghc -- -o ReductionHS.out Reduction.hs
