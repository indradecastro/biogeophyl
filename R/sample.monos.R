sample.monos <-
function(monos){
      chosen <- sapply(seq_along(monos), function(x){
            desloc <- names(monos)[x]
            mono <- monos[[x]]
            chosen <- sample(mono$node, size=1, prob=mono$nsps)
            names(chosen) <- desloc
            return(chosen)
      })
      chosen.df <- data.frame("desloc.sp"=names(chosen), "node"=chosen)
      return(chosen.df)
}
