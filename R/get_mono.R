get_mono <-
function(tree, sisters){
      
      tree <- makeNodeLabel(tree, prefix = "n")
      res <- data.frame(matrix(ncol = 2, nrow = 0))
      names(res) <- c("node", "nsps")
      
      mc <- oldest.mrca(tree, sisters)
      des <- tips(tree, mc)
      pos <- nrow(res) + 1
      
      if (sum(sisters %in% des) != length(des)) {
            dece <- getDescendants(tree, mc)
            i = 0
            while (length(sisters) != 0) {
                  i = i + 1
                  pos <- nrow(res) + 1
                  des <- tree$tip.label[getDescendants(tree, dece[i])]
                  des <- des[!is.na(des)]
                  
                  if (sum(sisters %in% des) == length(des)) {
                        dd <- dece[i]
                        res[pos, 1] <- if(dd>(tree$Nnode+1)){tree$node.label[dd - (tree$Nnode + 1)]}else{tree$tip.label[dd]}
                        res[pos, 2] <- sum(sisters %in% des)
                        sisters <- sisters[!sisters %in% des]
                  }
            }
      } else{
            res[pos, 1] <- tree$node.label[mc - (tree$Nnode + 1)]
            res[pos, 2] <- length(des)
      }
      res <- res[order(res[, 1]), ]
      return(res)
}
