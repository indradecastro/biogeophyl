clean.sisters <-
function(sisters, mytrees) {
      onetree <- if(length(mytrees)>1){mytrees[[1]]}else{mytrees}
      
      missing <- as.character(unlist(sisters))[!as.character(unlist(sisters)) %in% onetree$tip.label]
      if(length(missing!=0)){
            cat("\nsome of the sister species were missing in the trees",
                "the were REMOVED from the list:", missing, "\n", sep="\n")
            }else{cat("Perfect, all sisters present in tree!\n")}
      present <- as.character(unlist(sisters))[as.character(unlist(sisters)) %in% onetree$tip.label]
      
      clean.sisters <- lapply(sisters, function(x){unlist(x)[as.character(unlist(x)) %in% mytrees[[1]]$tip.label]})
      rm.desloc.sps <- names(clean.sisters)[sapply(clean.sisters, length)==0]
      if(length(rm.desloc.sps!=0)){
            cat("\nsome of the unsequenced species have no sisters left",
                "they are impossible to fit into the trees",
                "the were REMOVED from the list:", rm.desloc.sps, "\n", sep="\n")
      }else{cat("Perfect, all unsequenced species have at least one sister!\n")}
      
      clean.sisters <- clean.sisters[sapply(clean.sisters, length)!=0]
      
      return(clean.sisters)
}
