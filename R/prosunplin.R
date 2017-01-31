prosunplin <-
function(alltrees, sisters, numtrees) {
  
  # check that sisters provided exist as tip-names in the 1st tree
  sisters <- clean.sisters(sisters, alltrees)
  
  # get nodes with monophyletic congeneric species
  monos <- lapply(alltrees, function(tree){
              lapply(sisters, function(desloc.sp){get_mono(tree,desloc.sp)})
  })
  
  # sample monos
  allnodes <- lapply(monos, sample.monos)
  
  # load sunplin
  # source("sunplin-functions.r")
  
  # walk through all the trees and perform:
  # 0- create file to dump created trees
  # 1- create and write temporary files
  # 2- execute sunplin.exp
  # 3- write-appended trees in existing file
  
  # 0- create file to dump created trees
  dest.file <- paste0(getwd(), "/results.expanded.trees.tre")

  # 1- create temporary file names
  tree.file <- paste0(tempdir(), "/tree")
  nodes.file <- paste0(tempdir(), "/nodes")
  
  expanded.trees <- mapply(FUN=function(tree, nodes){
        # 1- write temporary files
        write.tree(tree, file=tree.file)
        write.table(nodes, file=nodes.file,
                    quote=F, col.names=F, row.names=F, sep=" ")
  
        # 2- execute sunplin.exp
        exp.trees <- sunplin.expd(tree.file, nodes.file, numtrees, 2)
        
        # 3- write-appended trees in existing file
        tree.list <- lapply(1:numtrees, function(x){
              sunplin.n.tree(exp.trees, x, "tree")})
        class(tree.list) <- "multiPhylo"
        write.tree(tree.list, file=dest.file, append=T)
        return(tree.list)
  }, alltrees, allnodes)
  
  return(expanded.trees)
}
