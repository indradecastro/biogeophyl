sunplin.expd <-
function( fileTree, filePuts, numTree, method ){
    # dyn.load("sunplin.spn")
	return (.Call("expd", fileTree, filePuts, numTree, method))
}
