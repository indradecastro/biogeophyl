sunplin.dist <-
function( fileOrText, typeInput ){
    dyn.load("sunplin.spn")
	return (.Call("dist", fileOrText, typeInput))
}
