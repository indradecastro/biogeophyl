sunplin.n.tree <-
function ( fileOrText, nth, typeInput ){
	if( typeInput == "tree" ){
		write(fileOrText, "temp", sep = "\n")
		fileOrText = "temp"
	}
	ret <- strsplit(scan(fileOrText, what = character(), skip = nth, nlines = 1, quiet=T), " ")[[4]]
	return (read.tree(text=ret))
}
