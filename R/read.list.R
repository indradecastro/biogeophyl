read.list <-
function(file){
      # Read in the data
      x <- scan(file, what="", sep="\n")
      # Separate elements by one or more whitepace
      y <- strsplit(x, "[[:space:]]+")
      # Extract the first vector element and set it as the list element name
      names(y) <- sapply(y, `[[`, 1)
      # Remove the first vector element from each list element
      y <- lapply(y, `[`, -1)
      return(y)
}
