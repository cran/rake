"rakeadj" <-
function (rake, marg = stop("\nMarginal total population weights", 
    "(argument 'marg') must be specified."), verbose = FALSE) 
{
    if (!class(rake) == "rake") 
        stop("\nArgument 'rake' must be an object of class \"rake\".")
    if (sum(rownames(rake) %in% colnames(rake)) > 0) {
        print(rake)
        stop("\nRake rows and columns have shared names,\n", 
            "but must not in order for rake adjustment to succeed.")
    }
    if (class(marg) == "character") 
        marg <- read.table(marg, header = T)
    if (class(marg) == "data.frame") {
        margnames <- marg$name
        marg <- marg$weight
        names(marg) <- margnames
    }
    if (!is.vector(marg) || is.null(names(marg))) 
        stop("\nMarginal total weights (argument 'marg') ", "in unrecognized format.\n", 
            "Please refer to 'help(rakeadj)'.")
    rowtot <- marg[rownames(rake)]
    coltot <- marg[colnames(rake)]
    if (sum(rowtot) != sum(coltot)) {
        print(c(sum(rowtot), rowtot))
        print(c(sum(coltot), coltot))
        stop("\nSum of population marginal total weights ", "(argument 'marg')\n", 
            "of rows and columns are not the same, ", "but need to be for Raking to converge.")
    }
    rakesum <- sum(rake)
    adjust <- function(v, key) v * key/sum(v)
    oldrake <- 0
    count <- 0
    while (sum(oldrake == rake) != length(rake)) {
        oldrake <- rake
        for (nam in rownames(rake)) rake[nam, ] <- adjust(rake[nam, 
            ], marg[nam])
        for (nam in colnames(rake)) rake[, nam] <- adjust(rake[, 
            nam], marg[nam])
        count <- count + 1
    }
    if (verbose) 
        cat("The rake adjustment converged in ", count, " steps.\n", 
            sep = "")
    rake/sum(rake) * rakesum
}
