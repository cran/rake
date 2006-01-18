"simpleRake" <-
function (data, marg, colCol = 1, rowCol = 2, col = 3, forcefactor = FALSE, 
    weight = if ("weight" %in% names(data)) "weight" else 1, 
    verbose = FALSE) 
{
    if (missing(marg)) 
        stop("\nMarginal total population weights ", "(argument 'marg') must be specified.")
    r <- rake(data, colCol, rowCol, weight)
    r <- rakeadj(r, marg, verbose)
    predict(r, col, forcefactor)
}
