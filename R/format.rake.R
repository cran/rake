"format.rake" <-
function (x, ...) 
{
    rake <- x
    rake <- rakeTot(rake)
    rake <- format(rake, ...)
    rownames(rake)[nrow(rake)] <- "Column Total"
    colnames(rake)[ncol(rake)] <- "Row Total"
    rake
}
