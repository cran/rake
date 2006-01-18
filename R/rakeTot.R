"rakeTot" <-
function (x) 
{
    rake <- x
    rake <- cbind(rake, rowSums(rake))
    rake <- rbind(rake, colSums(rake))
    rake
}
