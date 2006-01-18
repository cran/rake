"predict.rake" <-
function (object, col = stop("Prediction column (argument 'col') must be specified."), 
    forcefactor = FALSE, data = eval(parse(text = dataname(rake)), 
        parent.frame(1)), ...) 
{
    rake <- object
    W <- rep(0, nrow(data))
    rowCol <- rowvar(rake)
    colCol <- colvar(rake)
    for (rnam in rownames(rake)) {
        for (cnam in colnames(rake)) {
            clas <- (data[, rowCol] == rnam & data[, colCol] == 
                cnam)
            W[clas] <- rake[rownames(rake) == rnam, colnames(rake) == 
                cnam]/sum(clas)
        }
    }
    v <- data[, col]
    if (class(v) == "character" || class(v) == "factor" || forcefactor) {
        datasum <- summary(as.factor(v))
        for (nam in names(datasum)) {
            datasum[nam] <- sum((v == nam) * W)
        }
        v <- summary(as.factor(v))
        est <- datasum
    }
    else {
        est <- v * W
    }
    list(data = v, weight = W, data.est = est)
}
