"rake" <-
function (data, colCol = 1, rowCol = 2, weight = if ("weight" %in% 
    names(data)) "weight" else 1) 
{
    if (class(weight) == "character") 
        weight <- data[, names(data) == weight]
    weight <- rep(weight, length = nrow(data))
    cols <- summary(as.factor(data[, colCol]))
    rows <- summary(as.factor(data[, rowCol]))
    cols <- cols[cols != 0]
    rows <- rows[rows != 0]
    r <- matrix(0, ncol = length(cols), nrow = length(rows))
    if (sum(names(rows) %in% names(cols)) > 0) {
        warning("Rake rows and columns have some shared names,\n", 
            "but must not in order for rake adjustment to succeed.\n")
    }
    rownames(r) <- names(rows)
    colnames(r) <- names(cols)
    for (rname in names(rows)) {
        for (cname in names(cols)) {
            r[rownames(r) == rname, colnames(r) == cname] <- sum(weight[data[, 
                colCol] == cname & data[, rowCol] == rname])
        }
    }
    attr(r, "dataname") <- deparse(substitute(data))
    attr(r, "class") <- "rake"
    if (class(rowCol) != "character" && !is.null(colnames(data))) 
        rowCol <- colnames(data)[rowCol]
    if (class(colCol) != "character" && !is.null(colnames(data))) 
        colCol <- colnames(data)[colCol]
    attr(r, "rowCol") <- rowCol
    attr(r, "colCol") <- colCol
    r
}
