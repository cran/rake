"print.rake" <-
function (x, ...) 
{
    rake <- x
    cat("  data: ", dataname(rake), "\n", "rowvar: ", rowvar(rake), 
        "\n", "colvar: ", colvar(rake), "\n", sep = "")
    rake <- rakeTot(rake)
    print(rake)
}
