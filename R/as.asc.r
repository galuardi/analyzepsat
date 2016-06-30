as.asc <- function (x, xll = 1, yll = 1, cellsize = 1, type = c("numeric", 
    "factor"), lev = levels(factor(x))) 
{
    type <- match.arg(type)
    if (!inherits(x, "matrix")) 
        stop("x should be a matrix")
    mode(x) <- "numeric"
    attr(x, "xll") <- xll
    attr(x, "yll") <- yll
    attr(x, "cellsize") <- cellsize
    attr(x, "type") <- type
    if (type == "factor") 
        attr(x, "levels") <- lev
    class(x) <- "asc"
    return(x)
}

getascattr <- function (xfrom, xto, type = c("numeric", "factor"), lev = NULL) 
{
    type <- match.arg(type)
    if (!inherits(xfrom, "asc")) 
        stop("xfrom should be an asc object")
    if (mode(xto) == "logical") {
        mode(xto) <- "numeric"
        xto <- xto + 1
    }
    attr(xto, "xll") <- attr(xfrom, "xll")
    attr(xto, "yll") <- attr(xfrom, "yll")
    attr(xto, "cellsize") <- attr(xfrom, "cellsize")
    attr(xto, "type") <- type
    if (type == "factor") {
        if (is.null(lev)) 
            lev <- levels(factor(xto))
        attr(xto, "levels") <- lev
    }
    class(xto) <- "asc"
    return(xto)
}