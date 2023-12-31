##' Apply function and return a data.table
##'
##' A convenience function that returns a data.table with a column
##' representing the input values and a column with results. This is
##' still experimental and will not work for many input structures.
##' 
##' @param ... arguments passed to lapply
##' @import data.table 
##' 
##' @details Only functions that return vectors are currently
##'     supported. dtapply should support functions that return
##'     data.frames.
##' @keywords internal
##' @return a data.table
##'
## generally applicable but still too early to export

### examples
## NMsim:::dtapply(setNames(1:4,letters[1:4]),sqrt)

dtapply <- function(X,FUN,...){
    res.list <- lapply(X,FUN,...)
    ## dt1 <- data.table(name=names(res.list),res=unlist(res.list))
    dt1 <- data.table(name=names(X),res=unlist(res.list))
    dt1[]
}
