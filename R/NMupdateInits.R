##' Create new Nonmem control stream with updated initial parameter values
##' @param file.mod The control stream to update. Will not be edited.
##' @param newfile New file to generate
##' @param fix Fix the values? Probably only TRUE is supported.
##' @return The resulting control stream contents as text
##' @keywords internal

NMupdateInits <- function(file.mod,newfile,fix){
    
    par.type <- NULL
    i <- NULL
    est <- NULL
    j <- NULL
    
    ext <- NMreadExt(fnExtension(file.mod,"ext"))
    pars.est <- ext$pars
    setnames(pars.est,"est","value")
    res <- NMreplaceInits(files=file.mod,newfile=newfile,inits=ext$pars,fix=fix,quiet=TRUE)
    invisible(res)
}