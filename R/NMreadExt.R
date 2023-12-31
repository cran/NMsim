##' Read information from Nonmem ext files
##'
##' @param path.ext Path to the ext file
##'
##' @return A list with a final parameter table and a table of the iterations
##' @keywords internal

NMreadExt <- function(path.ext){

    .Deprecated("NMdata::NMreadExt")
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ITERATION <- NULL
    variable <- NULL
    NMREP <- NULL
    par.type <- NULL
    parameter <- NULL
    i <- NULL
    j <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    res.NMdat <- NMreadTab(path.ext,as.fun="data.table",quiet=TRUE)
    ## compareCols(
    ##     res.NMdat
    ##    ,
    ##           fread(path.ext)
    ##           )
    ## res.NMdat
    ## res.NMdat[,sprintf("%d",ITERATION)]

    ## NONMEM USERS GUIDE
    ## INTRODUCTION TO NONMEM 7.5.0
    ## Robert J. Bauer
    ## ICON Plc
    ## Gaithersburg, Maryland
    ## February 23, 2021

    dt.codes <- fread(text="ITERATION,variable
-1e+09,est
-1000000001,se
-1000000002,eigCor
-1000000003,cond
-1000000004,stdDevCor
-1000000005,seStdDevCor
-1000000006,FIX
-1000000007,termStat
-1000000008,partLik")

    ## dt.codes

    res.NMdat <- mergeCheck(res.NMdat,dt.codes,by=cc(ITERATION),all.x=T,quiet=TRUE)
    ## res.NMdat

    
    pars <- res.NMdat[variable%in%dt.codes$variable,setdiff(colnames(res.NMdat),"OBJ"),with=FALSE]
    pars <- melt(pars,id.vars=cc(ITERATION,variable,NMREP),variable.name="parameter")
    pars <- dcast(pars,NMREP+parameter~variable,value.var="value")

    pars[,par.type:=NA_character_]
    pars[grepl("^THETA",parameter),par.type:="THETA"]
    pars[grepl("^OMEGA",parameter),par.type:="OMEGA"]
    pars[grepl("^SIGMA",parameter),par.type:="SIGMA"]
    pars[par.type=="THETA",i:=sub("THETA([0-9]+)","\\1",parameter)]
    pars[par.type=="OMEGA",i:=sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\1",parameter)]
    pars[par.type=="OMEGA",j:=sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\2",parameter)]
    pars[par.type=="SIGMA",i:=sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\1",parameter)]
    pars[par.type=="SIGMA",j:=sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\2",parameter)]
    cols <- cc(i,j)
    pars[,(cols):=lapply(.SD,as.integer),.SDcols=cols]

    ## what to do about OBJ? Disregard? And keep in a iteration table instead?
    iterations <- res.NMdat[as.numeric(ITERATION)>(-1e9),!("variable")] 
    iterations <- melt(iterations,id.vars=cc(ITERATION,NMREP))

    list(pars=pars,iterations=iterations)
}
