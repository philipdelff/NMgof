##' Get one plot of observations, predictions, and means vs time
##' @description A summary plot of the observations and predictions. These
##'     important diagnostics plots during model development and as to
##'     understand the dynamics of data and model. The function is very flexible
##'     in terms of what combinations of observations, population predictions,
##'     and individual predictions to plot, and how to represent them.
##' @param data The dataset to plot.
##' @param geom.dv How to plot observations. A string containing any subset of
##'     \itemize{ \item p: points \item l: lines \item c: confidence interval of
##'     mean at each value of col.ntim.  }
##' @param geom.pred How to plot population predictions. See geom.dv.
##' @param geom.ipred How to plot individual predictions. See geom.dv.
##' @param geom.dvmean How to plot means (at each value of col.ntim) of
##'     observations.
##' @param geom.predmean How to plot means (at each value of col.ntim) of
##'     population predictions.
##' @param geom.ipredmean How to plot means (at each value of col.ntim) of
##'     individual predictions.
##' @param col.id The name of the column containing subject ID's..
##' @param col.grp A grouping variable. Means will be grouped by this (and
##'     col.ntim), and plot will be facetted by grp too.
##' @param type.mean Passed to pmxtricks::means().
##' @param col.time name of column to plot observations against.
##' @param col.ntim name of column of nominal time used for calculation and
##'     plotting of means.
##' @param col.dv name of column containing observations.
##' @param col.ipred Name of column containing individual predictions. Default
##'     is searching for IPRED, then IPRE.
##' @param col.pred Name of column containing population predictions.
##' @param debug debug Start by calling browser()?
##' @param log.y Use logarithmic scale for y-axis?
##' @param args.plot.mean List of arguments to be passed to geoms applied to
##'     mean-type data, like mean(PRED) etc.
##' @param filter.rows Default is to only plot EVID==0 records. Set to FALSE, if
##'     this filtering is not wantered.
##' @import data.table
##' @import ggplot2
##' @examples
##' library(pmxtricks)
##' library(data.table)
##' data(pksim1,package="pmxtricks")
##' pksim2 <- copy(pksim1)
##' pksim2[,PRED:=DV+rnorm(.N)*10]
##' pksim2[,NOMTIME:=TIME]
##' pksim2[,TIME:=round(TIME+rnorm(.N),2)]
##' 
##' 
##' pmxtricks:::NMplotFit(pksim2,
##'            geom.dv = "p",
##'            geom.pred = "l",
##'            geom.dvmean = "lc",
##'            geom.predmean = "p",
##'            col.ntim = "NOMTIME",
##'            col.dv = "DV",
##'            col.ipred = NULL,
##'            col.pred = "PRED",
##'            type.mean = "arithmetic",
##'            debug=FALSE)+
##'     theme_pp()
##' 

###### Don't export

####### todo

## when plotting ipred (not mean.ipred), they must be grouped by ID and col.grp. Can we colour by grp in general?

### check validity of geom.dv etc

### I dont think IDwithin is working

## standard option to filter EVID==0

## ### This needs double check
###    pklong4 <- unique(pklong4,by=c(col.id,col.ntim,"variable","value"))

####### end todo 


NMplotFitOld <- function(data, 
                      geom.dv= "p",
                      geom.pred = "",
                      geom.ipred = "",
                      geom.dvmean = "pc",
                      geom.predmean = "l",
                      geom.ipredmean="",
                      col.grp,
                      type.mean = "arithmetic",
                      col.id = "ID",
                      col.time="TIME",
                      col.ntim = "NOMTIME",
                      col.dv = "DV",
                      col.ipred = c("IPRED","IPRE"),
                      col.pred = "PRED",
                      log.y=F,
                      args.plot.mean=NULL,
                      filter.rows,
                      debug=F){

    if(debug) browser()
    pklong0 <- copy(as.data.table(data))

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    value <- NULL
    type <- NULL
    variable <- NULL
    val.line <- NULL
    val.point <- NULL
    val.ci.l <- NULL
    val.ci.u <- NULL
    mean.grp.l <- NULL
    mean.grp.u <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(filter.rows)){
        if("EVID"%in%colnames(pklong0)){
            filter.rows <- "EVID==0"
        } else {
            filter.rows <- NULL
        }
    }
    
    if(!is.null(filter.rows) && !is.na(filter.rows)){
        pklong0 <- pklong0[eval(parse(text=filter.rows))]
    }
### look for ipred
    if(!is.null(col.ipred)) {
        if(length(col.ipred)==0) col.ipred <- NULL 
        found.ipred <- col.ipred %in% colnames(pklong0)
        if(sum(found.ipred)==0) {
            message("Individual predictions not found in data. Skipping.")
            col.ipred <- NULL
            if(nchar(geom.ipred)>0) {
                warning("geom.ipred is non-empty, but individual predictions not found in data. Skipping.")
                plot.ipred <- FALSE
            }
        } else {
            col.ipred <- col.ipred[found.ipred] [1]
        }
        
    }

### look for pred
    if(!is.null(col.pred)) {
        if(length(col.pred)==0) col.pred <- NULL 
        found.pred <- col.pred %in% colnames(pklong0)
        if(sum(found.pred)==0) {
            message("Individual predictions not found in data. Skipping.")
            col.pred <- NULL
            if(nchar(geom.predmean)) {
                warning("geom.predmean is non-empty, but population predictions not found in data. Skipping.")
                geom.predmean <- ""
            }
        } else {
            col.pred <- col.pred[found.pred] [1]
        }
    }

    ## we need grp until finding a way to use by in data.table that can be empty
    if(missing(col.grp)){
        col.grp <- tmpcol(pklong0,base="grp")
        pklong0[,c(col.grp):="All"]
    }


    if(missing(type.mean)){
        if(log.y) {
            type.mean <- "geometric"
            message("type.mean not specified. Geometric mean used.")
        } else {
            type.mean <- "arithmetic"
            message("type.mean not specified. Arithmetic mean used.")
        }
    }
    
    ## mvars <- c("DV","PRED","IPRED")[c("DV","PRED","IPRED")%in%colnames(pksim2)]
### these checks should be done first thing
    mvars <- c(col.dv,col.pred,col.ipred)
    stopifnot(all(mvars%in%colnames(pklong0)))
    id.vars <- c(col.id,"ROW",col.time,col.ntim,col.grp)
    stopifnot(all(id.vars%in%colnames(pklong0)))
    
    pklong1 <- melt(pklong0,id.vars=id.vars,measure.vars=mvars)

### adding means

    pklong3 <- copy(pklong1)
    
    pklong3[,c("mean.grp","mean.grp.l","mean.grp.u"):=means(value,ci=T,type=type.mean),by=c(col.grp,col.ntim,"variable")]
    
    pklong4 <- melt(pklong3,id.vars=c(col.id,col.grp,"ROW",col.time,col.ntim,"variable","mean.grp.l","mean.grp.u"),measure.vars=c("value","mean.grp"),variable.name="type")


    
    col.id.orig <- paste0(col.id,"orig")
    pklong4[,(col.id.orig):=get(col.id)]
    pklong4[,(col.id):=NULL]
    pklong4[,(col.id):=as.character(get(col.id.orig))]
    pklong4[type%in%c("mean.grp"),(col.id):="mean"]
    pklong4[type%in%c("mean.grp"),(col.id.orig):=NA]
### This needs double check
    pklong4 <- unique(pklong4,by=c(col.id,col.ntim,col.grp,"variable","value"))

    col.idwithin <- tmpcol(pklong4,base="IDwithin")
    pklong4[,(col.idwithin):=as.numeric(as.factor(get(col.id))),by=c(col.grp)]
    pklong4[,(col.idwithin):=as.factor(as.character(get(col.idwithin)))]

    
    
    prepare.geoms <- function(geom,var234,type234){

        if(grepl("l",geom)){
            pklong4[variable==var234&
                    type==type234,val.line:=value]
        }

        if(grepl("p",geom)){
            pklong4[variable==var234&
                    type==type234,val.point:=value]
        }

        if(grepl("c",geom)){
            pklong4[variable==var234&
                    type==type234,val.ci.l:=mean.grp.l]
            pklong4[variable==var234&
                    type==type234,val.ci.u:=mean.grp.u]
        }
        pklong4
    }

    prepare.geoms(geom.dvmean,var234=col.dv,type234="mean.grp")
    prepare.geoms(geom.predmean,var234=col.pred,type234="mean.grp")
    prepare.geoms(geom.ipredmean,var234=col.ipred,type234="mean.grp")
    prepare.geoms(geom.dv,var234=col.dv,type234="value")
    prepare.geoms(geom.pred,var234=col.pred,type234="value")
    prepare.geoms(geom.ipred,var234=col.ipred,type234="value")

    ## generate a lean scale without values that are not used    
    genScaleLevels <- function(col.val,col.var,values){
        ## browser()
        ## all type values
        ## levels(pklong4[,unique(get(col.var))])
        ## the ones to include in the scale
        ## pklong4[!is.na(get(col.val)),unique(get(col(var)))]
        ## the factor orders of these values
        ## this little detour with rows because col.val can be of length>1
        rows <- rowSums(!is.na(pklong4[,col.val,with=F]))>0
        Nval.fac.type <- as.numeric(as.factor(pklong4[rows,unique(get(col.var))]))
        ## the names of these values
        names.levels.type <- as.character(pklong4[rows,unique(get(col.var))])
        ## the number of factor levels
        n.levels.type <- length(levels(pklong4[,unique(get(col.var))]))
        values.fac.type <- rep(NA_integer_,n.levels.type)
        values.fac.type[Nval.fac.type] <- values[Nval.fac.type]

        list(names.levels=names.levels.type,
             values.fac=values.fac.type)
    }

    if("val.line"%in%colnames(pklong4)){
        scalevals.linetype <- genScaleLevels("val.line","variable",values=seq(1,4,1))
    }
    if("val.point"%in%colnames(pklong4)){
        scalevals.shape <- genScaleLevels("val.point","variable",values=seq(1,4,1))
    }

    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    if("mean"%in%pklong4[,get(col.idwithin)]) {
        l.ID <- levels(pklong4[,get(col.idwithin)])
        pklong4[,(col.idwithin):=factor(as.character(get(col.idwithin)),
                                        levels=c("mean",l.ID[l.ID!="mean"])
                                        )]
        colours <- c("#000000",gg_color_hue(length(unique(pklong4[,get(col.idwithin)]))))
    } else {
        colours <- gg_color_hue(length(unique(pklong4[,get(col.idwithin)])))
    }

    cols.levels <- intersect(c("val.line","val.point"),colnames(pklong4))
    scalevals.size <- genScaleLevels(cols.levels,"type",values=seq(1,4,1))
    scalevals.colour <-
        genScaleLevels(cols.levels,col.id,values=colours)

    pklong4[,grp.line.1:=paste(get(col.id),variable,type)]
    p1 <- ggplot(pklong4)
    if("val.line"%in%colnames(pklong4)){

        ## with colour - you easily get too many, so it's not good
        ## p1 <- p1+
        ##     geom_line(aes_string(col.time,y="val.line",colour=col.idwithin,linetype="variable",size="type"))+
        ##     scale_linetype_manual(breaks=scalevals.linetype$names.levels,values=scalevals.linetype$values.fac,name=NULL)
        p1 <- p1+
            geom_line(aes_string(col.time,y="val.line",linetype="variable",size="type",group="grp.line.1"),data=function(x) x[get(col.id)!="mean"])+
            ## geom_line(aes_string(col.ntim,y="val.line",linetype="variable",size="type"),data=function(x)subset(x,ID=="mean"))+
            do.call(geom_line,c(list(mapping=aes_string(col.ntim,y="val.line",linetype="variable",size="type"),data=function(x)subset(x,ID=="mean")),args.plot.mean))+
            
            scale_linetype_manual(breaks=scalevals.linetype$names.levels,values=scalevals.linetype$values.fac,name=NULL)
    }
    if("val.point"%in%colnames(pklong4)){
        ## with colour - you easily get too many, so it's not good
        ## p1 <- p1+
        ##     geom_point(aes_string(col.time,"val.point",colour=col.idwithin,size="type"))+
        ##     scale_shape_manual(breaks=scalevals.shape$names.levels,values=scalevals.shape$values.fac,name=NULL)
        p1 <- p1+
            geom_point(aes_string(col.time,"val.point",size="type"),data=function(x)subset(x,ID!="mean"))+
            ## geom_point(aes_string(col.ntim,"val.point",size="type"),data=function(x)subset(x,ID=="mean"))+
            do.call(geom_point,c(list(mapping=aes_string(col.ntim,y="val.point",size="type"),data=function(x)subset(x,ID=="mean")),args.plot.mean))+
            scale_shape_manual(breaks=scalevals.shape$names.levels,values=scalevals.shape$values.fac,name=NULL)
    }
    if(all(c("val.ci.l","val.ci.u")%in%colnames(pklong4))){
        ## geom_linerange(aes(NOMTIME,ymin=mean.grp.l,ymax=mean.grp.u,linetype=variable),data=function(d)d[variable=="DV"])+
        
        ## p1 <- p1 + geom_errorbar(aes_string(col.ntim,ymin="val.ci.l",ymax="val.ci.u"))
        ##                                           ,linetype="variable"))
        
        p1 <- p1 +
            do.call(geom_errorbar,c(list(mapping=aes_string(x=col.ntim,ymin="val.ci.l",ymax="val.ci.u")),args.plot.mean))
    }
    if(any(c("val.point","val.line")%in%colnames(pklong4))){
        p1 <- p1+
### this should only be in case of points or lines
            scale_size_manual(limits=scalevals.size$names.levels,values=scalevals.size$values.fac,name=NULL)
    }
### if colouring is not used, this must be disabled
    ## p1 <- p1 +
    ##     scale_colour_manual(breaks=scalevals.colour$names.levels,values=scalevals.colour$values.fac)
    
    if(log.y){
        p1 <- p1+scale_y_log10()
    }

    p1
    
}

