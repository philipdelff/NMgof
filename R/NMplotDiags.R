##' Plots of obs vs pop predictions and obs vs individual predictions
##'
##' @param data A data set with observations and model predictions
##' @details If EVID is amoung data columns, only EVID==0 will be
##'     considered.
##' @import data.table
##' @export

#### diagonal comparisons. Log is missing. Generalize splitting
#### (compound) and facetting (devpart+pop), add log and put into
#### function.


NMplotDiags <- function(data,by.split=NULL,facet=NULL,colour=NULL){
    
    if("EVID"%in%colnames(data)){
        data <- data[EVID==0]
    }

    if(is.null(by.split)){
        data.split <- list(" "=data)
    } else {
        if(!by.split%in%colnames(data)) stop("If used, by.split must refer to a column in ouput or input data.")
        data.split <- split(data,by=by.split,drop=TRUE) 
    }

    
    names.data.split <- names(data.split)
    plots.diag <- lapply(names.data.split,function(name.data){
        dt1 <- data.split[[name.data]]

        ## xymin <- min(dt1[,.(DV,PRED,IPRED)])
        xymin <- 0
        xymax <- 1.1*max(dt1[,.(DV,PRED,IPRED)])
        
        res <- list()
        res$pred.dv <- ggplot(dt1,aes_string("PRED","DV",colour=colour))+
            geom_point()+
            geom_abline(slope=1,intercept = 0,colour="blue",linetype="dashed")+
            geom_smooth(method="loess",formula=y~x,se=FALSE,colour="red",size=1)+
            labs(x="Population predictions",y="Observations",title=paste(name.data))+
            ## coord_cartesian(xlim=c(xymin, xymax), 
            ##    ylim=c(xymin, xymax))## +
            ## coord_fixed(ratio = 1)
            theme(aspect.ratio=1)
        if(!is.null(facet)){
            res$pred.dv <- res$pred.dv +
                ## facet_wrap(as.formula(paste0("~",paste(facet,collapse="+"))),scales="free")
                facet_wrap(facet,scales="free")
        }
        
        res$ipred.dv <- res$pred.dv + aes(IPRED,DV) +
            labs(x="Individual predictions",y="Observations",title=paste(name.data))
        res
    })
    if(is.null(by.split)) {
        plots.diag <- plots.diag[[1]]
    } else {
        names(plots.diag) <- names.data.split
        plots.diag <- do.call(c,plots.diag)
    }
    
    plots.diag    
}
