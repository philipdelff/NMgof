#### diagonal comparisons. Log is missing. Generalize splitting
#### (compound) and facetting (devpart+pop), add log and put into
#### function.


NMplotDiags <- function(data,by.split=NULL,facet=NULL){
    plots.diag <- list()
    model <- paste(unique(data[,model],collapse="+"))

    data <- data[EVID==0]
    if(is.null(by.split)){
        data.split <- list(" "=data)
    } else {
        data.split <- split(data,by=by.split)
    }

    names.data.split <- names(data.split)
    plots.diag <- lapply(names.data.split,function(name.data){
        dt1 <- data.split[[name.data]]
        res <- list()
        res$pred.dv <- ggplot(dt1,aes(DV,PRED,colour=dose))+
            geom_point()+
            geom_abline(slope=1,intercept = 0,colour="blue",linetype="dashed")+
            geom_smooth(method="loess",se=FALSE,colour="red")+
            labs(x="Observations",y="Population predictions",title=paste(name.data))
        if(!is.null(facet)){
            res$pred.dv <- res$pred.dv +
                facet_wrap(as.formula(paste0("~",paste(facet,collapse="+"))),scales="free")
        }
        
        res$ipred.dv <- res$pred.dv + aes(DV,IPRED) +
            labs(x="Observations",y="Individual predictions",title=paste(name.data))
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
