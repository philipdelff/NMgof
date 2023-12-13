##' @param data The data set to plot. Must include a EVID column - will pick EVID==0

diagsExposure <- function(data,by=NULL,arm=NULL,analyte=NULL,col.id="ID"){

    
    outputs <- list()

    dt.obs <- as.data.table(data)[EVID==0]
    dt.sum.dv <- dt.obs[,.(AUC=trapez(TIME,DV),CMAX=max(DV),type.meas="DV"),by=c(col.id,by,arm,analyte)]
    dt.sum.ipred <- dt.obs[,.(AUC=trapez(TIME,IPRED),CMAX=max(IPRED),type.meas="IPRED"),by=c(col.id,by,arm,analyte)]
    dt.sum.pred <- dt.obs[,.(AUC=trapez(TIME,PRED),CMAX=max(PRED),type.meas="PRED"),by=c(col.id,by,arm,analyte)]

    dt.sum <- rbind(dt.sum.dv,dt.sum.ipred,dt.sum.pred)
    dt.sum.l <- melt(dt.sum,measure.vars=cc(AUC,CMAX),variable.name="parameter")
    dt.sum.lw <- dcast(dt.sum.l,paste0(paste(c(col.id,by,arm,analyte,"parameter"),collapse="+"),"~type.meas"),value.var="value")
    dt.sum.lwl <- melt(dt.sum.lw,measure.vars=cc(IPRED,PRED))
    

    dt.sum.lwlw <- dcast(dt.sum.lwl,
        paste0(paste(c(col.id,by,arm,analyte,"parameter","DV"),collapse="+"),"~variable")
    )
    ## use NMplotDiags
    outputs$diagsExposure <- NMplotDiags(dt.sum.lwlw,by.split=c("parameter",analyte),facet=by,colour=arm)
    
    outputs
}
