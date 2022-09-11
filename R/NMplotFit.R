
##' @import data.table
##' @importFrom NMcalc means

## This should be generalized with arguments for PRED, IPRED and
## DV.

## col.nomtime must be an argument
NMplotFit <- function(dt,models,type.mean="geometric",by.all,by.split){
    dt <- dt[EVID==0]
    dt <- dt[model%in%models]

    
    
    dt <- rbind(transform(dt[model==dt[1,model]],model=NA,PRED=NA,IPRED=NA,type="obs"),
                transform(dt,DV=NA,type="model"),
                fill=T)

    
    dt[,(c("mPRED")):=means(PRED,type=type.mean,ci=FALSE),by=by.all]
    dt[,(c("mIPRED")):=means(IPRED,type=type.mean,ci=FALSE),by=by.all]
    dt[,(c("mDV","mDVll","mDVul")):=means(DV,type=type.mean,ci=TRUE),by=by.all]

    plots <- list()

    
    data.split <- split(dt,by=by.split)
    plots.preds.time <- lapply(names(data.split),function(x){
        if(nrow(data.split[[x]])==0) return(NULL)
        ggplot(data.split[[x]],aes(NOMTIME,mDV))+
            geom_point()+
            geom_errorbar(aes(NOMTIME,ymin=mDVll,ymax=mDVul),colour="gray")+
            geom_line(aes(NOMTIME,mPRED,colour=model),data=function(y)y[type=="model"])+
            geom_line(aes(NOMTIME,mIPRED,colour=model),linetype=2,data=function(y)y[type=="model"])+
## facet needs to be based on arguments analyte and by?
            facet_grid(CMT~DOSE,scales="free_y")+
            labs(title=x)
    })

    plots.preds.time
}
