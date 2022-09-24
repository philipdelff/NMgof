
##' @import data.table
##' @importFrom NMcalc means

## This should be generalized with arguments for PRED, IPRED and
## DV.

## col.nomtime must be an argument
NMplotFit <- function(dt,models=NULL,type.mean="geometric",by.split,col.grp,col.analyte=NULL,col.nomtime,colour="model",facets="CMT~DOSE"){
    

    dt <- dt[EVID==0]
    if(!is.null(models)){
        dt <- dt[model%in%models]
    }
    
    ##    by.all.1 <- cc(model,devpart,DOSE,dose,NOMTIME,PARENT,METAB,ABD)
    by.all <- c(by.split,col.nomtime,"model",col.grp,col.analyte)
    
    dt <- rbind(transform(dt[model==dt[1,model]],model=NA,PRED=NA,IPRED=NA,type="obs"),
                transform(dt,DV=NA,type="model"),
                fill=T)

    
    dt[,(c("mPRED")):=means(PRED,type=type.mean,ci=FALSE),by=by.all]
    dt[,(c("mIPRED")):=means(IPRED,type=type.mean,ci=FALSE),by=by.all]
    dt[,(c("mDV","mDVll","mDVul")):=means(DV,type=type.mean,ci=TRUE),by=by.all]

    plots <- list()

    if(is.null(by.split)){
        data.split <- list(" "=dt)
    } else {
        data.split <- split(dt,by=by.split)
    }
    plots.preds.time <- lapply(names(data.split),function(x){
        if(nrow(data.split[[x]])==0) return(NULL)
        ggplot(data.split[[x]],aes_string(col.nomtime,"mDV"))+
            geom_point()+
            geom_errorbar(aes(ymin=mDVll,ymax=mDVul),colour="gray")+
            geom_line(aes_string(y="mPRED",colour=colour),data=function(y)y[type=="model"])+
            geom_line(aes_string(y="mIPRED",colour=colour),linetype=2,data=function(y)y[type=="model"])+
            ## facet needs to be based on arguments analyte and by?
            ## facet_grid(CMT~DOSE,scales="free_y")+
            facet_grid(as.formula(facets),scales="free_y")+
            labs(title=x)
    })

    plots.preds.time
}
