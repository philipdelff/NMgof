
##' @import data.table
##' @importFrom NMcalc means
##' @export

## This should be generalized with arguments for PRED, IPRED and
## DV.

## col.nomtime must be an argument
NMplotFit <- function(dt,models=NULL,type.mean="geometric",by.split=NULL,col.grp=NULL,col.nomtime,ci=TRUE,colour="model",colour.dv=FALSE,facets="CMT~DOSE",simplify=TRUE,debug=F){
    if(debug) browser()
    
    dt <- dt[EVID==0]
    if(!is.null(models)){
        dt <- dt[model%in%models]
    }
    
    ##    by.all.1 <- cc(model,devpart,DOSE,dose,NOMTIME,PARENT,METAB,ABD)
    by.all <- unique(c(by.split,col.nomtime,"model",col.grp))
    
    dt <- rbind(transform(dt[model==dt[1,model]],model=NA,PRED=NA,IPRED=NA,type="obs"),
                transform(dt,DV=NA,type="model"),
                fill=T)
    
    
    dt.pred <- dt[,append(list(
        PRED=means(PRED,type=type.mean,ci=FALSE)
       ,IPRED=means(IPRED,type=type.mean,ci=FALSE)
    ),
    setNames(means(DV,type=type.mean,ci=TRUE),c("mDV","mDVll","mDVul"))
    ),by=by.all]
    
    dt.means <- melt(dt.pred,measure.vars=cc(PRED,IPRED),variable.name="type.pred",value.name="value.pred")
    dt.means[,type:="means"]
    dt.all <- rbind(dt,dt.means,fill=T)

    
    if(is.null(by.split)){
        data.split <- list(" "=dt.all)
    } else {
        data.split <- split(dt.all,by=by.split)
    }
    plots.preds.time <- lapply(names(data.split),function(x){
        if(nrow(data.split[[x]])==0) return(NULL)
        p1 <- ggplot(data.split[[x]],aes_string(col.nomtime,"mDV"))+
            geom_point(aes(shape="Observations"))
        if(colour.dv) p1 <- p1+aes_string(colour=colour)
        if(ci){
            if(colour.dv){
                p1 <- p1 +
                    geom_errorbar(aes(ymin=mDVll,ymax=mDVul),data=function(y)y[type=="means"])
            } else {
                p1 <- p1 +
                    geom_errorbar(aes(ymin=mDVll,ymax=mDVul),colour="gray",data=function(y)y[type=="means"])
            }
        }
        
        p1 <- p1+
            ## geom_line(aes_string(y="mPRED",colour=colour),data=function(y)y[type=="model"])+
            ## geom_line(aes_string(y="mIPRED",colour=colour),linetype=2,data=function(y)y[type=="model"])+
            geom_line(aes_string(y="value.pred",colour=colour,linetype="type.pred"),data=function(y)y[type=="means"&!is.na(model)])+
            ## facet needs to be based on arguments analyte and by?
            ## facet_grid(CMT~DOSE,scales="free_y")+
            labs(title=x,colour="",linetype="",shape="",y="") + 
            scale_colour_discrete(na.translate = F)
        p1
        if(!is.null(facets)){
            p1 <- p1 + facet_grid(as.formula(facets),scales="free_y") 
        }
    })
    if(simplify && length(plots.preds.time)==1) {
        plots.preds.time <- plots.preds.time[[1]]
    }

    plots.preds.time
}
