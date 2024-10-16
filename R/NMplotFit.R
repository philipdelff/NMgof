##' @param cols.pred Columns which means will be plotted with lines. Name the elements to change legend texts (Say c("Individual predictions"="IPRED")).
##' @import data.table
##' @importFrom NMcalc means
##' @export

## This should be generalized with argument for DV.

## col.nomtime must be an argument
NMplotFit <- function(dt,models=NULL,type.mean="geometric",by.split=NULL,col.grp=NULL,col.nomtime,ci=TRUE,colour="model",colour.dv=FALSE,facets="CMT~DOSE",simplify=TRUE,cols.pred=c("Population predictions"="PRED","Individual predictions"="IPRED"),debug=F){
    if(debug) browser()

    if(!is.data.table(dt)) dt <- as.data.table(dt)
    if("EVID" %in% colnames(dt)){
        dt <- dt[EVID==0]
    }
    
    if(!is.null(models)){
        dt <- dt[model%in%models]
    }
    
    ##    by.all.1 <- cc(model,devpart,DOSE,dose,NOMTIME,PARENT,METAB,ABD)
    by.all <- unique(c(by.split,col.nomtime,"model",col.grp))

    dtmod <- dt[model==dt[1,model]]
    dtmod[,(c("model",cols.pred)):=NA][
       ,type:="obs"]

    dt <- rbind(dtmod,
                transform(dt,DV=NA,type="model"),
                fill=T)
    
    
    dt.pred <- dt[,append(lapply(.SD,means,type=type.mean,ci=FALSE),
                          setNames(means(DV,type=type.mean,ci=TRUE),c("mDV","mDVll","mDVul"))
                          )
                         ,.SDcols=cols.pred
                 ,by=by.all]
    

    dt.means <- melt(dt.pred,measure.vars=cols.pred,variable.name="type.pred",value.name="value.pred")
    dt.means[,type:="means"]
    if(!is.null(names(cols.pred))){
        
        setnames(dt.means,"type.pred","type.pred.raw")
        dt.means <- mergeCheck(dt.means,data.table(type.pred.raw=cols.pred,type.pred=names(cols.pred)),by="type.pred.raw",quiet=TRUE)
    }
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
