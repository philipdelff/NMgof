gofRun <- function(run,by,arm,analyte,names.etas,dir.diag,tspd="TAD",script=NULL){

    
    ## dt.run <- dtall[model==run]
    dt.run <- NMscanData(run,file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
    details <- list(model=NMinfo(dt.run,"details")$model)

    if(is.null(dt.run$devpart)) repair.data(dt.run)

    dt.run[,byarm:=paste(get(by),get(arm))]


    plots <- list()

    
    ## individual plots
    dt.run.expanded <- NMexpandDoses(dt.run)
    plots$ind <- ggIndProfs(dt.run.expanded,par.prof=analyte,amt="AMT",id="subjid",grp="byarm",NPerSheet=15,quiet=TRUE)

    ## resids vs time and preds
    data.split <- split(dt.run[EVID==0],by=c(by,analyte))
    plots$gof <- lapply(names(data.split),function(x){
        NMplotGOF(data.split[[x]],colour=arm,tspd="TAD",title=paste(details$model,x))
    })

    plots.gof <- lapply(names(data.split),function(x){
        NMplotGOF(data.split[[x]],colour=arm,tspd="TAD",arrange=FALSE,title=paste(details$model,x))
    })
    plots$cwres_panel <- lapply(plots.gof,function(x) with(x, cwres.pred+cwres.time+cwres.tspd+ cwres.tspd+xlim(c(0,48))+plot_layout(guides="collect")))

    
#### diagonal comparisons. Log is missing. Generalize splitting
#### (compound) and facetting (devpart+pop), add log and put into
#### function.
    plots.diag <- list()
    ## pred vs obs and ipred vs obs split by devpart and dose
    plots.diag$pred.dv.parent <- ggplot(dt.run[EVID==0&PARENT==1],aes(DV,PRED,colour=dose))+
        geom_abline(slope=1,intercept = 0)+
        geom_point()+
        facet_wrap(~devpart+pop,scales="free")+
        labs(x="Observations",y="Population predictions",title=paste(details$model, "VX-548"))

    
    plots.diag$ipred.dv.parent <- plots.diag$pred.dv.parent + aes(DV,IPRED) +
        labs(x="Observations",y="Individual predictions",title=paste(details$model, "VX-548"))

    plots.diag$pred.dv.metab <- plots.diag$pred.dv.parent %+% dt.run[EVID==0&PARENT==0]+
        labs(title=paste(details$model, "M6-548"))
    plots.diag$ipred.dv.metab <- plots.diag$ipred.dv.parent %+% dt.run[EVID==0&PARENT==0]+
        labs(title=paste(details$model, "M6-548"))
    plots$diags_pred_dv <- plots.diag
    
    
    ## ETAS
### pop: abd, bun, healthy
    plots$bsv <- NMplotBSV(data=dt.run,regex.eta="^ET",covs.num=cc(AGE,DOSE,WT)
                          ,covs.char=cc(devpart,formul,sex,pop,GISURGE1,GISURGE2)
                          ,save=F,show=F,title=details$model,names.eta=names.eta
                          ,return.data=TRUE)

### BSV against dose, by devpart
    data.split <- split(dt.run,by=by)
    plots$bsv_dose_bydevpart <- lapply(names(data.split),function(name){
        dt <- data.split[[name]]
        NMplotBSV(dt,regex.eta="^ET",names.eta=names.eta,
                  covs.char="dose",save=F,show=F,
                  title=paste(unique(dt[,model]),name))$iiv.covc$dose
    })
    
    plots$bsv$etas <- NULL
    plots$bsv$etas.l <- NULL

    
    
    plots.pairs <- compareModels(dt.run,models=details$model,tspd=tspd,dir.diag=dir.diag,save=FALSE,show=FALSE)
    plots <- c(plots,plots.pairs$plots)

    plots.run <- list(details=details,plots=plots)



    return(plots.run)    
}


## either dt and models or runs
compareModels <- function(dt=NULL,models=NULL,runs=NULL,tspd=tspd,dir.diag,save=TRUE,show=FALSE){
    
    if(!is.null(runs)){
        dt <- NMscanMultiple(files=runs,file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
        models <- unique(dt[,model])
    }
    
    
    by.all <- cc(model,STUDY,devpart,DOSE,dose,NOMTIME,PARENT,METAB,ABD)
    by.split <- cc(devpart,pop)

    details <- list(models=models)
    
    dt <- dt[EVID==0]
    dt <- dt[model%in%models]

    
    ## dt <- rbind(transform(dt[model==dt[1,model]],model=NA,PRED=NA,IPRED=NA,type="obs"),
    ##             transform(dt,DV=NA,type="model"),
    ##             fill=T)

    plots <- list()
    plots$preds_time_geomean <- NMplotPredTime(dt=dt,models=models,type.mean="geometric",by.all=by.all,by.split=by.split)
    plots$preds_time_median <- NMplotPredTime(dt=dt,models=models,type.mean="median",by.all=by.all,by.split=by.split)
    plots$preds_time_24_geomean <- NMplotPredTime(dt=dt[NOMTIME<=24],models=models,type.mean="geometric",by.all=by.all,by.split=by.split)
    plots$preds_time_24_median <- NMplotPredTime(dt=dt[NOMTIME<=24],models=models,type.mean="median",by.all=by.all,by.split=by.split)

    
    
    
    ## induction models - predictions against each other
    data.cast <- dcast(dt,REC+dose+compound+devpart+pop~model,value.var="PRED")

    dt[,type:="model"]
    cnames <- unique(dt[type=="model",model])
    if(length(cnames)>1){
        data.split <- split(data.cast,by=by.split)
        names.data.split <- names(data.split)
        plots$pairs.PRED <- lapply(names.data.split,function(x)ggpairs(data.split[[x]],columns=cnames,title=x))

        data.cast <- dcast(dt[MAD==1],REC+dose+compound+devpart+pop~model,value.var="IPRED")
        data.split <- split(data.cast,by=by.split)
        names.data.split <- names(data.split)
        plots$pairs.IPRED <- lapply(names.data.split,function(x)ggpairs(data.split[[x]],columns=cnames))
    }


### CWRES vs time
    plots$cwres_time <- list()
    plots$cwres_time$parent <- ggplot(dt[type=="model"&PARENT==1],aes(TIME,CWRES,colour=model))+
        geom_smooth(se=FALSE)+
        facet_grid(compound~devpart+pop+dose,scales="free_x")+
        labs(x="Time (hours)",title=paste(details$model,"VX-548"))

    plots$cwres_time$metab <- plots$cwres_time$parent %+% dt[type=="model"&PARENT==0]+
        labs(x="Time (hours)",title=paste(details$model,"M6-548"))

    dt[,(c("mAUTO")):=means(AUTO,type="median",ci=FALSE),by=by.all]
    dt[,(c("mCLPEFF")):=means(AUTO*CLP,type="median",ci=FALSE),by=by.all]

#### AUTO vs time
    plots$auto_time <- ggplot(dt[type=="model"&PARENT==1],aes(NOMTIME,mAUTO,colour=model))+
        geom_line()+
        facet_grid(.~devpart+pop+dose,scales="free_x")

#### CL vs time
    plots$cl_time <- ggplot(dt[type=="model"&PARENT==1],aes(NOMTIME,mCLPEFF,colour=model))+
        geom_line()+
        facet_grid(.~devpart+pop+dose,scales="free_x")


    plots.run <- list(details=details,plots=plots)

    
    names.plots <- names(plots.run$plots)
    ## if(length(plots.run$details$models)>1){
    string.models <- paste(plots.run$details$models,collapse="_")
    dir.diag <- file.path(dir.diag,string.models)
    ## } else {
    ## }
    if(!dir.exists(dir.diag)) dir.create(dir.diag)

    if(show||save){    
        silent <- lapply(names.plots,function(x)
            ggwrite(plots.run$plots[[x]],file=file.path(dir.diag,paste0(x,"_",string.models,".pdf")),onefile=TRUE,canvas="wide-screen",save=save,show=show)
            )
    }
    
    return(plots.run)
}


