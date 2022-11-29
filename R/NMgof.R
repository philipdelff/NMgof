##' search for models and update or overwrite goodness of fit plots
##'
##' @param dir.models Directory in which to look for models.
##' @param models Paths to specific Nonmem models to analyze. If
##'     specified, dir.models not used.
##' @param fun.find.models A function to apply to filenames found in
##'     dir.models before accepting them as output control streams. If
##'     models not specified, the default is to only look at files
##'     ending in .lst.
##' @param dir.diag
##' @param fun.gof
##' @param update.only Use TRUE to only run on a model if the model
##'     was run since last time GOF plots were generated. You should
##'     use update.only if you have changed the GOF plot function and
##'     want to rerun for all models. You probably don´t want to use
##'     update.only in combination with secs.rep because then you are
##'     asking for a loop of forced updates.
##' @param secs.rep
##' @param hours.run How many hours would you like to keep looking for
##'     updates every secs.rep seconds? Default is 24 hours if
##'     secs.rep is provided.
##' @param models file names to look for in dir.models. Notice, just
##'     file names, not with directories. Use either models or
##'     fun.find.models
##' @param wipe.existing Not implemented yet. Intented to mean that
##'     everything existing in the dir will be deleted if NMgof runs
##'     succesfully.
##' @param ... Arguments sent to fun.gof
##' @import data.table
##' @export

NMgof <- function(dir.models,dir.diag,models,fun.find.models,fun.repair.data=NULL,fun.gof,update.only=TRUE,secs.rep,hours.run,script=NULL){

    if(missing(fun.find.models)) fun.find.models <- NULL 
    if(missing(models)) models <- NULL 
    stopifnot(is.null(fun.find.models)||is.null(models))
    if(is.null(models) && is.null(fun.find.models)){
        fun.find.models <- function(dir)list.files(dir,pattern=".*\\.lst",full.names=TRUE)
    }

    if(missing(secs.rep)) secs.rep <- NULL
    if(!is.null(secs.rep) && missing(hours.run)){
        hours.run <- 24
    } else if (missing(hours.run)) {
        hours.run <- 0
    }
    if(is.null(secs.rep)) secs.rep <- 0
    time.now <- 0
    time.stop <- Sys.time()+hours.run * 3600

    fun.file.rds <- function(dir.diag,model) file.path(dir.diag,paste0(model,"/NMgof_",model,"_gof_runtime.rds"))

    fun.run <- function(nmod){
        
        model <- dt.mods[nmod,model]
        ## cat(sprintf("\n----------- model %s ---------------\n",model))
        
        path.lst <- dt.mods[nmod,path.lst]

        ## dt.run <- dtall[model==run]

### read and prepare data start
        this.file.data <- fnExtension(fnAppend(path.lst,"input"),".rds")
        if(!file.exists(this.file.data)) this.file.data <- "extract"
        
        ## dt.run <- NMscanData(run,file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
        dt.run <- NMscanData(path.lst,file.data=this.file.data)
        meta <- NMdata:::NMinfoDT(dt.run)
        details <- meta$details

        if(!is.null(fun.repair.data)) dt.run <- fun.repair.data(dt.run)
        NMdata:::writeNMinfo(dt.run,meta=meta)
        
### read and prepare data end

        dir.diag.nmod <- file.path(dir.diag,dt.mods[nmod,run])
        if(!dir.exists(dir.diag.nmod)) dir.create(dir.diag.nmod)

        ## make sure fun.gof is a list
        for(nfun in seq_along(fun.gof)){
            
            ## plots.run <- try(fun.gof[[nfun]]$fun(dt=dt.run,fun.gof[[nfun]]$args))
            plots.run <- try(
                do.call(fun.gof[[nfun]]$fun,c(fun.gof[[nfun]]$args,list(dt=dt.run)))
                             )
            
### save plots
            if(!"try-error"%in%class(plots.run)){
                names.plots <- names(plots.run)
                
                silent <- lapply(names.plots,function(x){
                    file.out <- file.path(dir.diag.nmod,paste0(x,"_",model,".pdf"))
                    message(paste("Writing",file.out))
                    ## res <- try(ggwrite(plots.run[[x]],file=file.path(dir.diag.nmod,paste0(x,"_",model,".pdf")),onefile=TRUE,canvas="wide-screen",script=script,save=TRUE,show=F))

                    res <- try(writer(plots.run[[x]],file=file.out,save=T,show=F,formats="png",script=script))
                    if("try-error"%in%class(res)) try(dev.off())
                    res
                })

### save plots done
            }
        }
        
        info.save <- list(model=model
                         ,time=Sys.time()
                          )
        
        saveRDS(info.save,file=dt.mods[nmod,path.info])
        
        
    }

    while(time.now<time.stop){
        
        dt.mods <- data.table(
            path.lst = if(is.null(models)){
                           fun.find.models(dir.models)
                       } else {
                           models
                       }
        )
        
        
        dt.mods[,mtime:=file.info(path.lst)$mtime ][
           ,model:=sub("run","",basename(path.lst)) ][
           ,model:=sub("\\..+","",model) ][
           ,run:=fnExtension(basename(path.lst),"") ][
           ,path.info:=fun.file.rds(dir.diag,run) ][
           ,gofs.exist:=file.exists(path.info) ][
           ,ROW:=1:.N ][
            ## ,time.gof:=as.POSIXct("1900-01-01 00:00:00")
           ,time.gof:=Sys.time()
        ]

        
        if(any(dt.mods[,gofs.exist])){
            dt.mods[gofs.exist==TRUE,time.gof:=readRDS(path.info)$time,by=.(ROW)]
        }

        
        if(update.only){
            dt.mods <- dt.mods[!gofs.exist|mtime>time.gof]
        }

        if(nrow(dt.mods)==0) {
            ##message("No models to process. Exiting.")
            ## return(invisible(NULL))
        } else {
            setorder(dt.mods,-mtime)
            print(dt.mods)
            Nmods <- dt.mods[,.N]

            for(nmod in 1:Nmods){
                model <- dt.mods[nmod,model]
                cat(sprintf("\n----------- model %s (%d/%d) ---------------\n",model,nmod,Nmods))

                tmp <- fun.run(nmod)
                ## tmp <- fun.run(nmod)
            }
        }
        time.now <- Sys.time()
        if((time.now+secs.rep)<time.stop) {
            Sys.sleep(secs.rep)
        }
    }

    return(invisible(NULL))
}



NMgofSingle <- function(dir.models,dir.diag,models,fun.find.models,fun.repair.data=NULL,fun.gof,update.only=TRUE,secs.rep,hours.run,script=NULL,...){

    if(missing(fun.find.models)) fun.find.models <- NULL 
    if(missing(models)) models <- NULL 
    stopifnot(is.null(fun.find.models)||is.null(models))
    if(is.null(models) && is.null(fun.find.models)){
        fun.find.models <- function(dir)list.files(dir,pattern=".*\\.lst",full.names=TRUE)
    }

    if(missing(secs.rep)) secs.rep <- NULL
    if(!is.null(secs.rep) && missing(hours.run)){
        hours.run <- 24
    } else if (missing(hours.run)) {
        hours.run <- 0
    }
    if(is.null(secs.rep)) secs.rep <- 0
    time.now <- 0
    time.stop <- Sys.time()+hours.run * 3600

    fun.file.rds <- function(dir.diag,model) file.path(dir.diag,paste0(model,"/NMgof_",model,"_gof_runtime.rds"))

    fun.run <- function(nmod){
        
        model <- dt.mods[nmod,model]
        ## cat(sprintf("\n----------- model %s ---------------\n",model))
        
        path.lst <- dt.mods[nmod,path.lst]

        ## dt.run <- dtall[model==run]

### read and prepare data start
        this.file.data <- fnExtension(fnAppend(path.lst,"input"),".rds")
        if(!file.exists(this.file.data)) this.file.data <- "extract"
        
        ## dt.run <- NMscanData(run,file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
        dt.run <- NMscanData(path.lst,file.data=this.file.data)
        details <- NMdata:::NMinfoDT(dt.run,"details")

        if(!is.null(fun.repair.data)) dt.run <- fun.repair.data(dt.run)
        
### read and prepare data end

        dir.diag.nmod <- file.path(dir.diag,dt.mods[nmod,run])
        if(!dir.exists(dir.diag.nmod)) dir.create(dir.diag.nmod)

        ## make sure fun.gof is a list
        if(!is.list(fun.gof)) fun.gof=list(fun.gof)        
        for(nfun in seq_along(fun.gof)){
            
            plots.run <- try(fun.gof[[nfun]](dt=dt.run,dir.diag=dir.diag,...))
            
### save plots
            if(!"try-error"%in%class(plots.run)){
                names.plots <- names(plots.run)
                
                silent <- lapply(names.plots,function(x){
                    res <- try(ggwrite(plots.run[[x]],file=file.path(dir.diag.nmod,paste0(x,"_",model,".pdf")),onefile=TRUE,canvas="wide-screen",script=script,save=TRUE,show=F))
                    if("try-error"%in%class(res)) dev.off()
                    res
                })

### save plots done
            }
        }
        
        info.save <- list(model=model
                         ,time=Sys.time()
                          )
        
        saveRDS(info.save,file=dt.mods[nmod,path.info])
        
        
    }

    while(time.now<time.stop){
        
        dt.mods <- data.table(
            path.lst = if(is.null(models)){
                           fun.find.models(dir.models)
                       } else {
                           models
                       }
        )
        
        
        dt.mods[,mtime:=file.info(path.lst)$mtime ][
           ,model:=sub("run","",basename(path.lst)) ][
           ,model:=sub("\\..+","",model) ][
           ,run:=fnExtension(basename(path.lst),"") ][
           ,path.info:=fun.file.rds(dir.diag,run) ][
           ,gofs.exist:=file.exists(path.info) ][
           ,ROW:=1:.N ][
            ## ,time.gof:=as.POSIXct("1900-01-01 00:00:00")
           ,time.gof:=Sys.time()
        ]

        
        if(any(dt.mods[,gofs.exist])){
            dt.mods[gofs.exist==TRUE,time.gof:=readRDS(path.info)$time,by=.(ROW)]
        }

        
        if(update.only){
            dt.mods <- dt.mods[!gofs.exist|mtime>time.gof]
        }

        if(nrow(dt.mods)==0) {
            ##message("No models to process. Exiting.")
            ## return(invisible(NULL))
        } else {
            setorder(dt.mods,-mtime)
            print(dt.mods)
            Nmods <- dt.mods[,.N]

            for(nmod in 1:Nmods){
                model <- dt.mods[nmod,model]
                cat(sprintf("\n----------- model %s (%d/%d) ---------------\n",model,nmod,Nmods))

                tmp <- fun.run(nmod)
                ## tmp <- fun.run(nmod)
            }
        }
        time.now <- Sys.time()
        if((time.now+secs.rep)<time.stop) {
            Sys.sleep(secs.rep)
        }
    }

    return(invisible(NULL))
}
