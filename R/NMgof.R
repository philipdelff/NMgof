##' search for models and update or overwrite goodness of fit plots
##'
##' @param dir.models
##' @param dir.diag
##' @param models
##' @param fun.find.models
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
##' @param wipe.existing Not implemented yet
##' @param ... Arguments sent to fun.gof
##' @import data.table
##' @export


NMgof <- function(dir.models,dir.diag,models,fun.find.models,fun.repair.data=NULL,fun.gof,update.only=TRUE,secs.rep,hours.run,script=NULL,...){

    if(missing(fun.find.models)) fun.find.models <- NULL 
    if(missing(models)) models <- NULL 
    stopifnot(is.null(fun.find.models)||is.null(models))
    if(is.null(models) && is.null(fun.find.models)){
        fun.find.models <- function(dir)list.files(dir,pattern=".*\\.lst",full.names=TRUE)
    }
    if(!missing(secs.rep) && missing(hours.run)){
        hours.run <- 24
    } else if (missing(hours.run)) {
        hours.run <- 0
    }
    time.now <- 0
    time.stop <- Sys.time()+hours.run * 3600

    

    fun.file.rds <- function(dir.diag,run) file.path(dir.diag,paste0(run,"/NMgof_",run,"_gof_runtime.rds"))

    fun.run <- function(nmod){
        model <- dt.mods[nmod,model]
        cat(sprintf("\n----------- model %s ---------------\n",model))
        
        path.lst <- dt.mods[nmod,path.lst]

        ## dt.run <- dtall[model==run]

### read and prepare data start
        this.file.data <- fnExtension(fnAppend(path.lst,"input"),".rds")
        if(!file.exists(this.file.data)) this.file.data <- "extract"
        
        ## dt.run <- NMscanData(run,file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
        dt.run <- NMscanData(path.lst,file.data=this.file.data)
        details <- NMinfo(dt.run,"details")

        if(!is.null(fun.repair.data)) dt.run <- fun.repair.data(dt.run)
        
### read and prepare data end
        
        plots.run <- try(fun.gof(dt=dt.run,dir.diag=dir.diag,...))

### save plots
        if(!"try-error"%in%class(plots.run)){
            names.plots <- names(plots.run)
            
            dir.diag.nmod <- file.path(dir.diag,model)
            if(!dir.exists(dir.diag.nmod)) dir.create(dir.diag.nmod)
            
            
            silent <- lapply(names.plots,function(x)
                ggwrite(plots.run[[x]],file=file.path(dir.diag.nmod,paste0(x,"_",model,".pdf")),onefile=TRUE,canvas="wide-screen",script=script,save=TRUE,show=F)
                )
            
### save plots done

            info.save <- list(model=model
                             ,time=Sys.time()
                              )
            
            saveRDS(info.save,file=dt.mods[nmod,path.info])
            
        }
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
           ,time.gof:=as.POSIXct("1900-01-01 00:00:00")
        ]

        if(any(dt.mods[,gofs.exist])){
            dt.mods[gofs.exist==TRUE,time.gof:=readRDS(path.info)$time,by=.(ROW)]
        }

        if(update.only){
            dt.mods <- dt.mods[!gofs.exist|mtime>time.gof]
        }

        if(nrow(dt.mods)==0) {
            message("No models to process. Exiting.")
            return(invisible(NULL))
        }
        setorder(dt.mods,-mtime)
        print(dt.mods)
        
        for(nmod in 1:dt.mods[,.N]){
            tmp <- try(fun.run(nmod))
            ## tmp <- fun.run(nmod)
        }
        time.now <- Sys.time()
    }

    return(invisible(NULL))
}
