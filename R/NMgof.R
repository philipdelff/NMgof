##' search for models and update or overwrite goodness of fit plots
##'
##' @param dir.models Directory in which to look for models.
##' @param models Paths to specific Nonmem models to analyze. If
##'     specified, dir.models not used.
##' @param fun.find.models A function to apply to filenames found in
##'     dir.models before accepting them as output control streams. If
##'     models not specified, the default is to only look at files
##'     ending in .lst.
##' @param dir.diag The directory to write output to.
##' @param fun.gof A list of functions to be run. Instead of just a
##'     function, an element in the list can be a new list including
##'     \code{fun} (the function) and \code{args} (arguments with
##'     values to be passed to fun).
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
##' @param script If not NULL, outputs will be stamped with
##'     tracee::createStamp. `script` is intended to include a path to
##'     the script from which the code is run.
##' @param time.stamp Passed to tracee::writeFlextab and
##'     tracee::ggwrite as the time argument. This is further passed
##'     to tracee::createStamp. It will be the first string in the
##'     sectond line of the stamps. This string will be followed by
##'     the output file name. time.stamp="model" has a special meaning
##'     where the relative path (from working directory) to the output
##'     control stream of the model being processed. In this case,
##'     there will be no timestamp in stamps. The time stamp will also
##'     be suppressed with time.stamp="".
##' @param canvas Passed to ggwrite for plots.
##' @import data.table
##' @import NMdata
##' @export

NMgof <- function(dir.models,dir.diag,models,fun.find.models,fun.repair.data=NULL,fun.gof,update.only=TRUE,secs.rep,hours.run,formats.ft="png",script=NULL,time.stamp=NULL,canvas="standard",args.NMscanData=NULL){

    
    
    if(missing(fun.find.models)) fun.find.models <- NULL 
    if(missing(models)) models <- NULL 
    stopifnot(is.null(fun.find.models)||is.null(models))
    if(is.null(models) && is.null(fun.find.models)){
        fun.find.models <- function(dir)list.files(dir,pattern=".*\\.lst",full.names=TRUE)
    }

    if(!is.list(fun.gof)) {
        stop("fun.gof must be a list. Either containing a function and args or a list of combination of such.")
    }
    ## if only one gof function is given, it may be a list of just fun and args
    if(length(fun.gof)==2 && all(cc(fun,args)%in%names(fun.gof))){
        fun.gof <- list(fun.gof)
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
    list.args.all <- as.list(environment())
    
    fun.file.rds <- function(dir.diag,model) file.path(dir.diag,paste0(model,"/NMgof_",model,"_gof_runtime.rds"))

    fun.run <- function(nmod){
        
        
        model <- dt.mods[nmod,model]
        ## cat(sprintf("\n----------- Model %s ---------------\n",model))
        
        path.lst <- dt.mods[nmod,path.lst]

### time.stamp=="model" has a special meaning.
        if(!is.null(time.stamp)&&time.stamp=="model") time.stamp <- path.lst

### read and prepare data start
        this.file.data <- fnExtension(fnAppend(path.lst,"input"),".rds")
        if(!file.exists(this.file.data)) this.file.data <- "extract"
        
        ## dt.run <- NMscanData(run,file.data=function(x)fnExtension(fnAppend(x,"input"),".rds"))
### introduce args.NMscanData as an argument to NMgof.
        all.args <- c(list(path.lst,file.data=this.file.data),args.NMscanData)
        
        dt.run <- try(do.call(NMscanData,all.args))
        ## dt.run <- try(NMscanData(path.lst,file.data=this.file.data))
        
        if("try-error"%in%class(dt.run)) {
            message("Reading of model results data failed. Skipping this model.")
            return(NULL)
        }

        if(!is.null(fun.repair.data)) {
            meta <- NMdata:::NMinfoDT(dt.run)
            details <- meta$details

            dt.run.try <- try(fun.repair.data(dt.run))
            
            if("try-error"%in%class(dt.run.try)) {
                message("Data repair function failed. Not applied.")
            } else {
                dt.run <- dt.run.try
            }
            NMdata:::writeNMinfo(dt.run,meta=meta)
        }        
### read and prepare data end

        dir.diag.nmod <- file.path(dir.diag,dt.mods[nmod,run])
        if(!dir.exists(dir.diag.nmod)) dir.create(dir.diag.nmod)

        ## make sure fun.gof is a list
        for(nfun in seq_along(fun.gof)){
            
            ## plots.run <- try(fun.gof[[nfun]]$fun(dt=dt.run,fun.gof[[nfun]]$args))
            subset.run <- fun.gof[[nfun]]$subset
            if(is.null(subset.run)) subset.run <- "TRUE"
            
            plots.run <- try(
                do.call(
                    fun.gof[[nfun]]$fun
                   ,
                    c(fun.gof[[nfun]]$args,list(dt=dt.run[eval(parse(text=subset.run))]))
                )
            )
            
### save plots
            if(!"try-error"%in%class(plots.run)){
                names.plots <- names(plots.run)
                
                silent <- lapply(names.plots,function(x){
                    file.out <- file.path(dir.diag.nmod,paste0(x,"_",model,".pdf"))
                    message(paste("Writing",file.out))
                    ## res <- try(ggwrite(plots.run[[x]],file=file.path(dir.diag.nmod,paste0(x,"_",model,".pdf")),onefile=TRUE,canvas="wide-screen",script=script,save=TRUE,show=F))
                    
                    res <- try(
                        writer(plots.run[[x]],file=file.out,save=T,show=F,formats.ft=formats.ft,script=script,time=time.stamp,canvas=canvas)
                    )
                    if("try-error"%in%class(res)) try(dev.off())
                    res
                })

### save plots done
            }
        }
        
        info.save <- list(model=model
                         ,time=Sys.time()
                          ## ,hash=digest(list.args.all)
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


