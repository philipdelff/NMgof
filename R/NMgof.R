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


NMgof <- function(dir.models,dir.diag,models,fun.find.models,fun.gof,update.only=TRUE,secs.rep,hours.run,...){

    if(missing(fun.find.models)) fun.find.models <- NULL 
    if(missing(models)) models <- NULL 
    stopifnot(xor(is.null(fun.find.models),is.null(models)))
    if(!missing(secs.rep) && missing(hours.run)) hours.run <- 24
    time.now <- 0
    time.stop <- Sys.time()+hours.run * 3600
    

    fun.file.rds <- function(dir.diag,run) file.path(dir.diag,paste0(run,"/NMgof_",run,"_gof_runtime.rds"))

    while(time.now<time.stop){
        dt.mods <- data.table(
            path.lst = ifelse(is.null(models),
                              fun.find.models(dir.models)
                              models
                              )
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

### coment for FORCE creating all outputs ####
        if(update.only){
            dt.mods <- dt.mods[!gofs.exist|mtime>time.gof]
        }

        if(nrow(dt.mods)==0) {
            message("No models to process. Exiting.")
            return(invisible(NULL))}
        
        setorder(dt.mods,-mtime)

        ## plots <- lapply()
        dir.plots <- "outputs/PK_diagnostics"
        ##    for(model in dt.mods$model){
        for(nmod in 1:dt.mods[,.N]){
            model <- dt.mods[nmod,model]
            path.lst <- dt.mods[nmod,path.lst]

            ## by="devpart",arm="dose",analyte="compound",names.eta=names.eta,dir.diag=file.res("PK_diagnostics")
            plots.run <- try(fun.gof(run=path.lst,dir.diag=dir.diag,...))

### save plots
            if(!"try-error"%in%class(plots.run)){
                names.plots <- names(plots.run$plots)
                dir.diag.nmod <- file.path(dir.diag,plots.run$details$model)
                if(!dir.exists(dir.diag.nmod)) dir.create(dir.diag.nmod)
                
                silent <- lapply(names.plots,function(x)
                    ggwrite(plots.run$plots[[x]],file=file.path(dir.diag.nmod,paste0(x,"_",plots.run$details$model,".pdf")),onefile=TRUE,canvas="wide-screen",script=script)
                    )
### save plots done

                info.save <- list(model=model
                                 ,time=Sys.time()
                                  )
                
                saveRDS(info.save,file=dt.mods[nmod,path.info])
                
            }
        }
        time.now <- Sys.time()
    }
    return(invisible(NULL))
}
