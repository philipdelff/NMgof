##' @import data.table

### run gof plots on all updated models 
NMgof <- function(dir.models,fun.gof,dir.diag,fun.find.models,update.only=TRUE,script=NULL,...){
    
    if(missing(fun.find.models)){
        fun.find.models <- function(dir) list.files(dir,pattern=".lst$",full.names=T)
    }
    
    ## dt.mods <- data.table(
    ##     path.lst= list.files("models/Final_Auto",pattern=".lst$",full.names=T)
    ## )
    ## dt.mods <- dt.mods[!grepl("^sim",basename(path.lst))]

    dt.mods <- data.table(
        path.lst = fun.find.models(dir.models)
    )

    fun.file.rds <- function(dir.diag,run) file.path(paste0("outputs/PK_diagnostics/",run,"/NMgof_",run,"_gof_runtime.rds"))
    
    dt.mods[,mtime:=file.info(path.lst)$mtime]
    dt.mods[,model:=sub("run","",basename(path.lst))]
    dt.mods[,model:=sub("\\..+","",model)]
    dt.mods[,run:=fnExtension(basename(path.lst),"")]
    ## dt.mods[,path.info:=file.path(paste0("outputs/PK_diagnostics/",run,"/NMgof_",run,"_gof_runtime.rds"))]
    dt.mods[,path.info:=fun.file.rds(dir.diag,run)]
    dt.mods[,gofs.exist:=file.exists(path.info)]
    dt.mods[,ROW:=1:.N]
    dt.mods[,time.gof:=as.POSIXct("1900-01-01 00:00:00")]
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
        names.plots <- names(plots.run$plots)
        dir.diag.nmod <- file.path(dir.diag,plots.run$details$model)
        if(!dir.exists(dir.diag.nmod)) dir.create(dir.diag.nmod)
        
        silent <- lapply(names.plots,function(x)
            ggwrite(plots.run$plots[[x]],file=file.path(dir.diag.nmod,paste0(x,"_",plots.run$details$model,".pdf")),onefile=TRUE,canvas="wide-screen",script=script)
            )
### save plots done

        ## this should be part of gofRun
        info.save <- list(model=model
                         ,time=Sys.time()
                          )
        
        saveRDS(info.save,file=dt.mods[nmod,path.info])
        
    }

    return(invisible(NULL))
}
