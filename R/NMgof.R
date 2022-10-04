##' @import data.table
##' @export

### run gof plots on all updated models 
NMgof <- function(dir.models,fun.gof,dir.diag,fun.find.models,fun.repair.data=NULL,update.only=TRUE,script=NULL,...){
    
    if(missing(fun.find.models)){
        fun.find.models <- function(dir) list.files(dir,pattern=".lst$",full.names=T)
    }
    
    dt.mods <- data.table(
        path.lst = fun.find.models(dir.models)
    )

    ## fun.file.rds <- function(dir.diag,run) file.path(paste0("outputs/PK_diagnostics/",run,"/NMgof_",run,"_gof_runtime.rds"))
    fun.file.rds <- function(dir.diag,run) file.path(dir.diag,run,paste0("NMgof_",run,"_gof_runtime.rds"))
    
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
        return(invisible(NULL))
    }
    
    setorder(dt.mods,-mtime)
    print(dt.mods)
    

    ## plots <- lapply()
    ##    for(model in dt.mods$model){

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
    for(nmod in 1:dt.mods[,.N]){
        tmp <- try(fun.run(nmod))
    }

    return(invisible(NULL))
}
