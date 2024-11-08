writeList <- function(list,model,dir,fun.path,formats.ft="png",formats.gg="png",...){
    
    ## if(is.list(model)) {
    ##     model <- model$label
    ## }

    
    if(missing(fun.path)) {
        fun.path <- pathStruct()
    }

    
    dir.model <- fun.path("dummy",dir,model) |> dirname()
    if(!file.exists(dir.model)) dir.create(dir.model)
    
    names.out <- names(list)
    res <- lapply(1:length(list),function(n){
        file.out <- fun.path(names.out[n],dir=dir,model=model)
        writer(x=list[[n]],file=file.out,
               formats.ft=formats.ft,
               formats.gg=formats.gg,model=model,
               ...)
    })
    return(invisible(res))
}
