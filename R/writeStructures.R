pathStruct <- function(structure="model/file_model"){

    fun.path <- NULL
    if(structure=="model/file_model"){
        fun.path <- function(name,dir,model){
            if(is.list(model)) {
                model <- model$mod 
            }
            model.name <- basename(model) |> fnExtension("")
            
            file.path(dir,
                      model.name,
                      fnAppend(name,model.name,allow.noext=TRUE)
                      )
        }
    }

    if(is.null(fun.path)){
        stop("structure not recognized.")
    }

    fun.path
}
