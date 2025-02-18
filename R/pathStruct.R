##' Create a function to derive output file paths
##'
##' Output file paths may depend on the model. pathStruct can be used
##' to generate such functions based on a simple structure name.
##'
##' @examples
##' path.outputs <- pathStruct(structure="model/file_model")
##' 

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

    ## "gof1.png", model="103", > "103/103-gof1.png"
    ## name="gof1", model="103", > "103/103-gof1.png"
    if(structure=="model/model-file"){
        fun.path <- function(name,dir,model){
            if(is.list(model)) {
                model <- model$mod 
            }
            ## model.lst -> model
            model.name <- basename(model) |> fnExtension("")
            
            file.path(dir,
                      model.name,
                      paste(model.name,name,sep="-")
                      )
        }
    }

    
    if(is.null(fun.path)){
        stop("structure not recognized.")
    }

    fun.path
}
