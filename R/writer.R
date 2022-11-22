#### One interface for ggwrite and writeFlextab

## add ,args.ggwrite and and args.flextable

##' @export
writer <- function(x,file,save,show,formats,script=NULL){

    
    if("flextable" %in% class(x)) {
        writeFlextab(x,file=file,script=script,formats="png")
    } else {
        if(is.list(x) && !is.ggplot(x)){
            x <- x[!sapply(x,is.null)]
        }
        res <- try(ggwrite(x,file=file,script=script,save=save,show=show,onefile=TRUE,canvas="wide-screen"))
        ## if("try-error"%in%class(res)) browser()
        res 
    }
    

}
