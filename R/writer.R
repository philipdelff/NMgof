#### One interface for ggwrite and writeFlextab

## add ,args.ggwrite and and args.flextable

##' @export
writer <- function(x,file,save,show,formats.ft,script=NULL,canvas="standard",...){

    
    if("flextable" %in% class(x)) {
        writeFlextab(x,file=file,script=script,formats=formats.ft,...)
    } else {
        if(is.list(x) && !is.ggplot(x)){
            x <- x[!sapply(x,is.null)]
        }
        res <- try(ggwrite(x,file=file,script=script,save=save,show=show,onefile=TRUE,canvas=canvas))
        ## if("try-error"%in%class(res)) browser()
        res 
    }
    

}
