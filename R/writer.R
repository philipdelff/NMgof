##' One interface for ggwrite and writeFlextab
##' @param x
##' @param file
##' @param save
##' @param show
##' @param formats.ft
##' @param script
##' @param canvas
##' @param ...
##' @import tracee
##' @export

## add ,args.ggwrite and and args.flextable
writer <- function(x,file,save,show,formats.ft,formats.gg,script=NULL,canvas="standard",...){

    
    if("flextable" %in% class(x)) {
        ftwrite(x,file=file,script=script,formats=formats.ft,...)
    } else {
        if(is.list(x) && !is.ggplot(x)){
            x <- x[!sapply(x,is.null)]
        }
        
        res <- try(ggwrite(x,file=file,script=script,save=save,show=show,onefile=TRUE,canvas=canvas,
                           formats=formats.gg))
        ## if("try-error"%in%class(res)) browser()
        res 
    }
    

}
