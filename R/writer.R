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

## this should be exported from tracee, not from NMgof/NMauto

## add ,args.ggwrite and and args.flextable
writer <- function(x,file,save,show,formats.ft,formats.gg,script=NULL,canvas="standard",time=NULL,...){
    
    if("flextable" %in% class(x)) {
        ftwrite(x,file=file,script=script,formats=formats.ft,...)
    } else if(is.data.frame(x)) {
        ## requires NMdata 0.1.7
        NMwriteData(x,file=file,formats.write="rds",script=script,genText=FALSE)
        ## NMstamp(x,script=script,model=time)
        ## saveRDS(x,file=fnExtension(file,"rds"))
    } else {
        if(is.list(x) && !is.ggplot(x) && ! "gtable"%in%class(x) ){
            x <- x[!sapply(x,is.null)]
        }
        ## message("Calling ggwrite()")
        res <- try(ggwrite(x,file=file,script=script,save=save,show=show,onefile=TRUE,canvas=canvas,
                           useNames=TRUE,
                           formats=formats.gg))
        ## if("try-error"%in%class(res)) browser()
        res 
    }
    
}
