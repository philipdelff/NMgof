#### diagonal comparisons. Log is missing. Generalize splitting
#### (compound) and facetting (devpart+pop), add log and put into
#### function.
NMplotDiags <- function(data){
    plots.diag <- list()
    ## pred vs obs and ipred vs obs split by devpart and dose
    plots.diag$pred.dv.parent <- ggplot(data[EVID==0&PARENT==1],aes(DV,PRED,colour=dose))+
        geom_abline(slope=1,intercept = 0)+
        geom_point()+
        facet_wrap(~devpart+pop,scales="free")+
        labs(x="Observations",y="Population predictions",title=paste(details$model, "VX-548"))

    
    plots.diag$ipred.dv.parent <- plots.diag$pred.dv.parent + aes(DV,IPRED) +
        labs(x="Observations",y="Individual predictions",title=paste(details$model, "VX-548"))

    plots.diag$pred.dv.metab <- plots.diag$pred.dv.parent %+% data[EVID==0&PARENT==0]+
        labs(title=paste(details$model, "M6-548"))
    plots.diag$ipred.dv.metab <- plots.diag$ipred.dv.parent %+% data[EVID==0&PARENT==0]+
        labs(title=paste(details$model, "M6-548"))
    plots$diags_pred_dv <- plots.diag
    
}
