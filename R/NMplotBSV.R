##' Generate distribution plots of between-occasion variability terms
##' from Nonmem
##' @param data A dataset - will be converted to data.frame so
##'     data.table is OK.
##' @param regex.eta A regular expression defining the naming of the
##'     ETA's of interest.
##' @param col.id The name of the id column in data. Default is ID
##'     like Nonmem.
##' @param covs.num Names of columns containing numerical covariates
##'     to plot the random effects against.
##' @param covs.char Names of columns containing categorical
##'     covariates to plot the random effects against.
##' @param fun.file If saving plots, this function can be used to
##'     translate the file names. The inputs given to the function
##'     argument are "iov_pairs.png" and "iov_covs_n.png".
##' @param save Save the generated plots?
##' @param script If saving the plots, a stamp to add. See ggstamp.
##' @param return.data If TRUE, the identified ETA's together with
##'     subject id and covariates will be returned in both wide and
##'     long format. If FALSE, you just get the plots.
##' @param debug Start by running browser()?
##' @import ggplot2
##' @import data.table
##' @import stats
##' @importFrom NMdata findCovs findVars
##' @importFrom GGally ggpairs
##' @family Plotting
##' @export

NMplotBSV <- function(data,regex.eta="^ETA",names.eta=NULL,col.id="ID",covs.num,covs.char,fun.file=identity,save=FALSE,show=TRUE,script=NULL,return.data=FALSE,title=NULL,structure="flat",debug=F){

    if(debug) {browser()}
    
    pkpars <- as.data.table(data)

    if(missing(covs.num)) covs.num <- NULL
    if(missing(covs.char)) covs.char <- NULL

#### Section start: dummy variables, only not to get NOTE's in pacakge checks ####

    value  <- NULL
    ..density..  <- NULL
    predicted <- NULL
    val.cov <- NULL

### Section end: dummy variables, only not to get NOTE's in pacakge checks



    
    all.output <- list()
    
    names.etas <-
        names(pkpars)[
            grep(regex.eta,names(pkpars))
        ]

    ## if specified to be a covariate, drop the eta
    names.etas <- setdiff(names.etas,c(covs.num,covs.char))
    
    ## only the ones that vary
    ## names.etas.var <- names(which(
    ##     sapply(pkpars[,names.etas,with=F],function(x)length(unique(x)))  > 1
    ## ))
    names.etas.var <- colnames(
        findCovs(
            findVars(pkpars[,c(col.id,names.etas),with=F])
           ,by=col.id)
    )
    names.etas.var <- setdiff(names.etas.var,col.id)

    
    if(!is.null(names.eta)){
        ## only using relevant names.eta entries
        names.eta <- names.eta[intersect(names(names.eta),names.etas.var)]
        to.remove <- intersect(as.character(names.eta),colnames(pkpars))
        if(length(to.remove)) pkpars[,(to.remove):=NULL]
        setnames(pkpars,names(names.eta),as.character(names.eta),skip_absent=TRUE)

        idx <- match(names(names.eta),names.etas.var)
        names.etas.var[idx] <-
            as.character(names.eta[match(names.etas.var,names(names.eta))])
    }
    
    etas <- NULL
    etas.l <- NULL
    
    if(length(names.etas.var)){
        ##        etas <- pkpars[,c("ID", names.etas.var,covs.num,covs.char)]
        etas <- unique(pkpars[,c("ID", names.etas.var,covs.num,covs.char),with=F])
        
### etas against each other. Notice, omitting those = 0.
        etas[,(names.etas.var):=lapply(.SD,function(x){x[x==0] <- NA;x}),.SDcols=names.etas.var]
        points.and.smooth <- function(data, mapping, method="lm", ...){
            p <- ggplot(data = data, mapping = mapping) + 
                geom_point() + 
                geom_smooth(method=method, formula=y~x, ...)
            p
        }

        
        iiv.pairs <- ggpairs(etas,columns=names.etas.var,lower=list(continuous=points.and.smooth),title=title)
        ## if(save||show){
        ##     ggwrite(iiv.pairs,file=fun.file("iiv_pairs.png"),script=script,save=save,show=show)
        ## }
        all.output[["iiv.pairs"]]  <- iiv.pairs
        
        ## etas.l <- gather(etas,param,value,-1)
        etas.l <- melt(etas,id.vars=c(col.id,covs.num,covs.char),measure.vars=names.etas.var,value.name="value",variable.name="param")
        ##
        ## compare.names(etas,pkpars)
        ##   etas.l <- mergeCheck(etas.l,pkpars,by=c(col.id,covs.num,covs.char),allow.cartesian=TRUE)
        
        etas.l.actual <- subset(etas.l,value!=0)


#### this is the histograms of non-zeros and with gaussian approximations
        dat <- etas.l.actual

        grid <- with(dat, seq(min(value), max(value), length = 100))

        DT.dat <- as.data.table(dat)
        normaldens <-
            DT.dat[,list(
                predicted=grid,
                density=dnorm(grid, mean(value), sd(value))
            ),
            by="param"]
        
        gh2 <- ggplot(data = dat,aes(x = value)) +
            geom_vline(xintercept=0,colour="grey",linetype="dashed")+
            geom_histogram(aes(y = ..density..)) + 
            geom_line(data = normaldens, aes(x = predicted, y = density), colour = "red",size=1)+
            facet_wrap(~param,scales="free")+
            labs(title=title)

        ## if(save||show){
        ##     ggwrite(gh2,file=fun.file("hists_etas_actual_wgaussian.png"),script=script,save=save,show=show)
        ## }
        all.output[["hists.etas"]]  <- gh2
        
        
        plot.qq <- ggplot(dat,aes(sample=value))+
            geom_hline(yintercept=0,colour="grey",linetype="dashed")+
            geom_vline(xintercept=0,colour="grey",linetype="dashed")+
            geom_qq_line(colour=2,size=1.5)+
            geom_qq()+
            ## the theroretical identity line
            geom_abline(slope=1,intercept=0,linetype=2)+
            facet_wrap(~param)+
            labs(x="Theoretical",y="Observed",title=title)
        ## ggwrite(plot.qq,file=fun.file("qq_etas.png"),script=script,save=save,show=show)
        all.output[["qq.bsv"]]  <- plot.qq
        
        ## IIV random effects vs covariates
        if(!is.null(covs.num)){
            if(!all(covs.num%in%names(pkpars))){
                covs.num.drop <- setdiff(covs.num,names(pkpars))
                warning(paste0("The following numerical parameters were not found:\n",paste(covs.num.drop,collapse=", ")))
                covs.num <- setdiff(covs.num,covs.num.drop)
            }
            
            ## use only covariates that vary
            ## covs.num <- names(which(
            ##     sapply(pkpars[,covs.num,drop=F],function(x)length(unique(x)))  > 1
            ## ))
            covs.num <- colnames(findVars(pkpars[,covs.num,with=F]))
            
            etas.l2.n <- etas.l[,c(col.id,"param","value",covs.num),with=FALSE]

            if(
                length(setdiff(colnames(etas.l2.n),c("ID","param","value")))>0
            ){
                etas.covs.n <- melt.data.table(etas.l2.n,variable.name="cov",value.name="val.cov",measure.vars=names(etas.l2.n)[!names(etas.l2.n)%in%c("ID","param","value")])
                data.plot <- etas.covs.n[!grepl(regex.eta,cov)]
                p.iiv.covsn <- lapply(split(data.plot,by="cov"),
                                      function(dt){ggplot(dt,aes(val.cov,value))+
                                                       geom_point()+
                                                       geom_smooth(method="lm", formula=y~x)+
                                                       facet_wrap(~param,scales="free")+
                                                       labs(title=title,x=dt[,unique(cov)],y="Eta")
                                      })
                ## ggwrite(p.iiv.covsn,file=fun.file("iiv_covs_n.png"),script=script,save=save,show=show)
                if(structure=="flat"){
                    all.output <- c(all.output,
                                    setNames(p.iiv.covsn,paste0("iiv_covsn_",names(p.iiv.covsn)))
                                    )
                } else {
                    all.output[["iiv.covsn"]] <- p.iiv.covsn
                }
            }
        }
        if(!is.null(covs.char)){
            etas.l2.c <- etas.l[,c(col.id,"param","value",covs.char),with=F]
            DT <- data.table(etas.l2.c)
            DT2 <- melt(DT,measure.vars=covs.char,id.vars=c("ID","param","value"),value.name="val.cov",value.factor=T)
            sets <- split(DT2,by="variable")
            p.iiv.covsc <- lapply(sets,function(dat){
                ggplot(dat,aes(val.cov,value))+
                    geom_hline(yintercept=0,linetype=2) +
                    geom_boxplot(outlier.shape=NA,colour="blue")+
                    geom_jitter(height=0,width=.4,alpha=.5)+
                    facet_wrap(~param,scales="free_y")+
### this would bring in ggpubr as dependency. Maybe just call in scripts where needed?
                    ## rotate_x_text(45)+
                    labs(title=title,x=dat[,unique(variable)],y="Eta")
            })
##             ggwrite(p.iiv.covsc,file=fun.file("iiv_covs_c.png"),useNames=TRUE,script=script,save=save,show=show)
            if(structure=="flat"){
                all.output <- c(all.output,
                                setNames(p.iiv.covsc,paste0("iiv_covsc_",names(p.iiv.covsc)))
                                )
            } else {
                all.output[["iiv.covsc"]] <- p.iiv.covsc
            }
        }
    } else {
        message("No BSV random effects found in parameter table.")
    }
### I think this check can be done before all the potential plotting and then just exit earlier
    ## if there are no plots to return, we return NULL (instead of a list of length 0).
    if(length(all.output)==0&&!return.data){return(NULL)}
    if(return.data){
        all.output[["etas"]] <- etas
        all.output[["etas.l"]] <- etas.l
    }

    all.output
}
