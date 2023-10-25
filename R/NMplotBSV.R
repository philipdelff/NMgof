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

NMplotBSV <- function(data,regex.eta,names.eta=NULL,col.id="ID",covs.num,covs.char,save=FALSE,show=TRUE,return.data=FALSE,title=NULL,file.mod,structure="flat",debug=F){

    if(debug) {browser()}
    
    
#### Section start: dummy variables, only not to get NOTE's in pacakge checks ####

    value  <- NULL
    ..density..  <- NULL
    predicted <- NULL
    val.cov <- NULL

### Section end: dummy variables, only not to get NOTE's in pacakge checks

    if(missing(file.mod)) file.mod <- NULL
    ## if(is.null(file.mod)){
    ##     details <- NMinfo(data,"details")
    ##     file.mod <- details$file.mod
    ## }

### extract etas
    if(!is.null(file.mod)){
        file.phi <- fnExtension(file.mod,"phi")
        if(file.exists(file.phi)){
            dt.phi <- NMsim:::NMreadPhi(file.phi)
            dt.etas <- dt.phi[par.type=="ETA"]
            ## only the ones that vary
            dt.etas[,eta.var:=any(value!=0),by=.(parameter)]
            dt.etas.var <- dt.etas[eta.var>0]
        }
    } else {
        ## for now, we don't prioritize this. Better take them from .phi
        if(missing(regex.eta)) regex.eta <- "^ETA[1-9]$|^ET[A]{0,1}[1-9][0-9]$"
    }

### get eta labels
    
    if(is.null(names.eta)){
        if(!file.exists(file.mod)){
            ## this should not be an error - just show ETA 1, ETA 2, etc.
            stop("Either provide file.mod or")
        }
        names.eta <- identifyEtas(file.mod)
        
    }

    ## merge etas and labels
    dt.etas.var <- mergeCheck(dt.etas.var,names.eta,by="i",all.x=TRUE)


##### derive covariates
    pkpars <- as.data.table(data)
    pkpars <- findCovs(pkpars,by="ID",as.fun="data.table")
    if(missing(covs.num)) covs.num <- NULL
    if(missing(covs.char)) covs.char <- NULL
    if(!all(covs.num%in%names(pkpars))){
        covs.num.drop <- setdiff(covs.num,colnames(pkpars))
        warning(paste0("The following numerical parameters were not found:\n",paste(covs.num.drop,collapse=", ")))
        covs.num <- setdiff(covs.num,covs.num.drop)
        ## use only covariates that vary
        covs.num <- colnames(findVars(pkpars[,covs.num,with=F]))
    }
    if(!all(covs.char%in%names(pkpars))){
        covs.char.drop <- setdiff(covs.char,colnames(pkpars))
        warning(paste0("The following numerical parameters were not found:\n",paste(covs.char.drop,collapse=", ")))
        covs.char <- setdiff(covs.char,covs.char.drop)
        ## use only covariates that vary
        if(length(covs.char)){
            covs.char <- colnames(findVars(pkpars[,covs.char,with=F]))
        }
    }
    ## add covariates to etas
    if(is.character(pkpars[,ID])) dt.etas.var[,ID:=as.character(ID)]
    dt.etas.var <- mergeCheck(dt.etas.var,pkpars[,c("ID",covs.num,covs.char),with=FALSE],by=c("ID"))

    ## only keep non-0 ETAs (should be an option)
    ## dt.etas.var[value==0,value:=NA_real_]
    dt.etas.var <- dt.etas.var[value!=0]

    ## make a wide version for 
    etas.l <- dt.etas.var
    etas.w <- dcast(etas.l,ID~label,value.var="value")
    names.etas.var <- setdiff(colnames(etas.w),"ID")

    if(!length(names.etas.var)){        
        message("No BSV random effects found in parameter table.")
        return(invisible(NULL))
    }
    
##### this is if taking etas from the output tables
    ## else {
    ##     ## only using relevant names.eta entries
    ##     names.eta <- names.eta[intersect(names(names.eta),names.etas.var)]
    ##     to.remove <- intersect(as.character(names.eta),colnames(pkpars))
    ##     if(length(to.remove)) pkpars[,(to.remove):=NULL]
    ##     setnames(pkpars,names(names.eta),as.character(names.eta),skip_absent=TRUE)

    ##     idx <- match(names(names.eta),names.etas.var)
    ##     names.etas.var[idx] <-
    ##         as.character(names.eta[match(names.etas.var,names(names.eta))])
    ## }


    all.output <- list()
    if(F){

        names.etas <-
            names(pkpars)[
                grepl(regex.eta,names(pkpars))
            ]

        ## if specified to be a covariate, drop the eta
        names.etas <- setdiff(names.etas,c(covs.num,covs.char))
        
        ## only the ones that vary
        names.etas.var <- colnames(
            findCovs(
                findVars(pkpars[,c(col.id,names.etas),with=F])
               ,by=col.id)
        )
        names.etas.var <- setdiff(names.etas.var,col.id)
    }

    
    ## etas <- NULL
    ## etas.l <- NULL
    

    ##        etas <- pkpars[,c("ID", names.etas.var,covs.num,covs.char)]
    ## etas <- unique(pkpars[,c("ID", names.etas.var,covs.num,covs.char),with=F])
    ## etas.w <- dcast(dt.etas.var,ID~label,value.var="value")
    
### etas against each other. Notice, omitting those = 0.
    ##etas[,(names.etas.var):=lapply(.SD,function(x){x[x==0] <- NA;x}),.SDcols=names.etas.var]

    ## names.etas.var <- setdiff(colnames(etas.w),"ID")
    ## doing this before widening instead
    ## etas.w[,(names.etas.var):=lapply(.SD,function(x){x[x==0] <- NA;x}),.SDcols=names.etas.var]
    
    points.and.smooth <- function(data, mapping, method="lm", ...){
        p <- ggplot(data = data, mapping = mapping) + 
            geom_point() + 
            geom_smooth(method=method, formula=y~x, ...)
        p
    }

    
    iiv.pairs <- ggpairs(etas.w,columns=names.etas.var,lower=list(continuous=points.and.smooth),title=title)
    all.output[["iiv.pairs"]]  <- iiv.pairs

#### we already have the long format, right?
    ## etas.l <- gather(etas,param,value,-1)
    ## etas.l <- melt(etas.w,id.vars=c(col.id,covs.num,covs.char),measure.vars=names.etas.var,value.name="value",variable.name="param")
    

#### this is the histograms of non-zeros and with gaussian approximations
    dat <- etas.l
    dat[,param:=label]
    grid <- with(dat, seq(min(value), max(value), length = 100))

    normaldens <-
        dat[,list(
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
    all.output[["qq.bsv"]]  <- plot.qq
    
    ## IIV random effects vs covariates
    if(!is.null(covs.num)){
        
        
        etas.l2.n <- etas.l[,c(col.id,"param","value",covs.num),with=FALSE]

        if(
            length(setdiff(colnames(etas.l2.n),c("ID","param","value")))>0
        ){
            etas.covs.n <- melt.data.table(etas.l2.n,variable.name="cov",value.name="val.cov",measure.vars=names(etas.l2.n)[!names(etas.l2.n)%in%c("ID","param","value")])
### I think this one removes covariates that are etas. Not really
### necessary and if anything, it should be done much earlier.
            ## data.plot <- etas.covs.n[!grepl(regex.eta,cov)]
            p.iiv.covsn <- lapply(split(etas.covs.n,by="cov"),
                                  function(dt){ggplot(dt,aes(val.cov,value))+
                                                   geom_point()+
                                                   geom_smooth(method="lm", formula=y~x)+
                                                   facet_wrap(~param,scales="free")+
                                                   labs(title=title,x=dt[,unique(cov)],y="Eta")
                                  })

            if(structure=="flat"){
                all.output <- c(all.output,
                                setNames(p.iiv.covsn,paste0("iiv_covsn_",names(p.iiv.covsn)))
                                )
            } else {
                all.output[["iiv.covsn"]] <- p.iiv.covsn
            }
        }
    }
    if(length(covs.char)>0){
        
        etas.l2.c <- etas.l[,c(col.id,"param","value",covs.char),with=F]
        DT <- data.table(etas.l2.c)
        DT2 <- melt(DT,measure.vars=covs.char,id.vars=c("ID","param","value"),value.name="val.cov",value.factor=T)
        sets <- split(DT2,by="variable")
        p.iiv.covsc <- lapply(sets,function(dat){
            if(is.numeric(dat[,val.cov])) dat[,val.cov:=factor(val.cov)]
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

    ## if there are no plots to return, we return NULL (instead of a list of length 0).
    if(length(all.output)==0&&!return.data){return(NULL)}
    if(return.data){
        all.output[["etas"]] <- etas.w
        all.output[["etas.l"]] <- etas.l
    }

    all.output
}
