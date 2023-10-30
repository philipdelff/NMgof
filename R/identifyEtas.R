identifyEtas <- function(file.mod){

    lines <- c(NMreadSection(file.mod,section="PRED",keep.comments=FALSE),
               NMreadSection(file.mod,section="PK",keep.comments=FALSE))

    
    
    dt.code <- data.table(line.eta = lines[grepl("[^H]ETA",lines)])
    ## remove spaces
    dt.code[,line2:=gsub(" ","",line.eta)]

    dt.code <- dt.code[,.(text.eta=regmatches(line2, gregexpr("[^H]ETA\\(([0-9]+)\\)",line2))|>unlist()),by=.(line.eta,line2)]

    
    ## dt.code[,i:=as.numeric(gsub(".*[^H]ETA\\(([0-9]+)\\).*","\\1",line2))]
    dt.code[,i:=as.numeric(sub("\\(ETA\\(([1-9][0-9]*)\\)","\\1",text.eta))]
    
    dt.code[,LHS:=sub("(.*)=.*","\\1",line2)]

    dt.code.eta <- dt.code[,.(label=paste(unique(LHS),collapse=", "),
               code=paste(line2,collapse=", ")
               ),by=.(i,LHS)]

    ## if a LHS is affected by multiple ETAs we number them
    dt.code.eta[,nrep.LHS:=.N,by=.(LHS)]
    dt.code.eta[nrep.LHS>1,label:=paste0(label," - ETA(",i,")")]
    
    dt.code.eta


}
