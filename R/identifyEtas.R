identifyEtas <- function(file.mod){

    lines <- c(NMreadSection(file.mod,section="PRED",keep.comments=FALSE),
               NMreadSection(file.mod,section="PK",keep.comments=FALSE))
    
    dt.code <- data.table(line.eta = lines[grepl("[^H]ETA",lines)])
    dt.code[,line2:=gsub(" ","",line.eta)]

    dt.code[,i:=as.numeric(sub(".*[^H]ETA\\(([0-9]+)\\).*","\\1",line2))]
    dt.code[,LHS:=sub("(.*)=.*","\\1",line2)]

    dt.code.eta <- dt.code[,.(label=paste(unique(LHS),collapse=", "),
               code=paste(line2,collapse=", ")
               ),by=i]

    dt.code.eta


}
