
NMplotDoses <- function(doses,col.id="ID",col.cmt=NULL,col.time="TIME"){
    
    dt.doses <- NMexpandDoses(doses,col.id=col.id)
    dt.doses <- dt.doses[,.(AMT=sum(AMT)),by=c(col.id,col.cmt,col.time)]
    ggplot(dt.doses,aes_string(col.time,col.id,size="AMT"))+
        geom_point()

}
