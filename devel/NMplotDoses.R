NMplotDoses <- function(data){

    data <- NMexpandDoses(data)

    data[,.(AMT=sum(AMT)),by=.(ID,TIME)]

}
