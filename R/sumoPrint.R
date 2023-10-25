##' @importFrom rmarkdown render
sumoPrint <- function(dt,dir.diag="."){
    require(rmarkdown)
    
    lst <- NMinfo(dt,"details")$file.lst
    sumotemp <- tempfile()
    system(sprintf("sumo %s > %s",lst,sumotemp))
    mytext <- readLines(sumotemp) 
    mytext <- c("```",mytext,"```")
    cat(mytext, sep="  \n", file = "sumores.Rmd")
    render("sumores.Rmd", output_format=pdf_document(),output_file=file.path(dir.diag,"sumores.pdf"))
    NULL
}
