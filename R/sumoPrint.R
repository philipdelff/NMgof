##' @importFrom rmarkdown render
sumoPrint <- function(file.lst,dir.diag="."){
    require(rmarkdown)
    file.lst <- fnExtension(file.lst,"lst")
    
    sumotemp <- tempfile()
    system(sprintf("sumo %s > %s",file.lst,sumotemp))
    mytext <- readLines(sumotemp) 
    mytext <- c("```",mytext,"```")
    cat(mytext, sep="  \n", file = "sumores.Rmd")
    suppressMessages(render("sumores.Rmd", output_format=pdf_document(),output_file=file.path(dir.diag,"sumores.pdf")))
    NULL
}
