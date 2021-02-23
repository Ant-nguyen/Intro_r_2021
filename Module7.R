fileName <- 'HPV-10.txt' #Text file with the full HPV10 genome
HPV10 <- readChar(fileName,file.info(fileName)$size)
print(HPV10)
summary(HPV10)
substr(HPV10,443,1375) # minor capsid protein 443-1375 of the whole genome

#s3 class creation
capsid <- list(DNAseq= substr(HPV10,443,1375),size = nchar(substr(HPV10,443,1375)),Fiveto3 = TRUE)
class(capsid) <-"coding"
# s3 Constructor function for our classes
Coding <- function(DNA){
  cod <- list(DNAseq = DNA, size = nchar(DNA), Fiveto3 = TRUE)
  class(cod) <- "coding"
  return(cod)
}
Template <- function(DNA){
  temp <- list(DNAseq = DNA, size = nchar(DNA), Fiveto3 = FALSE)
  class(temp) <- "template"
  return(temp)
}
#custom print method for our new classes
print.coding <- function(item){
  cat("5\'",toupper(item$DNAseq),"3\'")
}

print.template <- function(item){
  cat("3\'",toupper(item$DNAseq),"5\'")
}
# s4 class creation
setClass("coding",representation(DNAseq = "character",size = "numeric",Fiveto3 ="logical"))
setClass("template",representation(DNAseq = "character",size = "numeric",Fiveto3 ="logical"))
capsid <- new("coding",DNAseq= substr(HPV10,443,1375),size = nchar(substr(HPV10,443,1375)),Fiveto3 = TRUE)
setMethod("show","coding",function(object){ cat("5\'",toupper(object@DNAseq),"3\'")})