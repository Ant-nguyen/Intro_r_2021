tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { outlier.vec[i] <- all(outliers[i,]) } 
  return(outlier.vec) }

#Test input
test <- array(data = c(c(1,2,3,4,5),c(10,15,25,30,15),c(20,25,40,20,100)),dim = c(5,3))

tukey.outlier <- function(x) { #possible tukey.outlier function
  quant <- quantile(x)
  iqr <- quant[4]-quant[2]
  return(any(x <(quant[2]-(1.5*iqr))|any(x > (quant[4]+(1.5*iqr)))))}