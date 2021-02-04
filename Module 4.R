Freq <- c(0.6,0.3,0.4,0.4,0.2,0.6,0.3,0.4,0.9,0.2)
BP <- c(103,87,32,42,59,109,78,205,135,176)
First <- c(1,1,1,1,0,0,0,0,NA,1)
Second <- c(0,0,1,1,0,0,1,1,1,1)
Final <- c(0,1,0,1,0,1,0,1,1,1)
docdf <- data.frame(Freq,BP,First,Second,Final,stringsAsFactors = FALSE)


# This function function will create a Boxplot based on MDs' rating of either Freq or Bp based on colm number
plotBox <- function(df,colm){
  if (colm !=1 && colm!=2){return("Please pick either colm 1 or 2")} # A Check to make sure users enter either 1 or 2 for colm
  docs <-vector() 
  zeros = vector() 
  ones = vector()
  for(i in 1:nrow(df)){
    docs<- c(docs,(sum(df[i,3:5],na.rm = TRUE)))
    if(docs[i] >1){ones <- c(ones,df[i,colm])} else{zeros <- c(zeros,df[i,colm])}
  }
  if(colm ==1){return(
    boxplot(ones,zeros,
    main= "Boxplot of frequency values based on overall MDs' rating",
    names= c("Concerned","Unconcerned"),
    ylab ="Frequency of hospital visits in a 12 month period"))
    }
  else if (colm==2){return( 
    boxplot(ones,zeros,
    main= "Boxplot of BP values based on overall MDs' rating",
    names= c("Concerned","Unconcerned"),
    ylab ="BP Values"))
  }
}

plotBox(docdf,1)
plotBox(docdf,2)
hist(Freq)