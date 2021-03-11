#The data we will be using
wong<- read.table("Wong.CSV",header = TRUE,sep = ",") #input our csv data

#Using base R plot function to make visualization of the data
plot(wong$piq,wong$viq,main = "Relationship between performance IQ and verbal IQ of coma patients",xlab = "Performance IQ",ylab = "Visual IQ")
lmiq <- lm(wong$viq~wong$piq)
abline(lmiq,col=2,lwd=3)

#Using Lattice package to visualize data. 
library(lattice)
xyplot(viq~piq, wong)
xyplot(viq~piq, wong,
       grid = TRUE,
       groups = sex, auto.key = TRUE,
       type = c("p","r"), lwd= 3)
xyplot(viq~piq|sex, wong,#Separate the sexes
       grid = TRUE,
       groups = sex,
       type = c("p","smooth"), lwd= 3)
xyplot(viq~piq|sex, wong, #create a smooth scatter 
       grid = TRUE,
       panel = panel.smoothScatter )

#Using ggplot2
library(ggplot2)
ourplot <- ggplot(wong,aes(piq,viq)) + geom_point()
ourplot + geom_smooth(method = 'lm') 
ourplot + geom_point(aes(col=sex)) + geom_smooth( method = "lm",se = FALSE, aes(group = sex, color = sex))
ourplot + geom_point(aes(col=sex)) + geom_smooth( method = "lm",se = FALSE, aes(group = sex, color = sex)) + facet_grid(~sex)
