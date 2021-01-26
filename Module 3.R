Name <-c("Jeb","Donald","Ted","Marco","Carly", "Hillary", "Bernie")
ABC <- c(4, 62, 51, 21, 2, 14, 15)
CBS <- c(12, 75, 43, 19, 1, 21, 19)
df <- data.frame(Name,ABC,CBS,stringsAsFactors = FALSE)
sum(df$ABC)
sum(df$CBS)
dif <- abs(df$ABC-df$CBS)
dif
df$diff <- dif
df
