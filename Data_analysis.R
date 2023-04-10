###TRYB WSADOWY
#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
# stop("Nalezy podejsc co najmniej jeden argument wejsciowy")
#}
#dane <- read.csv(file=args[1], header=TRUE)
####TRYB WSADOWY

#This way I can count how many groups there is
names(data_with_NA)[1] <- "ID"
my_table<-table(data_with_NA$ID)
my_table
how_many_groups<-length(names(my_table))
how_many_groups


for (col_name in names(dane)) {
  cat("Column ", col_name, ":\n")
  print(dane[[col_name]])
}

setwd("C:/Users/weron/Documents/Studia/PP/4 semestr/SAD/Projekt")

data_with_NA<-read.csv2("Dane.csv",header=TRUE)
data_with_NA


library("Hmisc")

Replace_blank_with_NA<-function(x)
{
  x<-replace(x,x=='',NA)
  return(x)
}

Remove_NA <- function(df){
  df<-Replace_blank_with_NA(df)
  for (col in names(df)) {
    #If NA in numeric column than replace with a median
    if(is.numeric(df[[col]]) & any(is.na(df[[col]]))) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    } #If NA in non numeric column, delete this row
  }
  
  complete.cases(df)
  ready<-df[complete.cases(df),]
  
  return(ready)
}


data<-Remove_NA(data_with_NA)
data

#IDEA 



