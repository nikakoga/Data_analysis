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
  change_report <- c()
  
  df<-Replace_blank_with_NA(df)
  for (col in names(df)) {
    #If NA in numeric column than replace with a median
    
    if(is.numeric(df[[col]]) & any(is.na(df[[col]]))) {
      NA_rows <- which(is.na(df[[col]]))  # get rows with missing values
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      change_report <- c(change_report, paste("missing data in column ", col, "in rows:", NA_rows,"replacing with a median",median(df[[col]])))
    }
  }
  
  #Now every NA in numeric columns are gone
  #I delete every row with NA i non numeric column
  complete.cases(df)
  ready<-df[complete.cases(df),]
  
  if(length(change_report) > 0) {
    writeLines(change_report, "change_report.txt")
  }
  
  return(ready)
}


data<-Remove_NA(data_with_NA)
data

#IDEA 



