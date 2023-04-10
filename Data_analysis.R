###TRYB WSADOWY
#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
# stop("Nalezy podejsc co najmniej jeden argument wejsciowy")
#}
#dane <- read.csv(file=args[1], header=TRUE)
####TRYB WSADOWY

#This way I can count how many groups there is



for (col_name in names(dane)) {
  cat("Column ", col_name, ":\n")
  print(dane[[col_name]])
}

setwd("C:/Users/weron/Documents/Studia/PP/4 semestr/SAD/Projekt")

data_with_NA<-read.csv2("Dane.csv",header=TRUE)

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
      change_report <- c(change_report, paste("Missing data in column", col, "in rows:", paste(NA_rows, collapse = ", "),"replacing with a median",median(df[[col]])))
    }
  }
  
  #Now every NA in numeric columns are gone
  #I delete every row with NA in non numeric column
  TRUE_FALSE_vector<-complete.cases(df) #returns FALSE for rows with NA
  Rows_to_delete<-which(TRUE_FALSE_vector==FALSE)
  ready<-df[complete.cases(df),]
  
  if (length(Rows_to_delete) > 0) {
    deleted <- paste("Lack of data in non-numeric column I removed rows index:", paste(Rows_to_delete, collapse = ", "))
    change_report<-append(change_report,deleted)
  }
  
  if(length(change_report) > 0) {
    writeLines(change_report, "raport.txt")
  }
  
  return(ready)
}


data<-Remove_NA(data_with_NA)

Select_and_count_groups<-function(df){
  names(df)[1] <- "ID"
  my_table<-table(df$ID)
  return (my_table)
}

Descriptive_statistics<-function(df)
{
  Selected_groups<-Select_and_count_groups(df)
  
  
  for(i in seq_along(Selected_groups))
  {
    write(Selected_groups[i], "raport.txt", append = TRUE)
  } 
  
    
}
  
Descriptive_statistics(data)


#IDEA 
for (col in names(df)){
  is.numeric(df[[col]])
  {
    paste("max",max(col),"min",min(col),"średnia",mean(col))
  }
}



