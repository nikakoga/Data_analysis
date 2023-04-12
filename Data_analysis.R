###TRYB WSADOWY
#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
# stop("Nalezy podejsc co najmniej jeden argument wejsciowy")
#}
#dane <- read.csv(file=args[1], header=TRUE)
####TRYB WSADOWY

#This way I can count how many groups there is


setwd("C:/Users/weron/Documents/Studia/PP/4 semestr/SAD/Projekt")
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


count_groups <- function(column) {
  unique_elements <- unique(column)
  num_unique <- length(unique_elements)
  counts <- table(column)
  
  df <- data.frame(ID = names(counts), count = as.numeric(counts))
  df
  write("\nGROPUS AND THEIR SIZE\n","raport.txt",append=TRUE)
  write.table(df,"raport.txt",append=TRUE, col.names = FALSE,row.names=FALSE)
  
  return(df)
}

Outliners_detection<-function(df)
{
  outliners=c()
  write("\n\nOUTLIERS\n","raport.txt",append=TRUE)
  for(col in names(df))
  {
   
    if(is.numeric(df[[col]]))
    {
      boxplot(df[[col]],
              ylab = col)
      
      outliners<-append(outliners,(paste(col,boxplot.stats(df[[col]])$out,"\n")))
      
    }
  }
  write(outliners,"raport.txt",append=TRUE)
  
}

Descriptive_statistics<-function(df)
{
  Outliners_detection(df)
  df_with_selected_groups<-count_groups(df[1])
  write("\n\nCHARACTERISTICS","raport.txt",append=TRUE)
  
  for(col in names(df))
  {
    if(is.numeric(df[[col]]))
       {
     
        write(paste('\n\n', col), "raport.txt", append = TRUE)

      s <- summary(df[[col]])
      capture.output(s, file = "raport.txt",append=TRUE)
    }
        
    
  }
    
}

data_with_NA<-read.csv2("Dane.csv",header=TRUE)
data<-Remove_NA(data_with_NA)
data
Descriptive_statistics(data)







