###TRYB WSADOWY
#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
# stop("Nalezy podejsc co najmniej jeden argument wejsciowy")
#}
#dane <- read.csv(file=args[1], header=TRUE)
####TRYB WSADOWY


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
  
}

Outliers_detection<-function(df)
{
  write("\n\nOUTLIERS\n","raport.txt",append=TRUE)
  for(col in names(df))
  {
   
    if(is.numeric(df[[col]]))
    {
      boxplot(df[[col]],
              ylab = col)
      
      #outliners<-append(outliners,(paste(col,boxplot.stats(df[[col]])$out,"\n")))
      outliers<-boxplot.stats(df[[col]])$out
      if(length(outliers)>0)
      {
        capture.output(cat(col,outliers,"\n"), file = "raport.txt",append=TRUE)
      }
      else
      {
        capture.output(cat(col,"no outliners","\n"), file = "raport.txt",append=TRUE)
      }
    }
  }
}

Descriptive_statistics<-function(df)
{
  Outliers_detection(df)
  count_groups(df[1])
  write("\n\nCHARACTERISTICS","raport.txt",append=TRUE)
  
  splitted_groups<-split(df,df[1])
  groupnames<-names(splitted_groups)
  
  colnames<-names(df[,-1])
  for(col in colnames)
  {
    for(group in groupnames)
    {
      if(is.numeric(splitted_groups[[group]][[col]]))
      {
        
        write(paste('\n\n', group, '\n', col), "raport.txt", append = TRUE)
        
        s <- summary(splitted_groups[[group]][[col]])
        capture.output(s, file = "raport.txt",append=TRUE)
      }
    }
  }
}

data_with_NA<-read.csv2("Dane.csv",header=TRUE)
data<-Remove_NA(data_with_NA)
Descriptive_statistics(data)






