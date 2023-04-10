setwd("C:/Users/weron/Documents/Studia/PP/4 semestr/SAD/Projekt")

data_with_NA<-read.csv2("Dane.csv",header=TRUE)
data_with_NA

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

install.packages("Hmisc")
library("Hmisc")

deal_with_NA<-function(x){
  
  vec_for_NA_in_column<-c()
  counter_NA_cells=0
  for (c in 1:ncol(x)) {
    for(r in 1:nrow(x))
    {
      # Check if the element is NA or empty
      if (is.na(col[r]) || col[r] == "") {
        vec_for_NA_in_column[counter_NA_cells]=r
        counter_NA_cells=counter_NA_cells+1
      }
    }
     if(length(vec_for_NA_in_column)>0)
     {
       #DORÓB ZAPIS DO PLIKU
       # Print a message indicating that the element is missing
       message(paste0("Missing value found at row ", r,"in index",cat(vec_for_NA_in_column,sep=" ")))
       
       if (is.numeric(x[, c]))
       {
         
       }
       
     }
  }
}

#Dealing with NA in data
impute_median<-function(df){
  #Creating logical vector that informs which columns contains numerical data
  is_numeric<-sapply(df,is.numeric)
  
  #selecting numeric and non numeric data
  data_numeric<-df[,is_numeric]
  data_not_numeric<-df[,!is_numeric]
  
  #changin NA in numeric data for median
 
  

    
  
  # Deleting rows with NA in columns that are non numeric
  data_not_numeric <- data_not_numeric[complete.cases(data_not_numeric), ]
  
  # Connecting data again in one data_frame
  df_final <- cbind(data_not_numeric,df_imputed)
  
  return (df_final)
}



Remove_NA <- function(df){
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
#IDEA 



for (col_name in names(dane)) {
  cat("Column ", col_name, ":\n")
  print(dane[[col_name]])
}
