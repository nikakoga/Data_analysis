###TRYB WSADOWY
#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
# stop("Nalezy podejsc co najmniej jeden argument wejsciowy")
#}
#dane <- read.csv(file=args[1], header=TRUE)
####TRYB WSADOWY


setwd("C:/Users/weron/Documents/Studia/PP/4_semestr/SAD/Projekt")
library("Hmisc")


Replace_blank_with_NA<-function(x)
{
  x<-replace(x,x=='',NA)
  return(x)
}

Remove_NA <- function(df_old) {
  change_report <- c()
  
  df <- Replace_blank_with_NA(df_old)
  
  for (col in names(df)) {
    # Check if NA in numeric column
    if (is.numeric(df[[col]]) & any(is.na(df[[col]]))) {
      NA_rows <- which(is.na(df[[col]]))  # get rows with missing values
      for (row in NA_rows) {
        # Calculate median based on values in the same group
        group_value <- df[[1]][row]
        median_value <- median(df[[col]][df[[1]] == group_value], na.rm = TRUE)
        df[[col]][row] <- median_value
        change_report <- c(change_report, paste("Missing data in column", col, "in group", df[row,1], "replacing with a median", median_value))
      }
    }
  }
  
  #Now every NA in numeric columns are gone
  #I delete every row with NA in non numeric column
  df <- df[complete.cases(df), ]
  
  if (nrow(df_old) > nrow(df)) {
    deleted <- paste("Lack of data in non-numeric column. Removed rows index:", paste(setdiff(rownames(df_old), rownames(df)), collapse = ", "))
    change_report <- append(change_report, deleted)
  }
  
  if (length(change_report) > 0) {
    writeLines(change_report, "raport.txt")
  }
  
  return(df)
} 


count_groups <- function(column) {
  unique_elements <- unique(column)
  num_unique <- length(unique_elements)
  counts <- table(column)
  
  df <- data.frame(ID = names(counts), count = as.numeric(counts))
  df
  write("\nGROPUS AND THEIR SIZE____________________\n","raport.txt",append=TRUE)
  write.table(df,"raport.txt",append=TRUE, col.names = FALSE,row.names=FALSE)
  
  
  
}

Outliers_detection<-function(df)
{
  splitted_groups<-split(df,df[1])
  groupnames<-names(splitted_groups)
  
  write("\n\nOUTLIERS____________________","raport.txt",append=TRUE)
  
  pdf(file= "Outliers.pdf" )
  
  
  colnames<-names(df[,-1])
  par(mfrow = c(1, length(splitted_groups)))
  for(col in colnames)
  {
    for(group in groupnames)
    {
      if(is.numeric(splitted_groups[[group]][[col]]))
      {
        
        boxplot(splitted_groups[[group]][[col]],
                xlab=group,
                ylab = col)
        outliers<-boxplot.stats(splitted_groups[[group]][[col]])$out
        mtext(paste("Outliers: ", paste(outliers, collapse = ", ")))
        
        
        if(length(outliers)>0)
        {
          capture.output(cat(col,group,outliers,"\n"), file = "raport.txt",append=TRUE)
        }
        else
        {
          capture.output(cat(col,group,"no outliners","\n"), file = "raport.txt",append=TRUE)
        }
        
      }
    }
    write("\n","raport.txt",append = TRUE)
  }
  
  dev.off()
}

Characteristics<-function(df)
{
  write("\n\nCHARACTERISTICS____________________","raport.txt",append=TRUE)
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

Descriptive_statistics<-function(df)
{
  count_groups(df[1])
  Outliers_detection(df)
  Characteristics(df)
  
}

Homogenity_of_variance_raport<-function(values, groups)
{
  library(car)
  
  p.value = leveneTest(values, groups)$"Pr(>F"[1]
  if(p.value < 0.05){
    write(paste("\n",colname,"wariancja niehomogeniczna"),"raport.txt",append=TRUE )
    return(FALSE)
  }
  return (TRUE)
}

Normal_distribution_raport<-function(data,colname,groupname)
{
  p.value = shapiro.test(data)$p.value
  if(p.value < 0.05){
    write(paste(colname,groupname,"p< 0.05 - nie można założyć zgodności z rozkładem normalnym"),"raport.txt",append=TRUE )
    return(FALSE)
  }
  return (TRUE)
}

Which_test_to_apply<-function(df)
{
  write("\n\nNORMAL DISTRIBUTION AND HOMOGENEITY OF VARIANCE____________________\n","raport.txt",append=TRUE )
  splitted_groups<-split(df,df[1])
  groupnames<-names(splitted_groups)
  colnames<-names(df[,-1])
  
  Not_homogenic<-c()
  Not_normal<-c()
  
  for(col in colnames)
  {
    if(is.numeric(df[[col]]))
    {
      if(!Homogenity_of_variance_raport(df[[col]], df[[1]]))
      {
        Not_homogenic<-append(Not_homogenic, col)
      }
      
      for(group in groupnames)
      {
        
        if(is.numeric(splitted_groups[[group]][[col]]))
        {
          if(!Normal_distribution_raport(splitted_groups[[group]][[col]],col,group))
          {
            Not_normal<-append(Not_normal,col)
          }
        }
      }
    }
    
  }
  
  return(list(Not_normal,Not_homogenic))
}

Kruskal_test<-function(dane,grupy,col)
{
  # kruskal.test(col ~ grupy, data = dane)
  # pvalueKWtest <- kruskal.test(col ~ grupy, data = dane)$p.value
  # pvalueKWtest
  # 
  # if(pvalueKWtest < 0.05){
  #   write(paste(col, "Test Kruskala < 0.05 - są różnice pomiędzy grupami"),"raport.txt",append=TRUE)
  #   return (TRUE)
  # }else{
  #   write(paste(col, "Test Kruskala > 0.05 - brak różnic pomiędzy grupami"),"raport.txt",append=TRUE)
  #   return (FALSE)
  #   
  # }
}

Anova_test<-function(dane,grupy,col)
{
  # pvalue<-summary(aov(col ~ grupy,data=dane))[[1]][["Pr(>F"]][[1]]
  # 
  # if(pvalue<0.05)
  # {
  #   return (TRUE)
  # }
  # else{
  #   return(FALSE)
  # }
}
Apply_test<-function(df,Not_normal,Not_homogenic){
  
  write("\n\nCORRELATION ANALYSIS____________________","raport.txt",append=TRUE)
  splitted_groups<-split(df,df[1])
  groupnames<-names(splitted_groups)
  
  colnames<-names(df[,-1])
  
  for(col in colnames)
  {
      if(is.numeric(df[[col]]))
      {
        if(group_number>2)
        {
          if(col %in% Not_normal||col %in% Not_homogenic)
          {
            Kruskal_test(df,groupnames,col)
          }
          
        }
        else
        {
          
        }
    }
  }
}

Correlation_analysis<-function(df)
{
  Result<-Which_test_to_apply(df)
  Not_normal<-Result[1]
  Not_homogenic<-Result[2]
}

data_with_NA<-read.csv2("Dane.csv",header=TRUE)
data<-Remove_NA(data_with_NA)
Descriptive_statistics(data)
Correlation_analysis(data)

# require(knitr)
# my_text <- "Hello pls start working"
# knit(text = my_text, output = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA.pdf")
# 
# install.packages("pdftools")
# library(pdftools)
# my_pdf<-"C:/Users/weron/Documents/Studia/PP/4_semestr/SAD/Projekt/TEST.pdf"
# my_picture<-"C:/Users/weron/Documents/Studia/PP/4_semestr/SAD/Projekt/OBRAZ.jpg"
# 
# 
# # konwertujemy plik PDF na obiekt R
# pdf <- pdf_convert(my_pdf)
# 
# # wstawiamy obraz
# pdf <- pdf_insert_image(pdf, "OBRAZ.jpg", page = n_pages(pdf)+1)
# 
# # zapisujemy zmodyfikowany plik PDF
# pdf_save(pdf, "TEST.pdf")
# 
# # usuwamy obiekt R
# pdf_close(pdf)



