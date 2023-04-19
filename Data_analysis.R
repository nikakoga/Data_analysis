###TRYB WSADOWY
#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
# stop("Nalezy podejsc co najmniej jeden argument wejsciowy")
#}
#dane <- read.csv(file=args[1], header=TRUE)
####TRYB WSADOWY


setwd("C:/Users/weron/Documents/Studia/PP/4_semestr/SAD/Projekt")
library("Hmisc")
library("ggpubr")


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

Homogenity_of_variance_raport<-function(values, groups,colname)
{
  library(car)
  
  p.value = leveneTest(values, groups)$"Pr(>F"[1]
  if(p.value < 0.05){
    write(paste("\n",colname,"\n","wariancja niehomogeniczna"),"raport.txt",append=TRUE )
    return(FALSE)
  }
    write(paste("\n",colname,"\nwariancja homogeniczna"),"raport.txt",append=TRUE )
    return (TRUE)
}

Normal_distribution_raport<-function(value,groups,colname)
{
  p.value = shapiro.test(value)$p.value
  if(p.value < 0.05){
    write(paste(colname,groups,"p < 0.05 - nie można założyć zgodności z rozkładem normalnym"),"raport.txt",append=TRUE )
    return(FALSE)
  }
  write(paste(colname,groups,"p > 0.05 - można założyć zgodność z rozkładem normalnym"),"raport.txt",append=TRUE )
  return (TRUE)
}

Density_normal_and_homogenic_info<-function(df)
{
  library("ggpubr")
  write("\n\nNORMAL DISTRIBUTION AND HOMOGENEITY OF VARIANCE____________________\n","raport.txt",append=TRUE )
  splitted_groups<-split(df,df[1])
  groupnames<-names(splitted_groups)
  colnames<-names(df[,-1])
  
  Homogenic<-c()
  Not_Normal<-c()
  
  pdf(file= "Density.pdf" )
  par(mfrow = c(1, length(splitted_groups)))

  for(col in colnames)
  {
    if(is.numeric(df[[col]]))
    {
      if(Homogenity_of_variance_raport(df[[col]], df[[1]],col))
      {
        Homogenic<-append(Homogenic,col)
      }
      
      for(group in groupnames)
      {
        
        if(is.numeric(splitted_groups[[group]][[col]]))
        {
          if(!Normal_distribution_raport(splitted_groups[[group]][[col]],col,group))
          {
            Not_Normal<-append(Not_Normal,col)
          }
          p<-ggdensity(df[df[[1]] == group, ],
                    x = col,
                    main = paste(col, group, sep = " "),
                    xlab = col
          )
          print(p)
        }
      }
    }
    
  }
  
  dev.off()
  
  return(list(Not_Normal,Homogenic))
}

Kruskal_test<-function(groups,values)
{
  p.value <- kruskal.test(values, groups)$p.value
 
  if(p.value < 0.05){
    write(paste("Test Kruskala",round(p.value,3),"< 0.05 - są różnice pomiędzy grupami"),"raport.txt",append=TRUE)
    Dunn_test(groups,values)
    return (TRUE)
  }else{
    write(paste("Test Kruskala",round(p.value,3), "> 0.05 - brak różnic pomiędzy grupami"),"raport.txt",append=TRUE)
    return (FALSE)
  }
}

Dunn_test<-function(groups,values)
{
  library("dunn.test")
  library("FSA")
  res<-dunnTest(values~groups)
  for (row in 1:nrow(res$res)) {
    p.value <- res$res[row, "P.adj"]
    groupsCompared  <- res$res[row, "Comparison"]
    
    if (p.value < 0.05){
      write(paste("Test Dunna",round(p.value,3),"< 0.05 - są różnice pomiędzy grupami ", groupsCompared),"raport.txt",append=TRUE)
    } else {
      #write(paste("Test Dunna",round(p.value,3), "> 0.05 - brak różnic pomiędzy grupami ", groupsCompared),"raport.txt",append=TRUE)
    }
  }
}

Anova_test<-function(groups,values)
{
  p.value<-summary(aov(values~groups))[[1]][["Pr(>F)"]][[1]]

  if(p.value<0.05)
  {
    write(paste("Test Anova", round(p.value,2),  "< 0.05 - są różnice pomiędzy grupami"), "raport.txt", append=TRUE)
    return (TRUE)
  }
  else{
    write(paste("Test Anova", round(p.value,2), "> 0.05 - brak różnic pomiędzy grupami"), "raport.txt", append=TRUE)
    return(FALSE)
  }
}

Apply_test<-function(df,Normal,Homogenic)
{
  
  write("\n\nCORRELATION ANALYSIS____________________","raport.txt",append=TRUE)
  splitted_groups<-split(df,df[1])
  groupnames<-names(splitted_groups)
  group_number <- length(groupnames)
  
  colnames<-names(df[,-1])
  
  for(col in colnames)
  {
      if(is.numeric(df[[col]]))
      {
        write(paste("\n",col),"raport.txt",append=TRUE)
        if(group_number>2)
        {
          if(col %in% Normal & col %in% Homogenic)
          {
            Anova_test(df[[1]],df[[col]])
          }
          else
          {
            Kruskal_test(df[[1]],df[[col]])
          }
        }
        if(group_number==2){
          if(col %in% Normal & col %in% Homogenic){
            T_Student(df[[1]],df[[col]])
          }
          else{
            Welch(df[[1]],df[[col]])
          }
        }
    }
  }
}

T_Student<-function(groups, values)
{
  res<-t.test(values~groups, var.equal = TRUE)
  if (res$p.value < 0.05) {
    write(paste("Test T-Studenta", round(res$p.value,2),  "< 0.05 - są różnice pomiędzy grupami"), "raport.txt", append=TRUE)
    return (TRUE)
  }
  else{
    write(paste("Test T-Studenta", round(res$p.value,2), "> 0.05 - brak różnic pomiędzy grupami"), "raport.txt", append=TRUE)
    return(FALSE)
  }
}


Welch<-function(groups, values)
{
  res<-t.test(values~groups, var.equal = FALSE)
  if (res$p.value < 0.05) {
    write(paste("Test Welcha", round(res$p.value,2),  "< 0.05 - są różnice pomiędzy grupami"), "raport.txt", append=TRUE)
    return (TRUE)
  }
  else{
    write(paste("Test Welcha", round(res$p.value,2), "> 0.05 - brak różnic pomiędzy grupami"), "raport.txt", append=TRUE)
    return(FALSE)
  }
}

Statistics_tests<-function(df)
{
  colnames<-list(names(df[,-1]))
  
  Result<-Density_normal_and_homogenic_info(df)
  Not_Normal<-as.list(unique((Result)[1][[1]]))
  Normal<- as.list(setdiff(colnames[[1]], Not_Normal))
  Homogenic<-as.list(unique((Result[2])[[1]]))
  # FYI
  # write(paste("\n\nNOT_NORMAL",list(Not_Normal)),"raport.txt",append=TRUE)
  # write(paste("\n\nALL",colnames),"raport.txt",append=TRUE)
  # write(paste("\n\nNORMAL",list(Normal)),"raport.txt",append=TRUE)
  # write(paste("\n\nHOMOGENIC",list(Homogenic)),"raport.txt",append=TRUE)
  Apply_test(df,Normal,Homogenic)
}

data_with_NA<-read.csv2("Dane.csv",header=TRUE)
data<-Remove_NA(data_with_NA)
Descriptive_statistics(data)
Statistics_tests(data)

