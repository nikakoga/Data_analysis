setwd("C:/Users/weron/Documents/Studia/PP/4 semestr/SAD/Projekt")
dane<-read.csv2("Dane.csv",header=TRUE)
dane


#This way I can count how many groups there is
names(dane)[1] <- "ID"
my_table<-table(dane$ID)
my_table
how_many_groups<-length(names(my_table))
how_many_groups

for (col_name in names(dane)) {
  cat("Column ", col_name, ":\n")
  print(dane[[col_name]])
}
