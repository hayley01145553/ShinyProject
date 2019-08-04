## DATA Cleansing ### 

# use library 
library(dplyr)
#source("helpers.R")

setwd("~/workspace/shinyProject")
df_raw_homicide = read.csv("./csv_data/homicide-database_raw.csv")
dim(df_raw_homicide)
names(df_raw_homicide)
names(df_raw_homicide) <- tolower(names(df_raw_homicide))

#drop column  
df_homicide <- data.frame()
df_homicide <- df_raw_homicide[ , !(grepl("^agency*",names(df_raw_homicide)))] #agency.code, agency.type, agecncy.name

# check data unique value by ecah cloumn
for(i in 1: length(names(df_homicide))){
  if(i %in% c(1,6)){next} #record.id, incident column skip

  print(paste("colnum name : "  ,names(df_homicide)[i]))
  print(distinct(df_homicide, eval(parse(text=names(df_homicide)[i]))))
  #print(paste("unique value : " , unique(df_homicide[names(df_homicide)[i]])))
  print(rep('*',10))
  
}
#na check 
apply(df_homicide, 2, function(x) any(is.na(x)))

write.csv(df_homicide, file='./csv_data/homicide_data.csv', row.names=F)
