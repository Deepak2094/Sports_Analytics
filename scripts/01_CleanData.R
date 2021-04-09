# Sports_Analytics Project
# Cleaning Data

# load libraries
  library(data.table)
  library(readxl)

  
# read in xlsx file. only reading in the sheet with data. Must use na = "NA" to read this in.
  data <- read_xlsx("../data/combine_draft_grades_04-20.xlsx", sheet = "combine_draft_grades_04-20",
                    na = "NA")
  
# turn data into a data.table  
  data <- data.table(data)
  data  

# view missing data
  colSums(is.na(data))
  
# combine data
  colnames(data)
  combine_data <- data[,c("player_name_pfr","college_combine","Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle","pos_combine")]
  combine_data <- combine_data[complete.cases(combine_data[,c("player_name_pfr","Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")]),]
  combine_data
  colSums(is.na(combine_data))  
  