# Sports_Analytics Project
# Cleaning Data

# load libraries
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(readxl)

  
# read in xlsx file. only reading in the sheet with data. Must use na = "NA" to read this in.
  data <- read_xlsx("../data/combine_draft_grades_04-20.xlsx", sheet = "combine_draft_grades_04-20",
                    na = "NA")
  
# turn data into a data.table  
  data <- data.table(data)
  data
  
# create csv of the data called rawdata.csv so it is easier to work with
  write.csv(data,"../data/rawdata.csv", row.names = F)
  
# view missing data - lots of missing data
  colSums(is.na(data))  
  
# convert Ht to numeric height in inches variable
  table(data$Ht, exclude = NULL)
  data <- data %>% separate(Ht,c("feet", "inches"), sep = "'", convert = TRUE, remove = FALSE) %>% 
    mutate(Ht = 12*feet + inches) %>% select(-c("inches","feet"))
  table(data$Ht, exclude = NULL)

### Cleaning and Creating Draft Variables #######    
# create a dummy variable 1 if drafted 0 if not drafted
  data$draft_dummy <- ifelse(is.na(data$Pick),0,1)
# change Pick NA's to a numeric variable
  table(data$Pick, exclude = NULL)
  # Change NA's to 300, the highest pick a player can be is 256, if we change NA's to 300, this allows us to keep the undrafter players in our analysis
  data$Pick <- ifelse(is.na(data$Pick),300,data$Pick)
  table(data$Pick, exclude = NULL)

### Approximate Value (Career_AV and Draft_AV) #####  
  table(data$Career_AV, exclude = NULL) # 2 negative numbers and 6104 NAs
  table(data$Draft_AV, exclude = NULL) # 3 negative numbers and 6353 NAs
  
  data[Career_AV<0,] 
  data[Draft_AV<0,]
  
  # I think we should change this negative values to 0
  data$Career_AV <- ifelse(data$Career_AV<0,0,data$Career_AV)
  data$Draft_AV <- ifelse(data$Draft_AV<0,0,data$Draft_AV)
  
  # players with NA for Career_AV
  table(data[is.na(Career_AV),]$Career_AV, data[is.na(Career_AV),]$G, exclude = NULL)
  table(data[is.na(G) | G<5,]$Career_AV, data[is.na(G) | G<5,]$G, exclude = NULL)
  data[is.na(Career_AV) & G>=0,]
  data[Career_AV<1 & G<5,]
  
  # this is a decision we need to make, but I think we should make all NAs to 0, and...
  # we should change add 1 to all Non NA Career_AV values. This was we can differentiate players with 0 AV to players who never even played a game.
  # Created a new variable called Career_AV_New for now
  # data$Career_AV_New <- data$Career_AV + 1
  # data$Career_AV_New <- ifelse(is.na(data$Career_AV_New),0,data$Career_AV_New)
  # table(data$Career_AV_New, exclude = NULL)
  data$Career_AV_New <- data$Career_AV
  data$Career_AV_New <- ifelse(is.na(data$Career_AV_New),0,data$Career_AV_New)
  table(data$Career_AV_New, exclude = NULL)

  
  
  
# view missing data - lots of missing data
  colSums(is.na(data))

### NFL Stat Variables - G, Passing_Cmp, Passing_Att, Passing_Yds, Passing_TD, Passing_Int, Rushing_Att, Rushing_Yds, Rushing_TD, Receiving_Rec, Receiving_Yds, Receiving_TD,Solo,Int,Sk
  # Change NA to 0 for these variables
  stats <- c("G","Passing_Cmp","Passing_Att","Passing_Yds","Passing_TD","Passing_Int","Rushing_Att","Rushing_Yds","Rushing_TD","Receiving_Rec","Receiving_Yds","Receiving_TD","Solo","Int","Sk")
  data[,(stats) := lapply(.SD,nafill,fill=0),.SDcols = stats]
  
### Need to find the average approximate value per game
  data[Career_AV_New>140,]
  data$AV_game <- round((data$Career_AV_New/data$G), digits = 2)
  
  table(data$Career_AV_New, exclude = NULL)
  table(data$G, exclude = NULL)
  
  table(data$AV_game, exclude = NULL)
  data[AV_game=="NaN",]
  data[AV_game==0,]
  
  
# create csv of the data called clean_data.csv so it is easier to work with
  write.csv(data,"../data/clean_data.csv", row.names = F)  
  
  
################## Combine Data plus Draft Pick and Career_AV ###########################################################################################  
# complete combine data - this are players with no missing data in any combine variables
  CombineVars <- c("college_combine","Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle","pos_combine","combine_yr")
  
  combine_data <- data[,c("player_name_pfr","college_combine","Ht","Wt","X40yd","Vertical",
                          "Bench","Broad_Jump","X3Cone","Shuttle","pos_combine","combine_yr",
                          "Pick","Career_AV_New","draft_dummy")]
  # keep only players who were invited to the combine- player_name_pfr is not missing
  combine_data <- combine_data[complete.cases(combine_data[,c("player_name_pfr")]),]  # 5785 players
  colSums(is.na(combine_data))
  
  # keep only players with no missing data in any combine variables- 2449 players
  # Every play does not have to do every drill, so there are less players with complete combine data
  complete_combine_data <- combine_data[complete.cases(combine_data[,c("player_name_pfr","Ht","Wt","X40yd",
                                                                       "Vertical","Bench","Broad_Jump","X3Cone",
                                                                       "Shuttle","combine_yr")]),] # 2449 players
  complete_combine_data

  # create csvs for combine data
  write.csv(combine_data,"../data/combine_data.csv", row.names = F)
  write.csv(complete_combine_data,"../data/complete_combine_data.csv", row.names = F)
  
  
  