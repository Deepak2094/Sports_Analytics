#Importing Required packages
library(tidyverse) 
library(gridExtra)
library(corrplot)
library(leaps)
library(glmnet)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggpubr)
library(factoextra)

#Importing the Fifa data file
NFL<-read.csv("complete_combine_data.csv",header = TRUE)

#Dropping columns
NFL.dc<-NFL[-c(1,2,12,13,14,15)]

# 0. Investigate data
dim(NFL.dc) #Dimesion rows and columns in the dataset
names(NFL.dc) # Column names
str(NFL.dc) # Data Types of each column
summary(NFL.dc) # Statistical Summary of the dataset

# Checking for Null Values
sum(is.na(NFL.dc))  #Checking for Null Values in column
Clean_Data=na.omit(NFL.dc) # Removing the rows with nulls

#2. Preparation of Dataset for K- Means Clustering
Clean_Data$FieldPosition <-NA 

# Multiple IF Condition is used to create the 4 Categories mentioned above
Clean_Data$FieldPosition <- ifelse(Clean_Data$pos_combine=='C'| Clean_Data$pos_combine=='OG'| Clean_Data$pos_combine=='OT'|Clean_Data$pos_combine=='OL'|Clean_Data$pos_combine=='QB'|Clean_Data$pos_combine=='TE'|Clean_Data$pos_combine=='WR','Offensive', 
                                          ifelse(Clean_Data$pos_combine=='CB'| Clean_Data$pos_combine=='DE'| Clean_Data$pos_combine=='DL'|Clean_Data$pos_combine=='DT'|Clean_Data$pos_combine=='EDGE'|Clean_Data$pos_combine=='FB'|Clean_Data$pos_combine=='ILB'|Clean_Data$pos_combine=='LB'|Clean_Data$pos_combine=='OLB'|Clean_Data$pos_combine=='RB'|Clean_Data$pos_combine=='S','Defense','Special'))  

cluster_data<-Clean_Data
#3 . Performing k- Means Clustering
set.seed(20)  
fviz_nbclust(cluster_data, kmeans, method = "wss") # Graph whih shows the bend 
Fifa_Cluster <- kmeans(scale(cluster_data[,1:8]), 3, nstart = 20) # Specifying column 13 and 29 with 8 groups 
Vector<-Fifa_Cluster$cluster


# Table to show The Cluster and split for the three categories
table(Fifa_Cluster$cluster, cluster_data$FieldPosition)

fviz_cluster(Fifa_Cluster, data = cluster_data[,1:8],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

#4. Creating a Player List Data Frame with Cluster Result added 
cluster_data[,"Cluster"] <-Fifa_Cluster$cluster
Player_Dataset<-cluster_data

#5. Exporting the dataset as a csv file to create a Dashboard in Tableau
write.csv(Player_Dataset,'\\Users\\deepak\\Documents\\DEAN\\STAT515\\FinalProject\\Player_Daa\.csv', row.names = FALSE)
