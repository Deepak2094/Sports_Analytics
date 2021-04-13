# 02_Modeling
# load libraries
  library(data.table)
  library(useful)

# read in data
  cc <- data.table(read.csv("../data/complete_combine_data.csv"))
  df <- data.table(read.csv("../data/clean_data.csv"))
  
##### K-means clustering #######
  set.seed(1234)
  clusters <- kmeans(x=cc[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 5)

  clusters
  
##### subset data by position ####
  table(cc$pos_combine)

  wr <- cc[pos_combine=="WR",]
  qb <- cc[pos_combine=="QB",]
  rb <- cc[pos_combine %in% c("RB","FB"),]  
  OffLine <- cc[pos_combine %in% c("C","OG","OL","OT"),]  
  DefLine <- cc[pos_combine %in% c("DE","DL","DT","EDGE"),]    
  te <- cc[pos_combine=="TE",]    
  lb <- cc[pos_combine%in%c("ILB","LB","OLB"),]
  db <- cc[pos_combine%in%c("CB","S"),]

## clusters for each position group ##
## WR ###  
  wr_clust <- kmeans(x=wr[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 3)
  wr_clust
  plot(wr_clust, data=wr)
  wr_clust_fit <- FitKMeans(wr[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], max.clusters=10, nstart=25)
  wr_clust_fit  
  PlotHartigan(wr_clust_fit) # 3 or 4 looks like good number of clusters
  wr_clust_3 <- kmeans(x=wr[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 3)
  wr_clust_4 <- kmeans(x=wr[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 4)

  wr <- cbind(wr, cluster3 = wr_clust_3$cluster)
  wr <- cbind(wr, cluster4 = wr_clust_4$cluster)

# average Pick, Career AV, and Draft dummy by cluster  
  wr[,mean(Pick), by=cluster3]
  wr[,mean(Career_AV_New), by=cluster3]
  wr[,mean(draft_dummy), by=cluster3]
  
  wr[,mean(Pick), by=cluster4][order(cluster4)]
  wr[,mean(Career_AV_New), by=cluster4][order(cluster4)]
  wr[,mean(draft_dummy), by=cluster4][order(cluster4)]
  
## Offensive Line ###
  ol_clust_fit <- FitKMeans(OffLine[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], max.clusters=10, nstart=25)
  ol_clust_fit  
  PlotHartigan(ol_clust_fit) # 3, 4 , 5 look like good number of clusters
  ol_clust_3 <- kmeans(x=OffLine[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 3)
  ol_clust_4 <- kmeans(x=OffLine[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 4)
  ol_clust_5 <- kmeans(x=OffLine[,c("Ht","Wt","X40yd","Vertical","Bench","Broad_Jump","X3Cone","Shuttle")], centers = 5)
  ol_clust_3
  ol_clust_4
  ol_clust_5
  
  OffLine <- cbind(OffLine, cluster3 = ol_clust_3$cluster)
  OffLine <- cbind(OffLine, cluster4 = ol_clust_4$cluster)
  OffLine <- cbind(OffLine, cluster5 = ol_clust_5$cluster)
  
  # average Pick, Career AV, and Draft dummy by cluster  
  ol_clust_3
  OffLine[,mean(Pick), by=cluster3][order(cluster3)]
  OffLine[,mean(Career_AV_New), by=cluster3][order(cluster3)]
  OffLine[,mean(draft_dummy), by=cluster3][order(cluster3)]
  ol_clust_4
  OffLine[,mean(Pick), by=cluster4][order(cluster4)]
  OffLine[,mean(Career_AV_New), by=cluster4][order(cluster4)]
  OffLine[,mean(draft_dummy), by=cluster4][order(cluster4)]  
  ol_clust_5
  OffLine[,mean(Pick), by=cluster5][order(cluster5)]
  OffLine[,mean(Career_AV_New), by=cluster5][order(cluster5)]
  OffLine[,mean(draft_dummy), by=cluster5][order(cluster5)] 
  
  ggplot(OffLine, aes(x=Pick, y= Career_AV_New)) + geom_point()
  ggplot(OffLine, aes(x=Pick, y= Career_AV_New, color = factor(cluster3))) + geom_point()

    

  
  
    