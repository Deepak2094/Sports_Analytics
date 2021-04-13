# 02_Modeling
# load libraries
  library(data.table)
  library(useful)

# read in data
  df <- data.table(read.csv("../data/clean_data.csv"))

# average approximate value per game by Pick
# only looking at drafted players
  drafted<-df[Pick!=300,] # 4058 players drafted
  # change AV_game NAs to 0
  drafted$AV_game <- ifelse(is.na(drafted$AV_game),0,drafted$AV_game)
  drafted[,Avg_AV_Pick:=round(mean(AV_game, na.rm = T),digits = 2), by=Pick]
  table(drafted$AV_game,exclude = NULL)
  table(drafted$Avg_AV_Pick,exclude = NULL)

  drafted
  
  plot(drafted$Pick,drafted$Avg_AV)
  # loses model to estimate an average AV by Pick
  demo_model <- loess(drafted$Avg_AV ~ drafted$Pick, span=.5)
  
  drafted$expect_AV <- predict(demo_model,drafted$Pick)
  drafted
  
  ggplot(drafted, aes(Pick, Avg_AV_Pick)) + geom_point() + geom_line(aes(Pick, expect_AV, color='red'),size=1.5)
  ggplot(drafted, aes(Pick, AV_game)) + geom_point()
  ggplot(drafted, aes(Pick, AV_game)) + geom_point() + geom_line(aes(Pick, expect_AV, color='red'),size=1.5)
  # ggplot(drafted, aes(Pick, AV_game)) + geom_point(aes(color = pos_draft)) + geom_line(aes(Pick, expect_AV, color='red'),size=1.5)
  # ggplot(drafted, aes(Pick, AV_game)) + geom_point(aes(color = FRANCHISE)) + geom_line(aes(Pick, expect_AV, color='red'),size=1.5)
  
  
# value above expected
  drafted$VAE <- drafted$AV_game - drafted$expect_AV

  drafted[,c("player_name_pfr","pos_draft","AV_game","expect_AV","VAE","college_combine","draft_year","Pick")][order(-VAE)]
# change FRANCHISE LVR to RAI
  drafted$FRANCHISE[drafted$FRANCHISE =="LVR"] <- "RAI"
  str(drafted)
  drafted[,mean(VAE), by=FRANCHISE][order(-V1)]
  drafted[,mean(VAE), by=pos_draft][order(-V1)]
  
  drafted[,mean(VAE), by=Rnd][order(-V1)][order(Rnd)]
  
#### do this for every position?? I actually don't think this is necessary ####
  table(drafted$pos_draft, exclude = NULL)
  drafted[,mean(VAE), by=FRANCHISE][order(-V1)]
  drafted[,mean(VAE), by=pos_draft][order(-V1)]
  
  Oline <- drafted[pos_draft %in% c("C","G","OL","T"),]
  Oline
  plot(Oline$Pick,Oline$Avg_AV)
  # loses model to estimate an average AV by Pick
  demo_model <- loess(Oline$Avg_AV ~ Oline$Pick, span=.5)
  
  Oline$expect_AV_pos <- predict(demo_model,Oline$Pick)
  Oline
  ggplot(Oline, aes(Pick, Avg_AV_Pick)) + geom_point() + geom_line(aes(Pick, expect_AV_pos, color='red'),size=1.5)
  ggplot(Oline, aes(Pick, AV_game)) + geom_point()
  ggplot(Oline, aes(Pick, AV_game)) + geom_point() + geom_line(aes(Pick, expect_AV_pos, color='red'),size=1.5)
  ggplot(Oline, aes(Pick, AV_game)) + geom_point(aes(color = FRANCHISE)) + geom_line(aes(Pick, expect_AV_pos, color='red'),size=1.5)

  Oline$VAE <- Oline$AV_game - Oline$expect_AV_pos
  Oline[,mean(VAE), by=FRANCHISE][order(-V1)]
  
  
  
  
    
  
  
  