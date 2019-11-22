#Data for model building

#Load in packages
library(pacman)
p_load(ballr, xgboost, caTools, dplyr, pbapply, tidyverse, purrr)

#-----Getting All Previous Player Data Since 1980 --------

#----Function to load all seasons - advanced statistics
advanced_library <- function(x){
  year <- NBAPerGameAdvStatistics(x) %>% dplyr::distinct()
  
  #To get season year in dataframe
  year <- cbind(year, x)
  
}


#Apply function to get all seasons since 1980
years <- c(1980:2019)
advanced_database <- lapply(years, advanced_library)

#Unlist the list into a dataframe
advanced_final <- bind_rows(advanced_database, .id = "column_label")

#getting total season stats for traded players
advanced_final <- advanced_final %>% group_by(player, age) %>% filter(row_number() == 1) %>% ungroup()

#-----Function to load all seasons - basic statistics
pergame_library <- function(x){
  year <- NBAPerGameStatistics(x)
  
  #To get season year in dataframe
  year <- cbind(year, x)
  
}

#Apply function
years <- c(1980:2019)
pergame_database <- lapply(years, pergame_library)

#Unlist the list into a dataframe
pergame_final <- bind_rows(pergame_database, .id = "column_label")

#Getting total seasons for traded
pergame_final <- pergame_final %>% group_by(player, age) %>% filter(row_number() == 1) %>% ungroup()


#-------Join the two to get a full player database
full_player_stats <- inner_join(pergame_final, advanced_final, by = c("player", "pos", "age", "x")) %>% as.data.frame()


#Remove not needed
rm(pergame_database, advanced_database, advanced_library, pergame_library, years)

#Getting rid of * in player column of full_player_stats
full_player_stats$player <- gsub("[*].*$","",full_player_stats$player)

#select only variables needed
full_player_stats <- full_player_stats %>% dplyr::select(player, age, pos, x, g.x, ows, dws, trb, ast, pts, tspercent, 
                                                         x3par, usgpercent, vorp, mp.x)


#Renaming season column 
colnames(full_player_stats)[4] <- "Season"


#Remove all players less than 21 games
full_player_stats2 <- full_player_stats %>% filter(g.x > 40)


#Read in All NBA selections

#Make Sure CSV Saved in UTF-8 Format to preserve accents
AllNBAPlayers <- read_csv("Documents/ThunderProject/AllNBAPlayers.csv")

AllNBAPlayers$Lg <- NULL

#Cleaning data prior to join
AllNBAPlayers <- AllNBAPlayers %>% gather(key = "Pos", value = "Player", Big:GuardB, -Tm)

#Changing season label format
AllNBAPlayers <- AllNBAPlayers %>% mutate(Season = as.numeric(paste(substr(AllNBAPlayers$Season, start = 1, stop = 2),
                                                                    str_sub(AllNBAPlayers$Season, start = -2), sep = "")))
#2000 Adjustment
AllNBAPlayers$Season <- ifelse(AllNBAPlayers$Season == 1900, 2000, AllNBAPlayers$Season)

#Filtering all seasons after 1988 (Three all NBA teams vs. Two)
AllNBAPlayers <- AllNBAPlayers %>% filter(Season > 1988) 

#Getting rid of position after name in dataframe
AllNBAPlayers <- AllNBAPlayers %>% separate(Player, into = c('player', 'extra'), sep = '\\s(?=\\S*?$)', convert = TRUE) %>% 
  select(Season, Tm, Pos, player)




#Join to full_player stats in preparation for modeling
full_player_stats3 <- left_join(full_player_stats2, AllNBAPlayers) 


#All NBA flag column
full_player_stats3 <- full_player_stats3 %>% mutate(AllNBA = ifelse(is.na(Tm) == TRUE, 0, 1)) 


#Filtering after 1988 season
full_player_stats3 <- full_player_stats3 %>% filter(Season > 1988)

#Confirming that # of all NBA players in full_player_stats2 = 465 (Number of all nba players since 1989)
full_player_stats3 %>% summarise(tot = sum(na.omit(AllNBA)))

#Cleaning further

#Selecting only variables needed for modeling 
full_player_stats4 <- full_player_stats3 %>% select(Season, pos, ows:pts, usgpercent, vorp, AllNBA)


#Scaling by season
full_player_stats4 <- full_player_stats4 %>% group_by(Season) %>% mutate(ows = scale(ows),
                                                                         dws = scale(dws),
                                                                         usgpercent = scale(usgpercent),
                                                                         trb = scale(trb),
                                                                         ast = scale(ast), 
                                                                         pts = scale(pts),
                                                                         vorp = scale(vorp)) %>% ungroup() %>%
  select(pos:AllNBA)


#Position category
full_player_stats4$pos <- ifelse(full_player_stats4$pos %in% c("PG", "SG", "SG-SF", "PG-SG", "PG-SF", "SG-PF"), "Guard",
                                 ifelse(full_player_stats4$pos %in% c("SF", "PF", "SF-SG", "PF-SF", "SF-PF"), "Forward", "Center"))






#-------Modeling Prep-------
#Training Set Split
set.seed(1998)
sample <- sample.split(full_player_stats4$AllNBA,SplitRatio = 0.75) 

train <- subset(full_player_stats4,sample == TRUE)
test<- subset(full_player_stats4,sample == FALSE)

#One hot encoding for positional 
labels <- train$AllNBA
ts_label <- test$AllNBA
train2 <- model.matrix(~.+0, data = train[,-9])
test2 <- model.matrix(~.+0,data = test[,-9])


#Preparing 
dtrain <- xgb.DMatrix(data = train2,label = labels) 
dtest <- xgb.DMatrix(data = test2,label=ts_label)



#Grid search for tuning parameters
searchGridSubCol <- expand.grid(subsample = c(0.8,0.9,1.0), 
                                colsample_bytree = c(0.6, 0.8, 1),
                                max_depth = c(4,6,8,10),
                                min_child = seq(1), 
                                eta = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)
)

ntrees <- 100


system.time(
  loglossHyperparameters <- pbapply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = ntrees, nfold = 10, showsd = TRUE, 
                             metrics = "logloss", verbose = TRUE, "eval_metric" = "logloss",
                             "objective" = "binary:logistic", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate
                             , print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    test_logloss <- tail(xvalidationScores$test_logloss_mean, 1)
    train_logloss <- tail(xvalidationScores$train_logloss_mean,1)
    output <- return(c(test_logloss, train_logloss, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))
    }))


outputallnba <- as.data.frame(t(loglossHyperparameters))

colnames(outputallnba) <- c("Testlogloss", "Trainlogloss", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild")


bstmodelcv <- xgb.cv(data =  dtrain, nrounds = 100, nfold = 10, showsd = TRUE, 
                         metrics = "logloss", verbose = TRUE, "eval_metric" = "logloss",
                         "objective" = "binary:logistic", "max.depth" = 4, "eta" = 0.10,                               
                         "subsample" = 0.8, "colsample_bytree" = 0.6,
                          print_every_n = 10, "min_child_weight" = 1, booster = "gbtree",
                         early_stopping_rounds = 10, 
                      watchlist = list(val=dtest,train=dtrain))

bstmodel <- xgboost(data =  dtrain, nrounds = bstmodelcv$best_iteration, showsd = TRUE,
                    verbose = TRUE,
                      "objective" = "binary:logistic", "max.depth" = 4, "eta" = 0.10,
                      "subsample" = 0.8, "colsample_bytree" = 0.6,
                      print_every_n = 10, "min_child_weight" = 1, booster = "gbtree",
                      early_stopping_rounds = 10)


#Predicting using model
library(caret)

#model prediction on data used to train
xgbpred <- predict (bstmodel,dtrain)
xgbpred1 <- ifelse (xgbpred > 0.5,1,0)

#confusion matrix
confusionMatrix (table(xgbpred1, labels))

#model prediction on test set
xgbpred <- predict (bstmodel,dtest)
xgbpred1 <- ifelse (xgbpred > 0.5,1,0)

#confusion matrix too see results
confusionMatrix (table(xgbpred1, ts_label))


#view variable importance plot
mat <- xgb.importance (feature_names = colnames(train2),model = bstmodel)
xgb.plot.importance (importance_matrix = mat[1:10]) 


#Predicting probs on projected careers df produced by distance based similarities
#Read in projected careers csv created earlier
projected_careers <- read_csv("Documents/ThunderApplication/Data/projeced_careers.csv")

#Selected Variables
projected_careers_sub <- projected_careers %>% ungroup() %>% select(season, pos, PPG:VORP)

#Changing column names for predictions
colnames(projected_careers_sub) <- c("Season", "pos", "pts", "ast", "trb", "ows", "dws", "usgpercent",
                                     "vorp")

projected_careers_sub <- projected_careers_sub %>% dplyr::select(Season, pos, ows, dws,
                                                                 trb, ast, pts, usgpercent, vorp)

projected_careers_sub$pos <- ifelse(projected_careers_sub$pos %in% c("PG", "SG", "SG-SF", "PG-SG", "PG-SF", "SG-PF"), "Guard",
                                ifelse(projected_careers_sub$pos %in% c("SF", "PF", "SF-SG", "PF-SF", "SF-PF"), "Forward", "Center"))


#Scale 
projected_careers_sub <- projected_careers_sub %>% group_by(Season) %>% mutate(ows = (ows - mean(full_player_stats$ows))/sd(full_player_stats$ows),
                                                                               dws = (dws - mean(full_player_stats$dws))/sd(full_player_stats$dws),
                                                                         trb = (trb - mean(full_player_stats$trb))/sd(full_player_stats$trb),
                                                                         ast = (ast - mean(full_player_stats$ast))/sd(full_player_stats$ast), 
                                                                         pts = (pts - mean(full_player_stats$pts))/sd(full_player_stats$pts),
                                                                         usgpercent = (usgpercent - mean(na.omit(full_player_stats$usgpercent)))/sd(na.omit(full_player_stats$usgpercent)),
                                                                         vorp = (vorp - mean(full_player_stats$vorp))/sd(full_player_stats$vorp)) %>% ungroup() %>%
  select(pos:vorp)


projected_careers_sub_m <- model.matrix(~.+0, data = projected_careers_sub)


#Putting predictions in dataframe
options(scipen = 999)
projected_careers$prob <- round(predict (bstmodel,projected_careers_sub_m), 4)


#Write to csv with probs
write_csv(projected_careers, "projected_career_wprobs.csv")

