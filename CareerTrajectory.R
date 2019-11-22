#Load in packages
library(pacman)
p_load(ballr, dplyr, tidyverse, readr)

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



#------Calculating Euc Distances between each player----------

#Clean up dataframe
dist_data <- full_player_stats %>% dplyr::select(player:age, x, g.x, gs, mp.x, fgpercent, x3ppercent, 
                                                    x2ppercent, efgpercent, ftpercent, orb, drb, trb, ast, stl, blk,
                                                    tov, pf, pts, per, tspercent, x3par, ftr, orbpercent, drbpercent, trbpercent,
                                                    astpercent, stlpercent, blkpercent, tovpercent, usgpercent, ows, dws, ws,
                                                    ws_48, obpm, dbpm, bpm, vorp)



#Glimpse
glimpse(dist_data)

#Selecting only variables needed: OWS, DWS, TRB, AST, PTS, TSPERCENT, x3par, ftr, usgpercent, vorp
dist_data2 <- dist_data %>% dplyr::select(player, age, ows, dws, trb, ast, pts, tspercent, 
                                                x3par, usgpercent, vorp, mp.x)

#Full Player subset with season for later
full_player_stats <- dist_data %>% dplyr::select(player, pos, age, x, ows, dws, trb, ast, pts, tspercent, 
                                                    x3par, usgpercent, vorp, mp.x)

colnames(full_player_stats)[4] <- "Season"



#Scale columns for distance calculations
dist_data2[,c(13:22)] <- scale(dist_data2[,c(3:12)])
names1 <- colnames(dist_data2[,c(1:12)])
names2 <- colnames(dist_data2[,c(3:12)]) %>% paste0("scaled") 
names <- c(names1, names2)
colnames(dist_data2) <- names

#Remove nas
dist_data2 <- na.omit(dist_data2)

#Row names = playername_age
dist_data2 <- dist_data2 %>% unite("id", player:age, sep = "_")
dist_data2 <- column_to_rownames(dist_data2, "id")
dist_data2 <- dist_data2 %>% as.matrix()

#Distances
x <- rdist(dist_data2[,11:20])
x <- x %>% as.data.frame()
rownames(x) <- rownames(dist_data2)
colnames(x) <- rownames(dist_data2)


#-------------2019 Players as starting points to career trajectory--------------
#Apply function to get all seasons since 1980
years <- c(2018:2019)
advanced_database <- lapply(years, advanced_library)

#Unlist the list into a dataframe
advanced_library <- bind_rows(advanced_database, .id = "column_label")


#Getting total columns for traded
advanced_nineteen_final <- advanced_library %>% group_by(player, age) %>% filter(row_number() == 1) %>% ungroup()

#Getting 2019 player data - basic stats
pergame_database <- lapply(years, pergame_library)

#Unlist the list into a dataframe
pergame_library <- bind_rows(pergame_database, .id = "column_label")

#Getting total columns for traded
pergame_nineteen_final <- pergame_library %>% group_by(player, age) %>% filter(row_number() == 1) %>% ungroup()

#Join the two to get one 2019 database
full_player_stats_nineteen <- inner_join(pergame_nineteen_final, advanced_nineteen_final,
                                         by = c("player", "pos", "age"))%>% as.data.frame()


#Selecting only variables needed that correspond to euc distances
full_player_stats_nineteen <- full_player_stats_nineteen %>% select(player, age, pos, x.x, ows, dws, trb, ast, 
                                                                    pts, tspercent, usgpercent, 
                                                                    vorp, mp.x)

full_player_stats_nineteen <- na.omit(full_player_stats_nineteen)


#Selecting only 2019 players and Porzingis 2018 version

K <- full_player_stats_nineteen %>%
  filter((player == "Kristaps Porziņģis" & x.x == 2018)) 

J <- full_player_stats_nineteen %>%
  filter((player == "JaKarr Sampson" & x.x == 2018)) 

full_player_stats_nineteen  <- full_player_stats_nineteen %>% filter(x.x == 2019 & player != "JaKarr Sampson") %>%
  rbind(K) %>% rbind(J)



#-------------Career Path Function---------
career_path <-function(y){

#Indexing distance column for player from distance matrix
x1 <- x %>% rownames_to_column('players') 
x1 <- x1[,c('players', y)] 
colnames(x1) <- c("players", "distance")  

#Sorting by euc distance ascending
x1 <- x1 %>% arrange(distance) %>% filter(grepl(str_sub(y, -2, -1), players)) %>%
  mutate(similarity_perc = (1)/(1+distance)) %>% 
  separate(players, into = c("player", "age"), sep = "_") %>%
  head(50)

#Changing class of age
x1$age <- as.numeric(x1$age)

#Get subset of full_player stats to only columns that I need
full_subset <- full_player_stats %>% select(player, age, pts, ast, trb, ows, dws, vorp, tspercent, usgpercent)
selector <- full_subset %>% filter(player %in% x1$player & age >= unique(x1$age)) %>% left_join(x1, by = c("player"="player"))

#Calculating changes in selected statistics from year to year after age of player
x2 <- selector %>% group_by(player) %>% arrange(age.x) %>% 
  mutate(pts_change = pts-lag(pts),
         weighted_pts = pts_change*similarity_perc,
         ast_change = ast-lag(ast),
         weighted_ast = ast_change*similarity_perc,
         trb_change = trb-lag(trb),
         weighted_trb = trb_change*similarity_perc,
         ows_change = ows-lag(ows),
         weighted_ows = ows_change*similarity_perc,
         dws_change = dws-lag(dws),
         weighted_dws = dws_change*similarity_perc,
         tspercent_change = tspercent-lag(tspercent),
         weighted_tspercent = tspercent_change*similarity_perc,
         usgpercent_change = usgpercent-lag(usgpercent),
         weighted_usgpercent = usgpercent_change*similarity_perc,
         vorp_change = vorp-lag(vorp),
         weighted_vorp = vorp_change*similarity_perc) %>%
        
  group_by(age.x)  %>% 
  summarise(adj_pts_change = sum(weighted_pts)/sum(similarity_perc),
            adj_ast_change = sum(weighted_ast)/sum(similarity_perc),
            adj_trb_change = sum(weighted_trb)/sum(similarity_perc),
            adj_ows_change = sum(weighted_ows)/sum(similarity_perc),
            adj_dws_change = sum(weighted_dws)/sum(similarity_perc),
            adj_tspercent_change = sum(weighted_tspercent)/sum(similarity_perc),
            adj_usgpercent_change = sum(weighted_usgpercent)/sum(similarity_perc),
            adj_vorp_change = sum(weighted_vorp)/sum(similarity_perc),
            tot = n())

#Getting rid of first NA column
x2 <- x2[2:nrow(x2),]

#Combining to player stats
player <- full_player_stats_nineteen %>% filter(player == substr(y, 1, nchar(y)-3))
x2 <- cbind(player, x2)

#Apply yearly change adjustments to players future years
first <- x2$pts[1] + x2$adj_pts_change[1]
PPG <- c(first, rep(NA, nrow(x2)-1))

first <- x2$ast[1] + x2$adj_ast_change[1]
AST <- c(first, rep(NA, nrow(x2)-1))

first <- x2$trb[1] + x2$adj_trb_change[1]
TRB <- c(first, rep(NA, nrow(x2)-1))

first <- x2$ows[1] + x2$adj_ows_change[1]
OWS <- c(first, rep(NA, nrow(x2)-1))

first <- x2$dws[1] + x2$adj_dws_change[1]
DWS <- c(first, rep(NA, nrow(x2)-1))


first <- x2$usgpercent[1] + x2$adj_usgpercent_change[1]
USG <- c(first, rep(NA, nrow(x2)-1))

first <- x2$vorp[1] + x2$adj_vorp_change[1]
VORP <- c(first, rep(NA, nrow(x2)-1))

x2 <- cbind(x2, PPG, AST, TRB, OWS, DWS, USG, VORP)

for(i in 2:nrow(x2)) {
  x2$PPG[i] <- ((x2$adj_pts_change[i]) + x2$PPG[i-1] )
  x2$AST[i] <- ((x2$adj_ast_change[i]) + x2$AST[i-1] )
  x2$TRB[i] <- ((x2$adj_trb_change[i]) + x2$TRB[i-1] )
  x2$OWS[i] <- ((x2$adj_ows_change[i]) + x2$OWS[i-1] )
  x2$DWS[i] <- ((x2$adj_dws_change[i]) + x2$DWS[i-1] )
  x2$USG[i] <- ((x2$adj_usgpercent_change[i]) + x2$USG[i-1] )
  x2$VORP[i] <- ((x2$adj_vorp_change[i]) + x2$VORP[i-1] )
}

#Cleaning dataframe
x2 <- x2 %>% select(player, age.x, pos, PPG:VORP, tot)

#Retirement rule (When 10% players in top 50 retire, player of interest retires)
x2 <- x2 %>% filter(tot > 0.1*tot[1]) %>% filter(PPG > 0 & AST > 0 & TRB > 0)

#Output of function
print(x2)

}


#Getting list of all 2019 players to input into function
full_player_stats_nineteen$id <- paste(full_player_stats_nineteen$player, full_player_stats_nineteen$age, 
                                       sep = "_")

player_names <- full_player_stats_nineteen$id


#Applying career projection to vector of 2019 player names
projected_careers <- lapply(player_names, career_path)

#Unlist projected careers to a dataframe
projected_careers <- bind_rows(projected_careers, .id = "id")
projected_careers[,5:11] <- round(projected_careers[,5:11],1)

#Getting rid of first row of Porzingis and Sampson as these seasons already occured
projected_careers <- projected_careers %>% filter(!(player == "Kristaps Porziņģis" & age.x == 23)) %>% 
  filter(!(player == "JaKarr Sampson" & age.x == 25))

#Season column
projected_careers <- projected_careers %>% group_by(id) %>% mutate(season = 2019+row_number())



#Write to csv
write_csv(projected_careers, "projeced_careers.csv")

#------Data now ready for boosting model application for future all-pro probabilities-------
