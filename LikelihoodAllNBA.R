#Read in projected careers file with probabilities
library(readr)
library(sciences)
library(matrixStats)
library(pbapply)
library(dplyr)
projected_careers <- read_csv("~/Documents/ThunderApplication/Data/projected_career_wprobs.csv")


#Filtering only the four players
projected_careers_filtered <- projected_careers %>% filter(player %in% c("Luka Dončić", "Kyrie Irving", 
                                                                "Stephen Curry", "Karl-Anthony Towns"))


#Probability Function
probability_function <- function(x, y, z){

  combos <- t(combn(z, x))
  
  probs <- matrix(nrow = nrow(combos), 1)
  
  selected_player <- projected_careers_filtered %>% filter(player == y)
  
  for (i in 1:nrow(probs)) {
    success <- selected_player %>% mutate(trial = row_number(),
                                success = ifelse(trial %in% as.numeric(combos[i,]), "success", "no_success"),
                                adjusted_prob = ifelse(success == "success", prob, (1-prob)), 
                                cum_prob = cumprod(adjusted_prob))
    
    probs[i,1] <- success[nrow(success), ncol(success)] %>% as.numeric()
    
  }
  
  total_prob <- probs %>% as.data.frame() %>% summarise(tot = sum(V1)) %>% as.data.frame()
  
  print(probs)
  
}

#Curry
selections <- c(0:9)

Curry <- pblapply(selections, probability_function, y = "Stephen Curry", z = 9)

Curry <- do.call(rbind,lapply(Curry, colSums)) %>% as.data.frame() %>% mutate(selections = 0:(n()-1),
                                                                              player = "Stephen Curry")
colnames(Curry) <- c("Probability", "Selections", "Player") 


#Doncic
selections <- c(0:15)

Doncic <- pblapply(selections, probability_function, y = "Luka Dončić", z = 15)

Doncic <- do.call(rbind,lapply(Doncic, colSums)) %>% as.data.frame() %>% mutate(selections = 0:(n()-1),
                                                                              player = "Luka Doncic")
colnames(Doncic) <- c("Probability", "Selections", "Player") 

#Towns
selections <- c(0:16)

Towns <- pblapply(selections, probability_function, y = "Karl-Anthony Towns", z = 16)

Towns <- do.call(rbind,lapply(Towns, colSums)) %>% as.data.frame() %>% mutate(selections = 0:(n()-1),
                                                                                player = "KAT")
colnames(Towns) <- c("Probability", "Selections", "Player") 

#Irving
selections <- c(0:12)

Irving <- pblapply(selections, probability_function, y = "Kyrie Irving", z = 12)

Irving <- do.call(rbind,lapply(Irving, colSums)) %>% as.data.frame() %>% mutate(selections = 0:(n()-1),
                                                                              player = "Kyrie")
colnames(Irving) <- c("Probability", "Selections", "Player") 


#Combining all probs
AllSelections <- rbind(Curry, Doncic, Towns, Irving)



#CURRY PROB
sc <- c(1:9)
ld <- c(0:15)
kat <- c(0:16)
kyrie <- c(0:12)

scenarios <- expand.grid(sc, ld, kat, kyrie)
scenarios <- scenarios %>% rowwise() %>% filter(Var1 > max(Var2, Var3, Var4))

                     
scenarios_curry <- left_join(scenarios, Curry, by = c("Var1" = "Selections")) %>% left_join(Doncic, by = c("Var2" = "Selections")) %>% left_join(Towns, by = c("Var3" = "Selections")) %>% 
  left_join(Irving, by = c("Var4" = "Selections")) %>% select(Probability.x, Probability.y, Probability.x.x, Probability.y.y) 

colnames(scenarios_curry) <- c("PlayerOne", "PlayerTwo", "PlayerThree", "PlayerFour")

scenarios_curry[is.na(scenarios_curry)] <- 0

#Row products
scenarios_curry$prob = apply(scenarios_curry, 1, prod, na.rm=T)

#Curry Prob
sum(scenarios_curry$prob)  



#LUKA PROB
sc <- c(0:9)
ld <- c(1:15)
kat <- c(0:16)
kyrie <- c(0:12)

scenarios <- expand.grid(ld, sc, kat, kyrie)
scenarios <- scenarios %>% rowwise() %>% filter(Var1 > max(Var2, Var3, Var4))


scenarios_luka <- left_join(scenarios, Doncic, by = c("Var1" = "Selections")) %>% left_join(Curry, by = c("Var2" = "Selections")) %>% left_join(Towns, by = c("Var3" = "Selections")) %>% 
  left_join(Irving, by = c("Var4" = "Selections")) %>% select(Probability.x, Probability.y, Probability.x.x, Probability.y.y) 

colnames(scenarios_luka) <- c("PlayerOne", "PlayerTwo", "PlayerThree", "PlayerFour")

#Getting rid of impossible scenarios (other players retiring not being able to achieve more All_NBA)
scenarios_luka[is.na(scenarios_luka)] <- 0


#Row products
scenarios_luka$prob = apply(scenarios_luka, 1, prod, na.rm=T)

#Doncic Prob
sum(scenarios_luka$prob)  



#Kyrie PROB
sc <- c(0:9)
ld <- c(0:15)
kat <- c(0:16)
kyrie <- c(1:12)


scenarios <- expand.grid(kyrie, ld, sc, kat)
scenarios <- scenarios %>% rowwise() %>% filter(Var1 > max(Var2, Var3, Var4))


scenarios_kyrie <- left_join(scenarios, Irving, by = c("Var1" = "Selections")) %>% left_join(Doncic, by = c("Var2" = "Selections")) %>% left_join(Curry, by = c("Var3" = "Selections")) %>% 
  left_join(Towns, by = c("Var4" = "Selections")) %>% select(Probability.x, Probability.y, Probability.x.x, Probability.y.y) 

colnames(scenarios_kyrie) <- c("PlayerOne", "PlayerTwo", "PlayerThree", "PlayerFour")

#Getting rid of impossible scenarios (other players retiring not being able to achieve more All_NBA)
scenarios_kyrie[is.na(scenarios_kyrie)] <- 0


#Row products
scenarios_kyrie$prob = apply(scenarios_kyrie, 1, prod, na.rm=T)

#Kyrie Prob
sum(scenarios_kyrie$prob)  




#KAT PROB
sc <- c(0:9)
ld <- c(0:15)
kat <- c(1:16)
kyrie <- c(0:12)



scenarios <- expand.grid(kat, kyrie, ld, sc)
scenarios <- scenarios %>% rowwise() %>% filter(Var1 > max(Var2, Var3, Var4))


scenarios_kat <- left_join(scenarios, Towns, by = c("Var1" = "Selections")) %>% left_join(Irving, by = c("Var2" = "Selections")) %>% left_join(Doncic, by = c("Var3" = "Selections")) %>% 
  left_join(Curry, by = c("Var4" = "Selections")) %>% select(Probability.x, Probability.y, Probability.x.x, Probability.y.y) 

colnames(scenarios_kat) <- c("PlayerOne", "PlayerTwo", "PlayerThree", "PlayerFour")

#Getting rid of impossible scenarios (other players retiring not being able to achieve more All_NBA)
scenarios_kat[is.na(scenarios_kat)] <- 0


#Row products
scenarios_kat$prob = apply(scenarios_kat, 1, prod, na.rm=T)

#Kat Prob
sum(scenarios_kat$prob)  



#TIES PROB
kat <- c(0:16)
kyrie <- c(0:16)
ld <- c(0:16)
sc <- c(0:16)



scenarios <- expand.grid(kat, kyrie, ld, sc)
scenarios <- scenarios %>% rowwise() %>% filter(
  
                                                #All Tie
                                                (Var1 == Var2 &
                                                Var2 == Var3 &
                                                Var3 == Var4) | 
                                                  
                                                  #Three Way Tie
                                                  ( Var1 == Var2 & Var1 == Var3 & Var1 > Var4 ) |
                                                  ( Var1 == Var2 & Var1 == Var4 & Var1 > Var3 ) |
                                                  ( Var1 == Var3 & Var1 == Var4 & Var1 > Var2 ) |
                                                  ( Var2 == Var3 & Var2 == Var4 & Var2 > Var1 ) |
                                                  
                                                  
                                                  #Two Way Tie
                                                 ( Var1 == Var2 & Var1 > Var3 & Var1 > Var4) |
                                                  ( Var1 == Var3 & Var1 > Var2 & Var1 > Var4) | 
                                                  ( Var1 == Var4 & Var1 > Var2 & Var1 > Var3) |
                                                  ( Var2 == Var3 & Var2 > Var1 & Var2 > Var4) | 
                                                  ( Var2 == Var4 & Var2 > Var3 & Var2 > Var1) |
                                                  ( Var3 == Var4 & Var3 > Var1 & Var1 > Var2)) 


scenarios_tie <- left_join(scenarios, Towns, by = c("Var1" = "Selections")) %>% left_join(Irving, by = c("Var2" = "Selections")) %>% left_join(Doncic, by = c("Var3" = "Selections")) %>% 
  left_join(Curry, by = c("Var4" = "Selections")) %>% select(Probability.x, Probability.y, Probability.x.x, Probability.y.y) 

colnames(scenarios_tie) <- c("PlayerOne", "PlayerTwo", "PlayerThree", "PlayerFour")

#Getting rid of impossible scenarios (other players retiring not being able to achieve more All_NBA)
scenarios_tie[is.na(scenarios_tie)] <- 0


#Row products
scenarios_tie$prob = apply(scenarios_tie, 1, prod, na.rm=T)

#Tie Prob
sum(scenarios_tie$prob)  


#Comparison of all probs , Should = 1
sum(scenarios_luka$prob) +
sum(scenarios_kat$prob) +
sum(scenarios_kyrie$prob) +
sum(scenarios_curry$prob) + 
sum(scenarios_tie$prob)


#-------
