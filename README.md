# Forecasting All-NBA Selection Probabilities

An ML forecasting project to predict future All-NBA selection probabilities for each player in the NBA. In this project I will use historic NBA player data to train a xGboost Model designed to predict the probability that a player will make an All NBA team. 


## R Script Guide:

**Boosting Model** - AllNBAProbModelScaled.R

This script is where I train a GBM using xGboost in R. This model is designed to predict the probability that each player will make an All NBA team.

<br/>

**Player Stat Projection** - CareerTrajectory.R

This script projects each player's stats for every season of their career using the methodology that I designed. These projected stats will go into the GBM model that I trained in the previous script. 

<br/>

**Example Application of Model** - LikelihoodAllNBA.R

An example of model application to four example players (Curry, Doncic, Towns, and Irving).

<br/>

**Interactive App of Results** - app.R

Script that powers the interactive app that displays model predictions for each player.

## Probability App 

Visit https://joesiwinski.shinyapps.io/AllNBAProjections/ to see every player's All NBA probability for each year of their career. This app was made using Shiny R.
