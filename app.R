#Read in projected careers file with probabilities
library(readr)
projected_careers <- read_csv("Documents/ThunderApplication/App/projected_career_wprobs.csv")

#Change age column name to age
colnames(projected_careers)[3] <- "age"

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(formattable)

#Organize by prob
projected_careers <- projected_careers %>% arrange(desc(prob)) %>% mutate(prob = percent(prob, digits = 2))

# Begin UI
ui <- fluidPage(

   # Application title
   titlePanel(("All-NBA Probability Forecast")),
   h4("Developed by Joe Siwinski"),
   
   # Sidebar with player inputs
   sidebarLayout(
      sidebarPanel(
         selectInput("playerone",
                     "Player 1:",
                     choices = (unique(projected_careers$player)),
                     selected = unique(projected_careers$player)[1]),
         selectInput("playertwo",
                     "Player 2:",
                     choices = (unique(projected_careers$player)),
                     selected = unique(projected_careers$player)[2])
      ),
      
      # Plot for AllNBA yearly Projections
      mainPanel(
         plotlyOutput("Plot")
      )
   )
)

# Server
server <- function(input, output) {
   
   output$Plot <- renderPlotly({
     x <- projected_careers %>% filter(player == input$playerone| player == input$playertwo) %>% 
       mutate(col = (ifelse(player == input$playerone, "blue",
                           "orange"))) %>% arrange(player)
     

     
     
     p <- x %>% ggplot(mapping = aes(x = season, y = prob, group = player, color = player,
                           label = player, label2 = age)) + geom_line() + geom_point() +
       scale_color_manual(values = unique(x$col), labels = unique(x$player)) +
       ylim(0,1) + xlim(2020,2036) + theme(panel.background = element_blank(),
                           axis.line = element_line(color = "black")) +
       ylab("All-NBA Probability") + xlab("Season Year") +
       labs(color = "Player:") +
       scale_y_continuous(labels = scales::percent)
     
     
    
     
     ggplotly(p, tooltip = c("group", "label2", "y"))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

