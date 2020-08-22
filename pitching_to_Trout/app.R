library(shiny)        # Allows for the interactive elements
library(tidyverse)    # Data manipulation and visualization
library(png)          # To put that plate image on the screen
library(shinythemes)  # For theme selection

trout_data <- read_rds("trout_data") # Data prepared in data downloader file

# Everything encased in this UI defines the layout of the app
ui <- fluidPage(
   theme = shinytheme("yeti"),
   titlePanel("Pitching To Mike Trout 2017-2020: A Shiny App by Michael Calabro"),
   navbarPage("Navbar",
   tabPanel("All Pitches",
        column(3, 
         selectInput("pitches",
                     "Pitches to include:",
                     c("2-Seam Fastball",
                       "4-Seam Fastball",
                       "Changeup",
                       "Curveball",
                       "Cutter",
                       "Sinker",
                       "Slider"),
                     selected = c("2-Seam Fastball",
                                  "4-Seam Fastball",
                                  "Changeup",
                                  "Curveball",
                                  "Cutter",
                                  "Sinker",
                                  "Slider"),
                     multiple = TRUE),
         selectInput("results",
                     "Pitch Results to include:",
                     c("Ball" = "ball",
                      "Called Strike"  = "called_strike",
                      "Foul"  = "foul",
                      "Hit In Play, Out"  = "in_play_out",
                      "Hit"  = "hit",
                      "Swinging Strike"  = "swinging_strike"),
                     selected = c("ball",
                                  "called_strike",
                                  "foul",
                                  "in_play_out",
                                  "hit",
                                  "swinging_strike"),
                     multiple = TRUE),
         sliderInput("balls",
                     "Balls in the Count",
                     min = 0,
                     max = 3,
                     value = c(0, 3)),
         sliderInput("strikes",
                     "Strikes in the Count",
                     min = 0,
                     max = 2,
                     value = c(0, 2)
                     ),
         selectInput("geom",
                     "Select Geom Display",
                     c("Point: Color = Pitch Type" = "Ppt",
                       "Point: Color = Pitch Result" = "Ppr",
                       "Bin: Color = Pitch Count" = "Bpc"),
                     selected = "Bpc"),
         br(),
         checkboxInput("nums",
                       "Show Zone Numbers:",
                       value = TRUE)
        ),

      column(4,
         plotOutput("allPlot", height = "530px"),
         img(src="plate.png", width = "70%", height = "50px")
      ),
      
      column(2,
             br(),
             tableOutput("allTable3")
      ),
      
      column(3,
         br(),
         tableOutput("allTable1"),
         br(),
         tableOutput("allTable2")
      )
   ),
   tabPanel("Batted Balls",
     h2("Coming Soon!")
   )
   )
)



server <- function(input, output) {
   
   output$allPlot <- renderPlot({
     
     num_x <- c(-.66, 0, .66, -.66, 0, .66, -.66, 0, .66, -1.3, 1.3, -1.3, 1.3)
     num_y <- c(1.95, 1.95, 1.95, 2.55, 2.55, 2.55, 3.15, 3.15, 3.15, 3.7, 3.7, 1.4, 1.4)
     val <- c(7, 8, 9, 4, 5, 6, 1, 2, 3, 11, 12, 13, 14)
     num_tab <- data.frame(num_x, num_y, val)
     
     trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2])) %>%
       ggplot(aes(x = plate_x, y = plate_z)) +
       {
         if(input$geom == "Bpc"){
         geom_bin2d(binwidth = c(0.33, 0.2323))}
       } +
       geom_segment(aes(x = -0.333, y = mean(sz_top), xend = -0.333, yend = mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = 0.333, y = mean(sz_top), xend = 0.333, yend = mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = -1, y = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot),
                        xend = 1, yend = ((mean(sz_top) - mean(sz_bot))/3) + mean(sz_bot)), color = "gray") +
       geom_segment(aes(x = -1, y = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3),
                        xend = 1, yend = mean(sz_top) - ((mean(sz_top) - mean(sz_bot))/3)), color = "gray") +
       geom_segment(aes(x = -1, y = mean(sz_top), xend = 1, yend = mean(sz_top)), size = 1.5) +
       geom_segment(aes(x = -1, y = mean(sz_bot), xend = 1, yend = mean(sz_bot)), size = 1.5) +
       geom_segment(aes(x = -1, y = mean(sz_top), xend = -1, yend = mean(sz_bot)), size = 1.5) +
       geom_segment(aes(x = 1, y = mean(sz_top), xend = 1, yend = mean(sz_bot)), size = 1.5) +
       {
         if(input$geom == "Ppt"){
           geom_point(aes(fill = pitch_name), shape = 21, size = 3, color = "black", stroke = 0.5)
         }else if(input$geom == "Ppr"){
           geom_point(aes(fill = description), shape = 21, size = 3, color = "black", stroke = 0.5)
         }
       } +
       {
         if(input$nums){
           geom_text(data = num_tab, aes(x = num_x, y = num_y, label = val), size = 8.5,color = "black")
         }
       } +
       ylim(1.03, 4.1) +
       xlim(-1.67, 1.67) +
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank())
   })
   
   output$allTable1 <- renderTable({
    table_data <- trout_data %>%
      mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                  ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
    table_data <- table_data %>%
       mutate(total = nrow(table_data))
    
    table_data %>%
       group_by(pitch_name, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(pitch_name, count, share) %>%
       arrange(desc(count))
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")
   
   output$allTable2 <- renderTable({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(description, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(description, count, share) %>%
       arrange(desc(count))
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")   
   
   output$allTable3 <- renderTable({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(zone, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       mutate(zone = as.integer(zone)) %>%
       select(zone, count, share) %>%
       arrange(desc(count))
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s") 
   
}

# Run the application 
shinyApp(ui = ui, server = server)

