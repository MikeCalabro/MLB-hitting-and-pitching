library(shiny)        # Allows for the interactive elements
library(tidyverse)    # Data manipulation and visualization
library(png)          # To put that plate image on the screen
library(shinythemes)  # For theme selection

# Reading in the data prepared in data_downloader
trout_data <- read_rds("trout_data")

# Everything encased in this UI defines the layout of the app
ui <- fluidPage(
  
   # App theme(color/text scheme) and App Title
   theme = shinytheme("yeti"),
   titlePanel("Pitching To Mike Trout 2016-2020: A Shiny App by Michael Calabro"),
   
   # navbarPage creates tabs at the top of the app to switch between
   navbarPage("Navbar", 
              
      # Everything within this tabPanel function is shown when the "Batted Balls" tab is clicked
      tabPanel("Batted Balls",
               
           # Every column() function creates a column on the screen, and the number associated column(3,) is its width
           # An entire screen fits 12 sections worth of column, so this column takes up 3/12 (.25) of the screen
           column(3, 
                  
                  # style.. allows me to set the background color for the column
                  style = "background-color:#D3D3D3;",
                  
                  # Inputs like selectInput, sliderInput.. create the widgets which affect the plots/tables
                  # The first argument is a label that can be referenced in the server
                  # The second argument is the label that will be shown in the App
                  # The third argument is a list of all the possible options for the user to pick from
                  # selected.. defines what is originaly chosen when the user opens the app
                  selectInput("bbgeom",
                              "Select Geom Display",
                              c("Point: Color = Event" = "Pe",
                                "Point: Color = Ball Flight" = "Pbf",
                                "Point: Color = Pitch Type" = "Ppt",
                                "Point: Color = Launch Speed" = "Pls",
                                "Point: Color = Launch Angle" = "Pla"
                              ),
                              selected = "Pbf"),
                  
                  # Each Input widget works in similar ways, with slightly different features
                  # With this select input, for example, I can make multiple selections
                  selectInput("bbpitches",              
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
                  
                  selectInput("bbflights",              
                              "Ball Flights to include:",  
                              c("Pop-up" = "popup",    
                                "Grounder" = "ground_ball",
                                "Fly Ball" = "fly_ball",
                                "Line Drive" = "line_drive"),
                              selected = c("popup",    
                                           "ground_ball",
                                           "fly_ball",
                                           "line_drive"),
                              multiple = TRUE),
                  
                  selectInput("bbevents",              
                              "Events to include:",  
                              c("Single" = "single",    
                                "Double" = "double",
                                "Triple" = "triple",
                                "Home Run" = "home_run",
                                "Field Out" = "field_out"),
                              selected = c("single",    
                                           "double",
                                           "triple",
                                           "home_run",
                                           "field_out"),
                              multiple = TRUE),
                  
                  # This column is nested within the column above
                  # Every nested column takes up x/12 of the space it is nested in
                  # So this column(6,) takes up 6/12 (.5) of the width of the column it is nested in
                  column(6,
                         sliderInput("bbballs",
                                     "Balls in the Count",
                                     min = 0,
                                     max = 3,
                                     value = c(0, 3)),
                         
                         sliderInput("bbstrikes",
                                     "Strikes in the Count",
                                     min = 0,
                                     max = 2,
                                     value = c(0, 2))
                  ),
                  column(6,
                         sliderInput("bbAngle",
                                     "Lauch Angle Range",
                                     min = -40,
                                     max = 70,
                                     value = c(-40, 70)),
                         sliderInput("bbSpeed",
                                     "Launch Speed Range",
                                     min = 0,
                                     max = 120,
                                     value = c(0, 120),
                                     step = 0.1)
                  )
                  
           ),
           
           # This entirely new column(4,) makes up the middle 4/12 of the screen (essentially the 'middle')
           column(4,
                  
                  # "bbplot" is defined in the server below as output$bbplot
                  # if it were a table, it would be called using tableOutput
                  plotOutput("bbPlot", height = "530px"),
                  
                  # plate.png is an image i screenshotted on my comp from the internet.. a home plate
                  img(src="plate.png", width = "68%", height = "50px")
           ),
           
           # This is the final column on my first screen (Takes up the final 5/12 of the screen)
           column(5,
                  
                # tabsetPanel() does the same thing as navbar, except it only cchanges the column on the screen it's in
                tabsetPanel(
                  
                  # Here, each tabPanel switches what you see in this column when you press the corresponding tab
                  tabPanel("Launch Chart",
                           
                       column(2,
                              
                              # Probably a better way to get this image lower, but this works for now
                              br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                              
                              # An image of a batter swinging, sneaky very important for the app
                              img(src="swing.png", height = "80px", width = "80px")),
                       
                       column(10,
                              plotOutput("launchPlot", height = "350px")     
                       ),
                       
                       # Since the columns above filled up the column they're encompassed in, this goes underneath
                       tableOutput("bbLaunchTable")
                  ),
                  
                  # When the tab 'Tables' is pressed, the screen defined below appears
                  tabPanel("Tables",
                           
                       column(5,
                              br(),
                              tableOutput("bbZoneTable")
                       ),
                       
                       column(7,
                              br(),
                              tableOutput("bbPitchTable"),
                              tableOutput("bbFlightTable"),
                              tableOutput("bbEventTable")
                       )
                  )
              )
          )
      ),           
     
     # New tabPanel results in an entirely new screen when "All Pitches" is clicked
     tabPanel("All Pitches",
          
        column(3, 
           style = "background-color:#D3D3D3;",
           selectInput("geom",
                       "Select Geom Display",
                       c("Point: Color = Pitch Type" = "Ppt",
                         "Point: Color = Pitch Result" = "Ppr",
                         "Bin: Color = Pitch Count" = "Bpc"),
                       selected = "Bpc"),
           
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
           
           br(),
           
           checkboxInput("nums",
                         "Show Zone Numbers:",
                         value = TRUE)
        ),
        
        
        column(4,
           plotOutput("allPlot", height = "530px"),
           img(src="plate.png", width = "70%", height = "50px")
        ),
      
        column(5,
               
             tabsetPanel(
               
               tabPanel("Pie Charts",
                        
                    column(6,
                           br(), br(), br(), br(),
                           plotOutput("Pie1", height = "280px")
                    ),
                    
                    column(6,
                           plotOutput("Pie2", height = "240px"),
                           plotOutput("Pie3", height = "240px")
                    )
               ),
               
               tabPanel("Tables",
                        
                    column(5,
                           br(),
                           tableOutput("zoneTable")
                    ),
                    
                    column(7,
                           br(),
                           tableOutput("typeTable"),
                           br(),
                           tableOutput("resultTable")
                    )
                )
             )
          )
      ),
     
     tabPanel("Pitch Selector",
        
        column(3,
               
          style = "background-color:#D3D3D3;",
          
          numericInput("psSpeed",
                       "What Is Your Max Fastball Speed?",
                       value = 98,
                       min = 88,
                       max = 105,
                       step = 1
                       ),
          
          selectInput("psPitch",
                      "What Pitches Can You Throw?",
                      c("2-Seam Fastball",    
                        "4-Seam Fastball",
                        "Changeup",
                        "Curveball",
                        "Cutter",
                        "Sinker",
                        "Slider"),
                      selected = c("4-Seam Fastball",
                                   "Changeup",
                                   "Curveball",
                                   "Slider"),
                      multiple = TRUE),
          
          column(6,
                 sliderInput("psBalls",
                             "Balls In The Count:",
                             value = c(0,3),
                             min = 0,
                             max = 3,
                             step = 1)
                 ),
          
          column(6,
                 sliderInput("psStrikes",
                             "Strikes In The Count:",
                             value = c(0,2),
                             min = 0,
                             max = 2,
                             step = 1)
                 ),
          
          
          selectInput("psBallStrike",
                      "Do You Want To Throw A Strike?",
                      c("Yes",
                        "I Don't Care"),
                      selected = "Yes"),
          
          selectInput("psGoal",
                      "Your Goal - Minimize the chance of Mike...",
                      c("Swinging at the Pitch" = "swing",
                        "Hitting the Ball In Play" = "hit_in_play",
                        "Hitting a Home Run" = "homerun",
                        "Hitting the Ball Hard" = "hit_hard",
                        "Hitting a Line Drive" = "line_drive"),
                      selected = "swing"),
          
          numericInput("psOptions",
                       "How Many Pitch Options Do You Want To Consider?",
                       value = 8,
                       min = 1,
                       max = 20,
                       step = 1),
          
          numericInput("psObs",
                       "Minimum Number Of Observations To Be Included",
                       value = 6,
                       min = 1,
                       max = 50,
                       step = 1)
          
        ),
        
        column(1),
        
        column(8,
               tableOutput("psTable")
        )
      )
   )
)



# The server is where all the data manipulation and plot making takes place
# In here I create different plots and tables which can react to inputs from the UI's widgets
server <- function(input, output) {
   
   # Each output$... creates an item (plot/table/text) that can be called in the UI
   # When you see plotOutput("allPlot") in the UI, it calls everything encased in this renderPlot() function

   #This particular Plot is the strikezone plot on the "All Pitches" page
   output$allPlot <- renderPlot({
     
     # This could have been done more efficiently, but it makes a table so I can display the strike zone numbers
     num_x <- c(-.66, 0, .66, -.66, 0, .66, -.66, 0, .66, -1.3, 1.3, -1.3, 1.3)
     num_y <- c(1.95, 1.95, 1.95, 2.55, 2.55, 2.55, 3.15, 3.15, 3.15, 3.7, 3.7, 1.4, 1.4)
     val <- c(7, 8, 9, 4, 5, 6, 1, 2, 3, 11, 12, 13, 14)
     num_tab <- data.frame(num_x, num_y, val)
     
     # Using the data that I downloaded from Statcast using the baseballr package
     trout_data %>%
       
       # Renaming some descriptions so they aren't as big on screen (Long names in the key squish the strikezone)
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       
       # All of this filtering means that only the data chosen from the widgets gets displayed on screen
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2])) %>%
       
       # Now the plot can be defined
       # The x and y arguments within ggplot define the location of all the points that will be plotted
       ggplot(aes(x = plate_x, y = plate_z)) +
       
       # Each if statement in this renderPlot defines how the plot should react if certain inputs are altered
       # This one says that the heat map bins should be shown on screen if that selection is made
       {
         if(input$geom == "Bpc"){
         geom_bin2d(binwidth = c(0.33, 0.2323))} # These numbers make the bins fit nicely in the strikezone
       } +
       
       # Each of these geom_segments define the strike zone outline that is shown on screen
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
       
       # These ifs are used to switch between the color of the points shown on screen (Could be simpler, should fix)
       {
         if(input$geom == "Ppt"){
           geom_point(aes(fill = pitch_name), shape = 21, size = 3, color = "black", stroke = 0.5)
         }else if(input$geom == "Ppr"){
           geom_point(aes(fill = description), shape = 21, size = 3, color = "black", stroke = 0.5)
         }
       } +
       
       # If the checkbox for showing numbers is pressed, this geom_text creates thos numbers
       {
         if(input$nums){
           geom_text(data = num_tab, aes(x = num_x, y = num_y, label = val), size = 8.5,color = "black")
         }
       } +
       
       # xlim and ylim define the size of the plot
       ylim(1.03, 4.1) +
       xlim(-1.67, 1.67) +
       
       # All of these element_blank()'s make the canvas blank, unlike base ggplot which has axis/grid defaults
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank())
   })
   
   # The only difference between the next three tables is what I 'group by' to create the rows
   output$typeTable <- renderTable({
     # Same mutating and filtering as for the plot
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
     # Created this new dataframe so that I could add this total row
     # This total row allows me to divide each group_by column by the total pitches so that I can make the share %s
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
    
    # This code creates the data in the table
    table_data %>%
       group_by(pitch_name, total) %>%      # Create buckets of rows defined by pitch_name 
       summarise(count = n()) %>%           # Create a column count which says how many observations there are for each group
       mutate(share = count/total) %>%      # Add the share column - the % of all pitches
       select(pitch_name, count, share) %>% # Selects the three columns to show
       arrange(desc(count))                 # Arranges the table with the highest counts at the top
   },
   striped = TRUE,  # Make the table striped
   bordered = TRUE, # Makes the table have a border
   spacing = "s")   # Defines the spacing between rows on the table
   
   # This tableOutput is the same as typeTable, but is grouped by pitch result (description)
   output$resultTable <- renderTable({
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
   
   # This tableOutput is the same as resultTable, but is grouped by zone location (description)
   output$zoneTable <- renderTable({
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
   
   # Creates the pie chart closest to the strike zone
   # This will always mirror the geom style in the strike zone
   output$Pie1 <- renderPlot({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
    # Each if makes it so that the pie chart closes to the strikezone corresponds to the color scheme of the zone
    if(input$geom == "Bpc"){
      pie_data <- table_data %>%
        mutate(total = nrow(table_data)) %>%
        group_by(zone, total) %>%
        summarise(count = n()) %>%
        mutate(share = count/total) %>%
        mutate(zone = as.integer(zone)) %>%
        select(zone, count, share)
      
      pie_data %>%
        ggplot(aes(x="", y=share, fill=zone))+
        geom_bar(width = 1, stat = "identity") + # Creates a bar plot with one bar
        coord_polar("y", start=0) +              # Puts the bar in polar coordinates to make a pie-chart
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_blank())
    }else if(input$geom == "Ppt"){
      pie_data <- table_data %>%
        mutate(total = nrow(table_data)) %>%
        group_by(pitch_name, total) %>%
        summarise(count = n()) %>%
        mutate(share = count/total) %>%
        select(pitch_name, count, share)
      
      pie_data %>%
        ggplot(aes(x="", y=share, fill=pitch_name))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_blank())
    }else if(input$geom == "Ppr"){
      pie_data <- table_data %>%
        mutate(total = nrow(table_data)) %>%
        group_by(description, total) %>%
        summarise(count = n()) %>%
        mutate(share = count/total) %>%
        select(description, count, share)
      
      pie_data %>%
        ggplot(aes(x="", y=share, fill=description))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_blank())
    }
     
   })
   
   # Creates the top pie chart on the right
   output$Pie2 <- renderPlot({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
     if(input$geom == "Ppt"){
       pie_data <- table_data %>%
         mutate(total = nrow(table_data)) %>%
         group_by(zone, total) %>%
         summarise(count = n()) %>%
         mutate(share = count/total) %>%
         mutate(zone = as.integer(zone)) %>%
         select(zone, count, share)
       
       pie_data %>%
         ggplot(aes(x="", y=share, fill=zone))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         theme(axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank())
     }else if(input$geom == "Bpc"){
       pie_data <- table_data %>%
         mutate(total = nrow(table_data)) %>%
         group_by(pitch_name, total) %>%
         summarise(count = n()) %>%
         mutate(share = count/total) %>%
         select(pitch_name, count, share)
       
       pie_data %>%
         ggplot(aes(x="", y=share, fill=pitch_name))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         theme(axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank())
     }else if(input$geom == "Ppr"){
       pie_data <- table_data %>%
         mutate(total = nrow(table_data)) %>%
         group_by(pitch_name, total) %>%
         summarise(count = n()) %>%
         mutate(share = count/total) %>%
         select(pitch_name, count, share)
       
       pie_data %>%
         ggplot(aes(x="", y=share, fill=pitch_name))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         theme(axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank())
     }
   })
   
   # Creates the bottom pie chart on the right
   output$Pie3 <- renderPlot({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
     if(input$geom == "Ppt"){
       pie_data <- table_data %>%
         mutate(total = nrow(table_data)) %>%
         group_by(description, total) %>%
         summarise(count = n()) %>%
         mutate(share = count/total) %>%
         select(description, count, share)
       
       pie_data %>%
         ggplot(aes(x="", y=share, fill=description))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         theme(axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank())
     }else if(input$geom == "Bpc"){
       pie_data <- table_data %>%
         mutate(total = nrow(table_data)) %>%
         group_by(description, total) %>%
         summarise(count = n()) %>%
         mutate(share = count/total) %>%
         select(description, count, share)
       
       pie_data %>%
         ggplot(aes(x="", y=share, fill=description))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         theme(axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank())
     }else if(input$geom == "Ppr"){
       pie_data <- table_data %>%
         mutate(total = nrow(table_data)) %>%
         group_by(zone, total) %>%
         summarise(count = n()) %>%
         mutate(share = count/total) %>%
         select(zone, count, share)
       
       pie_data %>%
         ggplot(aes(x="", y=share, fill=zone))+
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +
         theme(axis.ticks = element_blank(),
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               panel.background = element_blank())
     }
   })
   
   #
   # ONTO NAV_TAB 2 (Now NAV_TAB 1) - BATTED BALLS
   #
   
   # Creates the strike zone plot in the batted balls tab - similar to other strike zone
   output$bbPlot <- renderPlot({
     
     # This could have been done more efficiently, but it makes a table so I can display the strike zone numbers
     num_x <- c(-.66, 0, .66, -.66, 0, .66, -.66, 0, .66, -1.3, 1.3, -1.3, 1.3)
     num_y <- c(1.95, 1.95, 1.95, 2.55, 2.55, 2.55, 3.15, 3.15, 3.15, 3.7, 3.7, 1.4, 1.4)
     val <- c(7, 8, 9, 4, 5, 6, 1, 2, 3, 11, 12, 13, 14)
     num_tab <- data.frame(num_x, num_y, val)
     
     # Using the data that I downloaded from Statcast using the baseballr package
     trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
              launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
              launch_speed > input$bbSpeed[1],
              launch_speed < input$bbSpeed[2],
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2])) %>%
       # Now the plot can be defined
       # The x and y arguments within ggplot define the location of all the points that will be plotted
       ggplot(aes(x = plate_x, y = plate_z)) +
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
       # These ifs are used to switch between the color of the points shown on screen (Could be simpler, should fix)
       {
         if(input$bbgeom == "Pe"){
           geom_point(aes(fill = events), shape = 21, size = 3, color = "black", stroke = 0.5)
         }else if(input$bbgeom == "Pbf"){
           geom_point(aes(fill = bb_type), shape = 21, size = 3, color = "black", stroke = 0.5)
         }else if(input$bbgeom == "Ppt"){
           geom_point(aes(fill = pitch_name), shape = 21, size = 3, color = "black", stroke = 0.5)
         }else if(input$bbgeom == "Pla"){
           geom_point(aes(fill = launch_angle), shape = 21, size = 3, color = "black", stroke = 0.5)
         }else if(input$bbgeom == "Pls"){
           geom_point(aes(fill = launch_speed), shape = 21, size = 3, color = "black", stroke = 0.5)
         }
       } +
       geom_text(data = num_tab, aes(x = num_x, y = num_y, label = val), size = 8.5,color = "black") +
       # xlim and ylim define the size of the plot
       ylim(1.03, 4.1) +
       xlim(-1.67, 1.67) +
       # All of these element_blank()'s make the canvas blank, unlike base ggplot which has axis/grid defaults
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank())
   })
   
   # Creates the zone table under the Tables Tab - similar to tables is "All Pitches" tab
   output$bbZoneTable <- renderTable({
     table_data <- trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
              launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
              launch_speed > input$bbSpeed[1],
              launch_speed < input$bbSpeed[2],
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2]))
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
   
   # Creates the pitch type table under the Tables Tab
   output$bbPitchTable <- renderTable({
     table_data <- trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
              launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
              launch_speed > input$bbSpeed[1],
              launch_speed < input$bbSpeed[2],
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(pitch_name, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(pitch_name, count, share) %>%
       arrange(desc(count)) %>%
       head(n = 6L)
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")
   
   # Creates the ball flight table under the Tables Tab
   output$bbFlightTable <- renderTable({
     table_data <- trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
              launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
              launch_speed > input$bbSpeed[1],
              launch_speed < input$bbSpeed[2],
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(bb_type, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(bb_type, count, share) %>%
       arrange(desc(count))
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")
   
   # Creates the event table under the Tables Tab
   output$bbEventTable <- renderTable({
     table_data <- trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
              launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
              launch_speed > input$bbSpeed[1],
              launch_speed < input$bbSpeed[2],
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
     
     table_data %>%
       group_by(events, total) %>%
       summarise(count = n()) %>%
       mutate(share = count/total) %>%
       select(events, count, share) %>%
       arrange(desc(count)) %>%
       head(n = 4L)
   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s") 
   
   # Creates the launch angle/speed graphic
   output$launchPlot <- renderPlot({
     trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
              launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
              launch_speed > input$bbSpeed[1],
              launch_speed < input$bbSpeed[2],
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2])) %>%
       ggplot() +
       {
         # Using some trigonometry allows me to visualize launch angle and velocity in this plot
         if(input$bbgeom == "Pbf"){
           geom_segment(aes(x = 0, y = 0, xend = launch_speed*cos(launch_angle*pi/180),
                            yend = launch_speed*sin(launch_angle*pi/180), color = bb_type))
         }else if(input$bbgeom == "Pls"){
           geom_segment(aes(x = 0, y = 0, xend = launch_speed*cos(launch_angle*pi/180),
                            yend = launch_speed*sin(launch_angle*pi/180), color = launch_speed))
         }else if(input$bbgeom == "Pe"){
           geom_segment(aes(x = 0, y = 0, xend = launch_speed*cos(launch_angle*pi/180),
                            yend = launch_speed*sin(launch_angle*pi/180), color = events))
         }else if(input$bbgeom == "Ppt"){
           geom_segment(aes(x = 0, y = 0, xend = launch_speed*cos(launch_angle*pi/180),
                            yend = launch_speed*sin(launch_angle*pi/180), color = pitch_name))
         }else if(input$bbgeom == "Pla"){
           geom_segment(aes(x = 0, y = 0, xend = launch_speed*cos(launch_angle*pi/180),
                            yend = launch_speed*sin(launch_angle*pi/180), color = launch_angle))
         }
       } +
       # hline represents the ground
       geom_hline(yintercept = -10) +
       ylim(-35, 80) +
       
       # Removes all the background grid lines and gray color and axes
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank(),
             legend.position = "none")
   })
   
   # Creates the table that is shown under the launch plot
   output$bbLaunchTable <- renderTable({
     table_data <- trout_data %>%
       mutate(events = ifelse(events %in% c("double_play",
                                            "field_error",
                                            "fielders_choice",
                                            "fielders_choice_out",
                                            "force_out",
                                            "grounded_into_double_play",
                                            "sac_fly"), "field_out", events)) %>%
       filter(
         launch_angle %in% (input$bbAngle[1]:input$bbAngle[2]),
         launch_speed > input$bbSpeed[1],
         launch_speed < input$bbSpeed[2],
         pitch_name %in% input$bbpitches,
         events %in% input$bbevents,
         bb_type %in% input$bbflights,
         balls %in% (input$bbballs[1]:input$bbballs[2]),
         strikes %in% (input$bbstrikes[1]:input$bbstrikes[2]))
     table_data <- table_data %>%
       mutate(total = nrow(table_data))
    if(input$bbgeom == "Pe"){
      table_data %>%
        group_by(events, total) %>%
        summarise(count = n(), average_launch_speed = mean(launch_speed), average_launch_angle = mean(launch_angle)) %>%
        mutate(share = count/total) %>%
        select(events, count, share, average_launch_speed, average_launch_angle) %>%
        arrange(desc(count)) %>%
        head(n = 4L)
    }else if(input$bbgeom == "Pbf"){
      table_data %>%
        group_by(bb_type, total) %>%
        summarise(count = n(), average_launch_speed = mean(launch_speed), average_launch_angle = mean(launch_angle)) %>%
        mutate(share = count/total) %>%
        select(bb_type, count, share, average_launch_speed, average_launch_angle) %>%
        arrange(desc(count)) %>%
        head(n = 4L)
    }else if(input$bbgeom == "Ppt"){
      table_data %>%
        group_by(pitch_name, total) %>%
        summarise(count = n(), average_launch_speed = mean(launch_speed), average_launch_angle = mean(launch_angle)) %>%
        mutate(share = count/total) %>%
        select(pitch_name, count, share, average_launch_speed, average_launch_angle) %>%
        arrange(desc(count)) %>%
        head(n = 5L)
    }

   },
   striped = TRUE,
   bordered = TRUE,
   spacing = "s")
   
   #
   # ONTO NAV_TAB 3 - PITCH SELECTOR!
   #
   
   # Creates the Main Pitch Selector Table
   output$psTable <- renderTable({
     table_data <- trout_data %>%
       filter(release_speed < input$psSpeed + 1,
              pitch_name %in% input$psPitch,
              balls %in% (input$psBalls[1]:input$psBalls[2]),
              strikes %in% (input$psStrikes[1]:input$psStrikes[2]),
              {
                if(input$psBallStrike == "Yes"){
                  zone %in% (1:9)
                }else{
                  zone %in% (1:14)
                }})
     
     table_data <- table_data %>%
       mutate(total = nrow(table_data)) %>%
       select(pitch_name, zone, total, launch_speed, description, events, bb_type, type)
    
     # Turns the selected columns into their own individual dummy variable columns
     # Makes life easier when adding up certain observations
     table_data <- fastDummies::dummy_cols(table_data, select_columns = c("description", "events", "bb_type", "type"))
     
     final_data <- table_data %>%
       group_by(pitch_name, zone, total) %>%
       summarise(observations = n(),
                 no_swing = sum(description_ball) + sum(description_called_strike),
                 homerun = sum(events_home_run),
                 line_drive = sum(bb_type_line_drive),
                 hit_in_play = sum(type_X),
                 average_launch_speed = mean(launch_speed, na.rm = TRUE))
     
     # For some reason I had to separate this from the creation of final_data above
     final_data %>%
       filter(observations >= input$psObs) %>%
       mutate(swing_p = 1-(no_swing/observations),
              homerun_p = homerun/observations,
              line_drive_p = line_drive/observations,
              hit_in_play_p = hit_in_play/observations,
              zone = as.integer(zone)) %>%
       select(pitch_name, zone, observations, swing_p, hit_in_play_p,
              homerun_p, average_launch_speed, line_drive_p) %>%
       arrange(desc(observations)) %>%
       arrange(case_when(
         input$psGoal == "swing" ~ swing_p,
         input$psGoal == "hit_in_play" ~ hit_in_play_p,
         input$psGoal == "homerun" ~ homerun_p,
         input$psGoal == "hit_hard" ~ average_launch_speed,
         input$psGoal == "line_drive" ~ line_drive_p
       )) %>%
       rename(
         "Pitch Type" = pitch_name,
         "Zone" = zone,
         "Observations" = observations,
         "Swing %" = swing_p,
         "Ball Hit In Play %" = hit_in_play_p,
         "Homerun %" = homerun_p,
         "Line Drive %" = line_drive_p,
         "Average Launch Speed" = average_launch_speed
       ) %>%
       head(input$psOptions)
       
   }, 
   striped = TRUE,
   bordered = TRUE)
     
}

# Glues it all together and runs the application 
shinyApp(ui = ui, server = server)
