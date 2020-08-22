library(shiny)        # Allows for the interactive elements
library(tidyverse)    # Data manipulation and visualization
library(png)          # To put that plate image on the screen
library(shinythemes)  # For theme selection

trout_data <- read_rds("trout_data") # Data prepared in data downloader file

# Everything encased in this UI defines the layout of the app
ui <- fluidPage(
   # App theme(color/text scheme) and App Title
   theme = shinytheme("yeti"),
   titlePanel("Pitching To Mike Trout 2017-2020: A Shiny App by Michael Calabro"),
   # navbarPage creates tabs at the top of the app to switch between
   navbarPage("Navbar", 
   # Everything within this tabPanel function is shown when the "All Pitches" tab is clicked
   tabPanel("All Pitches",
        # Every column() function creates a column on the screen, and the number associated column(3) is its width
        column(3, 
         style = "background-color:#D3D3D3;",
         selectInput("geom",
                     "Select Geom Display",
                     c("Point: Color = Pitch Type" = "Ppt",
                       "Point: Color = Pitch Result" = "Ppr",
                       "Bin: Color = Pitch Count" = "Bpc"),
                     selected = "Bpc"),
         # Inputs like selectInput, sliderInput.. create the widgets which affect the plots/tables
         selectInput("pitches",              # The first argument is a label that can be referenced in the server
                     "Pitches to include:",  # The second argument is the label that will be shown in the App
                     c("2-Seam Fastball",    # These are all the possible options for the user to pick from
                       "4-Seam Fastball",
                       "Changeup",
                       "Curveball",
                       "Cutter",
                       "Sinker",
                       "Slider"),
                     selected = c("2-Seam Fastball", # These are the options that are selected when you open the app
                                  "4-Seam Fastball",
                                  "Changeup",
                                  "Curveball",
                                  "Cutter",
                                  "Sinker",
                                  "Slider"),
                     multiple = TRUE),               # Allows you to pick multiple options
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
         # br() creates space between widgets
         br(),
         checkboxInput("nums",
                       "Show Zone Numbers:",
                       value = TRUE)
        ),
      # New column every time this function is called
      column(4,
         # "allPlot" is defined in the server, and then is called here in the UI to be shown on screen
         # The same goes for any function that ends in 'Output' (plotOutput, tableOutput...)
         plotOutput("allPlot", height = "530px"),
         img(src="plate.png", width = "70%", height = "50px")
      ),
      
      column(5,
             tabsetPanel(
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
                        ),
               tabPanel("Pie Charts",
                        column(6,
                               br(), br(), br(), br(),
                               plotOutput("zonePie", height = "280px")
                               ),
                        column(6,
                               plotOutput("typePie", height = "280px"),
                               plotOutput("resultPie", height = "280px")
                               )
                        )
             )
          )

   ),
   # New tabPanel results in an entirely new screen when "Batted Balls" is clicked
   tabPanel("Batted Balls",
            column(3, 
                   style = "background-color:#D3D3D3;",
                   selectInput("bbgeom",
                               "Select Geom Display",
                               c("Point: Color = Event" = "Pe",
                                 "Point: Color = Ball Flight" = "Pbf",
                                 "Point: Color = Pitch Type" = "Ppt",
                                 "Point: Color = Launch Speed" = "Pls",
                                 "Point: Color = Launch Angle" = "Pla"
                               ),
                               selected = "Pe"),
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
                               value = c(0, 2)
                      )
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
                                      value = c(0, 120)
                          )
                   )
                   
            ),
            column(4,
                   plotOutput("bbPlot", height = "530px"),
                   img(src="plate.png", width = "68%", height = "50px")
            ),
            column(5,
            tabsetPanel(
              tabPanel("Launch Chart",
                       plotOutput("launchPlot", height = "350px"),
                       tableOutput("bbLaunchTable")
              ),
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
            
   )
   
   )
)



# The server is where all the data manipulation and plot making takes place
# In here I create different plots and tables which can react to inputs from the UI's widgets
server <- function(input, output) {
   
   # Each output$... creates an item (plot/table/text) that can be called in the UI
   # When you see plotOutput("allPlot") in the UI, it calls everything encased in this renderPlot() function
   #
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
   
   output$zonePie <- renderPlot({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
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
   })
   
   output$typePie <- renderPlot({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
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
   })
   
   output$resultPie <- renderPlot({
     table_data <- trout_data %>%
       mutate(description = ifelse(description == "hit_into_play_no_out", "hit",
                                   ifelse(description == "hit_into_play", "in_play_out", description))) %>%
       filter(pitch_name %in% input$pitches,
              description %in% input$results,
              balls %in% (input$balls[1]:input$balls[2]),
              strikes %in% (input$strikes[1]:input$strikes[2]))
     
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
   })
   
      
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
              launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
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
              launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
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
              launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
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
              launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
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
              launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
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
              launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
              pitch_name %in% input$bbpitches,
              events %in% input$bbevents,
              bb_type %in% input$bbflights,
              balls %in% (input$bbballs[1]:input$bbballs[2]),
              strikes %in% (input$bbstrikes[1]:input$bbstrikes[2])) %>%
       ggplot() +
       {
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
       geom_hline(yintercept = -10) +
       theme(axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             panel.grid = element_blank(),
             panel.background = element_blank(),
             legend.position = "none")
   })
   
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
         launch_speed %in% (input$bbSpeed[1]:input$bbSpeed[2]),
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
     
}

# Glues it all together and runs the application 
shinyApp(ui = ui, server = server)
