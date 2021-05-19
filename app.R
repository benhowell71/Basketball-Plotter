library(shiny)
library(DT)
library(tidyverse)
library(BasketballAnalyzeR)

###IMPORTANT NOTES
#you need to have the information in the dropdown menus selected BEFORE you click on the graph
#second, make sure you have downloaded the info from the table before closing or clicking the refresh button at the top
###the data doesn't save unless you download it

players <- read.csv("players.csv")
#prep rosters in a csv a load it in
#it's easier to put in more players through an outside thing this way
#just make sure that you have the entire active roster BEFORE you start charting a game
#you currently cannot add players once the App has started running

size <- 1
col <- "black"
#stuff for defining how the court looks

teams <- unique(players$Team)
#defining team choices

#here are the actions that are pre-loaded
#if you want something else, it's easy to add by just putting it in the correct category
#(if you want more categories available, that's fairly easy to do)
actions <- c("Pass", "Assist", "Rebound", "Turnover", "Shot", "Make")
hand <- c("Right", "Left")
shot_type <- c("Pull-up", "Layup", "Hook", "Dunk")
pick_roll <- c("True", "Pop", "Fade")
defense <- c("Man-to-man", "3-2 zone", "2-3 zone")

Date <- Sys.Date()

#function to define the court
#if you want to use it for WNBA games, you could, just need to change the command in the server area of the code to 'WNBA' from 'NBA'
#but it defaults to NBA for now
#college courts can get added if needed/requested
court <- function(..., league = "NBA") {
  
  crcl <- function(x0, y0, r, span = r, nsteps = 100) {
    x <- seq(x0 - span, x0 + span, length.out = nsteps)
    ylo <- y0 - sqrt(r^2 - (x - x0)^2)
    yup <- y0 + sqrt(r^2 - (x - x0)^2)
    data.frame(x = x, ylo = ylo, yup = yup)
  }
  
  size <- 1
  col <- "black"
  
  if (league == "NBA") {
    
    x <- y <- ylo <- yup <- NULL
    outbox <- data.frame(x = c(-250, -250, 250, 250, -250), 
                         y = c(10, 470, 470, 10, 10))
    FT <- crcl(0, 200, 60)
    halfcourt <- crcl(0, 470, 60)
    key <- data.frame(x = c(-80, -80, 80, 80, -80), 
                      y = c(200, 10, 10, 200, 200))
    keyins <- data.frame(x = c(-60, -60, 60, 60, -60), 
                         y = c(200, 10, 10, 200, 200))
    restr <- crcl(x0 = 0, y0 = 70, r = 40, nsteps = 200)
    rim <- crcl(x0 = 0, y0 = 65, r = 7.5)
    backboard <- data.frame(x = c(-30, 30), y = c(50, 50))
    crcl3pt <- crcl(x0 = 0, y0 = 65, r = 230.75, span = 220)
    ln3pt <- data.frame(x = c(-220, -220, crcl3pt$x, 220, 220), 
                        ylo = c(10, 10 + 169/12, crcl3pt$ylo, 10 + 169/12, 10), 
                        yup = c(10, 10 + 169/12, crcl3pt$yup, 10 + 169/12, 10))
    
    ggplot(...) +
      geom_path(data = outbox, aes(x, y), size = size, color = col) +
      geom_path(data = halfcourt, aes(x = x,  y = ylo), size = size, color = col) +
      geom_path(data = FT,
                aes(x = x, y = yup), size = size, color = col) +
      geom_path(data = FT, aes(x = x, y = ylo), linetype = "dashed", size = size, color = col) +
      geom_path(data = key, aes(x, y), size = size, color = col) +
      geom_path(data = keyins, aes(x, y), size = size,
                color = col) +
      geom_path(data = restr, aes(x = x, y = yup), size = size, color = col) +
      geom_path(data = rim, aes(x = x, y = ylo), size = size, color = col) +
      geom_path(data = rim, aes(x = x, y = yup), size = size, color = col) +
      geom_path(data = backboard, aes(x, y), lineend = "butt", size = size, color = col) +
      geom_path(data = ln3pt, aes(x = x, y = yup), size = size,
                color = col) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
  }
  
  else if (league == "WNBA") {
    
    x <- y <- ylo <- yup <- NULL
    outbox <- data.frame(x = c(-250, -250, 250, 250, -250), 
                         y = c(10, 470, 470, 10, 10))
    FT <- crcl(0, 200, 60)
    halfcourt <- crcl(0, 470, 60)
    key <- data.frame(x = c(-80, -80, 80, 80, -80), 
                      y = c(200, 10, 10, 200, 200))
    keyins <- data.frame(x = c(-60, -60, 60, 60, -60), 
                         y = c(200, 10, 10, 200, 200))
    restr <- crcl(x0 = 0, y0 = 70, r = 40, nsteps = 200)
    rim <- crcl(x0 = 0, y0 = 65, r = 7.5)
    backboard <- data.frame(x = c(-30, 30), y = c(50, 50))
    crcl3pt <- crcl(x0 = 0, y0 = 50, r = 220.15, span = 210.65)
    ln3pt <- data.frame(x = c(-210.65, -210.65, crcl3pt$x, 210.65, 210.65), 
                        ylo = c(10, 10 + 169/12, crcl3pt$ylo, 10 + 169/12, 10), 
                        yup = c(10, 10 + 169/12, crcl3pt$yup, 10 + 169/12, 10))
    
    ggplot(...) +
      geom_path(data = outbox, aes(x, y), size = size, color = col) +
      geom_path(data = halfcourt, aes(x = x,  y = ylo), size = size, color = col) +
      geom_path(data = FT,
                aes(x = x, y = yup), size = size, color = col) +
      geom_path(data = FT, aes(x = x, y = ylo), linetype = "dashed", size = size, color = col) +
      geom_path(data = key, aes(x, y), size = size, color = col) +
      geom_path(data = keyins, aes(x, y), size = size,
                color = col) +
      geom_path(data = restr, aes(x = x, y = yup), size = size, color = col) +
      geom_path(data = rim, aes(x = x, y = ylo), size = size, color = col) +
      geom_path(data = rim, aes(x = x, y = yup), size = size, color = col) +
      geom_path(data = backboard, aes(x, y), lineend = "butt", size = size, color = col) +
      geom_path(data = ln3pt, aes(x = x, y = yup), size = size,
                color = col) +
      coord_fixed() +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    
  } else {
    
    print("Try NBA or WNBA")
    
  }
  
}

# court(league = "NBA")
# court(league = "WNBA")

#this is all the user interface of the code
ui <- fluidPage(
  navbarPage("Basketball Plotter",
             tabPanel("Plotter",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(
                                selectInput("team", "Team:", choices = teams),
                                selectInput("defense", "Defensive Set-up:",
                                            choices = c(defense)),
                                selectInput("hand", "Shot Hand:",
                                            choices = c(NA, hand)),
                                width = 6
                              ),
                              column(
                                selectInput("player", "Player:",
                                            choices = c(players)),
                                
                                selectInput("action", "Action Taken:",
                                            choices = c(actions)),
                                
                                
                                selectInput("shot_type", "Shot Type:",
                                            choices = c(NA, shot_type)),
                                width = 6
                              )
                            ),
                            fluidRow(
                              column(
                                selectInput("pick_roll", "Pick and Roll Type?",
                                            choices = c(NA, "None", pick_roll)),
                                width = 6
                              )
                            ),
                            width = 6
                          ),
                          mainPanel(
                            plotOutput("court", click = "plot_click"),
                            width = 6
                          )
                        ),
                        hr(style = "border-color:black;"),
                        DTOutput("table"),
                        hr(style = "border-color:white;"))))

)

#this is the backend of the code
server <- function(input, output, session) {
  
  updateSelectInput(session, 
                    "team", choices = unique(players$Team))
  #updating which players you can select based on what team is currently selected
  observeEvent(input$team,
               {
                 df <- players %>%
                   filter(Team == input$team) %>%
                   dplyr::select(Player)
                 
                 updateSelectInput(session, "player", choices = df)
               })
  
  val <- reactiveValues(clickx = NULL, clicky = NULL)
  
  observe({
    input$plot_click
    isolate({
      val$clickx = c(val$clickx, input$plot_click$x)
      val$clicky = c(val$clicky, input$plot_click$y)
    })
  }) #adding clicks to list
  
  #plotting the clicked points on the court
  output$court <- renderPlot(
    court(league = "NBA") +
      #^^^ is the line you would change to 'WNBA'
    geom_point(aes(val$clickx, val$clicky), size = 3)
  )
  
  #https://stackoverflow.com/questions/49190820/create-data-set-from-clicks-in-shiny-ggplot
  #where I pulled some of the code from
  
  #this section creates a data.frame that updates for our inputs
  values <- reactiveValues()
  values$DT <- data.frame(
    Team = character(),
    Player = character(),
    x = numeric(),
    y = numeric(),
    action = character(),
    def_set_up = character(),
    hand = character(),
    shot_type = character(),
    pick_and_roll = character()
  )
  
  #here we take the plot_clicks and actually make the data.fram
  observeEvent(input$plot_click,
               {
                 add_row <- data.frame(
                   Team = input$team,
                   Player = input$player,
                   x = round(input$plot_click$x, digits = 2),
                   y = round(input$plot_click$y, digits = 2),
                   action = input$action,
                   def_set_up = input$defense,
                   hand = input$hand, 
                   shot_type = input$shot_type,
                   pick_and_roll = input$pick_roll
                   # result = input$result,
                   # def_set_up = input$defense
                 )
                 
                 values$DT <- rbind(values$DT, add_row)
               }
  )
  
  #rendering the table with the download CSV and Excel buttons
  output$table <- renderDT({
    values$DT %>% 
      datatable(extensions = c('Buttons', 'Responsive'), 
                caption = "Use 'CSV' or 'Excel' button to download results", 
                rownames = FALSE, options = list(dom = 'Btlip', buttons = c('csv', 'excel')))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
