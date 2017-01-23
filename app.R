library(shiny)

shinyApp(
  
  ui <- fluidPage(
    
    titlePanel("qb_scheduler"),
    
    navlistPanel(
        tabPanel("Tournament Information",    
        textInput("tournament_name", "Tournament Name"),
        textInput("location", "Tournament Location"),
        textInput("date", "Tournament Date")
                ),
        
        tabPanel("Tournament Format",
        selectInput("pool_count", "Number of Pools", 1:8),
        uiOutput("pool_input"),
        textAreaInput("pool_names",
                      "Pool Names (one per line)",
                      "",
                      width = "100%",
                      height = "100px")
                  ),
        
        tabPanel("Teams and Rooms",
        textAreaInput("room_names",
                      "Room Names (one per line, ordered by pool, including byes)",
                        "",
                       width = "100%",
                       height = "150px"),
        textAreaInput("team_names",
                      "Team Names (one per line, ordered by seed)",
                      "",
                      width = "100%",
                      height = "250px")
                 ),
        tabPanel("Brackets and Schedule", 
                 uiOutput("main"),
                 checkboxInput("orientation", "Landscape orientation", value = FALSE),
                 downloadButton("download")
                 )
    )
  ),
  
  server <- function(input, output) {
    
    teams <- reactive({
      strsplit(input$team_names, "\n")[[1]]
    })
    
    rooms <- reactive({
      strsplit(input$room_names, "\n")[[1]]
    })
    
    pools <- reactive({
      strsplit(input$pool_names, "\n")[[1]]
    })

    output$main <- renderUI ({
      tabs <- list()
      tabs[[1]] <- tabPanel("Brackets", tableOutput("brackets"))
      
      for (i in 2:(as.integer(input$pool_count) + 1)){
        tabs[[i]] <- tabPanel(pools()[i-1], tableOutput(paste0("schedule", i-1)))
      }
      
      do.call(tabsetPanel, tabs)
    })
    
    output$pool_input <- renderUI ({
      pool_count <- as.integer(input$pool_count)
      
      lapply(1:pool_count, function(i) {
        selectInput(inputId = paste0("poolsize", i), label = paste("Size of Pool ", i),
                    choices = 4:12)
      })
    })
    
    orientation <- reactive ({ ifelse(input$orientation, "landscape", "portrait") })
    
    brackets <- reactive({
      static_teams <- teams()
      
      brackets <- matrix(0, nrow = as.integer(max(pool_size(), na.rm = TRUE)), ncol = as.integer(input$pool_count))
      
      for(seed in 1:max(pool_size(), na.rm = TRUE)){
        direction <- if(seed %% 2 == 1) {1:input$pool_count} else {input$pool_count:1}
        for(pool in direction) {
          brackets[seed, pool] <- static_teams[1]
          static_teams <- static_teams[-1]
        }
      }
      
      return(brackets)
    })
    
    bracketer <- function() {
      static_teams <- teams()
      
      brackets <- matrix(0, nrow = as.integer(max(pool_size(), na.rm = TRUE)), ncol = as.integer(input$pool_count))
      
      for(seed in 1:max(pool_size(), na.rm = TRUE)){
        direction <- if(seed %% 2 == 1) {1:input$pool_count} else {input$pool_count:1}
        for(pool in direction) {
          brackets[seed, pool] <- static_teams[1]
          static_teams <- static_teams[-1]
        }
      }
      
      dimnames(brackets) <- list(NULL, pools()[1:input$pool_count])
      return(brackets)
    }
    
    brackets <- reactive({ bracketer() })
    output$brackets <- renderTable({ bracketer() })
    
    four_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "))
      schedule <- matrix(games, 3, 3,
                         dimnames = list(NULL, c("Round", "Room A", "Room B")))
      schedule <- set_rooms(schedule, rooms, ncol(schedule))
      return(schedule)
    }
    
    five_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[4], teams[5], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 
                 paste0("Bye: ", teams[5]),
                 paste0("Bye: ", teams[4]),
                 paste0("Bye: ", teams[3]),
                 paste0("Bye: ", teams[2]),
                 paste0("Bye: ", teams[1]))
      schedule <- matrix(games, 5, 3,
                         dimnames = list(NULL, c("Round", "Room A", "Room B", "Room C")))
      schedule <- set_rooms(schedule, c(rooms, "Bye"), ncol(schedule))
      return(schedule)
    }
    
    six_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 paste(teams[5], teams[3], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[3], teams[1], sep = " vs. "),
                 paste(teams[6], teams[4], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[6], teams[1], sep = " vs. "),
                 paste(teams[5], teams[6], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[4], teams[5], sep = " vs. "),
                 
                 paste(teams[6], teams[2], sep = " vs. "),
                 paste(teams[4], teams[3], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[3], teams[6], sep = " vs. "))
      schedule <- matrix(games, 5, 4,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C")))
      schedule <- set_rooms(schedule, rooms, ncol(schedule))
      return(schedule)
    }
    
    seven_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 "Round 6",
                 "Round 7",
                 paste(teams[1], teams[6], sep = " vs. "),
                 paste(teams[5], teams[7], sep = " vs. "),
                 paste(teams[4], teams[6], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[2], teams[7], sep = " vs. "),
                 
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[3], teams[7], sep = " vs. "),
                 paste(teams[2], teams[6], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[4], teams[7], sep = " vs. "),
                 paste(teams[3], teams[6], sep = " vs. "),
                 
                 paste(teams[3], teams[4], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 paste(teams[1], teams[7], sep = " vs. "),
                 paste(teams[6], teams[7], sep = " vs. "),
                 paste(teams[5], teams[6], sep = " vs. "),
                 paste(teams[4], teams[5], sep = " vs. "),
                 
                 paste0("Bye: ", teams[7]),
                 paste0("Bye: ", teams[6]),
                 paste0("Bye: ", teams[5]),
                 paste0("Bye: ", teams[4]),
                 paste0("Bye: ", teams[3]),
                 paste0("Bye: ", teams[2]),
                 paste0("Bye: ", teams[1]))
      schedule <- matrix(games, 7, 5,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C", "Bye")))
      schedule <- set_rooms(schedule, c(rooms, "Bye"), ncol(schedule))
      return(schedule)
    }
    
    eight_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 "Round 6",
                 "Round 7",
                 paste(teams[2], teams[7], sep = " vs. "),
                 paste(teams[3], teams[7], sep = " vs. "),
                 paste(teams[5], teams[6], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[5], teams[8], sep = " vs. "),
                 paste(teams[6], teams[8], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 
                 paste(teams[1], teams[8], sep = " vs. "),
                 paste(teams[4], teams[6], sep = " vs. "),
                 paste(teams[3], teams[8], sep = " vs. "),
                 paste(teams[5], teams[7], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[7], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 
                 paste(teams[3], teams[6], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[4], teams[7], sep = " vs. "),
                 paste(teams[4], teams[8], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[2], teams[6], sep = " vs. "),
                 
                 paste(teams[4], teams[5], sep = " vs. "),
                 paste(teams[2], teams[8], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 paste(teams[1], teams[6], sep = " vs. "),
                 paste(teams[6], teams[7], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 paste(teams[7], teams[8], sep = " vs. "))
      schedule <- matrix(games, 7, 5,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C", "Room D")))
      schedule <- set_rooms(schedule, rooms, ncol(schedule))
      return(schedule)
    }
    
    nine_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 "Round 6",
                 "Round 7",
                 "Round 8",
                 "Round 9",
                 paste(teams[1], teams[8], sep = " vs. "),
                 paste(teams[7], teams[9], sep = " vs. "),
                 paste(teams[6], teams[8], sep = " vs. "),
                 paste(teams[5], teams[7], sep = " vs. "),
                 paste(teams[4], teams[6], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[2], teams[9], sep = " vs. "),
                 
                 paste(teams[2], teams[7], sep = " vs. "),
                 paste(teams[1], teams[6], sep = " vs. "),
                 paste(teams[5], teams[9], sep = " vs. "),
                 paste(teams[4], teams[8], sep = " vs. "),
                 paste(teams[3], teams[7], sep = " vs. "),
                 paste(teams[2], teams[6], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[4], teams[9], sep = " vs. "),
                 paste(teams[3], teams[8], sep = " vs. "),
                 
                 paste(teams[3], teams[6], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[3], teams[9], sep = " vs. "),
                 paste(teams[2], teams[8], sep = " vs. "),
                 paste(teams[1], teams[7], sep = " vs. "),
                 paste(teams[6], teams[9], sep = " vs. "),
                 paste(teams[5], teams[8], sep = " vs. "),
                 paste(teams[4], teams[7], sep = " vs. "),
                 
                 paste(teams[4], teams[5], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 paste(teams[1], teams[9], sep = " vs. "),
                 paste(teams[8], teams[9], sep = " vs. "),
                 paste(teams[7], teams[8], sep = " vs. "),
                 paste(teams[6], teams[7], sep = " vs. "),
                 paste(teams[5], teams[6], sep = " vs. "),
                 
                 paste0("Bye: ", teams[9]),
                 paste0("Bye: ", teams[8]),
                 paste0("Bye: ", teams[7]),
                 paste0("Bye: ", teams[6]),
                 paste0("Bye: ", teams[5]),
                 paste0("Bye: ", teams[4]),
                 paste0("Bye: ", teams[3]),
                 paste0("Bye: ", teams[2]),
                 paste0("Bye: ", teams[1]))
      schedule <- matrix(games, 9, 6,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C", "Room D", "Bye")))
      schedule <- set_rooms(schedule, c(rooms, "Bye"), ncol(schedule))
      return(schedule)
    }
    
    ten_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 "Round 6",
                 "Round 7",
                 "Round 8",
                 "Round 9",
                 paste(teams[2], teams[9], sep = " vs. "),
                 paste(teams[3], teams[6], sep = " vs. "),
                 paste(teams[1], teams[8], sep = " vs. "),
                 paste(teams[5], teams[9], sep = " vs. "),
                 paste(teams[2], teams[10], sep = " vs. "),
                 paste(teams[3], teams[7], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[6], teams[9], sep = " vs. "),
                 paste(teams[3], teams[10], sep = " vs. "),
                 
                 paste(teams[1], teams[10], sep = " vs. "),
                 paste(teams[8], teams[10], sep = " vs. "),
                 paste(teams[7], teams[9], sep = " vs. "),
                 paste(teams[6], teams[8], sep = " vs. "),
                 paste(teams[5], teams[7], sep = " vs. "),
                 paste(teams[4], teams[6], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 
                 paste(teams[3], teams[8], sep = " vs. "),
                 paste(teams[2], teams[7], sep = " vs. "),
                 paste(teams[6], teams[10], sep = " vs. "),
                 paste(teams[1], teams[7], sep = " vs. "),
                 paste(teams[4], teams[8], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[2], teams[6], sep = " vs. "),
                 paste(teams[5], teams[10], sep = " vs. "),
                 paste(teams[4], teams[9], sep = " vs. "),
                 
                 paste(teams[4], teams[7], sep = " vs. "),
                 paste(teams[1], teams[9], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[4], teams[10], sep = " vs. "),
                 paste(teams[3], teams[9], sep = " vs. "),
                 paste(teams[2], teams[8], sep = " vs. "),
                 paste(teams[7], teams[10], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[5], teams[8], sep = " vs. "),
                 
                 paste(teams[5], teams[6], sep = " vs. "),
                 paste(teams[4], teams[5], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[1], teams[6], sep = " vs. "),
                 paste(teams[9], teams[10], sep = " vs. "),
                 paste(teams[8], teams[9], sep = " vs. "),
                 paste(teams[7], teams[8], sep = " vs. "),
                 paste(teams[6], teams[7], sep = " vs. "))
      schedule <- matrix(games, 9, 6,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C", "Room D", "Room E")))
      schedule <- set_rooms(schedule, rooms, ncol(schedule))
      return(schedule)
    }
    
    eleven_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 "Round 6",
                 "Round 7",
                 "Round 8",
                 "Round 9",
                 "Round 10",
                 "Round 11",
                 paste(teams[1], teams[10], sep = " vs. "),
                 paste(teams[9], teams[11], sep = " vs. "),
                 paste(teams[8], teams[10], sep = " vs. "),
                 paste(teams[7], teams[9], sep = " vs. "),
                 paste(teams[6], teams[8], sep = " vs. "),
                 paste(teams[5], teams[7], sep = " vs. "),
                 paste(teams[4], teams[6], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[2], teams[11], sep = " vs. "),
                 
                 paste(teams[2], teams[9], sep = " vs. "),
                 paste(teams[1], teams[8], sep = " vs. "),
                 paste(teams[7], teams[11], sep = " vs. "),
                 paste(teams[6], teams[10], sep = " vs. "),
                 paste(teams[5], teams[9], sep = " vs. "),
                 paste(teams[4], teams[8], sep = " vs. "),
                 paste(teams[3], teams[7], sep = " vs. "),
                 paste(teams[2], teams[6], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[4], teams[11], sep = " vs. "),
                 paste(teams[3], teams[10], sep = " vs. "),
                 
                 paste(teams[3], teams[8], sep = " vs. "),
                 paste(teams[2], teams[7], sep = " vs. "),
                 paste(teams[1], teams[6], sep = " vs. "),
                 paste(teams[5], teams[11], sep = " vs. "),
                 paste(teams[4], teams[10], sep = " vs. "),
                 paste(teams[3], teams[9], sep = " vs. "),
                 paste(teams[2], teams[8], sep = " vs. "),
                 paste(teams[1], teams[7], sep = " vs. "),
                 paste(teams[6], teams[11], sep = " vs. "),
                 paste(teams[5], teams[10], sep = " vs. "),
                 paste(teams[4], teams[9], sep = " vs. "),
                 
                 paste(teams[4], teams[7], sep = " vs. "),
                 paste(teams[3], teams[6], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[3], teams[11], sep = " vs. "),
                 paste(teams[2], teams[10], sep = " vs. "),
                 paste(teams[1], teams[9], sep = " vs. "),
                 paste(teams[8], teams[11], sep = " vs. "),
                 paste(teams[7], teams[10], sep = " vs. "),
                 paste(teams[6], teams[9], sep = " vs. "),
                 paste(teams[5], teams[8], sep = " vs. "),
                 
                 paste(teams[5], teams[6], sep = " vs. "),
                 paste(teams[4], teams[5], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 paste(teams[1], teams[11], sep = " vs. "),
                 paste(teams[10], teams[11], sep = " vs. "),
                 paste(teams[9], teams[10], sep = " vs. "),
                 paste(teams[8], teams[9], sep = " vs. "),
                 paste(teams[7], teams[8], sep = " vs. "),
                 paste(teams[6], teams[7], sep = " vs. "),
                 
                 paste0("Bye: ", teams[11]),
                 paste0("Bye: ", teams[10]),
                 paste0("Bye: ", teams[9]),
                 paste0("Bye: ", teams[8]),
                 paste0("Bye: ", teams[7]),
                 paste0("Bye: ", teams[6]),
                 paste0("Bye: ", teams[5]),
                 paste0("Bye: ", teams[4]),
                 paste0("Bye: ", teams[3]),
                 paste0("Bye: ", teams[2]),
                 paste0("Bye: ", teams[1]))
      schedule <- matrix(games, 11, 7,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C", "Room D", "Room E",
                                           "Room F")))
      schedule <- set_rooms(schedule, c(rooms, "Bye"), ncol(schedule))
      return(schedule)
    }
    
    twelve_team_RR <- function(teams, rooms){
      games <- c("Round 1",
                 "Round 2",
                 "Round 3",
                 "Round 4",
                 "Round 5",
                 "Round 6",
                 "Round 7",
                 "Round 8",
                 "Round 9",
                 "Round 10",
                 "Round 11",
                 paste(teams[2], teams[11], sep = " vs. "),
                 paste(teams[5], teams[9], sep = " vs. "),
                 paste(teams[5], teams[10], sep = " vs. "),
                 paste(teams[4], teams[12], sep = " vs. "),
                 paste(teams[8], teams[9], sep = " vs. "),
                 paste(teams[3], teams[4], sep = " vs. "),
                 paste(teams[8], teams[11], sep = " vs. "),
                 paste(teams[2], teams[7], sep = " vs. "),
                 paste(teams[3], teams[7], sep = " vs. "),
                 paste(teams[10], teams[12], sep = " vs. "),
                 paste(teams[1], teams[6], sep = " vs. "),
                 
                 paste(teams[1], teams[12], sep = " vs. "),
                 paste(teams[6], teams[8], sep = " vs. "),
                 paste(teams[3], teams[12], sep = " vs. "),
                 paste(teams[7], teams[9], sep = " vs. "),
                 paste(teams[2], teams[4], sep = " vs. "),
                 paste(teams[8], teams[10], sep = " vs. "),
                 paste(teams[3], teams[5], sep = " vs. "),
                 paste(teams[9], teams[11], sep = " vs. "),
                 paste(teams[4], teams[6], sep = " vs. "),
                 paste(teams[1], teams[11], sep = " vs. "),
                 paste(teams[5], teams[7], sep = " vs. "),
                 
                 paste(teams[3], teams[10], sep = " vs. "),
                 paste(teams[1], teams[7], sep = " vs. "),
                 paste(teams[4], teams[11], sep = " vs. "),
                 paste(teams[6], teams[10], sep = " vs. "),
                 paste(teams[5], teams[12], sep = " vs. "),
                 paste(teams[7], teams[11], sep = " vs. "),
                 paste(teams[2], teams[6], sep = " vs. "),
                 paste(teams[8], teams[12], sep = " vs. "),
                 paste(teams[1], teams[5], sep = " vs. "),
                 paste(teams[2], teams[9], sep = " vs. "),
                 paste(teams[4], teams[8], sep = " vs. "),
                 
                 paste(teams[4], teams[9], sep = " vs. "),
                 paste(teams[4], teams[10], sep = " vs. "),
                 paste(teams[1], teams[2], sep = " vs. "),
                 paste(teams[5], teams[11], sep = " vs. "),
                 paste(teams[6], teams[11], sep = " vs. "),
                 paste(teams[6], teams[12], sep = " vs. "),
                 paste(teams[7], teams[12], sep = " vs. "),
                 paste(teams[1], teams[10], sep = " vs. "),
                 paste(teams[2], teams[8], sep = " vs. "),
                 paste(teams[3], teams[8], sep = " vs. "),
                 paste(teams[3], teams[9], sep = " vs. "),
                 
                 paste(teams[5], teams[8], sep = " vs. "),
                 paste(teams[3], teams[11], sep = " vs. "),
                 paste(teams[6], teams[9], sep = " vs. "),
                 paste(teams[1], teams[8], sep = " vs. "),
                 paste(teams[7], teams[10], sep = " vs. "),
                 paste(teams[2], teams[5], sep = " vs. "),
                 paste(teams[1], teams[4], sep = " vs. "),
                 paste(teams[3], teams[6], sep = " vs. "),
                 paste(teams[9], teams[12], sep = " vs. "),
                 paste(teams[4], teams[7], sep = " vs. "),
                 paste(teams[2], teams[10], sep = " vs. "),
                 
                 paste(teams[6], teams[7], sep = " vs. "),
                 paste(teams[2], teams[12], sep = " vs. "),
                 paste(teams[7], teams[8], sep = " vs. "),
                 paste(teams[2], teams[3], sep = " vs. "),
                 paste(teams[1], teams[3], sep = " vs. "),
                 paste(teams[1], teams[9], sep = " vs. "),
                 paste(teams[9], teams[10], sep = " vs. "),
                 paste(teams[4], teams[5], sep = " vs. "),
                 paste(teams[10], teams[11], sep = " vs. "),
                 paste(teams[5], teams[6], sep = " vs. "),
                 paste(teams[11], teams[12], sep = " vs. "))
      schedule <- matrix(games, 11, 7,
                         dimnames = list(NULL, 
                                         c("Round", "Room A", "Room B", 
                                           "Room C", "Room D", "Room E",
                                           "Room F")))
      schedule <- set_rooms(schedule, rooms, ncol(schedule))
      return(schedule)
    }
    
    scheduler <- function(teams, rooms, pool_size){
      switch(pool_size,
             "4" = four_team_RR(teams, rooms),
             "5" = five_team_RR(teams, rooms),
             "6" = six_team_RR(teams, rooms),
             "7" = seven_team_RR(teams, rooms),
             "8" = eight_team_RR(teams, rooms),
             "9" = nine_team_RR(teams, rooms),
             "10" = ten_team_RR(teams, rooms),
             "11" = eleven_team_RR(teams, rooms),
             "12" = twelve_team_RR(teams, rooms))
    }
    
    set_rooms <- function(schedule, rooms, number){
      rooms <- c(c("Round"), rooms)
      dimnames(schedule) <- list(NULL, rooms[1:number])
      return(schedule)
    }
    
    pool1 <- reactive({ na.omit(brackets()[,1]) })
    pool2 <- reactive({ na.omit(brackets()[,2]) })
    pool3 <- reactive({ na.omit(brackets()[,3]) })
    pool4 <- reactive({ na.omit(brackets()[,4]) })
    pool5 <- reactive({ na.omit(brackets()[,5]) })
    pool6 <- reactive({ na.omit(brackets()[,6]) })
    pool7 <- reactive({ na.omit(brackets()[,7]) })
    pool8 <- reactive({ na.omit(brackets()[,8]) })
    
    room1 <- reactive({ rooms()[1:room_size(1)] })
    room2 <- reactive({ rooms()[(room_size(1)+1):room_size(2)] })
    room3 <- reactive({ rooms()[(room_size(2)+1):room_size(3)] })
    room4 <- reactive({ rooms()[(room_size(3)+1):room_size(4)] })
    room5 <- reactive({ rooms()[(room_size(4)+1):room_size(5)] })
    room6 <- reactive({ rooms()[(room_size(5)+1):room_size(6)] })
    room7 <- reactive({ rooms()[(room_size(6)+1):room_size(7)] })
    room8 <- reactive({ rooms()[(room_size(7)+1):room_size(8)] })
    
    schedule1 <- reactive({ scheduler(pool1(), room1(), pool_size()[1]) })
    output$schedule1 <- renderTable({ scheduler(pool1(), room1(), pool_size()[1]) })
    
    schedule2 <- reactive({ scheduler(pool2(), room2(), pool_size()[2]) })
    output$schedule2 <- renderTable({ scheduler(pool2(), room2(), pool_size()[2]) })
    
    schedule3 <- reactive({ scheduler(pool3(), room3(), pool_size()[3]) })
    output$schedule3 <- renderTable({ scheduler(pool3(), room3(), pool_size()[3]) })
    
    schedule4 <- reactive({ scheduler(pool4(), room4(), pool_size()[4]) })
    output$schedule4 <- renderTable({ scheduler(pool4(), room4(), pool_size()[4]) })
    
    schedule5 <- reactive({ scheduler(pool5(), room5(), pool_size()[5]) })
    output$schedule5 <- renderTable({ scheduler(pool5(), room5(), pool_size()[5]) })
    
    schedule6 <- reactive({ scheduler(pool6(), room6(), pool_size()[6]) })
    output$schedule6 <- renderTable({ scheduler(pool6(), room6(), pool_size()[6]) })
    
    schedule7 <- reactive({ scheduler(pool7(), room7(), pool_size()[7]) })
    output$schedule7 <- renderTable({ scheduler(pool7(), room7(), pool_size()[7]) })
    
    schedule8 <- reactive({ scheduler(pool8(), room8(), pool_size()[8]) })
    output$schedule8 <- renderTable({ scheduler(pool8(), room8(), pool_size()[8]) })
    
    pool_size <- reactive({
      pool_sizes <- c(input$poolsize1, input$poolsize2, input$poolsize3, input$poolsize4,
                      input$poolsize5, input$poolsize6, input$poolsize7, input$poolsize8)
      pool_size <- c(1)
      for (i in 1:input$pool_count){
        pool_size[i] <- pool_sizes[i]
      }
      return(pool_size)
    })
    
    room_size <- function(pool){
      total <- 0
      for(i in pool:1){
        room_size <- floor(as.integer(pool_size()[i])/2)
        total <- total + room_size
      }
      return(total)
    }
    
    output$download <- downloadHandler(
      filename = 'schedule.pdf',
      
      content = function(file) {
        out = knitr::knit2pdf('schedule.Rnw', compiler = 'xelatex')
        file.rename(out, file) # move pdf to file for downloading
      },
      
      contentType = 'application/pdf'
    )
    
  },
  
  options = list(height = 1000, width = 900)
  
)
