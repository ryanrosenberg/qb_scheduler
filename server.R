shinyServer(function(input, output) {
  output$team_seeding <- renderUI({
    ifelse(input$num_brackets == 1,
           bucket_list(
             header = "Brackets",
             add_rank_list("Bracket A",
                           labels = teams %>% 
                             filter(Site == input$site) %>% 
                             pull(Team),
                           input_id = "bracket_a")),
           bucket_list(
             header = "Brackets",
             add_rank_list("Teams",
                           labels = teams %>% 
                             filter(Site == input$site) %>% 
                             pull(Team),
                           input_id = "extra_teams"),
             add_rank_list("Bracket A",
                           labels = NULL,
                           input_id = "bracket_a"),
             add_rank_list("Bracket B",
                           labels = NULL,
                           input_id = "bracket_b")
           )
    )
  })
  
  bracket_sizes <- reactive({list(length(input$bracket_a),
                                  length(input$bracket_b))})
  
  output$bracket_format_select <- renderUI({
    lapply(1:input$num_brackets, function(i){
      selectInput(glue("bracket_{letters[i]}_format"),
                  glue("Bracket {LETTERS[i]} Format"),
                  choices = formats %>% 
                    filter(Teams == bracket_sizes()[i]) %>% 
                    pull(Format))
    })
  })
  
  output$bracket_name_entry <- renderUI({
    lapply(1:input$num_brackets, function(i){
      textInput(glue("bracket_{letters[i]}_name"),
                glue("Bracket {LETTERS[i]} Name"),
                glue("Bracket {LETTERS[i]}"))
    })
  })
  
  output$bracket_rooms_entry <- renderUI({
    lapply(1:input$num_brackets, function(i){
      textInput(glue("bracket_{letters[i]}_rooms"),
                glue("Bracket {LETTERS[i]} Rooms (separate with commas)"),
                paste('Room', 1:floor(as.numeric(bracket_sizes()[i])/2), collapse = ", "))
    })
  })
  
  num_games <- reactive({max(str_extract(input$bracket_a_format, '[0-9]+(?=\\sround)'),
                             str_extract(input$bracket_b_format, '[0-9]+(?=\\sround)'))})
  
  output$packet_selectors <- renderUI({
    lapply(1:num_games(), function(i){
      selectInput(glue("round_{i}_packet"),
                  glue("Round {i} Packet"),
                  choices = packets)
    })
  })
  
  assigned_packets <- reactive({
    packet_list <- list()
    map(1:num_games(), function(i){
      packet_list[[i]] <- input[[paste0('round_', i,"_packet")]]
    })
  })
  
  output$bracket_a_seeds <- function() {
    if(is.null(input$bracket_a)) {tibble("Team" = "",
                                         "Seed" = "")}
    else{tibble('Seed' = seq_along(input$bracket_a),
                'Team' = input$bracket_a) %>% 
        kable("html") %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)}
  }
  output$bracket_b_alt_seeds <- function() {
    if(input$num_brackets == 1 | is.null(input$bracket_b)) {tibble("Team" = "",
                                                                   "Seed" = "")}
    else{tibble('Seed' = seq_along(input$bracket_b),
                'Team' = input$bracket_b) %>% 
        kable("html") %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)}
  }
  
  bracket_a_rooms <- eventReactive(input$apply_names, {input$bracket_a_rooms})
  bracket_b_rooms <- eventReactive(input$apply_names, {input$bracket_b_rooms})
  
  bracket_a_schedule_plain <- reactive({
    formats$data[formats$Format == input$bracket_a_format] %>% 
      pluck(1) %>% 
      mutate_all(~ifelse(is.na(.), "", .)) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "(?<!1)1(?![0-3])", input$bracket_a[1])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "(?<!1)2", input$bracket_a[2])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "(?<!1)3", input$bracket_a[3])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "4", input$bracket_a[4])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "5", input$bracket_a[5])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "6", input$bracket_a[6])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "7", input$bracket_a[7])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "8", input$bracket_a[8])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "9", input$bracket_a[9])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "10", input$bracket_a[10])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "11", input$bracket_a[11])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "12", input$bracket_a[12])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "13", input$bracket_a[13]))
  })
  bracket_b_schedule_plain <- reactive({
    formats$data[formats$Format == input$bracket_b_format] %>% 
      pluck(1) %>% 
      mutate_all(~ifelse(is.na(.), "", .)) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "1", input$bracket_b[1])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "2", input$bracket_b[2])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "3", input$bracket_b[3])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "4", input$bracket_b[4])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "5", input$bracket_b[5])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "6", input$bracket_b[6])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "7", input$bracket_b[7])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "8", input$bracket_b[8])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "9", input$bracket_b[9])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "10", input$bracket_b[10])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "11", input$bracket_b[11])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "12", input$bracket_b[12])) %>% 
      mutate_at(vars(-Round), ~str_replace_all(., "13", input$bracket_b[13]))
  })
  
  output$bracket_a_schedule <- function() {
    bracket_a_schedule_plain() %>% 
      set_names(str_split(bracket_a_rooms(), ", ")[[1]] %>% 
                  {if(bracket_sizes()[[1]] %% 2 == 1 |
                      str_detect(input$bracket_a_format, "bye")) {append(., "BYE", 0)}
                    else{.}} %>% 
                  append("Round", 0)) %>% 
      kable("html") %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) 
  }
  output$bracket_b_schedule <- function() {
    bracket_b_schedule_plain() %>% 
      set_names(str_split(bracket_b_rooms(), ", ")[[1]] %>% 
                  {if(bracket_sizes()[[2]] %% 2 == 1 |
                      str_detect(input$bracket_b_format, "bye")) {append(., "BYE", 0)}
                    else{.}} %>% 
                  append("Round", 0)) %>% 
      kable("html") %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)
  }
  
  output$bracket_a_packet_schedule <- function() {
    bracket_a_schedule_plain() %>% 
      unite("all_teams", starts_with("Room"), remove = F) %>% 
      set_names(str_split(bracket_a_rooms(), ", ")[[1]] %>% 
                  append("all_teams", 0) %>% 
                  {if(bracket_sizes()[[1]] %% 2 == 1 |
                      str_detect(input$bracket_a_format, "bye")) {append(., "BYE", 0)}
                    else{.}} %>% 
                  append("Round", 0)) %>%
      mutate(Packet = assigned_packets()[1:dim(bracket_a_schedule_plain())[[1]]],
             packet_teams = str_split(Packet, " \\+ "),
             all_teams = str_replace_all(all_teams, " v ", "_"),
             all_teams = str_split(all_teams, "_"),
             duplicate = duplicated(Packet)) %>% 
      rowwise() %>%
      mutate(intersection = length(intersect(packet_teams, all_teams))) %>% 
      ungroup() %>%
      mutate(Packet = cell_spec(Packet, "html", 
                                background = ifelse(intersection > 0 | duplicate == T,
                                                    "#FF7E81", "#85FFAE")),
             Notes = case_when(duplicate == T ~ "Packet repeated",
                               intersection > 0 ~ "Team playing on own packet",
                               T ~ "")) %>%
      select(-all_teams, -packet_teams, -intersection, -duplicate) %>%
      kable("html", escape = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) 
  }
  output$bracket_b_packet_schedule <- function() {
    bracket_b_schedule_plain() %>% 
      unite("all_teams", starts_with("Room"), remove = F) %>% 
      set_names(str_split(bracket_b_rooms(), ", ")[[1]] %>% 
                  append("all_teams", 0) %>% 
                  {if(bracket_sizes()[[2]] %% 2 == 1 |
                      str_detect(input$bracket_b_format, "bye")) {append(., "BYE", 0)}
                    else{.}} %>% 
                  append("Round", 0)) %>%
      mutate(Packet = assigned_packets()[1:dim(bracket_b_schedule_plain())[[1]]],
             packet_teams = str_split(Packet, " \\+ "),
             all_teams = str_replace_all(all_teams, " v ", "_"),
             all_teams = str_split(all_teams, "_"),
             duplicate = duplicated(Packet)) %>% 
      rowwise() %>%
      mutate(intersection = length(intersect(packet_teams, all_teams))) %>% 
      ungroup() %>%
      mutate(Packet = cell_spec(Packet, "html",
                                background = ifelse(intersection > 0 | duplicate == T,
                                                    "#FF7E81", "#85FFAE")),
             Notes = case_when(duplicate == T ~ "Packet repeated",
                               intersection > 0 ~ "Team playing on own packet",
                               T ~ "")) %>%
      select(-all_teams, -packet_teams, -intersection, -duplicate) %>%
      kable("html", escape = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)
  }
  
  output$final_schedule <- downloadHandler(
    filename = 'myreport.pdf',
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )  
  
  bracket_a_name <- eventReactive(input$apply_names, {input$bracket_a_name})
  bracket_b_name <- eventReactive(input$apply_names, {input$bracket_b_name})
  
  output$bracket_a_title <- renderText(
    bracket_a_name()
  )
  
  output$bracket_b_title <- renderText(
    bracket_b_name()
  )
  
  
})
