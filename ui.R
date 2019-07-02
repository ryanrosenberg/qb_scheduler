shinyUI(
    fluidPage(
        titlePanel("ACF Scheduler"),
        tabsetPanel(
            tabPanel("Seeding",
                     column(width = 4,
                            selectInput("site", label = "Site:",
                                        choices = c("Northeast", "Midwest"),
                                        selected = "Midwest"),
                            selectInput('num_brackets', "Number of Brackets",
                                        choices = 1:2),
                            uiOutput('team_seeding')),
                     column(width = 4,
                            h2("Bracket A"),
                            tableOutput("bracket_a_seeds")),
                     column(width = 4,
                            h2("Bracket B"),
                            tableOutput("bracket_b_alt_seeds"))
            ),
            tabPanel("Scheduling",
                     fluidRow(
                         column(2, 
                                textInput('tournament_name',
                                          'Tournament Name'),
                                uiOutput('bracket_format_select'),
                                uiOutput('bracket_name_entry'),
                                uiOutput('bracket_rooms_entry'),
                                actionButton('apply_names',
                                             'Apply')),
                         column(5,
                                h2(textOutput('bracket_a_title')),
                                tableOutput("bracket_a_schedule")),
                         column(5,
                                h2(textOutput('bracket_b_title')),
                                tableOutput("bracket_b_schedule")))
            ),
            tabPanel("Packet Assignment",
                     column(2,uiOutput('packet_selectors'),
                            downloadButton('final_schedule')),
                     column(5,tableOutput("bracket_a_packet_schedule")),
                     column(5,tableOutput("bracket_b_packet_schedule"))
                     )
        )
    )
)
