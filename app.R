library(dashboardthemes)
library(DBI)
library(dplyr)
library(DT)
library(glue)
library(here)
library(lubridate)
library(pool)
library(sever)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(tuneR)
library(waiter)

source(here("www", "misc_pages.R"))
source(here("www", "theme.R"))
source(here("www", "tabs", "workout", "submodules", "lift_tabs.R"))

for (tab_dir in list.files(here("www", "tabs"))){
    for (r_file in list.files(here("www", "tabs", tab_dir), pattern = "*.R")) {
        source(here("www", "tabs", tab_dir, r_file))
    }
}

#### Define Header =============================================================
db_title <- shinyDashboardLogoDIY(
    boldText = "Gym",
    mainText = "Rat",
    textSize = 18,
    badgeText = "Kip",
    badgeTextColor = "white",
    badgeTextSize = 2,
    badgeBackColor = get_color("primary", "hex"),
    badgeBorderRadius = 3
)

db_header <- dashboardHeaderPlus(
    title = tagList(span(class = "logo-lg", db_title), strong("GR")),
    titleWidth = 250,
    disable = FALSE,
    left_menu = NULL,
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "th",
    fixed = FALSE,
    
    dropdownMenuOutput(outputId = 'header_todos')
)

#### Define Left Sidebar =======================================================
# Define Menu Items Here
workout_menu <- menuItem(
    text = "Workout",
    tabName = "workout_page",
    icon = icon("home")
)
# pred_menu <- menuItem(
#     text = "Predictions",
#     tabName = "prediction_page",
#     icon = icon("baseball-ball")
# )

# Put Menu in the Sidebar
db_left_sidebar <- dashboardSidebar(
    sidebarMenu(id = "sb_menu", workout_menu)
)

#### Define Body ===============================================================
# Define Tab Items
workout_tab <- tabItem(tabName = "workout_page", workout_ui("workout_module"))
# pred_tab <- tabItem(
#     tabName = "prediction_page", 
#     prediction_ui(
#         id = "prediction_page", 
#         pitcher_choices = pitcher_choices, 
#         batter_choices = batter_choices
#     )
# )

# Define the body object
db_body <- dashboardBody(
    # Add custom css file to the header
    tags$head(
        tags$link(
            rel = "stylesheet",
            type = "text/css",
            href = "custom.css"
        )
    ),
    
    # Add sever for server disconnect page, and waiter for loading page
    use_sever(),
    use_waiter(),
    waiter_show_on_load(custom_loading_page()),
    
    # Put all dashboard tabs here
    tabItems(
        workout_tab
    )
)

#### Assemble Dashboard ========================================================
ui <- dashboardPagePlus(
    dashboard_theme,
    
    header = db_header,
    sidebar = db_left_sidebar,
    body = db_body
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #### Initialize Application Server =========================================
    # The first thing I do here is source all of my helper files. I could do 
    # this in global.R, but I really like using waiter. It makes the app jump to
    # a loading page instantly. global.R is sourced before ui and server, so 
    # the loading screen wouldn't show until it was essentially done if I put 
    # this in global.R.
    # for (file in list.files(here::here("www", "helpers"))) {
    #     source(here::here("www", "helpers", file))
    # }
    
    # Initialize global variables
    global <- reactiveValues(
        test = NULL,
        con = {
            DBI::dbConnect(
                drv = RPostgres::Postgres(),
                dbname = "gym_rat",
                host = "192.168.1.23",
                port = 5432,
                user = "svc_gym_rat",
                password = "get_big",
                options = "-c search_path=kip"
            )
        }
    )
    
    # Once we are ready to go, hide the waiter
    waiter_hide()
    
    # This shows the sever page if the application disconnects from the server
    sever(html = sever_page())
    
    workout_server("workout_module", global$con)
}

# Run the application 
shinyApp(ui = ui, server = server)
