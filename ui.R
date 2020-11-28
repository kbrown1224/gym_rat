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
    fixed = FALSE
)

#### Define Left Sidebar =======================================================
# Define Menu Items Here
workout_menu <- menuItem(
    text = "Workout",
    tabName = "workout_page",
    icon = icon("home")
)

# Put Menu in the Sidebar
db_left_sidebar <- dashboardSidebar(
    sidebarMenu(id = "sb_menu", workout_menu),
    collapsed = TRUE
)

# Put power off button in the right sidebar
db_right_sidebar <- rightSidebar(
    actionBttn(
        inputId = "poweroff",
        label = "Power Off",
        icon = icon("power-off"),
        style = "pill",
        color = "success",
        size = "md",
        block = TRUE
    )
)

#### Define Body ===============================================================
# Define Tab Items
workout_tab <- tabItem(
    tabName = "workout_page", 
    
    workout_ui()
)

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
dashboardPagePlus(
    dashboard_theme,
    
    header = db_header,
    sidebar = db_left_sidebar,
    rightsidebar = db_right_sidebar,
    body = db_body
)
