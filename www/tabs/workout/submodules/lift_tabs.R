primary_lift_tab <- function(id, exercise_name, lift_max, primary_program) {
    ns <- NS(id)
    
    primary_sets <- 
        primary_program %>% 
        mutate(
            weight = as.integer(percent_of_max * lift_max),
            weight = 5 * floor(weight / 5)
        )
    
    formatted_sets <- tibble()
    for (set_i in 1:nrow(primary_sets)) {
        n_sets <- primary_sets[[set_i, "set_goal"]]
        
        formatted_sets <- bind_rows(
            formatted_sets,
            tibble(
                reps = rep(primary_sets[[set_i, "rep_goal"]], n_sets),
                weight = rep(primary_sets[[set_i, "weight"]], n_sets)
            )
        )
    }
    
    these_lifts <- list()
    for (i in 1:nrow(formatted_sets)) {
        reps <- formatted_sets[[i, "reps"]]
        lift_weight <- formatted_sets[[i, "weight"]]
        
        these_lifts[[i]] <- fluidRow(
            column(
                width = 6,
                tags$h2(
                    glue('{reps} at {lift_weight}')
                )
            ),
            column(
                width = 6,
                sliderInput(
                    inputId = ns(paste0("main_1_", i)),
                    label = "Reps Achieved",
                    min = 0,
                    max = reps,
                    value = 0,
                    step = 1,
                    animate = TRUE
                )
            )
        )
    }
    
    return(
        tabPanel(
            title = exercise_name,
            fluidPage(
                br(),
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "This lift historically",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = NULL,
                            plotOutput("main_lift")
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            title = "Sets",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = NULL,
                            do.call(fluidPage, these_lifts)
                        )
                    )
                )
            )
        )      
    )
}

secondary_lift_tab <- function(id, sets, reps, exercise_name) {
    ns = NS(id)
    
    formatted_sets <- tibble(
        reps = rep(reps, sets),
        weight = rep(20, sets)
    )
    
    
    these_lifts <- list()
    for (i in 1:nrow(formatted_sets)) {
        reps <- formatted_sets[[i, "reps"]]
        lift_weight <- formatted_sets[[i, "weight"]]
        
        these_lifts[[i]] <- fluidRow(
            column(
                width = 6,
                tags$h2(
                    glue('{reps} at {lift_weight}')
                )
            ),
            column(
                width = 6,
                sliderInput(
                    inputId = paste0("secondary_1_", i),
                    label = "Reps Achieved",
                    min = 0,
                    max = reps,
                    value = 0,
                    step = 1,
                    animate = TRUE
                )
            )
        )
    }
    
    return(
        tabPanel(
            title = exercise_name,
            fluidPage(
                br(),
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "This lift historically",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = NULL,
                            plotOutput(ns("secondary_lift"))
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            title = "Sets",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = NULL,
                            do.call(fluidPage, these_lifts)
                        )
                    )
                )
            )
        )
    ) 
}