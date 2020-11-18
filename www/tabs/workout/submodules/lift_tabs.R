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