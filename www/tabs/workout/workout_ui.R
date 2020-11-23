workout_ui <- function() {
    last_wo_table <- DTOutput("last_wo_table")
    begin_wo_bttn <- actionBttn(
        inputId = "begin_workout",
        label = "\nBegin Workout\n",
        style = "pill",
        color = "success",
        block = TRUE,
        size = 'lg'
    )
    
    choose_workout_ui <- fluidPage(
        fluidRow(column(width = 12, last_wo_table)),
        fluidRow(column(width = 12, begin_wo_bttn))
    )
    
    end_wo_bttn <- actionBttn(
        inputId = "end_workout",
        label = "\nEnd Workout\n",
        style = "pill",
        color = "success",
        block = TRUE,
        size = 'lg'
    )
    
    timer_vb <- valueBoxOutput('workout_timer', width = 12)
    muscle_groups_vb <- valueBoxOutput('muscle_groups', width = 12)
    
    workout_ui <- fluidPage(
        fluidRow(
            column(width = 2, timer_vb),
            column(width = 10, muscle_groups_vb)
        ),
        fluidRow(column(width = 12, uiOutput("workout_tabs"))),
        fluidRow(column(width = 12, end_wo_bttn))
    )
    
    workout_duration_vb <- valueBoxOutput('workout_duration', width = 12)
    total_weight_vb <- valueBoxOutput('total_weight', width = 12)
    set_summary_dt <- DTOutput('set_summary')
    
    set_summary_box <- box(
        title = "Workout Summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = NULL,
        
        set_summary_dt
    )
    
    return_home_button <- actionBttn(
        inputId = "return_home",
        label = "\nReturn Home\n",
        style = "pill",
        color = "success",
        block = TRUE,
        size = "lg"
    )
    
    workout_summary_ui <- fluidPage(
        fluidRow(
            column(width = 6, workout_duration_vb), 
            column(width = 6, total_weight_vb)
        ),
        fluidRow(column(width = 12, set_summary_box)),
        fluidRow(column(width = 12, return_home_button))
    )
    
    
    
    return(
        fluidPage(
            shinyjs::useShinyjs(),
            tags$div(id = "choose_workout_ui", choose_workout_ui),
            hidden(tags$div(id = "workout_ui", workout_ui)),
            hidden(tags$div(id = "workout_summary_ui", workout_summary_ui))
        )
    )
}

#### Individual UI's ===========================================================
lifts_ui <- function(lifts) {
    tabs <- list()
    for (lift_i in 1:nrow(lifts)) {
        
        sets <- lifts[[lift_i, "sets"]][[1]]
        exercise_name <- lifts[[lift_i, "exercise_name"]]
        input_id_base <- str_to_lower(str_replace_all(exercise_name, " ", "_"))
        set_components <- list()
        
        for (set_i in 1:nrow(sets)) {
            reps <- sets[[set_i, "rep_goal"]]
            weight <- sets[[set_i, "weight"]]
            
            set_components[[set_i]] <- fluidRow(
                column(
                    width = 4,
                    tags$h1(
                        glue('Goal: {reps} reps at {weight} lbs')
                    )
                ),
                column(
                    width = 4,
                    sliderInput(
                        inputId = paste0(input_id_base, "_weight_", set_i),
                        label = "Weight",
                        min = 0,
                        max = weight + 50,
                        value = weight,
                        step =5,
                        animate = FALSE,
                        ticks = FALSE
                    )
                    # knobInput(
                    #     inputId = paste0(input_id_base, "_weight_", set_i),
                    #     label = "Weight",
                    #     value = weight,
                    #     min = 5,
                    #     max = weight + 50,
                    #     step = 5,
                    #     displayInput = TRUE,
                    #     inputColor = get_color("primary", "hex"),
                    #     width = "100px",
                    #     height = "100px",
                    #     thickness = 0.25,
                    #     lineCap = "round",
                    #     fgColor = get_color("success", "hex"),
                    #     bgColor = "#161616",
                    #     fontSize = "30px"
                    # )
                    # numericInput(
                    #     inputId = paste0(input_id_base, "_weight_", set_i),
                    #     label = "Weight",
                    #     value = weight,
                    #     min = 5,
                    #     max = 200,
                    #     step = 5
                    # )
                ),
                column(
                    width = 4,
                    sliderInput(
                        inputId = paste0(input_id_base, "_", set_i),
                        label = "Reps Achieved",
                        min = 0,
                        max = reps,
                        value = 0,
                        step = 1,
                        animate = FALSE,
                        ticks = FALSE
                    )
                )
            )
        }
        
        tabs[[lift_i]] <- tabPanel(
            title = exercise_name,
            fluidPage(
                br(),
                fluidRow(
                    column(
                        width = 12,
                        box(
                            title = "This lift historically",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width = NULL,
                            
                            tags$h3("Coming Soon")
                            # plotOutput(outputId = outputId)
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        box(
                            title = "Sets",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = FALSE,
                            width = NULL,
                            do.call(fluidPage, set_components)
                        )
                    )
                )
            )
        ) 
    }
    
    return(do.call(tabsetPanel, tabs))
}
