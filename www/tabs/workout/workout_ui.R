#### Workout UI ================================================================
workout_ui <- function() {
    # Inputs and Outputs =======================================================
    # Data table to hold information on the last workout for each muscle group
    last_wo_table <- DTOutput("last_wo_table")
    
    # Button to trigger the beginning of a workout
    begin_wo_bttn <- actionBttn(
        inputId = "begin_workout",
        label = "Begin Workout",
        style = "pill",
        color = "success",
        block = TRUE,
        size = 'lg'
    )
    
    # Button to trigger the end of a workout
    end_wo_bttn <- actionBttn(
        inputId = "end_workout",
        label = "End Workout",
        style = "pill",
        color = "success",
        block = TRUE,
        size = 'lg'
    )
    
    # Button to trigger the cancelation of an active workout session
    cancel_wo_bttn <- actionBttn(
        inputId = "cancel_workout",
        label = "Cancel Workout",
        style = "pill",
        color = "warning",
        block = TRUE,
        size = "lg"
    )
    
    # Button to trigger returning to the home page from the workout summary
    return_home_button <- actionBttn(
        inputId = "return_home",
        label = "Return Home",
        style = "pill",
        color = "success",
        block = TRUE,
        size = "lg"
    )
    
    # Workout session value boxes
    timer_vb <- valueBoxOutput('workout_timer', width = 12)
    muscle_groups_vb <- valueBoxOutput('muscle_groups', width = 12)
    
    # Workout Summary details
    workout_duration_vb <- valueBoxOutput('workout_duration', width = 12)
    total_weight_vb <- valueBoxOutput('total_weight', width = 12)
    set_summary_dt <- DTOutput('set_summary')
    
    # Lift info modal value boxes
    most_weight_vb <- valueBoxOutput("most_weight", width = 12)
    most_reps_vb <- valueBoxOutput("most_reps", width = 12)
    
    # Plot for the history of a selected lift
    lift_history_plot <- plotOutput("lift_history")
    
    #### Boxes and Modals ======================================================
    # Modal to show historic data on a selected lift
    lift_info_modal <- bsModal(
        id = "lift_info_modal",
        title = "Lift History",
        trigger = "lift_info",
        size = "large",
        
        fluidRow(
            column(
                width = 6, 
                fluidRow(h2("Highest Weight"), align = "center"),
                fluidRow(most_weight_vb)
            ),
            column(
                width = 6, 
                fluidRow(h2("Highest Reps"), align = "center"),
                fluidRow(most_reps_vb)
            )
        ),
        fluidRow(lift_history_plot)
    )
    
    # Summary of the workout
    set_summary_box <- box(
        title = "Workout Summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = NULL,
        
        set_summary_dt
    )
    
    #### Sub UI's ==============================================================
    # Functionality to track a workout
    workout_ui <- fluidPage(
        fluidRow(
            column(width = 2, timer_vb),
            column(width = 10, muscle_groups_vb)
        ),
        fluidRow(column(width = 12, uiOutput("workout_tabs"))),
        br(),
        fluidRow(
            column(width = 6, end_wo_bttn),
            column(width = 6, cancel_wo_bttn)
        ),
        fluidRow(lift_info_modal)
    )
    
    # Landing page to select a workout
    choose_workout_ui <- fluidPage(
        fluidRow(column(width = 12, last_wo_table)),
        fluidRow(column(width = 12, begin_wo_bttn))
    )
    
    # Post workout summary page
    workout_summary_ui <- fluidPage(
        fluidRow(
            column(width = 6, workout_duration_vb), 
            column(width = 6, total_weight_vb)
        ),
        fluidRow(column(width = 12, set_summary_box)),
        fluidRow(column(width = 12, return_home_button))
    )
    
    #### Put it all together ===================================================
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
# Function for creating the lifts sub UI
# lifts - a dataframe indicating what lifts/sets/reps/weights for the UI to be 
# built around
lifts_ui <- function(lifts) {
    
    # Start by looping through each record of the data frame. Each record 
    # indicates a lift, with details about the sets, reps, and weight. Store
    # these tabs in a list, we will then call the tabsetPanel on this list of
    # tabPanels
    tabs <- list()
    for (lift_i in 1:nrow(lifts)) {
        
        # Extract relevant information for the lift in this iteration. Not 
        # necessary, I just think the code looks a bit cleaner
        sets <- lifts[[lift_i, "sets"]][[1]]
        exercise_name <- lifts[[lift_i, "exercise_name"]]
        exercise_id <- lifts[[lift_i, "exercise_id"]]
        min_weight = lifts[[lift_i, "min_weight"]] - 10
        max_weight = lifts[[lift_i, "max_weight"]] + 20
        input_id_base <- str_to_lower(str_replace_all(exercise_name, " ", "_"))
        
        # Each lift has a pre-defined number of sets. We will loop through the
        # sets object, which was a data frame nested in the input lifts data
        # frame. For each set, we will create a pair of input widgets to track
        # the reps and weight acheived.
        set_components <- list()
        for (set_i in 1:nrow(sets)) {
            
            # Extract relevant information from the set in this iteration. 
            # Again, not necessary, just like the cleanliness
            reps <- sets[[set_i, "rep_goal"]]
            weight <- sets[[set_i, "weight"]]
            
            # Define input widgets base on the reps and weight defined
            set_by_rep <- tags$h1(glue('Goal: {reps} reps at {weight} lbs'))
            weight_selection <- sliderInput(
                inputId = paste0(input_id_base, "_weight_", set_i),
                label = "Weight",
                min = min_weight,
                max = max_weight,
                value = weight,
                step = 5,
                animate = FALSE,
                ticks = FALSE
            )
            rep_selection <- sliderInput(
                inputId = paste0(input_id_base, "_", set_i),
                label = "Reps Achieved",
                min = 0,
                max = reps,
                value = 0,
                step = 1,
                animate = FALSE,
                ticks = FALSE
            )
            
            # Put this together to create a row for an individual set
            set_components[[set_i]] <- fluidRow(
                column(width = 4, set_by_rep),
                column(width = 4, weight_selection),
                column(width = 4, rep_selection)
            )
        }
        
        # Once complete, create a box for the lift of this iteration. The title 
        # is actually a button that triggers the lift information modal.
        lifts_box <- boxPlus(
            title = tagList(
                actionButton(
                    inputId = glue(
                        "info_{exercise_id}_{reps}_{weight}"
                    ),
                    label = "Sets",
                    icon = icon("chart-bar"),
                    class = "bttn bttn-pill bttn-sm bttn-success bttn-block bttn-no-outline shiny-bound-input",
                    width = "100%",
                    onclick = 'Shiny.onInputChange(\"lift_info\", this.id, {priority: \"event\"})'
                )
            ),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            closable = FALSE,
            width = NULL,

            do.call(fluidPage, set_components)
        )
        
        # Take the box defined above and put it inside of a tabPanel
        tabs[[lift_i]] <- tabPanel(
            title = exercise_name,
            fluidPage(br(), fluidRow(column(width = 12, lifts_box)))
        ) 
    }
    
    # Once we create tabPanel objects for each lift, put the tabPanels into the 
    # parent tabsetPanel object and return.
    return(do.call(tabsetPanel, tabs))
}
