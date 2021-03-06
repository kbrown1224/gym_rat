function(input, output, session) {
    
    #### Initialize Application Server =========================================
    # Initialize global variables
    global <- reactiveValues(
        workout_id = NULL,
        lifts = NULL,
        wo_start_dttm = NULL,
        wo_end_dttm = NULL
    )
    
    # Once we are ready to go, hide the waiter
    waiter_hide()
    
    # This shows the sever page if the application disconnects from the server
    sever(html = sever_page())
    
    # Turn off the system
    observeEvent(
        eventExpr = input$poweroff,
        handlerExpr = {
            confirmSweetAlert(
                session = session,
                inputId = "confirm_poweroff",
                title = "Are you sure you want to power off?",
                text = "Unfinished workouts will be lost."
            )
        }
    )
    
    observeEvent(
        eventExpr = input$confirm_poweroff,
        handlerExpr = {
            if (input$confirm_poweroff) {
                system("poweroff")
            }
        }
    )
    
    #### Select Workout UI =====================================================
    # Get a data frame of the last workout of each muscle group. Used for the 
    # selecting a workout
    last_wo_df <- reactive({get_last_workouts(db_con)})
    
    # Render the data table of last workouts
    output$last_wo_table <- renderDT({
        last_wo_df() %>% 
            mutate(
                `Last Workout` = strftime(
                    `Last Workout Dttm`,
                    format = "%m/%d"
                )
            ) %>% 
            select(`Workout Name`, `Last Workout`, `Days Ago`) %>% 
            datatable(
                style = "bootstrap",
                autoHideNavigation = TRUE,
                class = "cell-border stripe",
                selection = "single",
                options = list(
                    lengthChange = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    bSort = FALSE,
                    dom = 't',
                    columnDefs = list(
                        list(
                            className = 'dt-center', 
                            targets = 2:3
                        )
                    ),
                    headerCallback = DT::JS(
                        "function(thead) {",
                        "  $(thead).css('font-size', '40pt');",
                        "  $(thead).css('color', '#fff');",
                        "  $(thead).css('background-color', '#1c1c1c');",
                        # "  $(td).css('background-color', '#1c1c1c');",
                        "}"
                    )
                )
            ) %>% 
            formatStyle(
                columns = c(0, 1, 2, 3),
                fontWeight = "bold",
                fontSize = '30pt',
                backgroundColor = "#1c1c1c"
            )
            
    })
    
    # Observe button to begin workout
    observeEvent(
        eventExpr = input$begin_workout,
        handlerExpr = {
            selected_row <- input$last_wo_table_rows_selected
            if (!is.null(selected_row)) {
                # future({beepr::beep(8)})
                # Mark the beginning of the workout
                global$wo_start_dttm <- now()
                
                # Extract the workout group id from what was selected
                this_workout_group_id <- last_wo_df()[[selected_row, "Id"]]
                
                # Create the workout record in the database and record the id
                global$workout_id <- write_workout(
                    con = db_con,
                    start_dttm = global$wo_start_dttm,
                    workout_group_id = this_workout_group_id
                )
                
                # We need to know the primary workout number we should be on
                this_workout_number <- get_workout_number(
                    con = db_con,
                    workout_id = global$workout_id,
                    workout_group_id = this_workout_group_id
                )
                
                # We need to update the workout record with the workout we are on
                update_workout(
                    con = db_con,
                    workout_id = global$workout_id,
                    workout_number = this_workout_number
                )
                
                # We need the muscle groups to generate the workout
                muscle_groups <- get_muscle_groups(
                    con = db_con, 
                    workout_group_id = this_workout_group_id
                )
                
                # We need to separate the primary and secondary muscle groups, 
                # Because their structure is very different
                primary_muscle_groups <- 
                    muscle_groups %>% 
                    filter(!(muscle_group_id %in% c(7, 8))) %>% 
                    magrittr::extract2(1)
                
                # Get the exercises for the muscle groups in our workout
                global$exercises <- get_exercises(
                    con = db_con, 
                    primary_muscle_groups = primary_muscle_groups
                )
                
                # Now lets render the workout UI
                output$workout_tabs <- renderUI({
                    
                    # First, we pull out the primary lifts
                    primary_lifts <- 
                        global$exercises %>% 
                        filter(primary_lift)
                    
                    # Get the workout for the step in the program we are on
                    primary_program <- get_primary_program(
                        con = db_con,
                        this_workout_number = this_workout_number
                    )
                    
                    # We need the lift maxes for the primary lifts. We need this
                    # to fill in the primary program
                    lift_maxes <- get_maxes(
                        con = db_con,
                        exercise_ids = primary_lifts$exercise_id
                    )
                    
                    # Tidy up the primary lifts data frame
                    primary_lifts$sets <- list(primary_program, primary_program)
                    primary_lifts <- 
                        primary_lifts %>% 
                        inner_join(lift_maxes, by = "exercise_id")
                    
                    # sets is a nested data frame inside of primary lifts
                    for (i in 1:nrow(primary_lifts)) {
                        primary_lifts[[i, "sets"]][[1]] <-
                            primary_lifts[[i, "sets"]][[1]] %>% 
                            mutate(
                                weight = percent_of_max * primary_lifts[[i, "weight"]],
                                weight = 5 * floor(weight / 5)
                            ) %>% 
                            select(-percent_of_max)
                    }
                    
                    primary_lifts <-
                        primary_lifts %>% 
                        select(-weight) %>% 
                        unnest(sets) %>% 
                        group_by(exercise_id) %>% 
                        summarize(
                            min_weight = min(weight),
                            max_weight = max(weight)
                        ) %>% 
                        right_join(primary_lifts, by = "exercise_id")
                       
                    # Lets define the workout structure for the secondary lifts
                    # TODO - Move this to the database
                    rep_pattern <- list(
                        tibble(rep_goal = c(10, 10, 10)),
                        tibble(rep_goal = c(12, 12, 12)),
                        tibble(rep_goal = c(20, 20)),
                        tibble(rep_goal = c(10, 10, 10)),
                        tibble(rep_goal = c(12, 12, 12)),
                        tibble(rep_goal = c(20, 20))
                    )
                    
                    # Extract the secondary lifts and sample 3 lifts from each
                    # muscle group
                    secondary_lifts <-
                        global$exercises %>% 
                        filter(!primary_lift) %>% 
                        group_by(muscle_group_id) %>% 
                        slice_sample(n = 3) 
                    
                    secondary_lifts$sets <- rep_pattern
                    
                    # TODO - Get the proper weight for each exercise
                    # secondary_lifts$weight <- 30
                    
                    secondary_lifts <- 
                        secondary_lifts %>% 
                        unnest(sets) %>% 
                        group_by(exercise_id) %>% 
                        summarize(rep_goal = max(rep_goal)) %>% 
                        left_join(
                            y = get_past_exercise_summaries(
                                db_con, 
                                secondary_lifts$exercise_id
                            ),
                            by = "exercise_id"
                        ) %>% 
                        mutate(
                            rep_distance = abs(rep_goal - repetitions),
                            rep_factor = (repetitions - rep_goal) / repetitions,
                            rep_factor = rep_factor + 1,
                            max_weight = case_when(
                                rep_factor < 1 ~ max_weight * 1.05,
                                rep_factor > 1 ~ max_weight * rep_factor,
                                TRUE ~ max_weight * 1.1
                            ),
                            max_weight = 5 * ceiling(max_weight / 5),
                            min_weight = 5 * ceiling((0.75 * min_weight) / 5),
                            weight = max_weight
                        ) %>% 
                        group_by(exercise_id) %>% 
                        filter(rep_distance == min(rep_distance)) %>% 
                        select(exercise_id, max_weight, min_weight, weight) %>% 
                        right_join(secondary_lifts, by = "exercise_id") %>% 
                        mutate(
                            max_weight = replace_na(max_weight, 200),
                            min_weight = replace_na(min_weight, 5),
                            weight = replace_na(weight, 75)
                        )
                    
                    
                    # Same concept here as the primary lifts
                    for (i in 1:nrow(secondary_lifts)) {
                        secondary_lifts[[i, "sets"]][[1]] <-
                            secondary_lifts[[i, "sets"]][[1]] %>% 
                            mutate(weight = secondary_lifts[[i, "weight"]]) 
                    }
                    
                    
                    
                    # Lets put the primary and secondary lifts into a single 
                    # data frame. Then we can build the lifts ui
                    global$lifts <- bind_rows(primary_lifts, secondary_lifts)
                    lifts_ui(global$lifts)
                })
                
                # Swap the UIs and play a fun little message
                shinyjs::hide("choose_workout_ui")
                shinyjs::show("workout_ui")
                # future({system(glue('aplay -t wav {here("www", "gl.wav")}'))})
                future({set_fan_status("On")})
                
            } else {
                sendSweetAlert(
                    session = session,
                    title = "You Must Select a Workout",
                    type = "error"
                )   
            }
        }
    )
    
    observeEvent(
        eventExpr = input$cancel_workout,
        ignoreInit = TRUE,
        handlerExpr = {
            delete_workout(db_con, global$workout_id)
            
            shinyjs::hide("workout_ui")
            shinyjs::show("choose_workout_ui")
            future({set_fan_status("Off")})
            
        }
    )
    
    #### Workout UI ============================================================
    # Timer at the top of of the workout UI. Starts at the beginning of the 
    # workout
    output$workout_timer <- renderValueBox({
        invalidateLater(millis = 1000)
        workout_dur <- as.period(as.duration(Sys.time() - global$wo_start_dttm))
        h <- hour(workout_dur)
        m <- minute(workout_dur)
        s <- round(second(workout_dur))
        s <- str_pad(s, width = 2, side = "left", pad = "0")
        formatted_dur <- ifelse(h == 0, glue("{m}:{s}"), glue("{h}:{m}:{s}"))
        
        valueBox(
            value = formatted_dur,
            subtitle = "Workout Timer",
            icon = icon("clock"),
            color = "black",
            width = 12
        )
        
    })
    
    # Muscle group value box at the top of the workout UI
    output$muscle_groups <- renderValueBox({
        selected_row <- input$last_wo_table_rows_selected
        workout_name <- last_wo_df()[selected_row, "Workout Name"]
        valueBox(
            value = workout_name,
            subtitle = "Workout",
            icon = icon("dumbbell"),
            color = "red",
            width = 12
        )
    })
    
    # Observe button click on end workout
    observeEvent(
        eventExpr = input$end_workout,
        handlerExpr = {
            confirmSweetAlert(
                session = session,
                inputId = "confirm_end",
                title = "WAIT",
                text = "Are you sure you're are done pumping iron?"
            )
        }
    )
    
    # Observe end workout confirmation, store workout data, and prepare workout 
    # summary
    observeEvent(
        eventExpr = input$confirm_end,
        handlerExpr = {
            if (input$confirm_end) {
                # Async beep so it doesn't block progress
                # future({beepr::beep(8)})
                
                # This wasn't working if I simply used global$lifts, idk why, 
                # still needs some research.
                lifts <- global$lifts
                
                # Insert the lifts into the db then store the resulting ids
                lifts$lift_id = insert_lifts(
                    con = db_con,
                    workout_id = global$workout_id,
                    lifts_df = lifts
                )
                
                # Grab all of the inputs from the UI
                for (lift_i in 1:nrow(lifts)) {
                    # The lifts ui has creates all of its widgets with the 
                    # naming convention exercise_name_weight_setnumber
                    exercise_name <- lifts[[lift_i, "exercise_name"]]
                    sets <- lifts[[lift_i, "sets"]][[1]]
                    input_base <- paste0(
                        str_to_lower(str_replace_all(exercise_name, " ", "_")), 
                        "_"
                    )
                    
                    # Create vectors of all ui inputs for the given workout
                    reps_input_ids <- paste0(input_base, 1:nrow(sets))
                    weight_input_ids <- paste0(input_base, "weight_", 1:nrow(sets))
                    
                    # Get all of the rep information in the ui
                    reps <- c()
                    for (input_id in reps_input_ids) {
                        reps <- c(reps, input[[input_id]])
                    }
                    
                    # Get all of the weight information in the ui
                    weight <- c()
                    for (input_id in weight_input_ids) {
                        weight <- c(weight, input[[input_id]])
                    }
                    
                    # Put this data into lifts as a nested data frame
                    sets$rep_actual <- reps
                    sets$weight_actual <- weight
                    sets$set_number <- 1:nrow(sets)
                    lifts[[lift_i, "sets"]][[1]] <- sets
                }
                
                # Insert the exercises into the database
                insert_exercises(
                    con = db_con,
                    lifts_df = lifts
                )
                
                # Mark the end of the workout
                global$wo_end_dttm <- now()
                update_workout_end(con = db_con, workout_id = global$workout_id)
                
                # Now lets render the end workout outputs
                output$workout_duration <- renderValueBox({
                    # Calculate and format the workout duration string
                    workout_duration = round(global$wo_end_dttm - global$wo_start_dttm)
                    workout_dur <- as.period(as.duration(workout_duration))
                    h <- hour(workout_dur)
                    m <- minute(workout_dur)
                    s <- round(second(workout_dur))
                    
                    if (h == 0) {
                        duration_string <- glue("{m} and {s} seconds")
                    }
                    formatted_dur <- ifelse(
                        h == 0, 
                        glue("{m} minutes and {s} seconds"), 
                        glue("{h} hour, {m} minutes, and {s} seconds")
                    )
                    
                    valueBox(
                        value = formatted_dur,
                        subtitle = "Workout Duration",
                        icon = icon("clock"),
                        color = "black",
                        width = 12
                    )
                })
                
                # Calculate and format the total weight moved
                output$total_weight <- renderValueBox({
                    total_weight <- 
                        lifts %>% 
                        select(sets) %>% 
                        unnest(sets) %>% 
                        filter(rep_actual > 0) %>% 
                        summarize(total_weight = sum(weight_actual * rep_actual)) %>% 
                        magrittr::extract2(1)
                    
                    valueBox(
                        value = total_weight,
                        subtitle = "Total Weight Moved",
                        icon = icon("dumbell"),
                        color = "black",
                        width = 12
                    )
                })
                
                # Create a summary table from the lifts data frame
                output$set_summary <- renderDT({
                    lifts %>% 
                        select(-weight) %>% 
                        unnest(sets) %>% 
                        mutate(
                            hit_weight_goal = weight_actual >= weight, 
                            hit_rep_goal = rep_actual >= rep_goal
                        ) %>% 
                        group_by(
                            exercise_name, primary_lift, muscle_group_id
                        ) %>% 
                        summarize(
                            hit_rep_goal = paste0(
                                round(100 * sum(hit_rep_goal) / n()), "%"
                            ),
                            hit_weight_goal = paste0(
                                round(100 * sum(hit_weight_goal) / n()), "%"
                            ),
                            total_weight_moved = sum(weight_actual * rep_actual)
                        ) %>% 
                        arrange(muscle_group_id, desc(primary_lift)) %>% 
                        ungroup() %>% 
                        select(
                            exercise_name, 
                            hit_rep_goal, 
                            hit_weight_goal, 
                            total_weight_moved
                        ) %>% 
                        rename_all(
                            ~str_to_title(str_replace_all(., "_", " "))
                        ) %>% 
                        datatable(
                            escape = FALSE,
                            style = "bootstrap",
                            autoHideNavigation = TRUE,
                            class = "cell-border stripe",
                            selection = "single",
                            options = list(
                                lengthChange = FALSE,
                                paging = FALSE,
                                searching = FALSE,
                                bSort = FALSE,
                                dom = 't',
                                fnDrawCallback = DT::JS(
                                    'function(){
                              HTMLWidgets.staticRender();
                            }'
                                )
                            )
                        ) 
                })
                
                # Now lets 
                shinyjs::hide("workout_ui")
                shinyjs::show("workout_summary_ui")
                future({set_fan_status("Off")})
            }
        }
    )
    
    # This observes the lift info button. Each tab in the ui has this button.
    # All buttons trigger the same input, then we can refer to the value of that
    # input to see which one was clicked. 
    observeEvent(
        eventExpr = input$lift_info,
        handlerExpr = {
            # Get most reps and weight I've ever done for this weight
            exercise_id <- as.integer(str_split(input$lift_info, "_")[[1]][2])
            lift_mosts <- get_lift_mosts(db_con, exercise_id)
            
            # Once we have the required information, render the history plot. 
            # This shows the set|rep|weight data for the last 3 times this lifotiof njmrtm ntnm       t
            output$lift_history <- renderPlot(
                bg = 'transparent',
                expr = {
                    get_lift_history(db_con, exercise_id) %>% 
                        arrange(date) %>% 
                        mutate(label = glue("{repetitions} x {weight}")) %>%
                        ggplot(aes(x = set_number, y = weight)) +
                        geom_line(
                            color = get_color("primary", "hex"), 
                            size = 2, 
                            alpha = 0.5
                        ) +
                        geom_label_repel(
                            aes(label = label),
                            nudge_y = -1,
                            color = get_color("success", "hex")
                        ) +
                        facet_wrap(date ~ ., ncol = 1) +
                        labs(x = "Set", y = "Weight (lbs)") +
                        plot_theme() +
                        theme(
                            strip.text = element_text(colour = 'white'),
                            strip.background = element_rect(fill = "transparent")
                        )
                }
            )
            
            # Filter the lift mosts data to most weight, render the value box
            output$most_weight <- renderValueBox({
                details <- lift_mosts %>% filter(category == 'most_weight')
                subtitle <- strftime(details$workout_date, "%m/%d/%Y")
                value <- glue(
                    "{details$repetitions} reps at {details$weight} lbs"
                )

                valueBox(
                    value = value,
                    subtitle = subtitle,
                    icon = icon("hashtag"),
                    color = "red",
                    width = 12
                )
            })
            
            # Filter the lift most data to most reps, render the value box
            output$most_reps <- renderValueBox({
                details <- lift_mosts %>% filter(category == 'most_reps')
                subtitle <- strftime(details$workout_date, "%m/%d/%Y")
                value <- glue(
                    "{details$repetitions} reps at {details$weight} lbs"
                )

                valueBox(
                    value = value,
                    subtitle = subtitle,
                    icon = icon("hashtag"),
                    color = "red",
                    width = 12
                )
            })
            
            # Pop the modal
            toggleModal(
                session = session,
                modalId = "lift_info_modal",
                toggle = "open"
            )
        }
    )
    
    #### Workout Summary UI ====================================================
    observeEvent(
        eventExpr = input$return_home,
        handlerExpr = {
            shinyjs::hide("workout_summary_ui")
            shinyjs::show("choose_workout_ui")
        }
    )
}
