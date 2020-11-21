function(input, output, session) {
    
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
        workout_id = NULL,
        lifts = NULL,
        wo_start_dttm = NULL,
        wo_end_dttm = NULL
    )
    
    # Once we are ready to go, hide the waiter
    waiter_hide()
    
    # This shows the sever page if the application disconnects from the server
    sever(html = sever_page())
    
    
    last_wo_df <- reactive({
        tbl(db_con, "last_workout") %>% 
            collect() %>% 
            rename_all(~str_to_title(str_replace_all(., "_", " ")))
    })
    
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
                        "}"
                    )
                )
            ) %>% 
            formatStyle(
                columns = c(1, 2, 3),
                fontWeight = "bold",
                fontSize = '30pt'
            )
    })
    
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
    
    observeEvent(
        eventExpr = input$begin_workout,
        handlerExpr = {
            selected_row <- input$last_wo_table_rows_selected
            if (!is.null(selected_row)) {
                # beepr::beep(8)
                global$wo_start_dttm <- Sys.time()
                
                this_workout_group_id <- last_wo_df()[[selected_row, "Id"]]
                
                insert_statement <- glue_sql(
                    insert_wo_template,
                    start_dttm = now(),
                    workout_group_id = this_workout_group_id,
                    program_id = 1,
                    .con = db_con
                )
                insert_result <-  DBI::dbSendStatement(
                    db_con, 
                    insert_statement
                )
                global$workout_id <- DBI::dbFetch(res = insert_result)[[1]]
                DBI::dbClearResult(res = insert_result)
                
                wo_number <-
                    tbl(db_con, "workout") %>% 
                    filter(
                        workout_group_id == !!this_workout_group_id, 
                        id != !!global$workout_id
                    ) %>% 
                    arrange(desc(start_dttm)) %>%
                    head(1) %>% 
                    select(workout_number) %>%
                    collect() %>%
                    magrittr::extract2(1)
                
                if (isTruthy(wo_number < 16)) {
                    this_workout_number <- wo_number + 1
                } else {
                    this_workout_number <- 1
                }
                
                update_workout_statement <- glue_sql(
                    update_workout_number,
                    workout_number = this_workout_number,
                    workout_id = global$workout_id,
                    .con = db_con
                )
                update_result <- DBI::dbSendStatement(
                    db_con, update_workout_statement
                )
                DBI::dbClearResult(update_result)
                
                muscle_groups <-
                    tbl(db_con, "workout_group") %>% 
                    filter(id == !!this_workout_group_id) %>% 
                    select(muscle_group_id) %>% 
                    collect()
                
                primary_muscle_groups <- 
                    muscle_groups %>% 
                    filter(!(muscle_group_id %in% c(7, 8))) %>% 
                    magrittr::extract2(1)
                
                global$exercises <- 
                    tbl(db_con, "exercise") %>% 
                    filter(muscle_group_id %in% !! primary_muscle_groups) %>% 
                    select(
                        exercise_id = id,
                        exercise_name = name,
                        muscle_group_id,
                        primary_lift
                    ) %>% 
                    collect()
                
                output$workout_tabs <- renderUI({
                    
                    primary_lifts <- 
                        global$exercises %>% 
                        filter(primary_lift)
                    
                    primary_program <-
                        tbl(db_con, "program_steps") %>%
                        filter(
                            program_id == 1, 
                            workout_number == this_workout_number
                        ) %>%
                        arrange(step_number) %>%
                        select(rep_goal, percent_of_max) %>%
                        collect()
                    
                    secondary_lift_sets <- tibble(
                        sets = c(3, 3, 2),
                        reps = c(10, 12, 20)
                    )
                    
                    rep_pattern <- list(
                        tibble(rep_goal = c(10, 10, 10)),
                        tibble(rep_goal = c(12, 12, 12)),
                        tibble(rep_goal = c(20, 20)),
                        tibble(rep_goal = c(10, 10, 10)),
                        tibble(rep_goal = c(12, 12, 12)),
                        tibble(rep_goal = c(20, 20))
                    )
                    
                    secondary_lifts <-
                        global$exercises %>% 
                        filter(!primary_lift) %>% 
                        group_by(muscle_group_id) %>% 
                        slice_sample(n = 3) 
                    
                    secondary_lifts$sets <- rep_pattern
                    secondary_lifts$weight <- 30

                    lift_maxes <-
                        tbl(db_con, "personal_records") %>%
                        filter(
                            exercise_id %in% !!primary_lifts$exercise_id
                        ) %>%
                        select(exercise_id, weight) %>%
                        collect()
                    
                    primary_lifts$sets <- list(primary_program, primary_program)
                    primary_lifts <- 
                        primary_lifts %>% 
                        inner_join(lift_maxes, by = "exercise_id")
                    
                    for (i in 1:nrow(primary_lifts)) {
                        primary_lifts[[i, "sets"]][[1]] <-
                            primary_lifts[[i, "sets"]][[1]] %>% 
                            mutate(
                                weight = percent_of_max * primary_lifts[[i, "weight"]],
                                weight = 5 * floor(weight / 5)
                            ) %>% 
                            select(-percent_of_max)
                    }

                    for (i in 1:nrow(secondary_lifts)) {
                        secondary_lifts[[i, "sets"]][[1]] <-
                            secondary_lifts[[i, "sets"]][[1]] %>% 
                            mutate(weight = secondary_lifts[[i, "weight"]]) 
                    }
                    
                    global$lifts <- bind_rows(primary_lifts, secondary_lifts)
                    lifts_ui(global$lifts)
                })
                
                shinyjs::hide("choose_workout_ui")
                shinyjs::show("workout_ui")
                # system("aplay -t wav ~/HDD1/gym_rat/www/gl.wav")
                
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
        eventExpr = input$end_workout,
        handlerExpr = {
            # beepr::beep(8)
            # system("aplay -t wav ~/HDD1/gym_rat/www/fy.wav")
            lifts <- global$lifts
            
            insert <- 
                lifts %>% 
                mutate(
                    inserts = glue_sql(
                        "({workout_id}, {exercise_id})",
                        workout_id = global$workout_id,
                        exercise_id = exercise_id
                    )
                ) %>% 
                summarize(insert = paste0(inserts, collapse = ", ")) %>% 
                magrittr::extract2(1)
            
            insert_statement <- glue_sql(
                lift_insert_template, 
                insert = SQL(insert), 
                .con = db_con
            )
            
            insert_result <-  DBI::dbSendStatement(
                db_con, 
                insert_statement
            )
            lift_ids <- DBI::dbFetch(res = insert_result)[[1]]
            DBI::dbClearResult(res = insert_result)
            lifts$lift_id = lift_ids
            
            for (lift_i in 1:nrow(lifts)) {
                exercise_name <- lifts[[lift_i, "exercise_name"]]
                sets <- lifts[[lift_i, "sets"]][[1]]
                
                input_base <- paste0(
                    str_to_lower(str_replace_all(exercise_name, " ", "_")), 
                    "_"
                )
                
                reps_input_ids <- paste0(input_base, 1:nrow(sets))
                weight_input_ids <- paste0(input_base, "weight_", 1:nrow(sets))
                
                reps <- c()
                for (input_id in reps_input_ids) {
                    reps <- c(reps, input[[input_id]])
                }
                
                weight <- c()
                for (input_id in weight_input_ids) {
                    weight <- c(weight, input[[input_id]])
                }
                
                sets$rep_actual <- reps
                sets$weight_actual <- weight
                sets$set_number <- 1:nrow(sets)
                lifts[[lift_i, "sets"]][[1]] <- sets
            }
            
            inserts <- 
                lifts %>% 
                select(lift_id, sets) %>% 
                unnest(cols = c(sets)) %>% 
                filter(rep_actual > 0) %>% 
                mutate(
                    inserts = glue_sql(
                        "({lift_id}, {set_number}, {repititions}, {weight})",
                        lift_id = lift_id,
                        set_number = set_number,
                        repititions = rep_actual,
                        weight = weight_actual
                    )
                ) %>% 
                summarize(insert = paste0(inserts, collapse = ", ")) %>% 
                magrittr::extract2(1)
            
            insert_statement <- glue_sql(
                set_insert_template, 
                insert = SQL(inserts), 
                .con = db_con
            )
            insert_result <-  DBI::dbSendStatement(
                db_con, 
                insert_statement
            )
            DBI::dbClearResult(res = insert_result)
        
            global$wo_end_dttm <- Sys.time()
            
            output$workout_duration <- renderValueBox({
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
            
            output$set_summary <- renderDT({
                lifts %>% 
                    select(-weight) %>% 
                    unnest(sets) %>% 
                    mutate(
                        hit_weight_goal = weight_actual >= weight, 
                        hit_rep_goal = rep_actual >= rep_goal
                    ) %>% 
                    group_by(exercise_name, primary_lift, muscle_group_id) %>% 
                    summarize(
                        hit_rep_goal = paste0(round(100 * sum(hit_rep_goal) / n()), "%"),
                        hit_weight_goal = paste0(round(100 * sum(hit_weight_goal) / n()), "%"),
                        total_weight_moved = sum(weight_actual * rep_actual)
                    ) %>% 
                    arrange(muscle_group_id, desc(primary_lift)) %>% 
                    ungroup() %>% 
                    select(exercise_name, hit_rep_goal, hit_weight_goal, total_weight_moved) %>% 
                    rename_all(~str_to_title(str_replace_all(., "_", " "))) %>% 
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
                            dom = 't'
                            # headerCallback = DT::JS(
                            #     "function(thead) {",
                            #     "  $(thead).css('font-size', '40pt');",
                            #     "  $(thead).css('color', '#fff');",
                            #     "}"
                            # )
                        )
                    )
            })
            
            shinyjs::hide("workout_ui")
            shinyjs::show("workout_summary_ui")
        }
    )
    
    
    
    observeEvent(
        eventExpr = input$return_home,
        handlerExpr = {
            shinyjs::hide("workout_summary_ui")
            shinyjs::show("choose_workout_ui")
        }
    )
}