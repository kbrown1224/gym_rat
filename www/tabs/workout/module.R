insert_wo_template <- readr::read_file(
    here("www", "tabs", "workout", "insert_workout.sql")
)

update_workout_number <- readr::read_file(
    here("www", "tabs", "workout", "update_workout_number.sql")
)

workout_ui <- function(id) {
    ns <- NS(id)
    
    last_wo_table <- DTOutput(ns("last_wo_table"))
    begin_wo_bttn <- actionBttn(
        inputId = ns("begin_workout"),
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
        inputId = ns("end_workout"),
        label = "\nEnd Workout\n",
        style = "pill",
        color = "success",
        block = TRUE,
        size = 'lg'
    )
    
    workout_ui <- fluidPage(
        fluidRow(column(width = 12, uiOutput(ns("workout_tabs")))),
        fluidRow(column(width = 12, end_wo_bttn))
    )
    
    workout_summary <- box(
        title = "Workout Summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = NULL,
        
        tags$h1("Workout Summary Here")
    )
    
    return_home_button <- actionBttn(
        inputId = ns("return_home"),
        label = "\nReturn Home\n",
        style = "pill",
        color = "success",
        block = TRUE,
        size = "lg"
    )
    
    workout_summary_ui <- fluidPage(
        fluidRow(column(width = 12, workout_summary)),
        fluidRow(column(width = 12, return_home_button))
    )
    
    
    
    return(
        fluidPage(
            shinyjs::useShinyjs(),
            tags$div(id = ns("choose_workout_ui"), choose_workout_ui),
            hidden(tags$div(id = ns("workout_ui"), workout_ui)),
            hidden(tags$div(id = ns("workout_summary_ui"), workout_summary_ui))
        )
    )
}

workout_server <- function(id, con) {
    workout_server_function <- function(input, output, session) {
        global <- reactiveValues(
            workout_id = NULL,
            exercises = NULL,
            primary_program = NULL,
            primary_lifts = NULL,
            secondary_lifts = NULL
        )
        
        last_wo_df <- reactive({
            tbl(con, "last_workout") %>% 
                collect() %>% 
                rename_all(~str_to_title(str_replace_all(., "_", " ")))
        })
        
        output$last_wo_table <- renderDT({
             last_wo_df() %>% 
                select(-Id) %>% 
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
        
        observeEvent(
            eventExpr = input$begin_workout,
            handlerExpr = {
                selected_row <- input$last_wo_table_rows_selected
                if (!is.null(selected_row)) {
                    # beepr::beep(8)
  
                    this_workout_group_id <- last_wo_df()[[selected_row, "Id"]]
                    
                    insert_statement <- glue_sql(
                        insert_wo_template,
                        start_dttm = now(),
                        workout_group_id = this_workout_group_id,
                        program_id = 1,
                        .con = con
                    )
                    insert_result <-  DBI::dbSendStatement(
                        con, 
                        insert_statement
                    )
                    global$workout_id <- DBI::dbFetch(res = insert_result)[[1]]
                    DBI::dbClearResult(res = insert_result)
                    
                    wo_number <-
                        tbl(con, "workout") %>% 
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
                        .con = con
                    )
                    update_result <- DBI::dbSendStatement(
                        con, update_workout_statement
                    )
                    DBI::dbClearResult(update_result)
                    
                    muscle_groups <-
                        tbl(con, "workout_group") %>% 
                        filter(id == !!this_workout_group_id) %>% 
                        select(muscle_group_id) %>% 
                        collect()

                    primary_muscle_groups <- 
                        muscle_groups %>% 
                        filter(!(muscle_group_id %in% c(7, 8))) %>% 
                        magrittr::extract2(1)
                    
                    global$exercises <- 
                        tbl(con, "exercise") %>% 
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
                            tbl(con, "program_steps") %>%
                            filter(
                                program_id == 1, 
                                workout_number == this_workout_number
                            ) %>%
                            arrange(step_number) %>%
                            select(set_goal, rep_goal, percent_of_max) %>%
                            collect()
                        global$primary_program <- primary_program
                        global$primary_lifts <- primary_lifts
                        
                        tabs <- list()
                        
                        for (primary_lift_i in 1:nrow(primary_lifts)) {
                            primary_lift <- primary_lifts[primary_lift_i,]

                            lift_max <-
                                tbl(con, "personal_records") %>%
                                filter(
                                    exercise_id == !!primary_lift$exercise_id
                                ) %>%
                                select(weight) %>%
                                collect() %>%
                                magrittr::extract2(1)
                            
                            tabs[[primary_lift_i]] <- primary_lift_tab(
                              id = id,
                              exercise_name = primary_lift$exercise_name,
                              lift_max = lift_max,
                              primary_program = primary_program
                            )
                        }
                        
                        
                        
                        secondary_lift_sets <- tibble(
                            sets = c(3, 3, 2),
                            reps = c(10, 12, 20)
                        )
                        rep_pattern <- bind_rows(secondary_lift_sets, secondary_lift_sets)
                        
                        
                        secondary_lifts <-
                          global$exercises %>% 
                          filter(!primary_lift) %>% 
                          group_by(muscle_group_id) %>% 
                          slice_sample(n = 3) %>% 
                          bind_cols(rep_pattern)
                        
                        global$secondary_lifts <- secondary_lifts
                        
                        for (secondary_lift_i in 1:nrow(secondary_lifts)) {
                          tabs[[secondary_lift_i + 2]] <- secondary_lift_tab(
                            id = id,
                            sets = secondary_lifts[[secondary_lift_i, "sets"]],
                            reps = secondary_lifts[[secondary_lift_i, "reps"]],
                            exercise_name = secondary_lifts[[secondary_lift_i, "exercise_name"]]
                          )
                        }
                        
                        do.call(tabsetPanel, tabs)
                    })
                    
                    shinyjs::hide("choose_workout_ui")
                    shinyjs::show("workout_ui")
                    system("aplay -t wav ~/HDD1/gym_rat/www/gl.wav")
                    
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
                beepr::beep(8)
                # system("aplay -t wav ~/HDD1/gym_rat/www/fy.wav")
              
              
                browser()
                # global$secondary_lifts
                # global$primary_lifts
                # sum(global$primary_program$set_goal)
                # input[["Bench Press1"]]
                
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
    moduleServer(id, workout_server_function)
}