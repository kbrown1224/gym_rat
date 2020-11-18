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
        label = "Begin Workout",
        style = "pill",
        color = "success",
        block = TRUE
    )
    
    choose_workout_ui <- fluidPage(
        fluidRow(column(width = 12, last_wo_table)),
        fluidRow(column(width = 12, begin_wo_bttn))
    )
    
    end_wo_bttn <- actionBttn(
        inputId = ns("end_workout"),
        label = "End Workout",
        style = "pill",
        color = "success",
        block = TRUE
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
        label = "Return Home",
        style = "pill",
        color = "success",
        block = TRUE
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
            exercises = NULL
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
                            "  $(thead).css('font-size', '24pt');",
                            "  $(thead).css('color', '#fff');",
                            "}"
                        )
                    )
                ) %>% 
                formatStyle(
                    columns = c(1, 2, 3),
                    fontWeight = "bold",
                    fontSize = '18pt'
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
                                            inputId = paste0("main_1_", i),
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
                            
                            tabs[[primary_lift_i]] <- tabPanel(
                                title = primary_lift$exercise_name,
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
                        }
                        
                        secondary_lift_sets <- tibble(
                            sets = c(3, 3, 2),
                            reps = c(10, 12, 20)
                        )
                        secondary_lifts_1 <-
                            global$exercises %>% 
                                filter(
                                    !primary_lift, 
                                    muscle_group_id == primary_muscle_groups[1]
                                ) %>% 
                                sample_n(3)
                        
                        for (secondary_lift_i in 1:3) {
                          tabs[[secondary_lift_i + 2]] <- secondary_lift_tab(
                            id = id,
                            sets = secondary_lift_sets[[secondary_lift_i, "sets"]],
                            reps = secondary_lift_sets[[secondary_lift_i, "reps"]],
                            exercise_name = secondary_lifts_1[[secondary_lift_i, "exercise_name"]]
                          )
                        }
                        # for (secondary_lift_i in 1:3) {
                        #     sets <- secondary_lift_sets[[secondary_lift_i, "sets"]]
                        #     reps <- secondary_lift_sets[[secondary_lift_i, "reps"]]
                        #     
                        #     formatted_sets <- tibble(
                        #         reps = rep(reps, sets),
                        #         weight = rep(20, sets)
                        #     )
                        #     
                        #     
                        #     these_lifts <- list()
                        #     for (i in 1:nrow(formatted_sets)) {
                        #         reps <- formatted_sets[[i, "reps"]]
                        #         lift_weight <- formatted_sets[[i, "weight"]]
                        #         
                        #         these_lifts[[i]] <- fluidRow(
                        #             column(
                        #                 width = 6,
                        #                 tags$h2(
                        #                     glue('{reps} at {lift_weight}')
                        #                 )
                        #             ),
                        #             column(
                        #                 width = 6,
                        #                 sliderInput(
                        #                     inputId = paste0("secondary_1_", i),
                        #                     label = "Reps Achieved",
                        #                     min = 0,
                        #                     max = reps,
                        #                     value = 0,
                        #                     step = 1,
                        #                     animate = TRUE
                        #                 )
                        #             )
                        #         )
                        #     }
                        #     
                        #     secondary_lift <- secondary_lifts_1[secondary_lift_i, ]
                        #     
                        #     tabs[[secondary_lift_i + 2]] <- tabPanel(
                        #         title = secondary_lift$exercise_name,
                        #         fluidPage(
                        #             br(),
                        #             fluidRow(
                        #                 column(
                        #                     width = 6,
                        #                     box(
                        #                         title = "This lift historically",
                        #                         status = "primary",
                        #                         solidHeader = TRUE,
                        #                         collapsible = FALSE,
                        #                         width = NULL,
                        #                         plotOutput("secondary_lift")
                        #                     )
                        #                 ),
                        #                 column(
                        #                     width = 6,
                        #                     box(
                        #                         title = "Sets",
                        #                         status = "primary",
                        #                         solidHeader = TRUE,
                        #                         collapsible = FALSE,
                        #                         width = NULL,
                        #                         do.call(fluidPage, these_lifts)
                        #                     )
                        #                 )
                        #             )
                        #         )
                        #     )
                        # }
                        
                        secondary_lifts_2 <-
                            global$exercises %>% 
                            filter(
                                !primary_lift, 
                                muscle_group_id == primary_muscle_groups[2]
                            ) %>% 
                            sample_n(3)
                        for (secondary_lift_i in 1:3) {
                            sets <- secondary_lift_sets[[secondary_lift_i, "sets"]]
                            reps <- secondary_lift_sets[[secondary_lift_i, "reps"]]
                            
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
                                            inputId = paste0("secondary_2_", i),
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
                            
                            secondary_lift <- secondary_lifts_2[secondary_lift_i, ]
                            
                            tabs[[secondary_lift_i + 5]] <- tabPanel(
                                title = secondary_lift$exercise_name,
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
                                                plotOutput("secondary_lift")
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
                        }
                        
                        
                        do.call(tabsetPanel, tabs)
                        
                        # primary_program <- 
                        #     tbl(con, "program_steps") %>% 
                        #     filter(program_id == 1, workout_number == 1) %>% 
                        #     arrange(step_number) %>% 
                        #     select(set_goal, rep_goal, percent_of_max) %>% 
                        #     collect()
                        # 
                        # lift <-
                        #     global$exercises %>% 
                        #     filter(
                        #         primary_lift, 
                        #         muscle_group_id == primary_muscle_groups[1]
                        #     ) 
                        # 
                        # lift_max <- 
                        #     tbl(con, "personal_records") %>% 
                        #     filter(exercise_id == !!lift$exercise_id) %>% 
                        #     select(weight) %>% 
                        #     collect() %>% 
                        #     magrittr::extract2(1)
                        # 
                        # y <- primary_program %>% 
                        #     mutate(
                        #         weight = as.integer(percent_of_max * lift_max),
                        #         weight = 5 * floor(weight / 5)
                        #     )
                        # 
                        # test<- tibble()
                        # for (set_i in 1:nrow(y)) {
                        #     n_sets <- y[[set_i, "set_goal"]]
                        #     
                        #     test <- bind_rows(
                        #         test,
                        #         tibble(
                        #             reps = rep(y[[set_i, "rep_goal"]], n_sets),
                        #             weight = rep(y[[set_i, "weight"]], n_sets)
                        #         )
                        #     )
                        # }
                        # 
                        # these_lifts <- list()
                        # for (i in 1:nrow(test)) {
                        #     reps <- test[[i, "reps"]]
                        #     lift_weight <- test[[i, "weight"]]
                        #     
                        #     these_lifts[[i]] <- fluidRow(
                        #         column(
                        #             width = 6,
                        #             tags$h2(
                        #                 glue('{reps} at {lift_weight}')
                        #             )
                        #         ),
                        #         column(
                        #             width = 6,
                        #             sliderInput(
                        #                 inputId = paste0("main_1_", i),
                        #                 label = "Reps Achieved",
                        #                 min = 0,
                        #                 max = reps,
                        #                 value = 0,
                        #                 step = 1,
                        #                 animate = TRUE
                        #             )
                        #         )
                        #     )
                        # }
                        # 
                        # tabsetPanel(
                        #     tabPanel(
                        #         title = lift$exercise_name,
                        #         fluidPage(
                        #             br(),
                        #             fluidRow(
                        #                 column(
                        #                     width = 6,
                        #                     box(
                        #                         title = "This lift historically",
                        #                         status = "primary",
                        #                         solidHeader = TRUE,
                        #                         collapsible = FALSE,
                        #                         width = NULL,
                        #                         plotOutput("main_lift")
                        #                     )
                        #                 ),
                        #                 column(
                        #                     width = 6, 
                        #                     box(
                        #                         title = "Sets",
                        #                         status = "primary",
                        #                         solidHeader = TRUE,
                        #                         collapsible = FALSE,
                        #                         width = NULL,
                        #                         do.call(fluidPage, these_lifts)
                        #                     )
                        #                 )
                        #             )
                        #         )
                        #     )
                        # )
                        
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