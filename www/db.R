insert_wo_template <- readr::read_file(
    here("www", "tabs", "workout", "insert_workout.sql")
)

update_workout_number <- readr::read_file(
    here("www", "tabs", "workout", "update_workout_number.sql")
)

lift_insert_template <- readr::read_file(
    here("www", "tabs", "workout", "lift_insert.sql")
)

set_insert_template <- readr::read_file(
    here("www", "tabs", "workout", "set_insert.sql")
)

update_end_dttm_template <- readr::read_file(
    here("www", "tabs", "workout", "update_end_dttm.sql")
)

past_exercise_template <- readr::read_file(
    here("www", "tabs", "workout", "past_exercise_summary.sql")
)

get_last_workouts <- function (con) {
    return(
        tbl(con, "last_workout") %>% 
            filter(id != 4) %>% 
            arrange(desc(days_ago)) %>% 
            collect() %>% 
            rename_all(~str_to_title(str_replace_all(., "_", " ")))
    )
}

write_workout <- function(con, start_dttm, workout_group_id) {
    insert_statement <- glue_sql(
        insert_wo_template,
        start_dttm = start_dttm,
        workout_group_id = workout_group_id,
        program_id = 1,
        .con = con
    )
    insert_result <-  DBI::dbSendStatement(
        con, 
        insert_statement
    )
    workout_id <- DBI::dbFetch(res = insert_result)[[1]]
    DBI::dbClearResult(res = insert_result)
    
    return(workout_id)
}

get_workout_number <- function(con, workout_id, workout_group_id) {
    wo_number <-
        tbl(con, "workout") %>% 
        filter(
            workout_group_id == !!workout_group_id, 
            id != !!workout_id
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
    
    return(this_workout_number)
}

update_workout <- function(con, workout_id, workout_number) {
    update_workout_statement <- glue_sql(
        update_workout_number,
        workout_number = workout_number,
        workout_id = workout_id,
        .con = con
    )
    update_result <- DBI::dbSendStatement(
        con, update_workout_statement
    )
    DBI::dbClearResult(update_result)
}

get_muscle_groups <- function(con, workout_group_id) {
    return(
        tbl(con, "workout_group") %>% 
        filter(id == !!workout_group_id) %>% 
        select(muscle_group_id) %>% 
        collect()
    )
}

get_exercises <- function(con, primary_muscle_groups) {
    return(
        tbl(con, "exercise") %>% 
        filter(muscle_group_id %in% !!primary_muscle_groups) %>% 
        select(
            exercise_id = id,
            exercise_name = name,
            muscle_group_id,
            primary_lift
        ) %>% 
        collect()
    )
}

get_primary_program <- function(con, this_workout_number) {
    return(
        tbl(con, "program_steps") %>%
        filter(
            program_id == 1, 
            workout_number == this_workout_number
        ) %>%
        arrange(step_number) %>%
        select(rep_goal, percent_of_max) %>%
        collect()
    )
}

get_maxes <- function(con, exercise_ids) {
    return(
        tbl(con, "personal_records") %>%
            filter(
                exercise_id %in% exercise_ids
            ) %>%
            select(exercise_id, weight) %>%
            collect()
    )
}

insert_lifts <- function(con, workout_id, lifts_df) {
    insert <- 
        lifts_df %>% 
        mutate(
            inserts = glue_sql(
                "({workout_id}, {exercise_id})",
                workout_id = workout_id,
                exercise_id = exercise_id
            )
        ) %>% 
        summarize(insert = paste0(inserts, collapse = ", ")) %>% 
        magrittr::extract2(1)
    
    insert_statement <- glue_sql(
        lift_insert_template, 
        insert = SQL(insert), 
        .con = con
    )
    
    insert_result <-  DBI::dbSendStatement(
        con, 
        insert_statement
    )
    lift_ids <- DBI::dbFetch(res = insert_result)[[1]]
    DBI::dbClearResult(res = insert_result)
    return(lift_ids)
}

insert_exercises <- function (con, lifts_df) {
    inserts <- 
        lifts_df %>% 
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
        .con = con
    )
    insert_result <-  DBI::dbSendStatement(
        con, 
        insert_statement
    )
    DBI::dbClearResult(res = insert_result)
}

update_workout_end <- function(con, workout_id) {
    update_statement <- glue_sql(
        update_end_dttm_template, 
        workout_id = workout_id, 
        .con = con
    )
    update_result <-  DBI::dbSendStatement(
        con, 
        update_statement
    )
    DBI::dbClearResult(res = update_result)
}

delete_workout <- function(con, workout_id) {
    delete_statement <- glue_sql(
        "delete from workout where id = {id};",
        id = workout_id,
        .con = con
    )
    
    delete_result <- DBI::dbSendStatement(con, delete_statement)
    DBI::dbClearResult(delete_result)
}


get_past_exercise_summaries <- function(con, exercise_ids) {
    statement <- glue_sql(
        past_exercise_template,
        exercise_ids = SQL(glue("({paste(exercise_ids, collapse = ', ')})")),
        .con = con
    )
    
    return(DBI::dbGetQuery(con, statement))
}
