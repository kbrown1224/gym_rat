library(dashboardthemes)
library(DBI)
library(dplyr)
library(DT)
library(glue)
library(here)
library(lubridate)
library(pool)
library(sever)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(tuneR)
library(waiter)

source(here("www", "misc_pages.R"))
source(here("www", "theme.R"))
source(here("www", "db.R"))
source(here("www", "smart_house.R"))
# source(here("www", "workout_ui.R"))
for (tab_dir in list.files(here("www", "tabs"))){
    for (r_file in list.files(here("www", "tabs", tab_dir), pattern = "*.R")) {
        source(here("www", "tabs", tab_dir, r_file))
    }
}

db_con = DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "gym_rat",
    host = "192.168.1.23",
    port = 5432,
    user = "svc_gym_rat",
    password = "get_big",
    options = "-c search_path=kip"
)
