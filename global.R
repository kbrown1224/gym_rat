library(dashboardthemes)
library(DBI)
library(dplyr)
library(DT)
library(future)
library(future.callr)
library(ggplot2)
library(ggrepel)
library(glue)
library(here)
library(lubridate)
library(pool)
library(sever)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(tidyr)
# library(tuneR)
library(waiter)

plan(callr)

source(here::here("www", "misc_pages.R"))
source(here::here("www", "theme.R"))
source(here::here("www", "db.R"))
source(here::here("www", "smart_house.R"))

for (tab_dir in list.files(here::here("www", "tabs"))){
    for (r_file in list.files(here::here("www", "tabs", tab_dir), pattern = "*.R")) {
        source(here::here("www", "tabs", tab_dir, r_file))
    }
}

app_config <- config::get(config = "default", file = here::here("config.yaml"))
db_con = DBI::dbConnect(
    # drv = RPostgres::Postgres(),
    drv = RPostgreSQL::PostgreSQL(),
    dbname = app_config$dbname,
    host = app_config$host,
    port = app_config$port,
    user = app_config$user,
    password = app_config$password,
    options = app_config$options
)
