set_fan_status <- function(status) {
    url <- "http://192.168.1.23:8000/gym/set_plug_status"
    body <- jsonlite::toJSON(
        list(
            name = "Fan",
            status = status
        ),
        auto_unbox = TRUE
    )
    
    response <- httr::PUT(url = url, body = body)
    
    return(response$status_code)
}
