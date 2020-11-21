sever_page <- function() {
    tagList(
        tags$h1("Fuck"),
        tags$p("I really hope we didn't lose any data."),
        tags$button(
            "Reload Application", 
            onclick = "location.reload();", 
            class = "btn btn-default",
            style = glue(
                "color: #fff; background-color: {color};",
                color = db_colors[db_colors$color == "primary", "rgb"][[1]]
            )
        )
    )
}

custom_loading_page <- function() {
    tagList(
        spin_wave(),
        span("Loading Application", style="color:white;")
    )
}