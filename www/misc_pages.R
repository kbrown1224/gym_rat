sever_page <- function() {
    tagList(
        tags$h1("Oh No!"),
        tags$p("There seems to have been an issue."),
        tags$button(
            "Reload Application", 
            onclick = "location.reload();", 
            class = "btn btn-default",
            style = glue(
                "color: #fff; background-color: {color};",
                color = db_colors[db_colors$color == "primary", "rgb"][[1]]
            )
        ),
        tags$button(
            "Return to Portfolio", 
            onclick = "location.href='http://google.com';", 
            class = "btn btn-default",
            
            style = glue(
                "color: #fff; background-color: {color};",
                color = db_colors[db_colors$color == "success", "rgb"][[1]]
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