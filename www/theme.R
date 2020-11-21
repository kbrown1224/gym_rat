db_colors <- tibble(
    color = c("primary", "success", "info", "warning", "danger"),
    hex = c("#BB000E", "#E31C25", "#FFFFFF", "#FDA006", "#F1536E"),
    rgb = c(
        "rgb(187, 0, 14)", 
        "rgb(227, 28, 37)", 
        "rgb(255, 255, 255)", 
        "rgb(253,160,6)", 
        "rgb(241,83,110)"
    )
)

get_color <- function(color, type) {
    return(db_colors[db_colors$color == color, type][[1]])
}

dashboard_theme <- shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(205,205,205)"
    ,primaryFontColor = "rgb(255,255,255)"
    ,infoFontColor = "rgb(255,255,255)"
    ,successFontColor = "rgb(255,255,255)"
    ,warningFontColor = "rgb(255,255,255)"
    ,dangerFontColor = "rgb(255,255,255)"
    # ,bodyBackColor = "rgb(45,55,65)"
    ,bodyBackColor = "rgb(28, 28, 28)"
    
    ### header
    # ,logoBackColor = "rgb(70,80,90)"
    ,logoBackColor = "rgb(22, 22, 22)"
    
    # ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonBackColor = "rgb(22, 22, 22)"
    ,headerButtonIconColor = "rgb(25,35,45)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    # ,headerBackColor = "rgb(70,80,90)"
    ,headerBackColor = "rgb(22, 22, 22)"
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    # ,sidebarBackColor = "rgb(52,62,72)"
    ,sidebarBackColor = "rgb(34, 34, 34)"
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarUserTextColor = "rgb(205,205,205)"
    
    ,sidebarSearchBackColor = "rgb(45,55,65)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(45,55,65)"
    
    ,sidebarTabTextColor = "rgb(205,205,205)"
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0
    
    ,sidebarTabBackColorSelected = "rgb(70,80,90)"
    ,sidebarTabTextColorSelected = "rgb(255,255,255)"
    ,sidebarTabRadiusSelected = "5px"
    
    ,sidebarTabBackColorHover = "rgb(55,65,75)"
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "5px"
    
    ### boxes
    # ,boxBackColor = "rgb(52,62,72)"
    ,boxBackColor = "rgb(22, 22, 22)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(52,62,72)"
    ,boxPrimaryColor = get_color("primary", "rgb")
    ,boxInfoColor = get_color("info", "rgb")
    ,boxSuccessColor = get_color("success", "rgb")
    ,boxWarningColor = get_color("warning", "rgb")
    ,boxDangerColor = get_color("danger", "rgb")
    
    ,tabBoxTabColor = "rgb(52,62,72)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(205,205,205)"
    ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
    ,tabBoxBackColor = "rgb(52,62,72)"
    ,tabBoxHighlightColor = "rgb(70,80,90)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"
    
    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"
    
    ### tables
    ,tableBackColor = "rgb(52,62,72)"
    ,tableBorderColor = "rgb(70,80,90)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
    
)

plot_theme <- function(...) {
    return(
        ggthemes::theme_few() +
            theme(
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.background = element_blank(),
                plot.background = element_blank(),
                text = element_text(size = 18, color = "white"),
                axis.text.x = element_text(color = "white"),
                axis.text.y = element_text(color = "white"),
                ...
            )
    )
}

spinner <- function(ui_element) {
    return(
        withSpinner(
            ui_element = ui_element,
            type = 7,
            color = get_color("primary", "hex"),
            size = 2
        )
    )
}
