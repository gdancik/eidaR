
eidaR.env <- new.env()
eidaR.env$USE_PLOTLY <- TRUE

print_it <- function() {
    print(eidaR.env$USE_PLOTLY)
}

my_theme <- function(size = 12) {
  theme(text = element_text(size = size, family = c("Helvetica")))
}

add_margin <- function(gg) {
  x <- gg$x$layout$yaxis$ticktext
  gg$x$layout$yaxis$ticktext <- paste('   ', x)
  gg
}

display_plot <- function(g1, tooltip = c("x", "y"), formatDate = TRUE) {

  if (formatDate) {
    g1 <- g1 + scale_x_date(date_labels = "%m/%d") #,
    #date_breaks = "1 week", date_minor_breaks = "1 day")
  }

  if (!eidaR.env$USE_PLOTLY) {
    return(g1 + theme(legend.title = element_blank()))
  }

  (g1 + theme(legend.title = element_blank())) %>%
    ggplotly(tooltip = tooltip) %>% config(displayModeBar = FALSE) %>%
    add_margin() %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

# %>% partial_bundle()
}

saveUnemployment <- function(g, file, dir = getwd()) {
    file <- paste0(dir,'/',file)
    htmlwidgets::saveWidget((g%>%display_plot(tooltip = c("x","y","fill")) %>%
                          partial_bundle()), file = file)
}
