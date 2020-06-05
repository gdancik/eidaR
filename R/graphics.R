
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

  if (!USE_PLOTLY) {
    return(g1 + theme(legend.title = element_blank()))
  }

  (g1 + theme(legend.title = element_blank())) %>%
    ggplotly(tooltip = tooltip) %>% config(displayModeBar = FALSE) %>%
    add_margin()# %>% partial_bundle()
}

