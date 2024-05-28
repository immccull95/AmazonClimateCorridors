#### R libraries ####
library(shiny)
library(DT)
library(htmltools)

# made in LeastCostPaths_Characteristics.R
lookup <- read.csv("LCP_lookup_table.csv")

#### Main program ####
# with help from: https://clarewest.github.io/blog/post/making-tables-shiny/
# first, clean up table for Shiny app
lookup_shiny <- lookup
lookup_shiny$Length_km <- round(lookup_shiny$Length_km, digits=0)
lookup_shiny$Protection_pct <- round(lookup_shiny$Protection_pct, digits=0)
lookup_shiny$Roads_perkm <- round(lookup_shiny$Roads_perkm, digits=5)
names(lookup_shiny) <- c('Start','Start country','End','End country',
                         'Length (km)', 'Elevation gain (m)',
                         'Protection (%)','Total roads' ,'Roads per km')

server <- function(input, output) {
  output$mytable <- DT::renderDataTable(lookup_shiny,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
}