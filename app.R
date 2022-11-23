#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(Vizumap)
# library(tidyverse)
# library(broom)
# #library(lme4)
# library(brms)
# library(ggplot2)

# GRAPH section -------------------------

# set options for plots
#theme_set(theme_classic())

# Read in example dataset
 # dekeyser <- read_csv("https://janhove.github.io/visualise_uncertainty/dekeyser2010.csv")

# Draw a scatterplot
 # ggplot(data = dekeyser,
 #        aes(x = AOA,
 #            y = GJT)) +
 #   geom_point(shape = 1) +
 #   xlab("Age of L2 acquisition") +
 #   ylab("L2 grammar test score")

# now with uncertainty visualization using geom_smooth
 # linreg <- ggplot(data = dekeyser,
 #        aes(x = AOA,
 #            y = GJT)) +
 #   geom_point(shape = 1) +
 #   # simple linear regression ("lm") with 95% confidence band
 #   geom_smooth(method = "lm") +
 #   xlab("Age of L2 acquisition") +
 #   ylab("L2 grammar test score")


# MAP section --------------------------------------------

data(us_data)
head(us_data)
data(us_geo)

### biv map --------------------------------------------

# use one of four pre-prepared colour palettes
cmBivPal <- build_palette(name = "CyanMagenta")
view(cmBivPal)

# below, read.uv creates a df that is usable to build a map with the function after
poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")

# create map using the biv color palette and the poverty df
usBivMap <- build_bmap(data = poverty, geoData = us_geo,
                       id = "GEO_ID", terciles = TRUE)
view(usBivMap)

# generate legend to go with the map above
usBivKey <- build_bkey(data = poverty, terciles = TRUE)
view(usBivKey)

# final element to put into the plotRender in the server argument below
usbivplot <- attach_key(usBivMap, usBivKey)

#### glyph map ----------------------
data(us_data)
data(us_geo)

co_geo <- subset(us_geo, us_geo@data$STATE == "08")

us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
co_data <- subset(us_data, us_data$GEO.id2 > 8000 & us_data$GEO.id2 < 9000)
co_data <- read.uv(data = co_data, estimate = "pov_rate", error = "pov_moe")

# build the glyph map
usGlyphMapDif <- build_gmap(data = co_data, geoData = co_geo, id = "GEO_ID", size = 70, border = "county", glyph = "semi", palette = "Reds")
view(usGlyphMapDif)

#### pixelating map -------------------------

us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
ca_data <- subset(us_data, us_data$GEO.id2 > 6000 & us_data$GEO.id2 < 7000)
ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "pov_moe")
row.names(ca_data) <- seq(1, nrow(ca_data), 1)

# pixelate the shapefile with county polygons from california
ca_geo <- subset(us_geo, us_geo@data$STATE == "06")
pix <- pixelate(ca_geo, id = "region")

df <- data.frame(region = sapply(slot(ca_geo, "polygons"), function(x) slot(x, "ID")), name = unique(ca_geo@data$GEO_ID))
ca_data$region <- df[match(ca_data$GEO_ID, df$name), 1]
ca_data$region <- as.character(ca_data$region)

# check that values in shared column match
all(ca_data$region %in% pix$region)

# uniform distribution plot
unifPixMap <- build_pmap(data = ca_data, distribution = "uniform", pixelGeo = pix, id = "region", border = ca_geo)
view(unifPixMap)

### exceedance probability map -------------------

# load data
data(us_data)
data(us_geo)

# format the data
poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")

# check variable quantiles
quantile(us_data$pov_rate)

# define probability distribution (exponential distribution)
pd <- quote({ pexp(q, rate, lower.tail = FALSE) })

# define argument listing
args <- quote({ list(rate = 1/estimate) })

# capture distribution and arguments in a single list
pdflist <- list(dist = pd, args = args, th = 30)

# build the map
usExcMap <- build_emap(data = poverty, pdflist = pdflist, geoData = us_geo, id = "GEO_ID", key_label = "Pr[X > 30]")
view(usExcMap)

# Define UI -------------------------------------------
ui <- fluidPage(theme=shinytheme("paper"),
                
          navbar <- navbarPage(
                title = "Uncertainty visualizations",
                id = "navbar",
          
                # page1 'Home' content------------------
                tabPanel( title = "Home", value = "tab1",
                          
                          mainPanel(
                            
                            img(src='homepage.jpg', width="200 px", align = "right"),
                            
                            h1("Uncertainty visualisation"),
                            h4("This page showcases different types of uncertainty visualisation
                               for geospatial data. Navigate between the pages to explore."),
          
                            actionButton("redirect1", "Maps"),
                            actionButton("redirect2", "Graphs")
                            
                            ),
                          
                        ),
              # page2 'Maps' content------------------
              tabPanel( title = "Maps", value = "tab2",
                        mainPanel(
                          selectInput(inputId = "plotType", 
                                      label = "Select plot type", 
                                      choices = c("Bivariate", "Glyph", "Pixel", "Excedance"), selected = "Bivariate"),
                          h3("Bivariate uncertainty map for poverty in USA"),
                          p("The map below shows a map of the USA which displays poverty rates including an uncertainty measure. The coloring scheme is a combination of poverty rates and a margin of error for the poverty estimate. See the quadratic legend."),
                          plotOutput("plot_usmap"),
                          h3("Glyph uncertainty map for poverty in USA"),
                          plotOutput("plot_glyphmap"),
                          h3("Pixelated uncertainty map"),
                          plotOutput("plot_pixel"),
                          h3("Exceedance probability map"),
                          plotOutput("plot_exc")
                        )),
          
              # page3 'Graphs' content------------------
              tabPanel( title = "Graphs", value = "tab3"),
              #           mainPanel(
              #             h3("Linear regression with 95% confidence interval"),
              #             plotOutput("plot_linreg")
              #           )),

              # page4 'References' content------------------
              tabPanel( title = "References", value = "tab3",
                        mainPanel(
                          h1("Sources"),
                          p("The map visualizations used were built with the package Vizumap. Click below to go to the related publication."),
                          a(actionButton(inputId = "website", label = " https://joss.theoj.org/papers/10.21105/joss.02409", 
                                         icon = icon("newspaper", lib = "font-awesome")),
                            href="https://joss.theoj.org/papers/10.21105/joss.02409")
                        ))
))

# Define server logic ---------------------------
server <- function(input, output, session) {
          # plot linear regression with uncertainty
          #output$plot_linreg <- renderPlot(linreg)
  
          # plot bivariate us poverty with key
          output$plot_usmap <- renderPlot(usbivplot)
          
          # plot bivariate glyph map
          output$plot_glyphmap <- renderPlot(view(usGlyphMapDif))
          
          # plot pixelated map
          output$plot_pixel <- renderPlot(view(unifPixMap))
          
          # plot exceedance probability map
          output$plot_exc <- renderPlot(view(usExcMap))
  
          #handle redirect actionButton
          observeEvent(input$redirect1, {
            updateNavbarPage(session, "navbar", selected = "tab2") #redirect to 'Maps' subpage
          })
          observeEvent(input$redirect2, {
            updateNavbarPage(session, "navbar", selected = "tab3") #redirect to 'Graphs' subpage
          })
        }

# Run the application 
shinyApp(ui = ui, server = server)
