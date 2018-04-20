#
# Population growth in New Zealand Parliamentary Electorates
# 

library(shiny); library(tidyverse); library(leaflet); library(sp); library(rgdal); library(rgeos);

ui <- fluidPage(
   
   # Application title
   titlePanel("Population growth between 2006 and 2013 in New Zealand Parliamentary Electorates"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
         tags$head(
              tags$style(type="text/css", "select { max-width: 100px; }"),
              tags$style(type="text/css", ".span4 { max-width: 120px; }"),
              tags$style(type="text/css", ".well { max-width: 110px; }")
          ),
         radioButtons("choice", 
                      "Choose what pop. data you would like to see:",
                      choices=list("2013 Population" = "2013", "2006 Population" = "2006",
                                   "Change 2006-2013" = "Change"),
                      selected="2013"
                      )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          tags$head(
              tags$style(type="text/css", "select { max-width: 2000px; }"),
              tags$style(type="text/css", ".span4 { max-width: 2020px; }"),
              tags$style(type="text/css", ".well { max-width: 2010px; }")
          ),
         leafletOutput("map", height="800px")
      )
   )
)

# Define server logic required to draw the map with the correct layer
server <- function(input, output) {
 
## Note the following commented out code was run originally on my desktop to
## load the parliamentary boundary shapefile and population figures dataframe.
## The boundary spatial file is then simplified before being joined with the 
## population figure dataframe, so that it didn't
## take up as much space on the server and was quicker to load.  The results of this
## code was then saved in an RDS file, which is what is loaded in the app itself.  

   #epdata_pop <- read_excel("Electorate-profiles---raw-data.xlsx", sheet=2, skip=1, n_max=64) %>%
   #    mutate(code = 1:64)    
   #boundaries <- readOGR("GED Final 20140318_region.shp") %>%
   # spTransform(CRS("+proj=longlat +ellps=GRS80"))
    
   #boundaries2 <- gSimplify(boundaries, tol=0.0001, topologyPreserve=TRUE)
   #boundaries3 <- SpatialPolygonsDataFrame(boundaries2, data=boundaries@data)

   #boundaries$code <- as.integer(boundaries$code)
   #boundaries <- sp::merge(boundaries, epdata_pop, by="code")
   
   #boundaries3$code <- as.integer(boundaries3$code)
   #boundaries3 <- sp::merge(boundaries3, epdata_pop, by="code")
  
   boundaries3 <- readRDS("boundaries3.rds")

   m <- leaflet(boundaries3) %>%
       setView(lng=170, lat=-41, zoom =5) %>%
       addTiles()  
 
   output$map <- renderLeaflet({
       if (input$choice=="2006") { 
           pal = colorBin("YlOrRd", boundaries3@data$`Census Usually Resident Population Count 2006`)
           mytext <- paste("Name: ", boundaries3@data$name, "<br/>", 
                           "2006 Population: ", boundaries3@data$`Census Usually Resident Population Count 2006`, sep="") %>%
               lapply(htmltools::HTML)
           m %>%
               addPolygons(color="black", weight=3, fillOpacity=0.5, label=mytext,
                 fillColor=~pal(`Census Usually Resident Population Count 2006`)) %>%
               addLegend(pal=pal, values=~`Census Usually Resident Population Count 2006`, title="2006 Est. Population")
       
         } else {if (input$choice=="2013") { 
           pal = colorBin("YlOrRd", boundaries3@data$`Census Usually Resident Population Count 2013`)
           mytext <- paste("Name: ", boundaries3@data$name, "<br/> ", 
                           "2013 Population: ", boundaries3@data$`Census Usually Resident Population Count 2013`, sep="") %>%
           lapply(htmltools::HTML)
           m %>%
              addPolygons(color="black", weight=3, fillOpacity=0.5, label=mytext,
                           fillColor=~pal(`Census Usually Resident Population Count 2013`)) %>%
              addLegend(pal=pal, values=~`Census Usually Resident Population Count 2013`, title="2013 Est. Population")
           
            } else { 
            pal = colorBin("YlOrRd", boundaries3@data$`Change (%)`)
            mytext <- paste("Name: ", boundaries3@data$name, "<br/> ", 
                            "Change 2006 to 2013: ", boundaries3@data$`Change (%)`, "%", sep="") %>%
                lapply(htmltools::HTML)
            m %>%
               addPolygons(color="black", weight=3, fillOpacity=0.5, label=mytext,
                                fillColor=~pal(`Change (%)`)) %>%
                addLegend(pal=pal, values=~`Change (%)`, title="% Change")
                
            }
        } 
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

