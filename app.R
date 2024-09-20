#
# R SHINY Tutorial
# by Nadia Muhe, Map & Data Library, 2024
#========================================================

# Packages
library(shiny)
library(dplyr)
library(ggplot2)


# 1. User Interface Object
ui <- fluidPage(

  titlePanel("Toronto TTC Subway Delays"), 
  
  sidebarLayout(
    
    # CONTROL WIDGETS
    sidebarPanel(
      
      # SLIDER: YEAR
      sliderInput(inputId="year",
                  label="Range of Years:", 
                  min=2014, max=2024, 
                  value=c(2014,2024), 
                  sep=""),
      
      # RADIO BUTTON: Y-AXIS
      radioButtons(inputId = "yvariable",
                   label = "Y-Axis: Delay Variable to Plot:",
                   choices = list(
                     "Total Hours of Delay" = "totalhours",
                     "Average Minutes of Delay" = "averagemin")),
      
      # RADIO BUTTON: X-AXIS
      radioButtons(inputId = "xvariable",
                   label = "X-Axis: Time Variable to Plot:",
                   choices = list(
                     "Year" = "year",
                     "Month" = "month",
                     "Day of the Month" = "day",
                     "Hour of the Day" = "hourofday")),
      
      # CHECKBOX: DELAY CODES
      h5(strong("Show Most Common Delay Codes:")),
      checkboxInput(inputId="showcodes",
                         label="Yes", 
                         value=FALSE),
      
      
      # DROPDOWN MENU: SUBWAY STATION
      selectInput(inputId="subwaystation",
                  label="Select Specific Subway Station:",
                  choices = list(
                    "All Stations"="allstations",
                    "Kipling Station"="kipling", 
                    "Islington Station"="islington", 
                    "Royal York Station"="royalyork", 
                    "Old Mill Station"= "oldmill", 
                    "Jane Station" = "jane", 
                    "Runnymede Station" = "runnymede", 
                    "High Park Station" = "highpark", 
                    "Keele Station" = "keele", 
                    "Dundas West Station" = "dundaswest", 
                    "Lansdowne Station" = "lansdowne", 
                    "Dufferin Station" = "dufferin", 
                    "Ossington Station" = "ossington", 
                    "Christie Station" = "christie", 
                    "Bathurst Station" = "bathurst", 
                    "Spadina Station" = "spadina", 
                    "St George Station" = "stgeorge", 
                    "Bay Station" = "bay", 
                    "Bloor-Yonge Station" = "bloor", 
                    "Sherbourne Station" = "sherbourne", 
                    "Castle Frank Station" = "castlefrank", 
                    "Broadview Station" = "broadview", 
                    "Chester Station" = "chester", 
                    "Pape Station" = "pape", 
                    "Donlands Station" = "donlands", 
                    "Greenwood Station" = "greenwood", 
                    "Coxwell Station" = "coxwell", 
                    "Woodbine Station" = "woodbine", 
                    "Main Street Station" = "mainstreet", 
                    "Victoria Park Station" = "victoriapark", 
                    "Warden Station" = "warden", 
                    "Kennedy Station" = "kennedy", 
                    "Lawrence East Station" = "lawrenceeast", 
                    "Ellesmere Station" = "ellesmere", 
                    "Midland Station" = "midland", 
                    "Scarborough Centre Station" = "scarboroughcentre", 
                    "McCowan Station" = "mccowan",
                    "Vaughan MC Station" = "vaughanmc", 
                    "Highway 407 Station" = "highway407", 
                    "Pioneer Village Station" = "pioneervillage", 
                    "York University Station" = "yorkuniversity", 
                    "Finch West Station" = "finchwest", 
                    "Downsview Park Station" = "downsviewpark", 
                    "Sheppard West Station" = "sheppardwest", 
                    "Wilson Station" = "wilson", 
                    "Yorkdale Station" = "yorkdale", 
                    "Lawrence West Station" = "lawrencewest", 
                    "Glencairn Station" = "glencairn", 
                    "Eglinton West Station" = "eglintonwest", 
                    "St Clair West Station" = "stclairwest", 
                    "Dupont Station" = "dupont", 
                    "Museum Station" = "museum", 
                    "Queens Park Station" = "queenspark", 
                    "St Patrick Station" = "stpatrick", 
                    "Osgoode Station" = "osgoode", 
                    "St Andrew Station" = "standrew", 
                    "Union Station" = "union", 
                    "King Station" = "king", 
                    "Queen Station" = "queen", 
                    "Dundas Station" = "dundas", 
                    "College Station" = "college", 
                    "Wellesley Station" = "wellesley", 
                    "Rosedale Station" = "rosedale", 
                    "Summerhill Station" = "summerhill", 
                    "St Clair Station" = "stclair", 
                    "Davisville Station" = "davisville", 
                    "Eglinton Station" = "eglinton", 
                    "Lawrence Station" = "lawrence", 
                    "York Mills Station" = "yorkmills", 
                    "Sheppard Station" = "sheppard", 
                    "North York Centre" = "northyorkcentre", 
                    "Finch Station" = "finch", 
                    "Bayview Station" = "bayview", 
                    "Bessarion Station" = "bessarion", 
                    "Leslie Station" = "leslie", 
                    "Don Mills Station" = "donmills")),
      
      
      # TEXT: DATA SOURCE
      "Data collected from the City of Toronto's Open Data Portal:",
      htmltools::a("TTC Subway Delays Data.", href="https://open.toronto.ca/dataset/ttc-subway-delay-data/"),
      "It contains the TTC subway delays from January 2014 until August 2024.",
      htmltools::br(),
      "This app was developed by Nadia Muhe.",
      htmltools::br(),
      "The guide for this application can be found",
      htmltools::a("here.", href="https://mdl.library.utoronto.ca/technology/tutorials/introduction-r-shiny")
      
    ),
    
    # GRAPH & TABLE
    mainPanel(
        plotOutput("graph"), 
        tableOutput("codestable")
    )
  )
)

# 2. Server Function
server <- function(input, output){
  
  # IMPORT DATASET
  ttc<-readRDS("ttcdelays.rds")
  
  # FILTER DATASET
  ttcfiltered <- reactive({
    
    # Filter by Year
    ttc <- ttc %>% filter(year>=input$year[1] & year<=input$year[2])
    
    # Filter by Subway Station
    if(input$subwaystation!="allstations"){
      ttc <- ttc %>% filter(!!as.name(input$subwaystation)==1)
    }
    
    return(ttc)
  })

  
  # GRAPH OUTPUT
  output$graph<-renderPlot({

    # Prepare Graph Data
    ttcfiltered <- as.data.frame(ttcfiltered())
    ttcgraphdata <- ttcfiltered %>% 
      group_by(!!as.name(input$xvariable)) %>% 
      summarize(totalhours = sum(delayminutes, na.rm = TRUE)/60, 
                averagemin = mean(delayminutes, na.rm = TRUE))
    
    # Y-Axis Label
    ylabel <- "Total Hours of Delay"
    if(input$yvariable=="averagemin"){
      ylabel <- "Average Minutes of Delay"
    }

    # X-Axis Label
    xlabel <- "Year"
    if(input$xvariable=="month"){
      xlabel <- "Month"
    } else if(input$xvariable=="day"){
      xlabel <- "Day of the Week"
    } else if(input$xvariable=="hourofday"){
      xlabel <- "Hour of the Day"
    }

    # X-Axis Tick Marks
    xmin <- ttcgraphdata %>% summarize(min(!!as.name(input$xvariable))) %>% as.numeric()
    xmax <- ttcgraphdata %>% summarize(max(!!as.name(input$xvariable))) %>% as.numeric()

    # Graph
    ggplot(ttcgraphdata, aes(x=!!as.name(input$xvariable), 
                  y=!!as.name(input$yvariable))) + 
    geom_line(color="yellow3", linewidth=1) + 
    theme_bw(base_size=15) + 
    ylab(ylabel) + xlab(xlabel) + 
    ggtitle(paste0(ylabel, " by ", xlabel, " (n=", nrow(ttcfiltered), ")")) +
    scale_x_continuous(breaks=seq(xmin, xmax, 1))
    
  })

  
  # DELAY CODES DATA TABLE OUTPUT
  output$codestable<-renderTable({

    if(input$showcodes==TRUE){
      as.data.frame(ttcfiltered()) %>%
        group_by(code) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        filter(row_number()<=10) %>%  
        rename("Most Common Delay Codes"="code", "Delay Count"="count")
    }

  })

}

# 3. Create the Shiny Application
shinyApp(ui, server)
