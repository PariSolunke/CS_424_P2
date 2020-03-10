library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(grid)
library(leaflet)
library(DT)
library(stringi)
library(dplyr)
library(plyr)
library(maptools)
library(sp)
library(data.table)



x <- purrr::map("atlantic.txt", readr::read_lines)

x <- purrr::flatten_chr(x)

hurdat <- as.data.frame(x, stringsAsFactors = FALSE)
header_rows <- grep(pattern = "^[[:alpha:]]{2}[[:digit:]]{6}.+", x)

# Split header_rows into variables
hurdat <- tidyr::extract(
  data = hurdat,
  col = "x",
  into = c("Key", "Name", "Lines"),
  regex = paste0(
    "([:alpha:]{2}[:digit:]{6}),\\s+", # Key
    "([[:upper:][:digit:]-]+)\\s*,\\s+", # Name
    "([:digit:]+)," # Number of lines that follow
  ),
  remove = FALSE,
  convert = TRUE
)

# Fill headers down
hurdat <- tidyr::fill(data = hurdat, .data$Key, .data$Name, .data$Lines)

# Remove original header rows
hurdat <- hurdat[-header_rows, ]

# Split storm details into variables
hurdat <- tidyr::extract(
  data = hurdat,
  col = "x",
  into = c(
    "Year",
    "Month",
    "Date",
    "Hour",
    "Minute",
    "Record",
    "Status",
    "Lat",
    "LatHemi",
    "Lon",
    "LonHemi",
    "Wind",
    "Pressure",
    "NE34",
    "SE34",
    "SW34",
    "NW34",
    "NE50",
    "SE50",
    "SW50",
    "NW50",
    "NE64",
    "SE64",
    "SW64",
    "NW64"
  ),
  regex = paste0(
    "^([:digit:]{4})", # Year
    "([:digit:]{2})", # Month
    "([:digit:]{2}),\\s+", # Date
    "([:digit:]{2})", # Hour
    "([:digit:]{2}),\\s+", # Minute
    "([:alpha:]*),\\s+", # Record
    "([:alpha:]{2}),\\s+", # Status
    "([:digit:]{1,2}\\.[:digit:]{1})", # Latitude
    "([:alpha:]{1}),\\s+", # Hemisphere
    "([:digit:]{1,3}\\.[:digit:]{1})", # Longitude
    "([:alpha:]{1}),\\s+", # Hemisphere
    "([[:digit:]-]+),\\s+", # Wind
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+),\\s+", #
    "([[:digit:]-]+).*" #
  ),
  remove = FALSE,
  convert = TRUE
)

hurdat <- dplyr::mutate(
  .data = hurdat,
  Lat = dplyr::if_else(
    .data$LatHemi == "N", .data$Lat * 1, .data$Lat * -1
  ),
  Lon = dplyr::if_else(
    .data$LonHemi == "E", .data$Lon * 1, .data$Lon * -1
  )
)

hurdat$DateTime <- paste(
  paste(hurdat$Year, hurdat$Month, hurdat$Date, sep = "-"),
  paste(hurdat$Hour, hurdat$Minute, "00", sep = ":"),
  sep = " "
)

hurdat <- dplyr::select(
  .data = hurdat,
  .data$Key, .data$Name, .data$DateTime, .data$Record:.data$Lat,
  .data$Lon, .data$Wind:.data$NW64
)

hurdat$DateTime <- as.POSIXct(
  strptime(hurdat$DateTime, format = "%Y-%m-%d %H:%M:%S")
)
hurdat$Name[hurdat$Name == "UNNAMED"] <- hurdat$Key[hurdat$Name == "UNNAMED"]
hurdat$years<-year(hurdat$DateTime)


hurdat$Category[hurdat$Status == "TD"|hurdat$Status == "SD"] = 0.5
hurdat$Category[hurdat$Status == "TS"|hurdat$Status == "SS"] = 0.75
hurdat$Category[hurdat$Status == "HU" & hurdat$Wind>=64 & hurdat$Wind<=82] = 1
hurdat$Category[hurdat$Status == "HU" & hurdat$Wind>=83 & hurdat$Wind<=95] = 2
hurdat$Category[hurdat$Status == "HU" & hurdat$Wind>=96 & hurdat$Wind<=112] = 3
hurdat$Category[hurdat$Status == "HU" & hurdat$Wind>=113 & hurdat$Wind<=136] = 4
hurdat$Category[hurdat$Status == "HU" & hurdat$Wind>=137] = 5

hurdat$Name <- paste(hurdat$Name, "-", hurdat$years)
df<-hurdat
df$colour<-ifelse(df$Category == 0.5, "lightgreen",(ifelse(df$Category == 0.75, "darkgreen",(ifelse(df$Category ==1, "yellow",(ifelse(df$Category ==2, "orange",(ifelse(df$Category ==3, "darkorange",(ifelse(df$Category ==4, "red",(ifelse(df$Category ==5, "darkred","darkgrey")))))))))))))
df$colour[is.na(df$colour)] <- "darkgrey"
landfalls<-df[(df$Record=="L"),]


hurdat$Date =  format(hurdat$DateTime,format='%Y-%m-%d')






df1 = hurdat[c("Key","Wind")]
df2 = hurdat[c("Key","Pressure")]
nameKey = hurdat[c("Key","Name")]

df1 = ddply(df1, "Key", numcolwise(max))
df2 = ddply(df2, "Key", numcolwise(min))
nameKey = ddply(nameKey, "Key", function(nameKey) unique(nameKey))

hurricanes = merge(df1,df2, by.x = "Key", by.y = "Key")

df1 = hurdat[c("Key","DateTime")]
df1 = data.table(df1)
df1 = df1[,list(DateTime = min(DateTime)), by = Key]
df1 = data.frame(df1)

hurricanes = merge(hurricanes,df1, by.x = "Key", by.y = "Key")

df2 = hurdat[c("Key","Category")]
df2 = df2[!is.na(df2$Category),]
df2 = ddply(df2, "Key", numcolwise(max))
df2 = merge(df2,df1, by.x = "Key", by.y = "Key")
df2["Key"] <- NULL
df2$DateTime = year(df2$DateTime)
df2 = aggregate(rep(1, nrow(df2)), by = list(x = df2$Category, y = df2$DateTime), sum)
colnames(df2) = c("Category", "Year", "Count")

df2$TD[df2$Category == 0.5] <- df2$Count[df2$Category == 0.5]
df2$TS[df2$Category == 0.75] <- df2$Count[df2$Category == 0.75]
df2$HU1[df2$Category == 1] <- df2$Count[df2$Category == 1]
df2$HU2[df2$Category == 2] <- df2$Count[df2$Category == 2]
df2$HU3[df2$Category == 3] <- df2$Count[df2$Category == 3]
df2$HU4[df2$Category == 4] <- df2$Count[df2$Category == 4]
df2$HU5[df2$Category == 5] <- df2$Count[df2$Category == 5]

df2["Category"] <- NULL
df2["Count"] <- NULL
df2[is.na(df2)] <- 0

df2 = ddply(df2,"Year",numcolwise(sum))

Names<-as.array(unique(df$Name))
yr<-1851:2018
toph<-hurricanes[order(hurricanes$Wind, decreasing = TRUE),]
toph<-head(toph,10)

#yr<-append(yr,"All",after = 0)
#Names<-append(Names,"All",after = 0)



ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     
                     menuItem("Home",tabName = "Home", selected = TRUE),
                     dateInput("Date","Date:",value = "1851-06-25"),
                     selectInput("flt","Filter by:",c("None","Year","Name","TopTen"),selected = "None"),
                     conditionalPanel(
                       condition = "input.flt == 'Year'",
                       selectInput("ipyr", "Selecft the Year to visualize", yr, selected = "2005"),   
                     ),
                     conditionalPanel(
                       condition = "input.flt == 'Name'",
                       selectInput("ipnm", "Select the Name to visualize", Names,selected= "OSCAR - 2018"),   
                     ),
                     checkboxInput("land", "Only hurricanes which made Landfall", value = FALSE),
                     
                     menuItem("About",tabName = "About"))
  ),
  dashboardBody(
    tabItems(
      tabItem( tabName = "Home",
               fluidRow(
                 column(4,
                        fluidRow(
                          box( title = "List of Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               dataTableOutput("tab1")
                          )
                        )
                 ),
                 column(8,
                        fluidRow(
                          box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                              leafletOutput("leaf", height = 750)
                          ),
                        )
                 ),
                 
                 
               ),
               fluidRow(
                 box( title = "Hurricanes Per Year", solidHeader = TRUE, status = "primary", width = 12,
                      plotOutput("bar1",height = 600)
                 )
               ),
               
      ),
      tabItem(tabName = "About",  h1("Author: Ansul Goenka, Parikshit Solunke"), h1("Libraries used: shiny, shinydashboard, 
              ggplot2, lubridate, grid, leaflet, DT, stringi, dplyr"), h1("The Atlantic hurricane database (HURDAT2) 
              1851-2018 and the Northeast and North Central Pacific hurricane database (HURDAT2) 1949-2018: 
              http://www.nhc.noaa.gov/data/#hurdat"))
    ))
)



server <- function(input, output) {
  plotdata<-sp_lines
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  
  
  dayReactive <- reactive({
    if(stri_cmp(input$Date, "1851/06/25")!=0)
    {
      
      data = subset(hurdat,  hurdat$Date== input$Date)
    }
    else{
      data = hurdat
    }
  })
  
  
  
  output$bar1 <- renderPlot({
    data = df1
    data$Year = year(data$DateTime)
    data$DateTime = NULL
    data = as.data.frame(table(data$Year))
    colnames(data) <- c("Year", "Total")
    data = merge(data, df2, by.x = "Year", by.y = "Year")
    
    ggplot(data, aes(x=Year,group = 1),) + geom_line(aes(y=Total, color = 'Total')) + geom_line(aes(y=TD, color = 'TD')) + 
      geom_line(aes(y=TS, color = 'TS')) + geom_line(aes(y=HU1, color = 'HU1')) + geom_line(aes(y=HU2, color = 'HU2')) + 
      geom_line(aes(y=HU3, color = 'HU3')) + geom_line(aes(y=HU4, color = 'HU4')) + geom_line(aes(y=HU5, color = 'HU5')) +
      labs(x="Year", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_color_manual(values = c(Total= "black",TD="blue", TS="green", HU1 = "orange", HU2 = "red", HU3 = "grey", 
                                    HU4 = "darkgreen", HU5 = "darkblue")) + scale_x_discrete(breaks = seq(1851,2018, by = 10))
  })
  
  output$leaf <- renderLeaflet({
    if(stri_cmp(input$flt, "None")!=0)
    {
      if(input$flt=="Year")
      {
        df3<-df[(df$years==input$ipyr),]
      }
      if(input$flt=="Name")
      {
        df3<-df[(df$Name==input$ipnm),]
      }
      if(input$flt=="TopTen")
      {
        df3<-df[(df$Key==toph$Key),]
        
        
      }
      
    }
    else{
      df3<-df[(df$years>2004),]
    }
    if(input$land)
    {
      df3<-df3 %>%
        filter(Name %in% landfalls$Name)
    }
    
    Names2<-as.array(unique(df3$Name))
    map <- leaflet()
    map <- addTiles(map)
    for ( i in Names2) 
    {
      df4<-df3[(df3$Name==i),]
      
  
      map<-addCircles(map, lat=df4$Lat,lng=df4$Lon, weight =4,color=df4$colour)
      map<-addPolylines(map, lat=df4$Lat,lng=df4$Lon, weight =3,color="White",opacity = 0.60, highlightOptions = highlightOptions(color = "white",bringToFront = T))
      }
      
    map
  })
  
  output$tab1 <- DT::renderDataTable(
    DT::datatable({  
      hurricanes = merge(nameKey, hurricanes, by.x="Key", by.y="Key")
    }, 
    options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
}

shinyApp(ui = ui, server = server)