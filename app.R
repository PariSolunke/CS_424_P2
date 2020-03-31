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
library(RColorBrewer)
library(naniar)
library(leaflet.extras)


x <- purrr::map("atlantic.txt", readr::read_lines)

x <- purrr::flatten_chr(x)

atlantic <- as.data.frame(x, stringsAsFactors = FALSE)
header_rows <- grep(pattern = "^[[:alpha:]]{2}[[:digit:]]{6}.+", x)

# Split header_rows into variables
atlantic <- tidyr::extract(
  data = atlantic,
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
atlantic <- tidyr::fill(data = atlantic, .data$Key, .data$Name, .data$Lines)

# Remove original header rows
atlantic <- atlantic[-header_rows, ]

# Split storm details into variables
atlantic <- tidyr::extract(
  data = atlantic,
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

atlantic <- dplyr::mutate(
  .data = atlantic,
  Lat = dplyr::if_else(
    .data$LatHemi == "N", .data$Lat * 1, .data$Lat * -1
  ),
  Lon = dplyr::if_else(
    .data$LonHemi == "E", .data$Lon * 1, .data$Lon * -1
  )
)

atlantic$DateTime <- paste(
  paste(atlantic$Year, atlantic$Month, atlantic$Date, sep = "-"),
  paste(atlantic$Hour, atlantic$Minute, "00", sep = ":"),
  sep = " "
)

atlantic <- dplyr::select(
  .data = atlantic,
  .data$Key, .data$Name, .data$DateTime, .data$Record:.data$Lat,
  .data$Lon, .data$Wind:.data$NW64
)

atlantic$DateTime <- as.POSIXct(
  strptime(atlantic$DateTime, format = "%Y-%m-%d %H:%M:%S")
)
atlantic$Name[atlantic$Name == "UNNAMED"] <- atlantic$Key[atlantic$Name == "UNNAMED"]
atlantic$Year<-year(atlantic$DateTime)

atlantic$Name[atlantic$Name == "AL011900"] = "GALVESTON"

atlantic$Category[atlantic$Status == "TD"|atlantic$Status == "SD"] = 0.5
atlantic$Category[atlantic$Status == "TS"|atlantic$Status == "SS"] = 0.75
atlantic$Category[atlantic$Status == "HU" & atlantic$Wind>=64 & atlantic$Wind<=82] = 1
atlantic$Category[atlantic$Status == "HU" & atlantic$Wind>=83 & atlantic$Wind<=95] = 2
atlantic$Category[atlantic$Status == "HU" & atlantic$Wind>=96 & atlantic$Wind<=112] = 3
atlantic$Category[atlantic$Status == "HU" & atlantic$Wind>=113 & atlantic$Wind<=136] = 4
atlantic$Category[atlantic$Status == "HU" & atlantic$Wind>=137] = 5

atlantic$Name <- paste(atlantic$Name, "-", atlantic$Year)
atlantic$Date =  format(atlantic$DateTime,format='%Y-%m-%d')
atlantic<-atlantic %>% replace_with_na(replace = list(Pressure = -999))

x <- purrr::map("pacific.txt", readr::read_lines)

x <- purrr::flatten_chr(x)

pacific <- as.data.frame(x, stringsAsFactors = FALSE)
header_rows <- grep(pattern = "^[[:alpha:]]{2}[[:digit:]]{6}.+", x)

# Split header_rows into variables
pacific <- tidyr::extract(
  data = pacific,
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
pacific <- tidyr::fill(data = pacific, .data$Key, .data$Name, .data$Lines)

# Remove original header rows
pacific <- pacific[-header_rows, ]

# Split storm details into variables
pacific <- tidyr::extract(
  data = pacific,
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

pacific <- dplyr::mutate(
  .data = pacific,
  Lat = dplyr::if_else(
    .data$LatHemi == "N", .data$Lat * 1, .data$Lat * -1
  ),
  Lon = dplyr::if_else(
    .data$LonHemi == "E", .data$Lon * 1, .data$Lon * -1
  )
)

pacific$DateTime <- paste(
  paste(pacific$Year, pacific$Month, pacific$Date, sep = "-"),
  paste(pacific$Hour, pacific$Minute, "00", sep = ":"),
  sep = " "
)

pacific <- dplyr::select(
  .data = pacific,
  .data$Key, .data$Name, .data$DateTime, .data$Record:.data$Lat,
  .data$Lon, .data$Wind:.data$NW64
)

pacific$DateTime <- as.POSIXct(
  strptime(pacific$DateTime, format = "%Y-%m-%d %H:%M:%S")
)
pacific$Name[pacific$Name == "UNNAMED"] <- pacific$Key[pacific$Name == "UNNAMED"]
pacific$Year<-year(pacific$DateTime)
pacific<-pacific %>% replace_with_na(replace = list(Pressure = -999))

pacific$Category[pacific$Status == "TD"|pacific$Status == "SD"] = 0.5
pacific$Category[pacific$Status == "TS"|pacific$Status == "SS"] = 0.75
pacific$Category[pacific$Status == "HU" & pacific$Wind>=64 & pacific$Wind<=82] = 1
pacific$Category[pacific$Status == "HU" & pacific$Wind>=83 & pacific$Wind<=95] = 2
pacific$Category[pacific$Status == "HU" & pacific$Wind>=96 & pacific$Wind<=112] = 3
pacific$Category[pacific$Status == "HU" & pacific$Wind>=113 & pacific$Wind<=136] = 4
pacific$Category[pacific$Status == "HU" & pacific$Wind>=137] = 5
pacific$Name <- paste(pacific$Name, "-", pacific$Year)
pacific$Date =  format(pacific$DateTime,format='%Y-%m-%d')
pacific$Lon[pacific$Lon<0] = pacific$Lon[pacific$Lon<0] + 360



df1 = atlantic[c("Key","Wind")]
df2 = atlantic[c("Key","Pressure")]
nameKey = atlantic[c("Key","Name")]
df1 = ddply(df1, "Key", numcolwise(max))
df2 = ddply(df2, "Key", numcolwise(min))
nameKey = ddply(nameKey, "Key", function(nameKey) unique(nameKey))
hurricanes = merge(df1,df2, by.x = "Key", by.y = "Key")
df1 = atlantic[c("Key","DateTime")]
df1 = data.table(df1)
df1 = df1[,list(DateTime = min(DateTime)), by = Key]
df1 = data.frame(df1)
hurricanes = merge(hurricanes,df1, by.x = "Key", by.y = "Key")
df2 = atlantic[c("Key","Category")]
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
Names<-as.array(unique(atlantic$Name))
yr<-1851:2018
Wtoph<-hurricanes[order(hurricanes$Wind, decreasing = TRUE),]
Wtoph<-head(Wtoph,10)
Ptoph<-hurricanes[order(hurricanes$Pressure, decreasing = FALSE),]
Ptoph<-head(Ptoph,10)



dfP1 = pacific[c("Key","Wind")]
dfP2 = pacific[c("Key","Pressure")]
nameKeyP = pacific[c("Key","Name")]
dfP1 = ddply(dfP1, "Key", numcolwise(max))
dfP2 = ddply(dfP2, "Key", numcolwise(min))
nameKeyP = ddply(nameKeyP, "Key", function(nameKeyP) unique(nameKeyP))
hurricanesP = merge(dfP1,dfP2, by.x = "Key", by.y = "Key")
dfP1 = pacific[c("Key","DateTime")]
dfP1 = data.table(dfP1)
dfP1 = dfP1[,list(DateTime = min(DateTime)), by = Key]
dfP1 = data.frame(dfP1)
hurricanesP = merge(hurricanesP,dfP1, by.x = "Key", by.y = "Key")

NamesP<-as.array(unique(pacific$Name))
yrP<-1949:2018
WtophP<-hurricanesP[order(hurricanesP$Wind, decreasing = TRUE),]
WtophP<-head(WtophP,10)
PtophP<-hurricanesP[order(hurricanesP$Pressure, decreasing = FALSE),]
PtophP<-head(PtophP,10)


basemap = c("Stamen.Toner", "Default", "Esri.NatGeoWorldMap" ,"Esri.WorldTopoMap" )
colour = c("lightgreen","darkgreen","yellow","orange","darkorange","red","darkred","darkgrey")
status = c("TD/SD","TS/SS","Cat 1","Cat 2","Cat 3","Cat 4","Cat 5","Other")

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 2"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     
                     checkboxInput("fivehur", "Five Interesting Hurricanes", value = FALSE),
                     conditionalPanel(
                       condition = "input.fivehur == false",
                       
                       #Atlantic
                       selectInput("flt","Atlantic Storm Filters:",c("None","Date" ,"Year","Name","Top10(Wind)","Top10(Min Pressure)"),selected = "None"),
                       conditionalPanel(
                         condition = "input.flt == 'Year'",
                         selectInput("ipyr", "Selecft the Year to visualize", yr, selected = "2005"),   
                       ),
                       conditionalPanel(
                         condition = "input.flt == 'Date'",
                         dateInput("date","Date:",value = "1851-06-25"),  
                       ),
                       
                       conditionalPanel(
                         condition = "input.flt == 'Name'",
                         selectInput("ipnm", "Select the Name to visualize", Names,selected= "OSCAR - 2018"),   
                       ),
                       
                       #PACIFIC
                       
                       selectInput("flt2","Pacific Storm Filters:",c("None","Date" ,"Year","Name","Top10(Wind)","Top10(Min Pressure)"),selected = "None"),
                       conditionalPanel(
                         condition = "input.flt2 == 'Year'",
                         selectInput("ipyrp", "Selecft the Year to visualize", yrP, selected = "2005"),   
                       ),
                       conditionalPanel(
                         condition = "input.flt2 == 'Date'",
                         dateInput("datep","Date:",value = "1851-06-25"),  
                       ),
                       
                       conditionalPanel(
                         condition = "input.flt2 == 'Name'",
                         selectInput("ipnmp", "Select the Name to visualize", NamesP,selected= "WILLA - 2018"),   
                       ),
                       
                       
                       selectInput("Basemap", "Select a Basemap Style", basemap, selected = "Default"),
                       checkboxInput("land", "Only hurricanes which made Landfall", value = FALSE),
                     ),
                     conditionalPanel(
                       condition = "input.fivehur == true",
                       selectInput("list","Select a Hurricane", c("GALVESTON - 1900", "KATRINA - 2005", "IVAN - 2004", "JOHN - 1994", "PATRICIA - 2015"), selected = "GALVESTON - 1900")
                     ),
                     
                     
                     
                     menuItem(""))
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel( "Map",
                fluidRow(
                  column(6,
                         fluidRow(
                           box(title = "Atlantic Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               leafletOutput("leaf", height = 500)
                           ),
                         ),
                  ),
                  column(6,
                         fluidRow(
                           box(title = "Pacific Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               leafletOutput("leaf2", height = 500)
                           ),
                         ),
                  ),
                  
                )),
      tabPanel("Graph",
               fluidRow(
                 column(6,
                        fluidRow(
                          box( title = "Hurricanes Per Year", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("bar2",height = 300)
                          )
                        ),
                 ),
                 column(6,
                        fluidRow(
                          box( title = "Max Wind Speed", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("bar3",height = 300)
                          )
                        ),
                 ),),
               fluidRow(
                 column(6,
                        fluidRow(
                          box( title = "Min Pressure", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("bar4",height = 350)
                          )
                        ),
                 ),
                 
                 column(6,
                        fluidRow(
                          box( title = "Hurricanes Per Year", solidHeader = TRUE, status = "primary", width = 12,
                               plotOutput("bar1",height = 350)
                          )
                          
                        ), ),
               ),
      ),
      tabPanel("Table",
               fluidRow(
                 column(6,
                        fluidRow(
                          box( title = "List of Atlantic Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               dataTableOutput("tab1")
                          )
                        )
                 ),
                 column(6,
                        fluidRow(
                          box( title = "List of Pacific Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               dataTableOutput("tab2")
                          )
                        )
                 )
               )),
      tabPanel( "HeatMap", h4("Heatmap of landfalls to show which places are most vulnerable."),
                fluidRow(
                  column(6,
                         fluidRow(
                           box(title = "Atlantic Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               leafletOutput("leaf3", height = 500)
                           ),
                         ),
                  ),
                  column(6,
                         fluidRow(
                           box(title = "Pacific Hurricanes", solidHeader = TRUE, status = "primary", width = 12,
                               leafletOutput("leaf4", height = 500)
                           ),
                         ),
                  ),
                  
                )),
      tabPanel("About",  h4("Author: Ansul Goenka, Parikshit Solunke"), h4("Libraries used: shiny, shinydashboard, 
              ggplot2, lubridate, grid, leaflet, DT, stringi, dplyr"), h4("The Atlantic hurricane database (HURDAT2) 
              1851-2018 and the Northeast and North Central Pacific hurricane database (HURDAT2) 1949-2018: 
              http://www.nhc.noaa.gov/data/#hurdat"))
    ))
)



server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  
  
  
  
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
  
  output$bar2 <- renderPlot({
    data = df1
    data$Year = year(data$DateTime)
    data$DateTime = NULL
    data = as.data.frame(table(data$Year))
    colnames(data) <- c("Year", "Total")
    
    pac = pacific[c("Key","Year")] 
    pac = data.table(pac)
    pac = pac[,list(Year = min(Year)), by = Key]
    pac = data.frame(pac)
    pac = as.data.frame(table(pac$Year))
    colnames(pac) <- c("Year", "Count")
    tot = merge(data, pac, all = TRUE)
    tot[is.na(tot)] <- 0
    pac = tot
    pac$Total = NULL
    tot$Total = tot$Total + tot$Count
    tot$Count = NULL
    tot$Type = "Total"
    colnames(pac) <- c("Year", "Total")
    
    data$Type = "Atlantic"
    pac$Type = "Pacific"
    data = rbind(data,pac)
    data = rbind(data,tot)
    
    ggplot(data) + geom_bar(aes(x=Year,y=Total, fill = Type) ,stat="identity",position = position_dodge2()) + 
      labs(x="Day", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_x_discrete(breaks = seq(1851,2018, by = 10))
    
  })
  
  output$bar3 <- renderPlot({
    data = atlantic[c("Year","Wind")]
    data = ddply(data, "Year", numcolwise(max))
    
    pac = pacific[c("Year","Wind")] 
    pac = ddply(pac, "Year", numcolwise(max))
    
    data = merge(data, pac, by.x = "Year", by.y="Year", all = TRUE)
    colnames(data) <- c("Year","Atlantic","Pacific")
    
    ggplot(data, aes(x=Year,group = 1),) + geom_line(aes(y=Atlantic, color = 'Atlantic')) + 
      geom_line(aes(y=Pacific, color = 'Pacific'), na.rm = TRUE)
    
  })
  
  output$bar4 <- renderPlot({
    data = atlantic[c("Year","Pressure")]
    data = ddply(data, "Year", numcolwise(min))
    
    pac = pacific[c("Year","Pressure")] 
    pac = ddply(pac, "Year", numcolwise(min))
    
    data = merge(data, pac, by.x = "Year", by.y="Year", all = TRUE)
    colnames(data) <- c("Year","Atlantic","Pacific")
    data$Atlantic[data$Atlantic == -999 ] <- NA
    data$Pacific[data$Pacific == -999] <- NA
    data = data[!is.na(data$Atlantic) | !is.na(data$Pacific),]
    
    ggplot(data, aes(x=Year,group = 1),) + geom_line(aes(y=Atlantic, color = 'Atlantic'), na.rm = TRUE) + 
      geom_line(aes(y=Pacific, color = 'Pacific'), na.rm = TRUE)
    
  })
  
  
  
  
  #Atlantic
  output$leaf <- renderLeaflet({
    df<-atlantic
    df$colour<-ifelse(df$Category == 0.5, "lightgreen",(ifelse(df$Category == 0.75, "darkgreen",(ifelse(df$Category ==1, "yellow",
                                                                                                        (ifelse(df$Category ==2, "orange",(ifelse(df$Category ==3, "darkorange",(ifelse(df$Category ==4, "red",
                                                                                                                                                                                        (ifelse(df$Category ==5, "darkred","darkgrey")))))))))))))
    df$colour[is.na(df$colour)] <- "darkgrey"
    landfalls<-df[(df$Record=="L"),]
    
    if(input$fivehur){
      df3<-df[(df$Name==input$list),]
    }
    else if(stri_cmp(input$flt, "None")!=0)
    {
      if(input$flt=="Year")
      {
        df3<-df[(df$Year==input$ipyr),]
      }
      if(input$flt=="Name")
      {
        df3<-df[(df$Name==input$ipnm),]
      }
      if(input$flt=="Top10(Wind)")
      {
        df3<-df[(df$Key==Wtoph$Key),]
      }
      if(input$flt=="Top10(Min Pressure)")
      {
        df3<-df[(df$Key==Ptoph$Key),]
      }
      if(input$flt=="Date")
      {
        dfPoints<-df[(df$Date==input$date),]
        Names2<-as.array(unique(dfPoints$Name))
        dfPoints$Time<-format(dfPoints$DateTime, format="%H:%M:%S")
        dfPoints<-dfPoints[(dfPoints$Time=="00:00:00"),]
        
        df3<-df[(df$Year>2004),]
        
      }
      
      
      
    }
    else{
      df3<-df[(df$Year>2004),]
    }
    if(input$land)
    {
      df3<-df3 %>%
        filter(Name %in% landfalls$Name)
    }
    if(input$flt!="Date")
    {
      Names2<-as.array(unique(df3$Name))
    }
    
    map <- leaflet()
    if(input$Basemap == "Default")
      map <- addTiles(map)
    else
      map <- addProviderTiles(map, provider = input$Basemap)
    for ( i in Names2) 
    {
      df4<-df3[(df3$Name==i),]
      
      if(input$flt!="Date")
      {
        map<-addCircles(map, lat=df4$Lat,lng=df4$Lon, weight =4.5,color=df4$colour,popup = df4$Name)
      }
      map<-addPolylines(map, lat=df4$Lat,lng=df4$Lon, weight =1,color="White",opacity = 0.60, 
                        highlightOptions = highlightOptions(color = "white",bringToFront = T),popup = df4$Name)
    }
    if(input$flt=="Date")
    {
      if(nrow(dfPoints)>0)
      {
        map<-addCircles(map, lat=dfPoints$Lat,lng=dfPoints$Lon, weight =12,color=dfPoints$colour,popup = dfPoints$Name)
      }
    }
    map<-addLegend(map, position = "bottomright", colors = colour, labels = status)
    map
  })
  
  #Pacific
  output$leaf2 <- renderLeaflet({
    df<-pacific
    df$colour<-ifelse(df$Category == 0.5, "lightgreen",(ifelse(df$Category == 0.75, "darkgreen",(ifelse(df$Category ==1, "yellow",
                                                                                                        (ifelse(df$Category ==2, "orange",(ifelse(df$Category ==3, "darkorange",(ifelse(df$Category ==4, "red",
                                                                                                                                                                                        (ifelse(df$Category ==5, "darkred","darkgrey")))))))))))))
    df$colour[is.na(df$colour)] <- "darkgrey"
    landfalls<-df[(df$Record=="L"),]
    
    if(input$fivehur){
      df3<-df[(df$Name==input$list),]
    }
    else if(stri_cmp(input$flt2, "None")!=0)
    {
      if(input$flt2=="Year")
      {
        df3<-df[(df$Year==input$ipyrp),]
      }
      if(input$flt2=="Name")
      {
        df3<-df[(df$Name==input$ipnmp),]
      }
      if(input$flt2=="Top10(Wind)")
      {
        df3<-df[(df$Key==WtophP$Key),]
        
      }
      if(input$flt2=="Top10(Min Pressure)")
      {
        df3<-df[(df$Key==PtophP$Key),]
        
      }
      if(input$flt2=="Date")
      {
        dfPoints<-df[(df$Date==input$datep),]
        Names2<-as.array(unique(dfPoints$Name))
        dfPoints$Time<-format(dfPoints$DateTime, format="%H:%M:%S")
        dfPoints<-dfPoints[(dfPoints$Time=="00:00:00"),]
        
        df3<-df[(df$Year>2004),]
        
      }
      
    }
    else{
      df3<-df[(df$Year>2004),]
    }
    if(input$land)
    {
      df3<-df3 %>%
        filter(Name %in% landfalls$Name)
    }
    
    if(input$flt2!="Date")
    {
      Names2<-as.array(unique(df3$Name))
    }
    
    map <- leaflet()
    if(input$Basemap == "Default")
      map <- addTiles(map)
    else
      map <- addProviderTiles(map, provider = input$Basemap)
    for ( i in Names2) 
    {
      df4<-df3[(df3$Name==i),]
      
      if(input$flt2!="Date")
      {
        map<-addCircles(map, lat=df4$Lat,lng=df4$Lon, weight =4.5,color=df4$colour, popup = df4$Name)
      }
      map<-addPolylines(map, lat=df4$Lat,lng=df4$Lon, weight =1,color="White",opacity = 0.60, 
                        highlightOptions = highlightOptions(color = "white",bringToFront = T), popup = df4$Name)
    }
    if(input$flt2=="Date")
    {
      if(nrow(dfPoints)>0)
      {
        map<-addCircles(map, lat=dfPoints$Lat,lng=dfPoints$Lon, weight =12,color=dfPoints$colour,popup = dfPoints$Name)
      }
    }
    map<-addLegend(map, position = "bottomright", colors = colour, labels = status)
    map
  })
  
  
  output$leaf3 <- renderLeaflet({
    landfalls<-atlantic[(atlantic$Record=="L"),c(6,7)]
    
    
    map <- leaflet(landfalls) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addHeatmap(lng = ~Lon, lat = ~Lat,blur=8, radius =6)
    map
  })
  
  
  output$leaf4 <- renderLeaflet({
    landfalls<-pacific[(pacific$Record=="L"),c(6,7)]
    
    
    map <- leaflet(landfalls) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addHeatmap(lng = ~Lon, lat = ~Lat,blur=8, radius =6)
    map
  })
  
  
  output$tab1 <- DT::renderDataTable(
    DT::datatable({  
      hurricanes = merge(nameKey, hurricanes, by.x="Key", by.y="Key")
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$tab2 <- DT::renderDataTable(
    DT::datatable({  
      hurricanesP = merge(nameKeyP, hurricanesP, by.x="Key", by.y="Key")
    }, 
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
}

shinyApp(ui = ui, server = server)