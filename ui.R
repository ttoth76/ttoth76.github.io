#
# This is the user-interface definition of a Shiny web application. 

library(shiny)

library(ggplot2)
library(gridExtra)
library(ggthemes)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Read", tableOutput("contents")),
                  tabPanel("Plots", 
                           radioButtons("dist", "Distribution type:",
                                        c("Histogram" = "hist",
                                          "Boxplot" = "boxp",
                                          "Scatterplot" = "scat"), selected = "hist"),
                           checkboxGroupInput("checkbox", "ABV and/or IBU:",
                                              c("ABV" = "ABV",
                                                "IBU" = "IBU"), selected = "ABV"),
                           checkboxInput("regline", "Regression Line"), 
                           selectInput("state","Filter By State",choices = list(
                             "Select a state" = 'select',
                             "CO" =	"CO", "CA"=	"CA", "MI"=	"MI", "OR"=	"OR", "TX"=	"TX","PA"=	"PA",
                             "MA"=	"MA","WA"=	"WA","IN"=	"IN","WI"=	"WI","NC"=	"NC","IL"=	"IL",
                             "NY"=	"NY","VA"=	"VA","FL"=	"FL","OH"=	"OH","MN"=	"MN","AZ"=	"AZ",
                             "VT"=	"VT","ME"=	"ME","MO"=	"MO","MT"=	"MT","CT"=	"CT", "AK"=	"AK",
                             "GA"=	"GA","MD"=	"MD","OK"=	"OK","IA"=	"IA","ID"=	"ID","LA"=	"LA",
                             "NE"=	"NE","RI"=	"RI","HI"=	"HI","KY"=	"KY","NM"=	"NM","SC"=	"SC",
                             "UT"=	"UT","WY"=	"WY","AL"=	"AL","KS"=	"KS","NH"=	"NH","NJ"=	"NJ",
                             "TN"=	"TN","AR"=	"AR","DE"=	"DE","MS"=	"MS","NV"=	"NV","DC"=	"DC",
                             "ND"=	"ND","SD"=	"SD","WV"=	"WV")),
                           div("If no state is selected, than it will plot the full dataset", style = "color:blue"),
                           br(),
                           plotOutput("gmplot")),
                  tabPanel("Additional Plot", 
                           br(),
                           p("Based on the 3rd party data set the highest amount of beer consumption per capita by states 
                             can be observed in North Dakota, Montana and New Hampshire. Budweiser has a very low number 
                             of breweries in these states therefore these states potentially provide an opportunity to grow 
                             business and address industry production challenges."),
                           plotOutput("usmap_plot"))
      ))
  )
)

