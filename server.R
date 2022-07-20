#
# This is the server logic of a Shiny web application.

library(shiny)

server <- function(input, output) {
  
  # Data Read 
  datasetInput = reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = input$header)
  }) 
  
  output$contents <- renderTable({datasetInput()})
  
  output$gmplot = renderPlot({
    
    
    if (is.null(input$checkbox) && input$dist != "scat"){return(0)}
    
    if(input$dist == "hist" && input$state != "select") {
      h1 = ggplot(datasetInput()[datasetInput()$State==input$state,], 
                  aes(x=datasetInput()[datasetInput()$State==input$state,]$ABV)) + 
        geom_histogram(color="black", fill="white")+ ggtitle("ABV distribution") + xlab("ABV") + ylab("Frequency") 
      h2 = ggplot(datasetInput()[datasetInput()$State==input$state,], 
                  aes(x=datasetInput()[datasetInput()$State==input$state,]$IBU)) + geom_histogram(color="black", fill="white")+ ggtitle("IBU distribution") + xlab("IBU") + ylab("Frequency") 
      
      
      if (length(input$checkbox) ==1){
        if (input$checkbox == "ABV"){
          grid.arrange(h1, ncol=2)
        } else if (input$checkbox == "IBU"){
          grid.arrange(h2, ncol=2)
        }
      } else{
        grid.arrange(h1, h2, ncol=2)
      } 
      
    } else if(input$dist == "hist" && input$state == "select") {
      h1 = ggplot(datasetInput(), 
                  aes(x=datasetInput()$ABV)) + 
        geom_histogram(color="black", fill="white") + ggtitle("ABV distribution") + xlab("ABV") + ylab("Frequency") 
      h2 = ggplot(datasetInput(), 
                  aes(x=datasetInput()$IBU)) + geom_histogram(color="black", fill="white")+ ggtitle("IBU distribution") + xlab("IBU") + ylab("Frequency") 
      
      
      if (length(input$checkbox) ==1){
        if (input$checkbox == "ABV"){
          grid.arrange(h1, ncol=2)
        } else if (input$checkbox == "IBU"){
          grid.arrange(h2, ncol=2)
        }
      } else{
        grid.arrange(h1, h2, ncol=2)
      } 
      
    }
    
    if(input$dist == "boxp" && input$state != "select") {
      b1 = ggplot(data = datasetInput()[datasetInput()$State==input$state,], aes(x = "", y = ABV)) + 
        geom_boxplot() + ggtitle("ABV distribution") + xlab("X") + ylab("ABV") 
      b2 = ggplot(data = datasetInput()[datasetInput()$State==input$state,], aes(x = "", y = IBU)) + 
        geom_boxplot()+ ggtitle("IBU distribution") + xlab("X") + ylab("IBU") 
      
      if (length(input$checkbox) ==1){
        if (input$checkbox == "ABV"){
          grid.arrange(b1, ncol=2)
        } else if (input$checkbox == "IBU"){
          grid.arrange(b2, ncol=2)
        }
      } else{
        grid.arrange(b1, b2, ncol=2)
      } 
    } else if(input$dist == "boxp" && input$state == "select"){
      b1 = ggplot(data = datasetInput(), aes(x = "", y = ABV)) + 
        geom_boxplot() + ggtitle("ABV distribution") + xlab("X") + ylab("ABV") 
      b2 = ggplot(data = datasetInput(), aes(x = "", y = IBU)) + ggtitle("IBU distribution") + xlab("X") + ylab("IBU")+ geom_boxplot()
      
      if (length(input$checkbox) ==1){
        if (input$checkbox == "ABV"){
          grid.arrange(b1, ncol=2)
        } else if (input$checkbox == "IBU"){
          grid.arrange(b2, ncol=2)
        }
      } else{
        grid.arrange(b1, b2, ncol=2)
      } 
    }
    
    
    
    if(input$regline == TRUE && input$dist == "scat" && input$state != "select") {
      ggplot(data = datasetInput()[datasetInput()$State==input$state,], aes(x = ABV, y = IBU)) + geom_point(color = "#C8102E", shape=1) + 
        geom_smooth(method = "lm", color = "#00A1E1") + ggtitle("Correlation between IBU and ABV") + 
        xlab("ABV") + ylab("IBU") +
        theme(plot.title = element_text(hjust = 0.5))} 
    else if(input$regline == FALSE && input$dist == "scat" && input$state != "select") {
      ggplot(data = datasetInput()[datasetInput()$State==input$state,], aes(x = ABV, y = IBU)) + geom_point(color = "#C8102E", shape=1) + 
        xlab("ABV") + ylab("IBU") + ggtitle("Correlation between IBU and ABV") +
        theme(plot.title = element_text(hjust = 0.5))}
    else if(input$regline == FALSE && input$dist == "scat" && input$state == "select"){
      ggplot(data = datasetInput(), aes(x = ABV, y = IBU)) + geom_point(color = "#C8102E", shape=1) + 
        xlab("ABV") + ylab("IBU") + ggtitle("Correlation between IBU and ABV") +
        theme(plot.title = element_text(hjust = 0.5))
    }
    else if(input$regline == TRUE && input$dist == "scat" && input$state == "select"){
      ggplot(data = datasetInput(), aes(x = ABV, y = IBU)) + geom_point(color = "#C8102E", shape=1) + 
        geom_smooth(method = "lm", color = "#00A1E1") + ggtitle("Correlation between IBU and ABV") +
        xlab("ABV") + ylab("IBU") + ggtitle("Correlation between IBU and ABV") +
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    
  })
  
  output$usmap_plot = renderPlot({
    # Additional plot
    library(maps)
    library(mapproj)
    library(sf)
    library(usmap)
    library(urbnmapr)
    beers_consumption = data.frame(full=c("North Dakota","New Hampshire","Montana","South Dakota",    "Wisconsin","Nevada", "Vermont"             , "Nebraska" ,          
                                          "Texas","Maine"               , "Louisiana"           , "Mississippi",         
                                          "Iowa","Delaware"             ,"Wyoming"             , "South Carolina",      
                                          "New Mexico","Missouri"        ,     "Hawaii"              , "Oregon",              
                                          "West Virginia","Alabama"       ,       "Ohio"                 ,"Colorado",            
                                          "Arizona","Illinois"            , "Alaska"               ,"Pennsylvania",        
                                          "Minnesota","Oklahoma"          ,   "Kansas"               ,"District of Columbia",
                                          "Idaho","Florida"             , "North Carolina"       ,"Michigan",            
                                          "Arkansas","Virginia"         ,    "Rhode Island"         ,"Massachusetts",       
                                          "Tennessee","Indiana"         ,     "Georgia"              ,"California",          
                                          "Washington","Kentucky"       ,      "Maryland"             ,"New York",            
                                          "New Jersey","Connecticut"    ,      "Utah" ), 
                                   consumption=c(45.8, 43.9, 41.0, 38.9, 36.2, 35.8, 35.3, 35.2, 34.4, 34.0, 33.9, 33.9, 33.6, 33.6, 33.0, 32.7, 32.4, 31.0, 30.4,
                                                 30.3, 30.3, 30.2, 30.1, 30.0, 29.5, 29.1, 28.7, 28.6, 28.5, 28.3, 28.3, 28.3, 27.8, 27.4, 27.1, 26.8, 26.7, 26.7,
                                                 26.3, 26.2, 26.2, 25.9, 25.7, 25.5, 24.8, 24.4, 24.2, 22.4, 22.4, 22.1, 20.2))  
    us_map_fips = fips_info()
    consumption = merge(us_map_fips, beers_consumption, 'full')
    plot_usmap(data = consumption, values = "consumption", regions = "states", 
               labels = TRUE, label_color = "black") + 
      labs(title = "Beer Consumption by State per Capita") + 
      scale_fill_continuous(low = "white", high ="#C8102E", 
                            name = "gallons",label = scales::comma) + 
      theme(legend.position = "right") +
      theme(panel.background=element_blank()) 
  })
  
  #output$gmplot <- renderTable({datasetInput()[datasetInput()$State==input$state,]})
  
}
#shinyApp(ui, server)
