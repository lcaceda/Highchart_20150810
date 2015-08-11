require(rCharts)
require(rHighcharts)
library(shiny)
require(graphics)
library(rmarkdown)
library(ggmap)
library(quantmod)

# create series per county to tack to the legend: http://iswwwup.com/t/8fa30a16a5d7/shiny-using-highcharts-to-compre-two-datasets-of-different-length.html

#http://ramnathv.github.io/user2014-rcharts/#35
#http://stackoverflow.com/questions/31457930/rcharts-nvd3-2-d-zoom-possible
#https://gallery.shinyapps.io/105-plot-interaction-zoom/
#http://shiny.rstudio.com/gallery/plot-plus-three-columns.html

## ui.R

ui<- shinyUI(pageWithSidebar(
  headerPanel("Project Evaluation Tool"),
  
  sidebarPanel(
    tabsetPanel(
      tabPanel("Select Inputs",
               selectInput(inputId = "x",
                           label = "Choose X",
                           choices = c('Future.Congestion', 'Future.Accessibility', 'Future.Freight', 'Future.Deliverability',
                                       'Future.Air.Quality', 'Future.Volume', 'Future.Score', 'Current.Congestion',
                                       'Current.Safety', 'Current.Freight', 'Current.Buffer.Index', 'Current.Equitable.Target',
                                       'Current.Air.Quality', 'Current.Accessibility', 'Current.Score', 'BC.2015', 'BC.2040',
                                       'Annual.Benefit.2040', 'Annual.Cost'),selected = "Future.Score"),
               selectInput(inputId = "y",
                           label = "Choose Y",
                           choices = c('Future.Congestion', 'Future.Accessibility', 'Future.Freight', 'Future.Deliverability',
                                       'Future.Air.Quality', 'Future.Volume', 'Future.Score', 'Current.Congestion',
                                       'Current.Safety', 'Current.Freight', 'Current.Buffer.Index', 'Current.Equitable.Target',
                                       'Current.Air.Quality', 'Current.Accessibility', 'Current.Score', 'BC.2015', 'BC.2040',
                                       'Annual.Benefit.2040', 'Annual.Cost'),selected = "Current.Score"),
               selectInput(inputId  = "pointsize",
                           label = "Point Size : ",
                           choices = c('Future.Congestion', 'Future.Accessibility', 'Future.Freight', 'Future.Deliverability',
                                       'Future.Air.Quality', 'Future.Volume', 'Future.Score', 'Current.Congestion',
                                       'Current.Safety', 'Current.Freight', 'Current.Buffer.Index', 'Current.Equitable.Target',
                                       'Current.Air.Quality', 'Current.Accessibility', 'Current.Score', 'BC.2015', 'BC.2040',
                                       'Annual.Benefit.2040', 'Annual.Cost'),selected = "Annual.Benefit.2040")
#                selectInput(inputId="facet",
#                            label="Facet by Variable",
#                            choices=names('County'))
               
      ),
      tabPanel("About",
               h4("1. Purpose statement about Project Evaluation"),
               h4("2. Explanation of the values"),  
               h5("ID = ARCID"),
               h5("County = Jurisdictions"),
               h5("Annual Cost = ...."),
               h5("...."),
               h4("Timeline"),
               p(),
               h5("Created by Atlanta Regional Commission 2015")
      ))),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", 
               showOutput("myChart","highcharts"),
               showOutput("myChart2","highcharts")),
      
      tabPanel("Table", dataTableOutput('mytable'))
    ))#mainpanel
) #pagewithsidebar
) #shiny

server <- shinyServer(function(input, output,session) {
  Viz<-read.csv("data/Viz.csv", header = TRUE)
  
  output$myChart <- renderChart({
    p1<- highchartPlot(input$x, input$y, data = Viz , type = 'bubble', size = input$pointsize, groups = 'ID')
    p1$tooltip(useHTML= T, formatter = "#! function(d){return 'ARCID: ' + this.series.name + '<br>'+
                                                                'Size: ' + this.point.z + '<br>' +
                                                              'X: ' + this.point.x +', '+
                                                              'Y: ' + this.point.y ;}!#")
    p1$plotOptions(allowPointSelect = T)
    p1$addParams(dom = 'myChart')
    p1$params$width <- 800
    p1$params$height <- 700
    p1$legend(enabled = T, layout = verticalLayout(), floating = FALSE, align = 'right', itemHoverStyle = TRUE)

#   output$myChart2 <- renderChart({
    p1$chart(zoomType ='xy',  backgroundColor = NULL)
    p1$exporting(enabled = T)
    p1$colors("#eee347", "#d3f5de","#d3f5de","#d3f5de","#a30e5e","#a30e5e","#a30e5e","#2c7b72","#2c7b72","#2c7b72","#2c7b72","#2c7b72","#2c7b72",
              "#2c7b72","#2c7b72","#2c7b72","#9da4c9","#9da4c9",  
              "#666fb7","#d61f72", "#d61f72", "#d61f72","#d61f72", "#b066ed","#89e0ee", "#89e0ee",
              "#89e0ee","#89e0ee","#89e0ee","#89e0ee","#89e0ee","#89e0ee","#89e0ee","#89e0ee","#89e0ee","#89e0ee", "#5878ba","#5878ba","#5878ba","#5878ba","#5878ba","#5878ba","#ece378", "#ece378","#fa0312", "#fa0312", "#fa0312",
              "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312", "#fa0312",
              "#1a91a0","#1a91a0","#1a91a0","#1a91a0", "#f3c1d3", "#ed1a52", "#ed1a52", "#ed1a52", "#ed1a52", "#f1ed3a",
              "#f1ed3a", "#f1ed3a", "#f1ed3a", "#f1ed3a", "#f1ed3a", "#f1ed3a", "#f1ed3a", "#f1ed3a", "#f1ed3a","#f1ed3a","#46b18d",
              "#46b18d", "#1edfec", "#1edfec", "#1edfec", "#1edfec", "#1edfec", "#1edfec", "#1edfec", "#1edfec", "#1edfec", "#1edfec",
              "#1edfec", "#1edfec", "#978ca8","#41f1b6","#41f1b6","#41f1b6","#41f1b6","#41f1b6","#41f1b6","#41f1b6","#41f1b6","#41f1b6",
              "#41f1b6","#41f1b6","#41f1b6","#41f1b6","#eac627", "#eac627", "#eac627","#eac627", "#eac627","#eac627","#eac627",
              "#eac627","#0862b5", "#0862b5", "#0862b5", "#f845bc","#f845bc","#f845bc","#f845bc","#f845bc","#f845bc","#f845bc", "#2fc498",
              "#2fc498","#2fc498","#2fc498","#2fc498","#2fc498","#2fc498","#2fc498","#2fc498","#2fc498","#eac627", "#089fa0", "#089fa0",
              "#089fa0","#089fa0","#089fa0","#089fa0","#089fa0", "#089fa0","#9da4c9","#9da4c9","#9da4c9","#9da4c9","#9da4c9","#9da4c9", 
              "#fbcf09", "#fbcf09","#9da4c9","#9da4c9","#9da4c9","#9da4c9","#9da4c9","#9da4c9","#9da4c9", "#954275", "#954275","#954275",
              "#f42b2a","#f42b2a")

  output$mytable = renderDataTable({
    Viz
  })
  return(p1)
})
})

shinyApp(ui=ui, server=server)
