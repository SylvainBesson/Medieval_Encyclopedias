library(shiny)
library(plotly)



ui <- fluidPage(
  
  fluidRow(
    column(10,
           plotlyOutput("plot")
    ),
    column(12,
           sidebarPanel(
             selectInput("satuts", "Statuts:",
                         choices=allBooksDated$Statut, multiple = T),
             hr()
           )
    ),
    column(12,    
           textInput("titre", "Titre: ", "Tradition manuscrite choix des statuts")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    output$title <- input$title
    
    allBooksDatedSelect<- allBooksDated[which(allBooksDated$Statut%in%input$satuts),]
    
    p<-plotly::plot_ly(alpha = 0.6) %>%
      layout(title = input$titre, xaxis=list(title="Siècle"), yaxis=list(title="Nombre de copies"))
    
    for(i in unique(allBooksDatedSelect$bookName)){
      p<- plotly::add_histogram(p,x = as.character(as.numeric(gsub("([0-9][0-9]|[0-9][0-9][.][0-9])$", "", allBooksDatedSelect$meanDate[allBooksDatedSelect$bookName==i]))+1), name = i) 
    }
    p
  })
}


shinyApp(ui, server)