library(shiny)
library(plotly)



ui <- fluidPage(
  
  fluidRow(
    column(10,
           plotlyOutput("plot")
    ),
    column(10,
           sidebarPanel(
             selectInput("bookName", "Nom de l'oeuvre:",
                         choices=allBooksDated$bookName, multiple = T),
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
    
    allBooksDatedSelect<- allBooksDated[which(allBooksDated$bookName%in%input$bookName),]
    allBooksDatedSelect<- allBooksDatedSelect[allBooksDatedSelect$Statut!="NULL"]
    
    p <- plotly::plot_ly(alpha = 0.6) %>%
    layout(title = input$titre, xaxis=list(title="Statuts"), yaxis=list(title="Nombre de copies")) %>%
    plotly::add_histogram(x =  as.character(allBooksDatedSelect$Statut), name = i)

  })
}

shinyApp(ui, server) 