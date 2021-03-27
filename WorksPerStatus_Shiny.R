library(shiny)
library(plotly)



ui <- fluidPage(
  fluidRow(
    column(10,
           plotlyOutput("plot")
    ),
    column(12,
           sidebarPanel(
             selectInput("Statut", "Nom du statut:",
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
    
    allBooksDatedSelect<- allBooksDated[which(allBooksDated$Statut%in%input$Statut),]
    allBooksDatedSelect<- allBooksDatedSelect[allBooksDatedSelect$bookName!="NULL"]
    
    p <- plotly::plot_ly(alpha = 0.6) %>%
    layout(title = input$titre, xaxis=list(title="Oeuvre"), yaxis=list(title="Nombre de copies")) %>%
    plotly::add_histogram(x =  as.character(allBooksDatedSelect$bookName))
    
  })
}

shinyApp(ui, server) 