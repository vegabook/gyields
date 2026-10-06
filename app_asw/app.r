ui <- fluidPage (

    selectInput(inputId = "cCodes", label = "Choose countries", choices = cl, selected = cl, multiple = TRUE),
    sliderInput(inputId = "years", label = "Number of years", min = 0.5, max = 7, value = 1, round = FALSE),
    sliderInput(inputId = "numPCs", label = "Remove how many PCs?", min = 1, max = 6, value = 1, round = TRUE),
    actionButton(inputId = "go", label = "GO!"),
    plotOutput("asw")
)

server <- function(input, output) {

    asw_obj <- eventReactive(input$go, {
        bigasw(cCodes = input$cCodes, years = input$years)
    })

    #joutput$asw <- renderPlot({
    #    plot_bigasw(asw_obj, whichPC = input$numPCs, tobrowser = F, toshiny = T)
   # })
}


shinyApp(ui, server)


    
    
