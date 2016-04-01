library(shiny)

shinyUI(pageWithSidebar(
        # Application title
        headerPanel('Capstone'),
        
        textInput('input','Please enter words:')
        )
        mainPanel(
                verbatimTextOutput('oresult')
        )
)
