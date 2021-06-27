library(shiny)

shinyUI(fluidPage(
    titlePanel("Can I guess your next word?"),
    sidebarLayout(
        sidebarPanel(
            textInput("search.request", label = "Type your text here")#,
            #submitButton("go")

        ),
        mainPanel(
            h4("You have typed..."),
            textOutput("search.original"),
            h4("I think your next word would be..."),
            textOutput("search.result"),
            h4("So, I think what you want to say is..."),
            textOutput("search.composed"),
            h4("Not a match? The other possibilities I see are.."),
            tableOutput("word.df")
            
        )
    )
))
