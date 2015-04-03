shinyUI(
  fluidPage(
    titlePanel("Call Informer"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file', 'Choose XML File')
      ),
      mainPanel(
        tableOutput('result')
      )
    )
  )
)