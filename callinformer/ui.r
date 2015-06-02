shinyUI(
  fluidPage(
    titlePanel(''),
    sidebarLayout(
      sidebarPanel(
        fileInput('file', 'Выберите XML-файл:'),
        uiOutput('ui')
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.selection == 'in1'",
          tableOutput('result1')
        ),
        conditionalPanel(
          condition = "input.selection == 'in2'",
          tableOutput('result2')
        ),
        conditionalPanel(
          condition = "input.selection == 'in3'",
          tableOutput('result3')
        ),
        conditionalPanel(
          condition = "input.selection == 'in4'",
          tableOutput('result4')
        ),
        conditionalPanel(
          condition = "input.selection == 'in5'",
          tableOutput('result5')
        ),
        conditionalPanel(
          condition = "input.selection == 'in6'",
          tableOutput('result6')
        ),
        conditionalPanel(
          condition = "input.selection == 'in7'",
          tableOutput('result7')
        ),
        conditionalPanel(
          condition = "input.selection == 'in8'",
          tableOutput('result8')
        ),
        conditionalPanel(
          condition = "input.selection == 'in9'",
          tableOutput('result9')
        ),
        conditionalPanel(
          condition = "input.selection == 'in10'",
          tableOutput('result10')
        )
      )
    )
  )
)