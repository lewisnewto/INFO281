# INFO281

#shiny template
library(shiny)
library(shiny)
ui <- navbarPage("Income levels of New Zealanders",
                 tabPanel("Ethnic groups",plotOutput("ethnic_bar"), plotOutput("ethnic_line")),
                 tabPanel("Age groups"),
  theme = shinytheme("united"),
  sliderInput(inputId = "num",
              label = "Choose a year",
              value = 2016, min = 2008, max = 2019,
              format = "####", sep = ""),
  #plotOutput("ethnic_bar"),
  plotOutput("ethnic_line")
)
server <- function(input, output){
  output$ethnic_line <- renderPlot({
    ggplot(Ethnic_data, aes(x = factor(Year), group = 1)) +
      geom_line(aes(y = European), color ="blue") +
      geom_line(aes(y = Maori), color = "orange") +
      geom_line(aes(y = Pacific_Peoples), color = "grey") +
      geom_line(aes(y = Asian), color = "green") +
      geom_line(aes(y = MELAA), color = "yellow") +
      geom_line(aes(y = Other_Ethnicity), color = "red") +
      xlab(label = "year") +
      ylab("Average weekly income (NZD)") +
      labs(title = 'Income Growth of New Zealand Ethnic Groups')
  })  
    output$ethnic_bar <- renderPlot({
      filter(plot_dat, Year %in% input$num) %>%
        ggplot(data = ., aes(x = factor(ethnic), y = value)) +
        geom_bar(stat = "identity") 
      

  })
}
shinyApp(ui = ui, server = server)

