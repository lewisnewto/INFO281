# INFO281
#shiny template
library(shiny)
library(shiny)
ui <- navbarPage("Income levels of New Zealanders",
                 tabPanel("Ethnic groups", sliderInput(inputId = "num",
                                                       label = "Choose a year",
                                                       value = 2016, min = 2008, max = 2019,
                                                       format = "####", sep = ""),
                                                      plotOutput("ethnic_bar"), plotOutput("ethnic_line")),
                 tabPanel("Age groups", sliderInput(inputId = "num2",
                                                    label = "Choose a year",
                                                    value = 2017, min = 1998, max = 2019,
                                                    format = "####", sep = ""),
                                        plotOutput("age_bar"), plotOutput("age_line")),
  theme = shinytheme("united")
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
        geom_bar(stat = "identity") +
        xlab(label = "Ethnicity groups")+
        ylab("Average weekly income (NZD)")
      
  })
    output$age_bar <- renderPlot({
      filter(plot_age, Year %in% input$num2) %>%
        ggplot(data = ., aes(x = factor(age), y = value)) +
        geom_bar(stat = "identity") +
        xlab(label = "Age groups")+
        ylab("Average weekly income (NZD)")
    })
    output$age_line <- renderPlot({
      ggplot(age_dat, aes(x = factor(Year), group = 1))+
        geom_line(aes(y = twenty), color = "blue")+
        geom_line(aes(y = twentyfive), color = "red")+
        geom_line(aes(y = thirty), color = "grey")+
        geom_line(aes(y = thirtyfive), color = "green")+
        geom_line(aes(y = forty), color = "orange")+
        geom_line(aes(y = fortyfive), color = "yellow")+
        geom_line(aes(y = fifty), color = "black")+
        geom_line(aes(y = fiftyfive), color = "purple")+
        geom_line(aes(y = sixty), color = "cyan")+
        geom_line(aes(y = sixtyfive), color = "pink")+
        xlab(label = "Year")+
        ylab("Average weekly income (NZD)")+
        labs(title = 'Income Growth of New Zealand Age Groups') 
    })
}
shinyApp(ui = ui, server = server)

