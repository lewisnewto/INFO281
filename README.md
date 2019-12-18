# INFO281

#load packages

library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(quantmod)

#line plot ethnic groups

ggplot(Ethnic_dat, aes(x = factor(Year), group = 1)) +
  geom_line(aes(y = European), color ="blue") +
  geom_line(aes(y = Maori), color = "orange") +
  geom_line(aes(y = Pacific_Peoples), color = "grey") +
  geom_line(aes(y = Asian), color = "green") +
  geom_line(aes(y = MELAA), color = "yellow") +
  geom_line(aes(y = Other), color = "red") +
  xlab(label = "year") +
  ylab("Average weekly income (NZD)") +
  labs(title = 'Income Growth of New Zealand Ethnic Groups')
  
#bar graph all ethnic groups
plot_dat <- gather(Ethnic_dat, key =  "ethnic", value = "value", -Year, -Total)

ggplot(data = plot_dat, aes(x = factor(Year), y = value, fill = ethnic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = .40))

#All years ethnic plot

ggplot(data = plot_dat, aes(x = factor(ethnic), y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year)

#Yearly ethnic plot

filter(plot_dat, Year == "2017") %>%
  ggplot(data = ., aes(x = factor(ethnic), y = value)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("green", "blue", "orange", "yellow", "red", "grey"))+
  xlab(label = "Ethnicity groups")+
  ylab("Average weekly income (NZD)")

#change colnames
colnames(age_dat)[colnames(age_dat)%in% c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")] <- c("Year", "Total", "twenty", "twentyfive", "thirty", "thirtyfive", "forty", "fortyfive", "fifty", "fiftyfive", "sixty", "sixtyfive")


#line plot age groups

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

#bar graph all age groups
plot_age <- gather(age_dat, key =  "age", value = "value", -Year, -Total)

ggplot(data = plot_age, aes(x = factor(Year), y = value, fill = age)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = .40))


#Yearly age plot

filter(plot_age, Year == "2017") %>%
  ggplot(data = ., aes(x = factor(age), y = value)) +
  geom_bar(stat = "identity") +
  xlab(label = "Age groups")+
  ylab("Average weekly income (NZD)")

output$age_bar <- renderPlot({
  filter(plot_age, Year %in% input$num2) %>%
    ggplot(data = ., aes(x = factor(age), y = value)) +
    geom_bar(stat = "identity") +
    xlab(label = "Age groups")+
    ylab("Average weekly income (NZD)")
})

sliderInput(inputId = "num2",
            label = "Choose a year",
            value = 2017, min = 1998, max = 2019,
            format = "####", sep = ""),

server <- function(input, output){
  output$ethnic_line <- renderPlot({
    ggplot(Ethnic_dat, aes(x = factor(Year), group = 1)) +
      geom_line(aes(y = European), color ="blue") +
      geom_line(aes(y = Maori), color = "orange") +
      geom_line(aes(y = Pacific_Peoples), color = "grey") +
      geom_line(aes(y = Asian), color = "green") +
      geom_line(aes(y = MELAA), color = "yellow") +
      geom_line(aes(y = Other), color = "red") +
      xlab(label = "year") +
      ylab("Average weekly income (NZD)") +
      labs(color = "Legend")
    labs(title = 'Income Growth of New Zealand Ethnic Groups')
    
