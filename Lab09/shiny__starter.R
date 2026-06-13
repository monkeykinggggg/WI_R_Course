library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)

##### Import danych

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

##### Zrób apkę

ui <- page_navbar(
  title = "My Application",
  inverse = TRUE,
  nav_panel(title = "Page 1", page_fluid(
    layout_sidebar(
      sidebar = sidebar(
        sliderInput(
          inputId = "ideology",
          label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
          min = 1,
          max = 5,
          value = 3,
          step = 1
        )
      ),
      navset_tab(
        nav_panel("Tab1",plotOutput(outputId = "histogram_pid7")),
        nav_panel("Tab2",plotOutput(outputId = "histogram_trump"))
      )
      
    ) 
  )),
  nav_panel(title = "Page 2", page_fluid(
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          inputId = "gender",
          label = "Select Gender:",
          c("1" = 1, "2"=2)
        )
      ),
      plotOutput(outputId = "gender_plot")
    ) 
  )),
  nav_panel(title = "Page 3", page_fluid(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "region",
          label = "Select Region",
          choices = sort(unique(dat$region))
        ),
      ),
      DTOutput("outcome_table", height = 500)
    ) 
  ))
)
  
server<-function(input,output){
  
	#####Wskazówka: tworząc tabelę danych na stronie 3, być może będziesz musiał/a dostosować argument height w funkcji dataTableOutput. Spróbuj wartości height=500.
  output$gender_plot <- renderPlot({
    dat %>% filter(gender %in% input$gender) %>% ggplot(aes(educ, pid7)) + geom_jitter() + geom_smooth(method="lm")
  })
  output$histogram_pid7 = renderPlot({
    dat %>% filter(ideo5 == input$ideology) %>% ggplot(aes(pid7)) + geom_bar() + labs(x = "7 Point Party ID, 1=Very D, 7=Very R", y="Count")
  })
  output$histogram_trump = renderPlot({
    dat %>% filter(ideo5 == input$ideology) %>% ggplot(aes(CC18_308a)) + geom_bar() + labs(x= "Trump Support", y="Count")
  })
  output$outcome_table = renderDT(
    dat %>% filter(region == input$region)
  )
} 

shinyApp(ui,server)