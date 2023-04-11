library(shiny)
library(tidyverse)

#load data
pokedex = read_csv("data/pokedex.csv")

ui = navbarPage(
  title = "Pokédex",
  tabPanel(
    title = "Input / Visualization",
    titlePanel(title = "Pokémon Comparison:"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Name1", 
                    label = "First Pokémon", 
                    choices = unique(pokedex$Name)),
        selectInput(inputId = "Form1", 
                    label = "First Form", 
                    choices = unique(pokedex$Form)),
        selectInput(inputId = "Name2", 
                    label = "Second Pokémon", 
                    choices = unique(pokedex$Name)),
        selectInput(inputId = "Form2", 
                    label = "Second Form", 
                    choices = unique(pokedex$Form)),
        checkboxInput(inputId = "Filter_Name",
                      label = "Filter Table to Selected Names",
                      value = FALSE),
        checkboxInput(inputId = "Filter_Form",
                      label = "Filter Table to Selected Forms",
                      value = FALSE)
        ),
      mainPanel(plotOutput("plot"))
    )),
  tabPanel(
    title = "Table",
    dataTableOutput("table")
    ),
  tabPanel(
    title = "About",
    includeMarkdown("about.Rmd")))

server = function(input, output) {
  
  form1 = reactive({
    pokedex |> 
      filter(Name == input$Name1)
  })
  
  form2 = reactive({
    pokedex |> 
      filter(Name == input$Name2)
  })
  
  names = reactive({
    pokedex |> 
      filter(Name == input$Name1 | Name == input$Name2)
  })
  
  observeEvent(
    eventExpr = input$Name1,
    handlerExpr = {
      updateSelectInput(inputId = "Form1", choices = form1()$Form)
    }
  )
  
  observeEvent(
    eventExpr = input$Name2,
    handlerExpr = {
      updateSelectInput(inputId = "Form2", choices = form2()$Form)
    }
  )
  
  observeEvent(
    eventExpr = input$Name1,
    handlerExpr = {
      updateSelectInput(inputId = "Form1", choices = names()$Form)
    }
  )
  
  output$plot = renderPlot({
    if(input$Name1 == input$Name2){
      pokedex |> 
        filter(Name == input$Name1 | Name == input$Name2) |> 
        filter(Form == input$Form1 | Form == input$Form2) |> 
        pivot_longer(HP:Speed, names_to = "Stat", values_to = "Count") |> 
        group_by(Name, Form, Stat) |> 
        summarise(Count = sum(Count)) |> 
        ggplot() +
        aes(x = Stat, y = Count, fill = Form) |> 
        geom_bar(stat = "identity", position = "Dodge") +
        theme_bw()
    }
    else {
      pokedex |> 
        filter(Name == input$Name1 | Name == input$Name2) |> 
        filter(Form == input$Form1 | Form == input$Form2) |> 
        pivot_longer(HP:Speed, names_to = "Stat", values_to = "Count") |> 
        group_by(Name, Form, Stat) |> 
        summarise(Count = sum(Count)) |> 
        ggplot() +
        aes(x = Stat, y = Count, fill = Name) |> 
        geom_bar(stat = "identity", position = "Dodge") +
        theme_bw()
    }
    
  })
  
  output$table = renderDataTable({
    tab = pokedex |> 
      calc_rating()
    if(input$Filter_Name){
      tab = tab |> 
        filter(Name == input$Name1 | Name == input$Name2)
    }
    if(input$Filter_Form){
      tab = tab |> 
        filter(Form == input$Form1 | Form == input$Form2)
    }
    
    tab
  })
}
shinyApp(ui = ui, server = server)
