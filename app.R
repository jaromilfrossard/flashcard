library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(dplyr)
library(tidyr)

style_category = function(x){
  paste0("{",x,"}")
}

flashcard <- read.delim("https://raw.githubusercontent.com/jaromilfrossard/flashcard/master/data/flashcard.csv",sep = ";")

cbg_choices <- sort(c(unique(flashcard$category)))



shinyApp(
  ui = f7Page(
    title = "Flashcard",
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Category", 
                side = "left", 
                theme = "light", 
                effect = "cover",
                f7CheckboxGroup(
                    inputId = "category_selector",
                    label ="Select categories",
                    choices = cbg_choices,
                    selected = cbg_choices[1])
        )
      ),
      navbar = f7Navbar(
        title = "Flashcard",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = TRUE,
        rightPanel = FALSE
      ),
      f7Tabs(
        animated = TRUE,
        #swipeable = TRUE,
        f7Tab(
          tabName = "Learn",
          icon = f7Icon("play"),
          active = TRUE,
          f7Card(
            f7Margin(p(textOutput("category")),side="top"),
            f7Align(h2(textOutput("front")),"center")),
          f7Card(
            height=400,
            f7Align(h2(textOutput("back")),"center"),
            f7Margin(h4(textOutput("example")),side="top")),
          f7Card(
            f7Block(
              hairline = TRUE,
              strong = TRUE,
              f7Button("show", label = "Show")))
        ),
        f7Tab(
          tabName = "Cards",
          icon = f7Icon("table"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Preview",
              uiOutput("flashcard")))
          )
        )
      )
    ),
  server = function(input, output, session) {
    
    # river plot
    

    state <- reactiveVal(0L) 
    current_cards <- reactiveVal(flashcard%>%filter(category%in%c(cbg_choices[1])))
    idcard <- reactiveVal(sample(nrow(flashcard%>%filter(category%in%c(cbg_choices[1]))),1))
   
    
    observeEvent(input$category_selector,{
      current_cards(flashcard%>%filter(category%in%c(input$category_selector)))

      output$flashcard <-renderUI({
        f7Table(current_cards())
      })
      
    })
    
    
    ## default card
    output$category <- renderText({style_category(current_cards()$category[idcard()])})
    output$front <- renderText({current_cards()$front[idcard()]})
    output$back <- renderText({" "})
    output$example <- renderText({" "})
    
    
    observeEvent(input$show, {
      state((state() +1)%%2)
      switch(as.character(state()),
        "0" = {
          idcard(sample(nrow(current_cards()),1))
          output$category <- renderText({style_category(current_cards()$category[idcard()])})
          output$front <- renderText({current_cards()$front[idcard()]})
          output$back  <- renderText({" "})
          output$example <- renderText({" "})
          },
        "1" = {
          output$back <- renderText({current_cards()$back[idcard()]})
          output$example <- renderText({current_cards()$example[idcard()]})
        }
      )
    })
    
    
  
    
    # send the theme to javascript
    # observe({
    #   session$sendCustomMessage(
    #     type = "ui-tweak",
    #     message = list(os = input$theme, skin = input$color)
    #   )
    # })
    
  }
)