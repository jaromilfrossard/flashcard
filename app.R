library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(apexcharter)

style_category = function(x){
  paste0("{",x,"}")
}



shinyApp(
  ui = f7Page(
    title = "Flashcard",
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Category", side = "left", theme = "light", "Blabla", effect = "cover")
      ),
      navbar = f7Navbar(
        title = "Flashcard",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = FALSE,
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
            f7Align(h2(textOutput("definition")),"center")),
          f7Card(
            height=400,
            f7Align(h2(textOutput("word")),"center"),
            f7Margin(p(textOutput("example")),side="top")),
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
    flashcard <- read.delim("https://raw.githubusercontent.com/jaromilfrossard/flashcard/master/data/flashcard.csv",sep = ";")

    
    dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
    
    
    
    output$flashcard <-renderUI({
      f7Table(flashcard)
      })
    
    state <- reactiveVal(0L) 
    idcard <- reactiveVal(sample(nrow(flashcard),1))
    
    
    ## default card
    output$category <- renderText({style_category(flashcard$category[idcard()])})
    output$word <- renderText({flashcard$word[idcard()]})
    output$example <- renderText({" "})
    output$definition <- renderText({" "})
    
    
    observeEvent(input$show, {
      state((state() +1)%%2)
      switch(as.character(state()),
        "0" = {
          print(head(flashcard))
          idcard(sample(nrow(flashcard),1))
          output$category <- renderText({style_category(flashcard$category[idcard()])})
          output$definition <- renderText({flashcard$definition[idcard()]})
          output$example <- renderText({" "})
          output$definition <- renderText({" "})
          },
        "1" = {
          print("1")
          output$word <- renderText({flashcard$word[idcard()]})
          output$example <- renderText({flashcard$example[idcard()]})
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