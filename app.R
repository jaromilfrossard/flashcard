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
            f7Margin(p(textOutput("cat")),side="top"),
            f7Align(h2(textOutput("faceA")),"center")),
          f7Card(
            height=400,
            f7Align(h2(textOutput("faceB")),"center")),
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
    flashcard <- read.delim("../data/flashcard.csv",sep = ";")

    
    dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
    
    
    
    output$flashcard <-renderUI({
      f7Table(flashcard)
      })
    
    state <- reactiveVal(0L) 
    print("newstate")
    idcard <- reactiveVal(sample(nrow(flashcard),1))
    
    
    ## default card
    output$cat <- renderText({style_category(flashcard[idcard(),1])})
    output$faceA <- renderText({flashcard[idcard(),2]})
    output$faceB <- renderText({" "})
    
    
    observeEvent(input$show, {
      state((state() +1)%%2)
      switch(as.character(state()),
        "0" = {
          idcard(sample(nrow(flashcard),1))
          output$cat <- renderText({style_category(flashcard[idcard(),1])})
          output$faceA <- renderText({flashcard[idcard(),2]})
          output$faceB <- renderText({" "})
          },
        "1" = {
          output$faceB <- renderText({flashcard[idcard(),3]})
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