library(plotly)
library(multichull)
library(shinythemes)
shinyServer(function(input , output){

  ############# Load data operations

  #load files
  # function to load data
  data <- reactive({
    file <- input$loadfile
    if(is.null(file)){
      return(NULL)
    }
    dat <- read.csv(file = file$datapath, header = T, sep = ",")
    if(ncol(dat) == 2){
      dat <- cbind("Complexity" = dat$V1, "(mis-) fit" = dat$V2)
    }else{
      name <- c('Complexity', rep('(mis-) fit', ncol(dat)-1))
      colnames(dat) <- name
    }

    return(dat)
  })
  output$ui <- renderTable({
    dat <- data()
    dat
  })# end render table


  ############# Chull options

  # start chull
  but1 = reactiveValues(but1=FALSE)
  but2 = reactiveValues(but2=FALSE)

  observeEvent(input$stChull,
               isolate({but1$but1=TRUE
               but2$but2=FALSE
               }))

  observeEvent(input$stMultiChull,
               isolate({but1$but1=FALSE
               but2$but2=TRUE
               }))

  chullstart <- eventReactive(input$stChull, {
    multichull::CHull(data(),input$bound,input$PercentageFit)
  })

  # start multichull

  Multichullstart <- eventReactive(input$stMultiChull, {
    multichull::MultiCHull(data(),input$boundM,input$PercentageFitM)
  })

  ############# Results

  ### summary tables
  output$print <- renderPrint({
    if(but1$but1)
      print(chullstart())
    else if(but2$but2)
      print(Multichullstart())
    else
      return()
  })
  output$summary <- renderPrint({
    if(but1$but1)
      summary(chullstart())
    else if(but2$but2)
      summary(Multichullstart())
    else
      return()

  })




  ### plotly
  output$plotly <- renderPlotly({

    if(but1$but1){
      ch <- chullstart()

      line <- list(
        type = "line",
        line = list(dash='dot', width = 1, color = "black"),
        xref = "x",
        yref = "y"
      )

      len <- nrow(ch$Hull)
      lines <- list()
      for (i in 2:len) {
        line[["x0"]] <- ch$Hull$complexity[i-1]
        line[["x1"]] <- ch$Hull$complexity[i]
        line[["y0"]] <- ch$Hull$fit[i-1]
        line[["y1"]] <- ch$Hull$fit[i]
        lines <- c(lines, list(line))
      }

      a <- list(
        x=ch$Solution$complexity, y=ch$Solution$fit,
        text = "Selected model", showarrow=TRUE,
        arrowhead=7, ax=-20, ay=20
      )

      plotly::plot_ly(data=ch$OrigData, x = ~complexity,
              y = ~fit, mode = 'markers', type = 'scatter',
              text = ~paste("Fit: ", fit, "Complexity: ", complexity)) %>%

        layout(title = "Convex hull",
               annotations=a,
               shapes=lines)
    }else if(but2$but2){

    }else
      return()


  })

  ############# Save

  ############# About

})# shinyServer
