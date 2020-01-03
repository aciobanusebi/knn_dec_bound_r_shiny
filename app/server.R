shinyServer(function(input, output, session) {
   
  data <- reactive({
    if(is.null(input$file)) {
      return()
    }
    
    x <- try(read.table(input$file$datapath,sep=","), 
             silent = TRUE)
    
    if (class(x) == "try-error") {
      return()
    }
    
    if(ncol(x) != 3) {
      return()
    }
    
    if(!is.numeric(x[,1]) || !is.numeric(x[,2])) {
      return()
    }
    
    x[,3] <- as.factor(x[,3])
    
    x.range <- range(x[,1])
    y.range <- range(x[,2])
    updateTextInput(session, "x_min", value = x.range[1])
    updateTextInput(session, "x_max", value = x.range[2])
    updateTextInput(session, "y_min", value = y.range[1])
    updateTextInput(session, "y_max", value = y.range[2])
    
    x
  })
  
  k <- reactive({
    k <- as.numeric(input$k)
    if(is.null(k) || is.na(k)) {
      return()
    }
    if(!is.wholenumber(k) || k <= 0 || k%%2==0) {
      return()
    }
    k
  })
  
  x_min <- reactive({
    x_min <- as.numeric(input$x_min)
    if(is.null(x_min) || is.na(x_min)) {
      return()
    }
    x_min
  })
  
  x_max <- reactive({
    x_max <- as.numeric(input$x_max)
    if(is.null(x_max) || is.na(x_max)) {
      return()
    }
    x_max
  })
  
  y_min <- reactive({
    y_min <- as.numeric(input$y_min)
    if(is.null(y_min) || is.na(y_min)) {
      return()
    }
    y_min
  })
  
  y_max <- reactive({
    y_max <- as.numeric(input$y_max)
    if(is.null(y_max) || is.na(y_max)) {
      return()
    }
    y_max
  })
  
  observeEvent(input$file,{
    data()
  })
  
  output$plot <- renderPlot({
    input$go
    withBusyIndicatorServer("go", {
      isolate({
        if(is.null(data())){
          message <- HTML("Please select a valid CSV file. 
          <br><b>first column (Ox)</b> - values on horizontal axis<br>
          <b>second column (Oy)</b> - values on vertical axis<br>
          <b>third column (class)</b> - the label of that point.<br>Example of contents of such a file:<br>
          <code>
          1,1,0<br>
          2,1,1<br>
          3,2,1<br>
          5,2,0<br>
          1,3,1<br>
          2,3,1<br>
          4,3,0<br>
          5,4,0<br>
          1,5,0<br>
          4,5,1<br>
          </code>")
          if(!is.null(input$file)) {
            sendSweetAlert(
              session = session, title = "Error", text = message, type = "error", html = TRUE
            )
          } else {
            sendSweetAlert(
              session = session, 
              title = "Info", 
              text = message, type = "info", html = TRUE
            )
          }
          return()
        }
        if(is.null(k())) {
          sendSweetAlert(
            session = session, title = "Error", text = "k must be an ODD natural number...", type = "error"
          )
          return()
        }
        if(is.null(x_min())) {
          sendSweetAlert(
            session = session, title = "Error", text = "Minimum value on horitonzal axis must be a number...", type = "error"
          )
          return()
        }
        if(is.null(x_max())) {
          sendSweetAlert(
            session = session, title = "Error", text = "Maximum value on horitonzal axis must be a number...", type = "error"
          )
          return()
        }
        if(x_min()>x_max()) {
          sendSweetAlert(
            session = session, title = "Error", text = "Minimum value on horizontal axis must be LOWER THAN maximum value on horizontal axis...", type = "error"
          )
          return()
        }
        if(is.null(y_min())) {
          sendSweetAlert(
            session = session, title = "Error", text = "Minimum value on vertical axis must be a number...", type = "error"
          )
          return()
        }
        if(is.null(y_max())) {
          sendSweetAlert(
            session = session, title = "Error", text = "Maximum value on vertical axis must be a number...", type = "error"
          )
          return()
        }
        if(y_min()>y_max()) {
          sendSweetAlert(
            session = session, title = "Error", text = "Minimum value on vertical axis must be LOWER THAN maximum value on vertical axis...", type = "error"
          )
          return()
        }
        result <- try(decisionBoundariesKnn(train=data(),
                                            k=k(),
                                            xlim=c(x_min(),x_max()),
                                            ylim=c(y_min(),y_max())))
        if (class(result) == "try-error") {
          sendSweetAlert(
            session = session, title = "Error", text = "Internal error... Please refresh the page...", type = "error"
          )
        }
      })
    })
  })
  
  output$auxPlot <- renderPlot({
    input$go
    withBusyIndicatorServer("go", {
      isolate({
        if(is.null(data())){
          return()
        }
        if(is.null(k())) {
          return()
        }
        if(k() != 1) {
          return()
        }
        if(is.null(x_min())) {
          return()
        }
        if(is.null(x_max())) {
          return()
        }
        if(x_min()>x_max()) {
          return()
        }
        if(is.null(y_min())) {
          return()
        }
        if(is.null(y_max())) {
          return()
        }
        if(y_min()>y_max()) {
          return()
        }
        result <- try(triangulatePlot(train=data()))
        if (class(result) == "try-error") {
          sendSweetAlert(
            session = session, title = "Error", text = "Internal error... Please refresh the page...", type = "error"
          )
        }
    })
  })
})
})
