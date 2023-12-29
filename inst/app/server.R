

shinyServer(function(input, output, session){

  # bslib::bs_themer()

  Sys.sleep(0.125)
  hide(id = "loader", anim = TRUE, animType = "fade")

  session$onSessionEnded(function() { })

  # ----------------------------- DATA SECTION ---------------------------------

  data <- options()$easyPlot.shiny.data
  name <- options()$easyPlot.shiny.name

  observe({
    if (input$navbar == "quit") {
      stopApp()
    }
  })

  Code_Data <- reactiveValues(data = "")

  observe({
    if (is.null(data)) {
      hide(id = "my_data-recode_class_dynamic", anim = TRUE, animType = "fade")
      hide(id = "my_data", anim = TRUE, animType = "fade")
      hide(id = "my_data_hr", anim = TRUE, animType = "fade")

    } else {
      updateCheckboxInput(session, inputId = "my_data", value = TRUE)
    }
  })

  observe({
    if (input$my_data) {

      updateCheckboxInput(session, inputId = "exampleData", value = FALSE)

      updateCheckboxInput(session, inputId = "upload_data", value = FALSE)

      updateButton(session, inputId = "readyButton", value = FALSE,
                   style = "danger", icon = icon("ban"))
    }
  })


  observe({
    if (input$exampleData) {

      updateCheckboxInput(session, inputId = "my_data", value = FALSE)

      updateCheckboxInput(session, inputId = "upload_data", value = FALSE)

      updateButton(session, inputId = "readyButton", value = FALSE,
                   style = "danger", icon = icon("ban"))
    }
  })


  observe({
    if (input$upload_data) {

      updateCheckboxInput(session, inputId = "my_data", value = FALSE)

      updateCheckboxInput(session, inputId = "exampleData", value = FALSE)

      updateButton(session, inputId = "readyButton", value = FALSE,
                   style = "danger", icon = icon("ban"))
    }
  })


  observeEvent(input$skip, {
      value <- as.integer(input$skip)
      req(value)
      if (is.na(value) || value < 1) {
          showModal(modalDialog(
              title = "Invalid Input",
              "Please enter an integer between bigger than 0",
              easyClose = TRUE
          ))
          updateNumericInput(session, inputId = "skip", value = 1)  # Reset input
      }
  })


  dataset1 <- reactive({

    mtcars_c <- mtcars
    mtcars_c$cyl <- factor(mtcars$cyl)
    mtcars_c$vs <- factor(mtcars$vs)
    mtcars_c$gear <- factor(mtcars$gear)

    Code_Data$example <- input$Data

    shinyjs::html(id = "description", html = descriptions())
    switch(input$Data,
           "faithful" = faithful,
           "iris" = iris,
           "mtcars" = mtcars_c,
           "attitude" = attitude,
           "diamonds" = as.data.frame(diamonds)
    )

  })

  source("dataset_descriptions.R", local = TRUE)


  # readyButton
  observeEvent(input$readyButton, ({
    if (input$readyButton) {
      updateButton(session, inputId = "readyButton",
                   style = "success", icon = icon("thumbs-up"))
      #updateCheckboxInput(session, inputId = "exampleData", value = FALSE)

    }
    else updateButton(session, inputId = "readyButton", style = "danger",
                      icon = icon("ban"))
  }))

  uploadedData2 <- reactive({

      upFile <- input$uploaded

      # Code_Data$upload <- substr(upFile$name, 1, nchar(upFile$name) - 4)
      Code_Data$upload <- NULL

      if (is.null(upFile)) {

          return(NULL)

      } else {

          updateCheckboxInput(session, inputId = "my_data", value = FALSE)
          updateCheckboxInput(session, inputId = "exampleData", value = FALSE)
          skip_rows <- as.integer(input$skip)

          if (is.na(skip_rows) || skip_rows < 1) {
              return(NULL)
          }

          if (any(endsWith(upFile$name, c(".csv", ".txt")))) {

              Code_Data$upload <- substr(upFile$name, 1, nchar(upFile$name) - 4)

              output$data_format <- renderText("csvtxt")
              outputOptions(output, "data_format", suspendWhenHidden = FALSE)

              x <- tryCatch({
                data.table::fread(upFile$datapath,
                                  header = input$header,
                                  sep = input$sep,
                                  quote = input$quote,
                                  dec = input$dec,
                                  stringsAsFactors = input$strings_as_factor,
                                  skip = skip_rows - 1,
                                  check.names = TRUE,
                                  data.table = FALSE)

              }, error = function(e) {
                closeAlert(session, "alert1")
                if (input$sep == input$dec) {
                  createAlert(session,
                              anchorId = "dataAlert1",
                              alertId = "alert1",
                              title = "Seperator and decimal cannot be the same",
                              content = "",
                              dismiss = FALSE,
                              style = "Warning")
                } else {
                  createAlert(session,
                              anchorId = "dataAlert1",
                              alertId = "alert1",
                              title = "Data could not have been loaded",
                              content = "",
                              dismiss = FALSE,
                              style = "Warning")
                }

                NULL
              })

              # Re-code logical variables into factors depending on a widget
                x <- tryCatch({
                  if (input$logicals_as_factor) {
                    ind_logical <- sapply(x, is.logical)
                    x[ind_logical] <- lapply(x[ind_logical], as.factor)
                  }
                  x
                }, error = function(e) {
                  x
                })

                # remove empty columns
                x <- tryCatch({
                  if (ncol(x) > 1) {
                    x <- x[ ,colSums(is.na(x)) < nrow(x)]
                  }
                  x
                }, error = function(e) {
                  x
                })

              return(x)
          }

          if (any(endsWith(upFile$name, c(".xlsx", ".xls")))) {

              Code_Data$upload <- substr(upFile$name, 1, nchar(upFile$name) - 5)

              output$data_format <- renderText("xlsx")
              outputOptions(output, "data_format", suspendWhenHidden = FALSE)

              skip_rows_xlsx <- input$skip_xlsx

              req(data_xlsx_wb())
              req(input$select_xlsx_sheet)

              select_xlsx_named_region <- input$select_xlsx_named_region
              select_xlsx_named_region <- if (select_xlsx_named_region == "None") NULL else select_xlsx_named_region

              x <- tryCatch({
                openxlsx::readWorkbook(data_xlsx_wb(),
                                       sheet = input$select_xlsx_sheet,
                                       detectDates = input$detect_dates_xlsx,
                                       startRow = as.integer(skip_rows_xlsx),
                                       namedRegion = select_xlsx_named_region) # named_regions_xlsx[2]) # NULL)
              }, error = function(e) {

                closeAlert(session, "alert1")
                createAlert(session,
                            anchorId = "dataAlert1",
                            alertId = "alert1",
                            title = "Data could not have been loaded",
                            content = "Hint: conversion of dates may have gone wrong. Uncheck 'Detect dates'",
                            dismiss = FALSE,
                            style = "Warning")
                NULL
                }
              )

              if (!is.null(x)) {
                if (input$logicals_as_factor_xlsx) {
                  ind_logical <- sapply(x, is.logical)
                  x[ind_logical] <- lapply(x[ind_logical], as.factor)
                }

                if (input$strings_as_factor_xlsx) {
                  ind_logical <- sapply(x, is.character)
                  x[ind_logical] <- lapply(x[ind_logical], as.factor)
                }
              }

              return(x)
          }
      }
  })


  uploadedData <- debounce(uploadedData2, 500)

  observe({

    is_data_uploaded <- !is.null(uploadedData())

    if (is_data_uploaded) {

      upFile <- input$uploaded
      csv_ <- any(endsWith(upFile$name, c(".csv", ".txt")))
      xlsx_ <- any(endsWith(upFile$name, c(".xlsx", ".xls")))

      if (csv_) {

        file_name <- substr(upFile$name, 1, nchar(upFile$name) - 4)
        # n_ <- nchar(paste0(file_name, " <- read.csv("))
        n_ <- nchar(paste0(file_name, " <- data.table::fread("))

        blank_ <- paste0(rep(" ", n_), collapse = "")

        # header_ <- ifelse(input$header, yes = "", no = paste0(",\n", blank_, "header = TRUE"))
        header_ <- paste0(",\n", blank_, "header = ", input$header)

        sep_ <- ifelse(input$sep == "auto", yes = "", no = paste0(",\n", blank_, "sep = '", input$sep, "'"))

        quote_ <- ifelse(input$quote == "\"", yes = "", no = paste0(",\n", blank_, "quote = '", input$quote, "'"))

        dec_ <- ifelse(input$dec ==  ".", yes = "", no = paste0(",\n", blank_, "dec = '", input$dec, "'"))

        strings_as_factor_ <- ifelse(input$strings_as_factor == FALSE, yes = "", no = paste0(",\n", blank_, "stringsAsFactors = ", input$strings_as_factor))

        skip_ <- ifelse(input$skip - 1 == 0, yes = "", no = paste0(",\n", blank_, "skip = ", as.integer(input$skip) - 1))

        x_comment <- "# Load dataset (update the file path)"

        x_code <- paste0(file_name,
                         " <- data.table::fread('", upFile$name, "'",
                         header_,
                         sep_,
                         quote_,
                         dec_,
                         strings_as_factor_,
                         skip_,
                         paste0(",\n", blank_, "check.names = TRUE"),
                         paste0(",\n", blank_, "data.table = FALSE"),
                         ")")

        x_full_code <- paste0("\n",
                              x_comment,
                              "\n",
                              x_code,
                              "\n")

        Code$load_data <- x_full_code
        Code_hi$load_data <- x_full_code
        Code_ba$load_data <- x_full_code
        Code_box$load_data <- x_full_code

      } else {

        file_name <- substr(upFile$name, 1, nchar(upFile$name) - 5)
        n_ <- nchar(paste0(file_name, " <- openxlsx::read.xlsx("))
        blank_ <- paste0(rep(" ", n_), collapse = "")
        start_row__ <- as.integer(input$skip_xlsx)

        sheet_ <- paste0(",\n", blank_, "sheet = '", input$select_xlsx_sheet, "'")
        detect_dates_ <- ifelse(input$detect_dates_xlsx == FALSE, yes = "", no = paste0(",\n", blank_, "detectDates = TRUE"))
        start_row_ <- ifelse(start_row__ == 1, yes = "", no = paste0(",\n", blank_, "startRow = ", start_row__))
        named_region_ <- ifelse(input$select_xlsx_named_region == "None", yes = "", no = paste0(",\n", blank_, "namedRegion = '", input$select_xlsx_named_region, "'"))

        x_comment <- "# Load dataset (update the file path)"

        x_code <- paste0(file_name,
                         " <- openxlsx::read.xlsx('", upFile$name, "'",
                         sheet_,
                         detect_dates_,
                         start_row_,
                         named_region_,
                         ")")

        x_full_code <- paste0("\n",
                              x_comment,
                              "\n",
                              x_code,
                              "\n")

        Code$load_data <- x_full_code
        Code_hi$load_data <- x_full_code
        Code_ba$load_data <- x_full_code
        Code_box$load_data <- x_full_code

      }

    } else {

      Code$load_data <- NULL
      Code_hi$load_data <- NULL
      Code_ba$load_data <- NULL
      Code_box$load_data <- NULL
    }

    if (input$my_data | input$exampleData) {
      Code$load_data <- NULL
      Code_hi$load_data <- NULL
      Code_ba$load_data <- NULL
      Code_box$load_data <- NULL
    }

  })


  data_xlsx_wb <- reactive({

    upFile <- input$uploaded

    if (any(endsWith(upFile$name, c(".xlsx", ".xls")))) {

      wb <- openxlsx::loadWorkbook(upFile$datapath)

      # Sheets
      sheets_xlsx <- openxlsx::sheets(wb)
      updateSelectInput(session,
                        inputId = "select_xlsx_sheet",
                        choices = sheets_xlsx)

      nr <- openxlsx::getNamedRegions(wb)
      nr_attr <- attributes(nr)$sheet
      ind_nr <- which(sheets_xlsx %in% nr_attr)
      nr <- as.character(nr[ind_nr])
      named_regions_xlsx <- c("None", nr)
      updateSelectInput(session,
                        inputId = "select_xlsx_named_region",
                        choices = named_regions_xlsx)

      return(wb)
    }
    return(NULL)
  })


  observe({
      req(input$uploaded)
      updateButton(session,
                   inputId = "readyButton",
                   value = FALSE,
                   style = "danger",
                   icon = icon("ban"))
  })


  dataForTable2 <- reactive({

    if (input$my_data & !is.null(data)) {

      closeAlert(session, "alert1")
      x <- data

    } else if (input$exampleData) {

      closeAlert(session, "alert1")
      x <- dataset1()

    } else {

      # No data has been uploaded
      if (is.null(uploadedData())) {

        createAlert(session,
                    anchorId = "dataAlert1",
                    alertId = "alert1",
                    title = "Please provide a data frame with at least two variables",
                    content = "",
                    dismiss = FALSE)
        x <- NULL

      # Data has been aleady uploaded
      } else {

        # Checkbox is not checked
        if (!input$upload_data) {

            createAlert(session,
                        anchorId = "dataAlert1",
                        alertId = "alert1",
                        title = "Please provide a data frame with at least two variables",
                        content = "",
                        dismiss = FALSE)
            x <- NULL
        # Checkbox is checked and data is uploaded
        } else {

            closeAlert(session, "alert1")
            x <- uploadedData()
        }
      }
    }
    # Store dataset
    dataForTable(x)
    dataForTableBackup(x)
    head(x)
})


  # Recoding classes of variables
  dataForTable <- reactiveVal(NULL)
  dataForTableBackup <- reactiveVal(NULL)
  source("recode_variables_server.R", local = TRUE)$value


  output$table <- DT::renderDataTable({
      req(dataForTable())
      req(dataForTable2())
      check <- dataForTable2() # needed for updating
      dataForTable()
      },
      colnames = paste0(colnames(dataForTable()), "\n", "(", sapply(dataForTable(), function(x) class(x)[1]), ")"),
      server = TRUE,
      class = "cell-border stripe hover",
      caption = "",
      #extensions = "TableTools",
      extensions = c("Buttons", "KeyTable"),
      options = list(
          columnDefs = list(list(className = 'dt-center', targets = 0:ncol(dataForTable()))),
          keys = TRUE, # KeyTable
          lengthMenu = list(c(25, 50, 100,  -1), c("25", "50", "100", "All")),
          searchHighlight = TRUE,
          search = list(regex = TRUE)#,
          # dom = 'T<"clear">lfrtip',
          # dom = 'Bfrtip',
          #   tableTools = list(sSwfPath = DT::copySWF("www", pdf = TRUE),
          #                     aButtons = list("copy", "pdf"))
          # )x
          # buttons = c("copy", "csv", "excel", "pdf", "print")

      ), filter = "top"
  )


  filtered_data <- reactive({

      check <- dataForTable2() # need it for updating
      req(input$table_rows_all)
      req(dataForTable())

      dt <- dataForTable()[input$table_rows_all, ]
      dt
  })


  plotData <- reactive ({

    d <- filtered_data()

    if (is.data.frame(d) & length(d) > 1) {

        ready <- input$readyButton
        exampleDat <- input$exampleData
        my_data <- input$my_data
        upload_data <- input$upload_data

        d <- d[complete.cases(d), ] # TODO: add this part to reproducible code

        # test1 <- sapply(d, is.character)
        # test2 <- sapply(d, is.logical)
        #
        # if (sum(test1) > 0) {
        #   d[ ,test1] <- sapply(d[ , test1], as.factor)
        # }
        # if (sum(test2) > 0) {
        #   d[ ,test2] <- sapply(d[ ,test2], as.numeric)
        # }

        if (nrow(d) == 0) {
          Code_Data$name <- NULL
          return(NULL)
        }

        closeAlert(session, alertId = "alert1")
        closeAlert(session, alertId = "alert2")
        closeAlert(session, alertId = "alert3")
        closeAlert(session, alertId = "alert4")
        closeAlert(session, alertId = "alert5")

        if (my_data & !upload_data & !exampleDat) {
            Code_Data$name <- name
            return(d)
        } else if (ready & upload_data & !my_data & !exampleDat) {
            Code_Data$name <- Code_Data$upload
            return(d)
        } else if (exampleDat & !upload_data & !my_data) {
            Code_Data$name <- Code_Data$example
            return(d)
        } else {
            Code_Data$name <- NULL
            return(NULL)
        }

    } else {
      Code_Data$name <- NULL
      return(NULL)
    }
  })


  observe({
      if (is.null(plotData())) {
          updateAceEditor(session, editorId = "print_code_sc",  value = "")
          updateAceEditor(session, editorId = "print_code_hi",  value = "")
          updateAceEditor(session, editorId = "print_code_ba",  value = "")
          updateAceEditor(session, editorId = "print_code_box", value = "")
      }
  })


  observe({

    if (!input$exampleData & !input$my_data & !input$upload_data) {

      createAlert(session,
                 anchorId = "dataAlert1",
                 alertId = "alert1",
                 title = "Please provide a data frame with at least two variables",
                 content = "",
                 dismiss = FALSE)
      createAlert(session,
                  anchorId = "dataAlert2",
                  alertId = "alert2",
                  title = "Please provide a data frame with at least two variables",
                  content = "",
                  dismiss = FALSE)
      createAlert(session,
                  anchorId = "dataAlert3",
                  alertId = "alert3",
                  title = "Please provide a data frame with at least two variables",
                  content = "",
                  dismiss = FALSE)
      createAlert(session,
                  anchorId = "dataAlert4",
                  alertId = "alert4",
                  title = "Please provide a data frame with at least two variables",
                  content = "",
                  dismiss = FALSE)
      createAlert(session,
                  anchorId = "dataAlert5",
                  alertId = "alert5",
                  title = "Please provide a data frame with at least two variables",
                  content = "",
                  dismiss = FALSE)
    }
  })


  observe({

      if (input$upload_data & !input$readyButton) {

          if (!input$exampleData & !input$my_data & !is.null(uploadedData())) {
              createAlert(session,
                          anchorId = "dataAlert2_upload",
                          alertId = "alert2_upload",
                          title = "If your uploaded dataset is ready to visualise then press the red button in the upload section",
                          content = "",
                          dismiss = FALSE)
              createAlert(session,
                          anchorId = "dataAlert3_upload",
                          alertId = "alert3_upload",
                          title = "If your uploaded dataset is ready to visualise then press the red button in the upload section",
                          content = "",
                          dismiss = FALSE)
              createAlert(session,
                          anchorId = "dataAlert4_upload",
                          alertId = "alert4_upload",
                          title = "If your uploaded dataset is ready to visualise then press the red button in the upload section",
                          content = "",
                          dismiss = FALSE)
              createAlert(session,
                          anchorId = "dataAlert5_upload",
                          alertId = "alert5_upload",
                          title = "If your uploaded dataset is ready to visualise then press the red button in the upload section",
                          content = "",
                          dismiss = FALSE)
          }

      } else {

              closeAlert(session, alertId = "alert2_upload")
              closeAlert(session, alertId = "alert3_upload")
              closeAlert(session, alertId = "alert4_upload")
              closeAlert(session, alertId = "alert5_upload")
      }
  })


  # ----------------------------- SCATTERPLOT SECTION --------------------------

  observe({
    if (!is.null( plotData() )){
      updateSelectInput(session, inputId = "x_input_sc",
                        choices =  names(plotData()),
                        selected = names(plotData())[1])
    }
    else {
      updateSelectInput(session, inputId = "x_input_sc",
                        choices =  "none",
                        selected = "none")
    }
  })


  output$dynamic_range_x <- renderUI({

    if (!is.null(plotData())) {

      inX <- input$x_input_sc
      var_x <- plotData()[ ,inX]

      if (is.numeric(var_x) | inherits(var_x, "Date") | inherits(var_x, "POSIXct")) {

        min_x <- min(var_x)
        max_x <- max(var_x)

        return(sliderInput(inputId = "x_range_sc",
                    label = paste0("Range of '", input$x_input_sc,"':"),
                    min = min_x,
                    max = max_x,
                    value = c(min_x, max_x),
                    step = (max_x - min_x) / 100))
      }

      if (is.factor(var_x)) {

        lev <- levels(var_x)

        if (length(lev) < 7) {
          return(checkboxGroupInput(inputId = "x_range_sc_factor",
                                    label = "Levels:",
                                    choices = lev,
                                    selected = lev))
        } else {
          return(selectInput(inputId = "x_range_sc_factor",
                             label = "Levels:",
                             choices = lev,
                             selected = lev,
                             multiple = TRUE))
        }
      }
    } else {
      return(NULL)
    }
  })


  observe({
    plotData <- plotData()
    if (!is.null(plotData)) {
      updateSelectInput(session, inputId = "y_input_sc",
                        choices =  names(plotData),
                        selected = names(plotData)[2])
    } else {
      updateSelectInput(session, inputId = "y_input_sc",
                        choices =  "none",
                        selected = "none")
    }
  })


  output$dynamic_range_y <- renderUI({

    if (!is.null(plotData())) {

      inY <- input$y_input_sc
      var_y <- plotData()[ ,inY]

      if (is.numeric(var_y) | inherits(var_y, "Date") | inherits(var_y, "POSIXct")) {

        min_y <- min(var_y)
        max_y <- max(var_y)

        return(sliderInput(inputId = "y_range_sc",
                           label = paste0("Range of '", input$y_input_sc,"':"),
                           min = min_y,
                           max = max_y,
                           value = c(min_y, max_y),
                           step = (max_y - min_y) / 100))
      }

      if (is.factor(var_y)) {

        lev <- levels(var_y)

        if (length(lev) < 7) {
          return(checkboxGroupInput(inputId = "y_range_sc_factor",
                                    label = "Levels:",
                                    choices = lev,
                                    selected = lev))
        } else {
          return(selectInput(inputId = "y_range_sc_factor",
                             label = "Levels:",
                             choices = lev,
                             selected = lev,
                             multiple = TRUE))
        }
      }
    } else {
      return(NULL)
    }
  })



  output$dyn_palette_sc <- renderUI({

    if (input$change_color_sc == "Colour by") {
      var <- input$color_by_sc

      if (var != "none" & var != "") {

        if (is.numeric(dataScatter()[ ,var])) {

          observeEvent(input$reset_count_sc, {
            updateColourInput(session, inputId = "low_sc" , value = "#132B43")
            updateColourInput(session, inputId = "high_sc", value = "#56B1F7")
          })

          return(list(colourInput(inputId = "high_sc", label = "Gradient high:",
                                  showColour = "background",
                                  value = "#56B1F7"),
                      colourInput(inputId = "low_sc", label = "Gradient low:",
                                  showColour = "background",
                                  value = "#132B43"),

                      actionButton(inputId = "reset_count_sc", "Reset colour:")))


        }
        if (is.factor(dataScatter()[ ,var])) {

          return(selectInput(inputId = "colour_by_type_sc", label = "Type:",
                             choices = c("Default" = "default", "Qualitative" = "qual",
                                         "Sequential" = "seq", "Diverging" = "div")))
        }
      } else {
        return()
      }
    } else {
      return()
    }
  })


  output$dyn_palette_sc2 <- renderUI({

    if (input$change_color_sc == "Colour by") {

      if (input$color_by_sc != "none" & input$color_by_sc != "") {

        if (is.factor(dataScatter()[ ,input$color_by_sc]) & !is.null(input$colour_by_type_sc)) {

          if (input$colour_by_type_sc == "default") {
            return()
          }

          if (input$colour_by_type_sc == "qual") {
            return(sliderInput(inputId = "palette_sc", label = "Palette",
                               min = 1, max = 8, value = 1, step = 1))
          }
          if (input$colour_by_type_sc == "div") {
            return(sliderInput(inputId = "palette_sc", label = "Palette",
                               min = 1, max = 9, value = 1, step = 1))
          }
          else {
            return(sliderInput(inputId = "palette_sc", label = "Palette",
                               min = 1, max = 18, value = 1, step = 1))
          }
        }
      }
    }

  })


  # Title
  observe({
    toggle(id = "show_title_widgets_sc", anim = TRUE,
           time = 0.3, condition = input$add_title_sc)
  })

  # Theme
  observe({
    toggle(id = "show_theme_widgets_sc", anim = TRUE,
           time = 0.3, condition = input$change_theme_sc)
  })

  # Plot size
  observe({
    toggle(id = "show_size_sc", anim = TRUE,
           time = 0.3, condition = input$ch_size_sc)
  })

  # SEED
  Seed <- NULL
  observeEvent(input$point_app_sc == "Jitter", {
    Seed <<- sample(1:5000, 1)
  })




  # FACETS:
  #Grid
  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "x_facet_sc",
                      choices = c("none" = ".", vars))

    updateSelectInput(session, inputId = "y_facet_sc",
                      choices = c("none" = ".", vars))

    if (input$faceting_sc == "none" | input$faceting_sc == "Wrap") {
      updateSelectInput(session, inputId = "x_facet_sc",
                        choices = c("none" = ".", vars),
                        selected = ".")

      updateSelectInput(session, inputId = "y_facet_sc",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })

  # Wrap
  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "wrap_sc",
                      choices = c("none" = ".", vars))

    if (input$faceting_sc == "none" | input$faceting_sc == "Grid" ) {
      updateSelectInput(session, inputId = "wrap_sc",
                        choices = c("none" = ".", vars),
                        selected = ".")
      updateSelectInput(session, inputId = "wrap_sc2",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })

  observe ({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    if (input$wrap_sc == ".") {
      updateSelectInput(session, inputId = "wrap_sc2",
                        choices = c("none" = "."))
    }
    else {
      vars <- vars[!is.element(vars, input$wrap_sc)]
      updateSelectInput(session, inputId = "wrap_sc2",
                        choices = c("none" = ".", vars))
    }
  })

  # # LOG X
  # output$dyn_x_log_sc <- renderUI({
  #
  #   validate(
  #     need(is.numeric(dataScatter()[ ,input$x_input_sc]),
  #          "'log' is not meaningful for factors/dates/datetimes/logicals
  #          "))
  #   checkboxInput(inputId = "x_log_sc", label = "X-log:",
  #                 value = FALSE)
  # })
  #
  # output$dyn_y_log_sc <- renderUI({
  #
  #   validate(
  #     need(is.numeric(dataScatter()[ ,input$y_input_sc]),
  #          "'log' is not meaningful for factors/dates/datetimes/logicals
  #          ")
  #     )
  #   checkboxInput(inputId = "y_log_sc", label = "Y-log:",
  #                 value = FALSE)
  # })

  # ERRORBARS DYNAMIC SECTION

  output$errbar_out_sc <- renderUI({

    plot_data <- plotData()

    if (!is.null(plot_data)) {

      if (is.factor(plot_data[ ,input$x_input_sc]) & !is.factor(plot_data[ ,input$y_input_sc])) {

        return(radioButtons(inputId = "errbar_sc",
                     label = "Errorbars:",
                     choices = c("none" = "none", "Mean +- SE" = "SE", "95% CI" = "95"),
                     selected = "none"))
      } else {
        return(radioButtons(inputId = "errbar_sc",
                     label = "Errorbars:",
                     choices = c("none"),
                     selected = "none"))
      }
    } else {
      return(radioButtons(inputId = "errbar_sc",
                          label = "Errorbars:",
                          choices = c("none"),
                          selected = "none"))
    }
  })

  # LOESS DYNAMIC SECTION:

  output$loess_out_sc <- renderUI({
    plot_data <- plotData()
    if (!is.null(plot_data)) {

      if (!is.factor(plot_data[ ,input$x_input_sc]) & !is.factor(plot_data[ ,input$y_input_sc]) ){
        return(radioButtons(inputId = "loess_sc",
                     label = "Loess",
                     choices = c("none", "Loess", "Loess + SE"),
                     selected = "none"))
      }
      else {
        return(radioButtons(inputId = "loess_sc",
                     label = "Loess",
                     choices = c("none"),
                     selected = "none"))
      }
    }
    else {
      return(radioButtons(inputId = "loess_sc",
                          label = "Loess",
                          choices = c("none"),
                          selected = "none"))
    }
  })


  # DYNAMIC LABELS:
  observe({
    updateTextInput(session, inputId = "label_x_sc",
                    value = input$x_input_sc)
  })

  observe({
    updateTextInput(session, inputId = "label_y_sc",
                    value = input$y_input_sc)
  })

  # POINTS APPERANCE
  observe({

    plot_data <- plotData()
    vars <- names(plot_data)[sapply(plot_data, is.factor)]
    vars_num <- names(plot_data)[sapply(plot_data, is.numeric)]

    # POINTSIZE
    updateSelectInput(session,
                      inputId = "point_size_by_sc",
                      choices = vars_num)
    # SHAPE
    updateSelectInput(session, inputId = "shape_sc",
                      choices = c("none", vars),
                      selected = "none")
    # COLOUR
    updateSelectInput(session,
                      inputId = "color_by_sc",
                      choices = c("none", names(plot_data)),
                      selected = "none")
  })


  # RESET BUTTON FOR SIZE OF THE PLOT
  observeEvent(input$reset_sc, {
    updateSliderInput(session, "width_sc" , "Plot Width (px)", value = 700)
    updateSliderInput(session, "height_sc", "Plot Height (px)", value = 500)
  })


  Subset <- reactiveValues()

  dataScatter <- reactive({

    if (!is.null(plotData() )) {

        if (input$show_range_sc) {

          data_sc <- plotData()

          inX <- input$x_input_sc
          inY <- input$y_input_sc
          var_x <- data_sc[ ,inX]
          var_y <- data_sc[ ,inY]

          if (is.numeric(var_y)) {
            min_y <- min(var_y)
            max_y <- max(var_y)
          }

          len1 <- length(levels(var_x))
          len2 <- length(levels(var_y))

          is_date_x <- is.date(var_x)
          is_datetime_x <- is.datetime(var_x)
          is_numeric_logical_x <- is.numeric_logical(var_x)

          is_date_y <- is.date(var_y)
          is_datetime_y <- is.datetime(var_y)
          is_numeric_logical_y <- is.numeric_logical(var_y)

          if (is.numeric(var_x) | is_date_x | is_datetime_x) {

            rangeX <- input$x_range_sc
            Subset$x <- NULL

            if (!is.null(rangeX[1]) & !is.null(rangeX[2])) {

              # Don't allow taking a log of a Date/Datetime
              if (!input$x_log_sc | (input$x_log_sc & (is_date_x | is_datetime_x | is_numeric_logical_x))) {

                Subset$logx <- NULL
                Code$range_xfac <- NULL

                if (rangeX[1] != min(var_x) | rangeX[2] != max(var_x)) {

                  Code$logx <- NULL

                  # if Date
                  if (is_date_x) {

                    Subset$x <- paste0(input$x_input_sc, " >= '", rangeX[1], "' & ",
                                       input$x_input_sc, " <= '", rangeX[2], "'")
                    Code$range1 <- paste0(" + \n  scale_x_date(limits = as.Date(c('", rangeX[1], "', '", rangeX[2], "')))")

                  } else if (is_datetime_x) {

                      Subset$x <- paste0(input$x_input_sc, " >= '", rangeX[1], "' & ",
                                         input$x_input_sc, " <= '", rangeX[2], "'")
                      Code$range1 <- paste0(" + \n  scale_x_datetime(limits = as.POSIXct(c('", rangeX[1], "', '", rangeX[2], "')))")

                  # if numeric
                  } else {
                    Subset$x <- paste0(input$x_input_sc, " >= ", rangeX[1], " & ",
                                       input$x_input_sc, " <= ", rangeX[2])
                    Code$range1 <- paste0(" + \n  scale_x_continuous(limits = c(", rangeX[1], ", ", rangeX[2], "))")
                  }

                } else {

                  Code$logx <- NULL
                  Code$range_xfac <- NULL
                  Code$range1 <- NULL

                }

              } else { # taking a log

                Subset$logx <- paste0(" + \n  scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                       ", labels = trans_format('log10', math_format(10^.x)))")

                if (rangeX[1] != min(var_x) | rangeX[2] != max(var_x)) {

                  Subset$x <- paste0(input$x_input_sc, " >= ", rangeX[1], " & ",
                                     input$x_input_sc, " <= ", rangeX[2])
                  Code$range_xfac <- NULL
                  Code$logx <- NULL

                  Code$range1 <- paste0(" + \n  scale_x_continuous(limits = c(", rangeX[1], ", ", rangeX[2], ")",
                                         ", breaks = trans_breaks('log10', function(x) 10^x),\n",
                                         "                     ",
                                         "labels = trans_format('log10', math_format(10^.x))) ")
                } else {
                  Subset$x <- NULL
                  Code$range_xfac <- NULL
                  Code$range1 <- NULL
                  Code$logx <- paste0(" + \n  scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                       ", labels = trans_format('log10', math_format(10^.x)))")

                }
              }
            }
            data_sc <- data_sc[var_x >= rangeX[1] & var_x <= rangeX[2], ]
          }



          if ((is.numeric(var_y) | is_date_y | is_datetime_y | is_numeric_logical_y) & nrow(data_sc) > 1) {

            rangeY <- input$y_range_sc
            Subset$y <- NULL


             if (!is.null(rangeY[1]) & !is.null(rangeY[2])) {


              # Don't allow taking a log of a Date/Datetime
              if (!input$y_log_sc | (input$y_log_sc & (is_date_y | is_datetime_y | is_numeric_logical_y))) {

                Subset$logy <- NULL
                Code$range_yfac <- NULL

                if (rangeY[1] != min_y | rangeY[2] != max_y) {

                  Code$logy <- NULL

                  # if Date
                  if (is_date_y) {

                      Subset$y <- paste0(input$y_input_sc, " >= '", rangeY[1], "' & ",
                                         input$y_input_sc, " <= '", rangeY[2], "'")
                      Code$range2 <- paste0(" + \n  scale_y_date(limits = as.Date(c('", rangeY[1], "', '", rangeY[2], "')))")

                    } else if (is_datetime_x) {

                      Subset$y <- paste0(input$y_input_sc, " >= '", rangeY[1], "' & ",
                                         input$y_input_sc, " <= '", rangeY[2], "'")
                      Code$range2 <- paste0(" + \n  scale_y_datetime(limits = as.POSIXct(c('", rangeY[1], "', '", rangeY[2], "')))")

                    # if numeric
                    } else {
                      Subset$y <- paste0(input$y_input_sc, " >= ", rangeY[1], " & ",
                                         input$y_input_sc, " <= ", rangeY[2])
                      Code$range2 <- paste0(" + \n  scale_y_continuous(limits = c(", rangeY[1], ", ", rangeY[2], "))")
                    }

                } else {

                  Subset$y <- NULL
                  Code$range_yfac <- NULL
                  Code$range2 <- NULL
                  Code$logy <- NULL
                }

              } else {

                Subset$logy <- paste0(" + \n  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                       ", labels = trans_format('log10', math_format(10^.x)))")
                if (rangeY[1] != min_y | rangeY[2] != max_y) {
                  Subset$y <- paste0(input$y_input_sc, " >= ", rangeY[1], " & ",
                                     input$y_input_sc, " <= ", rangeY[2])
                  Code$range_yfac <- NULL
                  Code$logy <- NULL

                  Code$range2 <- paste0(" + \n  scale_y_continuous(limits = c(", rangeY[1], ", ", rangeY[2], ")",
                                         ", breaks = trans_breaks('log10', function(x) 10^x),\n",
                                         "                     ",
                                         "labels = trans_format('log10', math_format(10^.x))) ")
                }
                else {
                  Subset$y <- NULL
                  Code$range_yfac <- NULL
                  Code$range2 <- NULL
                  Code$logy <- paste0(" + \n  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                       ", labels = trans_format('log10', math_format(10^.x)))")

                }
              }
            }
            data_sc <- data_sc[var_y >= rangeY[1] & var_y <= rangeY[2], ]
          }


          if (is.factor(var_x)) {
            Code$logx <- NULL
            Subset$logx <- NULL
            lev_x <- input$x_range_sc_factor

            cond1 <- length(lev_x) != len1
            if (cond1) {

              if (length(lev_x) > 1) {
                Code$range1 <- NULL
                Code$range_xfac <- paste0(" + \n  scale_x_discrete(limits = c(", "'",
                                           paste(lev_x, collapse = "', '"), "'", "))")
                Subset$x <- paste0(inX, " %%in%% ", "c('", paste(lev_x, collapse = "', '"), "')")
              }
              if (length(lev_x) == 1) {
                Code$range1 <- NULL
                Code$range_xfac <- paste0(" + \n  scale_x_discrete(limits = ", "'",
                                           paste(lev_x), "'", ")")
                Subset$x <- paste0(inX, " == ", "'", paste(lev_x), "'")
              }
            }
            if (!cond1) {
              Code$range1 <- NULL
              Code$range_xfac <- NULL
              Subset$x <-NULL
            }
            data_sc <- data_sc[var_x %in% lev_x, ]
          }

          if (is.factor(var_y)) {
            Code$logy <-NULL
            Subset$logy <- NULL
            lev_y <- input$y_range_sc_factor
            cond2 <- length(lev_y) != len2
            if (cond2) {

              if (length(lev_y) > 1) {
                Code$range2 <- NULL
                Subset$y <- paste0(inY, " %%in%% ", "c('", paste(lev_y, collapse = "', '"), "')")
                Code$range_yfac <- paste0(" + \n  scale_y_discrete(limits = c(", "'",
                                           paste(lev_y, collapse = "', '"), "'", "))")
              }
              if (length(lev_y) == 1) {
                Code$range2 <- NULL
                Subset$y <- paste0(inY, " == ", "'", paste(lev_y), "'")
                Code$range_yfac <- paste0(" + \n  scale_y_discrete(limits = ", "'",
                                           paste(lev_y), "'", ")")
              }
            }
            if (!cond2) {
              Code$range2 <- NULL
              Code$range_yfac <- NULL
              Subset$y <- NULL
            }
            data_sc <- data_sc[var_y %in% lev_y, ]
          }

          return(data_sc)

        } else {

          Subset$x <- NULL  ; Code$range1 <- NULL
          Subset$y <- NULL  ; Code$range2 <- NULL

          Code$range_xfac <- NULL ; Code$range_yfac <- NULL

          Code$logx <- NULL ; Code$logy <- NULL


          data_sc <- plotData()
          var_x <- data_sc[ ,input$x_input_sc]
          var_y <- data_sc[ ,input$y_input_sc]

          if (input$x_log_sc & !any(c(is.factor(var_x),
                                      is.date(var_x),
                                      is.datetime(var_x),
                                      is.numeric_logical(var_x)))) {

            Code$logx <- paste0(" + \n  scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                 ", labels = trans_format('log10', math_format(10^.x)))")
          }
          if (input$y_log_sc & !any(c(is.factor(var_y),
                                      is.date(var_y),
                                      is.datetime(var_y),
                                      is.numeric_logical(var_y)))) {

            Code$logy <- paste0(" + \n  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                 ", labels = trans_format('log10', math_format(10^.x)))")
          }
          return(plotData())
        }
    }
  })


  # PLOT -----------------------------------------------------------------------
  output$scatter_ui <- renderUI ({

    plotOutput(outputId = "Scatterplot",
               height = input$height_sc,
               width = input$width_sc,
               click = "scatter_click",
               dblclick = "scatter_dblclick",
               brush = brushOpts(id = "scatter_brush", resetOnNew = TRUE
               ))


  })

  output$Scatterplot <- renderPlot({

    dataScatter <- dataScatter()

    if (!is.null(dataScatter))  {

      pl <- ggplot(dataScatter,
                   aes(x = .data[[input$x_input_sc]],
                       y = .data[[input$y_input_sc]]))


      Size <- ifelse(input$point_size_sc != 2, paste0(", size = ", input$point_size_sc), "")

      Alpha <- ifelse(input$opacity_sc != 1, paste0(", alpha = ", input$opacity_sc), "")

      Colour <- ifelse(input$color_sc == '#000000' | input$color_sc == 'black', "", paste0(", colour = ", "'", input$color_sc, "'"))

      Aes_size <- NULL ; Aes_col <- NULL ; Aes_shape <- NULL


      # ERRORBARS

      if (!is.null(input$errbar_sc) & input$errbar_sc == "SE") {
        pl <- pl + stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),
                                geom = "errorbar", color = input$err_col_sc,
                                width = input$err_width_sc)  +

          stat_summary(fun.y = mean, colour = input$err_col_sc, geom = "point",
                       size = input$err_size_sc)


        Code$errbar <- paste0(" + \n  stat_summary(fun.data = 'mean_cl_normal', fun.args = list(mult = 1), geom = 'errorbar'",
                              ", colour = ", "'", input$err_col_sc, "'", ", width = ",input$err_width_sc, ")",
                              " + \n",
                              "  stat_summary(fun.y = mean, colour = ",
                              "'", input$err_col_sc, "'",
                              ", geom = 'point', size = ", input$err_size_sc, ")")
      }

      if (input$errbar_sc == "95") {
        pl <- pl + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                                color = input$err_col_sc, width = input$err_width_sc)  +

          stat_summary(fun.y = mean, colour = input$err_col_sc, geom = "point",
                       size = input$err_size_sc)

        Code$errbar <- paste0(" + \n  stat_summary(fun.data = 'mean_cl_normal', geom = 'errorbar', colour = ",
                              "'", input$err_col_sc, "'", ", width = ", input$err_width_sc, ")",
                              " + \n",
                              "  stat_summary(fun.y = mean, colour = ", "'", input$err_col_sc, "'",
                              ", geom = 'point', size = ", input$err_size_sc, ")")

      }

      if (input$errbar_sc == "none") {
        Code$errbar <- NULL
      }


      if (input$point_app_sc != "Jitter") {

        if (input$point_app_sc == "none") {
          Unique <- "identity"
          Unique2 <- ""
        }
        if (input$point_app_sc == "Remove duplicates") {
          Unique <- "unique"
          Unique2 <- ", stat = 'unique'"
        }


        if (input$change_point_size_sc == "Size") {

          if (input$change_color_sc == "Colour") {
            pl <- pl + geom_point(size = input$point_size_sc, alpha = input$opacity_sc,
                                  color = input$color_sc, stat = Unique)

            Code$points <- paste0(" + \n  geom_point(", syntax(c(Unique2, Colour, Size, Alpha)), ")")

          } else {
            pl <- pl + geom_point(size = input$point_size_sc, alpha = input$opacity_sc,
                                  stat = Unique)

            Code$points <- paste0(" + \n  geom_point(", syntax(c(Unique2, Size, Alpha)), ")")
          }
        }


        if (input$change_point_size_sc == "Size by") {

          if (input$change_color_sc == "Colour") {
            pl <- pl + geom_point(aes(size = .data[[input$point_size_by_sc]]),
                                  alpha = input$opacity_sc, color = input$color_sc,
                                  stat = Unique)

            Code$points <- paste0(" + \n  geom_point(", syntax(c(Unique2, Colour, Alpha)), ")")

            Aes_size <- paste0(", size = ", input$point_size_by_sc, "")
          }

          else {
            pl <- pl + geom_point(aes(size = .data[[input$point_size_by_sc]]),
                                  alpha = input$opacity_sc,
                                  stat = Unique)

            Code$points <- paste0(" + \n  geom_point(", syntax(c(Unique2, Alpha)), ")")


            Aes_size <- paste0(", size = ", input$point_size_by_sc, "")
          }
        }
      }

      # JITTER ------------------------------------------------------------------------

      Code$seed <- NULL

      if (input$point_app_sc == "Jitter") {

        Code$seed <- paste0("\n# Set seed for reproducibility\nset.seed(", Seed, ")\n")
        set.seed(Seed)

        if (input$change_point_size_sc == "Size") {

          if (input$change_color_sc == "Colour") {

            pl <- pl + geom_jitter(size = input$point_size_sc, alpha = input$opacity_sc,
                                   colour = input$color_sc)

            Code$points <- paste0(" + \n  geom_jitter(", syntax(c(Colour, Size, Alpha)), ")")
          }
          else {
            pl <- pl + geom_jitter(size = input$point_size_sc, alpha = input$opacity_sc)

            Code$points <- paste0(" + \n  geom_jitter(", syntax(c(Size, Alpha)), ")")
          }
        }


        if (input$change_point_size_sc == "Size by") {

          if (input$change_color_sc == "Colour") {
            pl <- pl + geom_jitter(aes(size = .data[[input$point_size_by_sc]]),
                                   alpha = input$opacity_sc, color = input$color_sc)

            Code$points <- paste0(" + \n  geom_jitter(", syntax(c(Colour, Alpha)), ")")


            Aes_size <- paste0(", size = ", input$point_size_by_sc, "")
          }

          else {
            pl <- pl + geom_jitter(aes(size = .data[[input$point_size_by_sc]]),
                                   alpha = input$opacity_sc)

            Code$points <- paste0(" + \n  geom_jitter(", syntax(Alpha), ")")

            Aes_size <- paste0(", size = ", input$point_size_by_sc, "")
          }
        }
      }


      if (input$shape_sc != "none") {
        pl <- pl + aes(shape = .data[[input$shape_sc]])

        Aes_shape <- paste0(", shape = ", input$shape_sc)
      }


      if (input$shape_sc == "none") {
        Aes_shape <- ""
      }


      if (input$change_color_sc == "Colour by" & input$color_by_sc != "none") {

        pl <- pl + aes(color = .data[[input$color_by_sc]])

        Aes_col <- paste0(", colour = ", input$color_by_sc)


        if (is.factor(dataScatter[ ,input$color_by_sc])) {

          if (!is.null(input$colour_by_type_sc)) {

            if (input$colour_by_type_sc != "default" ) {

              if(!is.null(input$palette_sc)) {

                pl <- pl + scale_colour_brewer(type = input$colour_by_type_sc, palette = input$palette_sc)


                Code$gradient <- paste0(" + \n  scale_colour_brewer(type = ", "'",input$colour_by_type_sc, "'",
                                         ", palette = ", input$palette_sc, ")")
              }
            }
            if (input$colour_by_type_sc == "default") {
              Code$gradient <- NULL
            }
          }
        }
        if (is.numeric(dataScatter[ ,input$color_by_sc])) {

          if (!is.null(input$low_sc) & !is.null(input$high_sc)) {
            pl <- pl + scale_colour_gradient(low = input$low_sc, high = input$high_sc)


            Code$gradient <- ifelse(input$low_sc != '#132B43' | input$high_sc != '#56B1F7',
                                     paste0(" + \n  scale_colour_gradient(low = ", "'",input$low_sc,"'",
                                     ", high = ", "'",input$high_sc,"'", ")"), "")
          }
        }

      }

      # Find out if x and y are logical variables encoded by 0-1 (numeric 0,1)
      x_var_sc <- dataScatter[ ,input$x_input_sc]
      y_var_sc <- dataScatter[ ,input$y_input_sc]
      is_logical_numeric_x <- is.numeric_logical(x_var_sc)
      is_logical_numeric_y <- is.numeric_logical(y_var_sc)

      var_x_code <- ifelse(is_logical_numeric_x,
                           paste0("as.integer(" , input$x_input_sc, ")"),
                           input$x_input_sc)
      var_y_code <- ifelse(is_logical_numeric_y,
                           paste0("as.integer(" , input$y_input_sc, ")"),
                           input$y_input_sc)

      Aes <- paste0(Aes_col, Aes_size, Aes_shape)
      Code$AES <- Aes
      Code$gg <- paste0("\n# Graph\n",
                        "ggplot(data = ", Code_Data$name,
                        ", aes(x = ", var_x_code,
                        ", y = ", var_y_code,
                        Aes, "))")



      if (input$change_color_sc == "Colour" | input$color_by_sc == "none") {
        Code$gradient <- NULL
      }


      if (input$loess_sc == "Loess + SE") {

        Col2 <-  input$loess_col2_sc
        Size2 <- input$loess_size2_sc
        Fill2 <- input$loess_fill2_sc
        Span2 <- input$loess_span2_sc

        Col2_b <-  ifelse(Col2 != '#3366FF', paste0(", colour = ", "'", Col2, "'"), "")
        Size2_b <- ifelse(Size2 != 1, paste0(", linewidth = ", Size2), "")
        Span2_b <- ifelse(Span2 != 0.75, paste0(", span = ", Span2), "")
        Fill2_b <- ifelse(Fill2 != 'gray60', paste0(", fill = ", "'", Fill2, "'"), "")

        cond <- FALSE
        if (input$color_by_sc != "none") {
          cond <- is.factor(plotData()[ ,input$color_by_sc]) & input$change_color_sc == "Colour by"
        }
        if (cond) {
          pl <- pl + stat_smooth(fill = Fill2,
                                 linewidth = Size2,
                                 span = Span2)

          Code$smooth <- paste0(" + \n  stat_smooth(", syntax(c(Fill2_b, Size2_b, Span2_b)), ")")

        } else {
          pl <- pl + stat_smooth(colour = Col2,
                                 fill = Fill2,
                                 linewidth = Size2,
                                 span = Span2)

          Code$smooth <- paste0(" + \n  stat_smooth(", syntax(c(Fill2_b, Col2_b, Size2_b, Span2_b)), ")")
        }
      }


      if (input$loess_sc == "Loess") {

        Col1 <-  ifelse(input$loess_col1_sc != '#3366FF', paste0(", colour = ", "'",input$loess_col1_sc,"'"), "")
        Size1 <- ifelse(input$loess_size1_sc != 1, paste0(", linewidth = ", input$loess_size1_sc), "")
        Span1 <- ifelse(input$loess_span1_sc != 0.75, paste0(", span = ", input$loess_span1_sc),"")

        cond <- FALSE
        if (input$color_by_sc != "none") {
          cond <- is.factor(plotData()[ ,input$color_by_sc]) & input$change_color_sc == "Colour by"
        }
        if (cond) {
          pl <- pl + stat_smooth(se = FALSE,
                                 linewidth = input$loess_size1_sc,
                                 span = input$loess_span1_sc)

          Code$smooth <- paste0(" + \n  stat_smooth(se = FALSE", Size1, Span1, ")")
        }
        else {
          pl <- pl + stat_smooth(se = FALSE,
                                 linewidth = input$loess_size1_sc,
                                 span = input$loess_span1_sc,
                                 colour = input$loess_col1_sc)

          Code$smooth <- paste0(" + \n  stat_smooth(se = FALSE", Col1, Size1, Span1, ")")
        }

      }

      if (input$loess_sc == "none") {
        Code$smooth <- NULL
      }




      if (input$coord_flip_sc) {
        validate(
          need(!(input$faceting_sc == "Wrap" & input$scales_sc != "fixed"),
               "ggplot2 does not currently support free scales with
               a non-cartesian coord or coord_flip."))

        pl <- pl + coord_flip()

        Code$flip <- paste0(" + \n  coord_flip()")
      }


      if (!input$coord_flip_sc) {
        Code$flip <- NULL
      }


      # Grid
      facets_sc <- paste(input$x_facet_sc, "~", input$y_facet_sc)

      if (input$x_facet_sc != "." & input$y_facet_sc != ".") {
        validate(
          need(input$x_facet_sc != input$y_facet_sc,
               "Faceting variables can only appear in `rows` or `cols` not both.")
          )
      }


      Scales_sc <- input$scales_sc
      print_scales_sc  <- paste0(", scales = ", "'", Scales_sc , "'")
      if (Scales_sc == "fixed") {
        print_scales_sc <- ""
      }

      if (facets_sc == ". ~ .") {
        Code$grid <- NULL
      }

      if (facets_sc != ". ~ .") {
        pl <- pl + facet_grid(facets_sc, scales = Scales_sc)

        Code$grid <- paste0(" + \n  facet_grid(", facets_sc, print_scales_sc, ")")
      }


      if (input$wrap_sc == "." & input$wrap_sc2 == ".") {
        Code$wrap <- NULL
      }


      if (input$wrap_sc != "." & input$wrap_sc2 == ".") {
        pl <- pl + facet_wrap(as.formula(paste("~", input$wrap_sc)), scales = Scales_sc)

        Code$wrap <- paste0(" + \n  facet_wrap(~ ", input$wrap_sc, print_scales_sc, ")")

      }

      if (input$wrap_sc != "." & input$wrap_sc2 != ".") {
        pl <- pl + facet_wrap(as.formula(paste("~", input$wrap_sc, "+", input$wrap_sc2)),
                              scales = Scales_sc)

        Code$wrap <- paste0(" + \n  facet_wrap(~ ", input$wrap_sc, " + ", input$wrap_sc2,
                             print_scales_sc, ")")
      }

      if (!input$x_log_sc & !input$y_log_sc) {
        Code$scales <- NULL
      }

      if (input$x_log_sc & is.numeric(x_var_sc) & !is.numeric_logical(x_var_sc)) {

        pl <- pl + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x)))
        Code$scales <- "\nlibrary(scales)"
      }

      if (input$y_log_sc & is.numeric(y_var_sc) & !is.numeric_logical(y_var_sc)) {

        pl <- pl + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                 labels = trans_format("log10", math_format(10^.x)))
        Code$scales <- "\nlibrary(scales)"
      }

      # LABELS
      if (input$label_x_sc != input$x_input_sc) {
        pl <- pl + xlab(input$label_x_sc)

        Code$labx <- paste0(" + \n  xlab(", "'", input$label_x_sc, "'", ")")
      }

      if (input$label_x_sc == input$x_input_sc) {
        Code$labx <- NULL
      }

      if (input$label_y_sc != input$y_input_sc) {
        pl <- pl + ylab(input$label_y_sc)

        Code$laby <- paste0(" + \n  ylab(", "'", input$label_y_sc, "'", ")")
      }

      if (input$label_y_sc == input$y_input_sc) {
        Code$laby <- NULL
      }


      # THEME
      Theme_sc <- input$theme_sc
      Size_sc <- input$theme_size_sc
      Font_sc <- input$theme_font_sc

      if (Theme_sc == "Grey") {
        pl <- pl + theme_gray(Size_sc, Font_sc)
        Code$theme <- paste0(" + \n  theme_grey(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "White") {
        pl <- pl + theme_bw(Size_sc, Font_sc)
        Code$theme <- paste0(" + \n  theme_bw(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "Linedraw") {
        pl <- pl + theme_linedraw(Size_sc, Font_sc)
        Code$theme <- paste0(" + \n  theme_linedraw(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "Light") {
        pl <- pl + theme_light(Size_sc, Font_sc)
        Code$theme <- paste0(" + \n  theme_light(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "Minimal") {
        pl <- pl + theme_minimal(Size_sc, Font_sc)
        Code$theme <- paste0(" + \n  theme_minimal(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "Classic") {
        pl <- pl + theme_classic(Size_sc, Font_sc)
        Code$theme <- paste0(" + \n  theme_classic(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "Dark") {
          pl <- pl + theme_dark(Size_sc, Font_sc)
          Code$theme <- paste0(" + \n  theme_dark(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }
      if (Theme_sc == "Void") {
          pl <- pl + theme_void(Size_sc, Font_sc)
          Code$theme <- paste0(" + \n  theme_void(", Size_sc, ", ", "'",Font_sc,"'", ")")
      }


      if (Size_sc == 11 & Font_sc == "sans" & Theme_sc == "Grey") {
        Code$theme <- NULL
      }


      # LEGEND
      pl <- pl + theme(legend.position = input$legend_sc)

      if (input$legend_sc == "right") {
        Code$leg <- NULL
      }
      if (input$legend_sc != "right") {
        Code$leg <- paste0(" + \n  theme(legend.position = ", "'", input$legend_sc, "'", ")")
      }

      # TITLE
      if (input$title_sc != "") {
        pl <- pl + labs(title = input$title_sc) +
          theme(plot.title = element_text(colour = input$title_color_sc,
                                          size = input$title_size_sc, vjust = 1.5))
        Code$title <- paste0(" + \n  labs(title = ", "'", input$title_sc, "'", ")",
                             " + \n  theme(plot.title = element_text(colour = ", "'",
                             input$title_color_sc, "'", ", size = ", input$title_size_sc,
                             ", vjust = 1.5", "))")
      }

      if (input$title_sc == "") {
        Code$title <- NULL
      }

      # Zoom graph when dbl click on market region
      if (!is.null(sc_ranges$x) | !is.null(sc_ranges$y)) {

        xlim_ <- sc_ranges$x
        ylim_ <- sc_ranges$y

        xlim_ <- convert_numeric_to_datetime(convert_numeric_to_date(sc_ranges$x, is.date(x_var_sc)),
                                             is.datetime(x_var_sc))
        ylim_ <- convert_numeric_to_datetime(convert_numeric_to_date(sc_ranges$y, is.date(y_var_sc)),
                                             is.datetime(y_var_sc))

        pl <- pl + coord_cartesian(xlim = xlim_, ylim = ylim_)
      }


      updateAceEditor(session, editorId = "print_code_sc", value = plot_code_sc() )

      plots$pl <- pl
      pl
    }
  })

  plots <- reactiveValues()
  Code <- reactiveValues()


  plot_code_sc <- reactive({

    Subset$and <- ifelse(!is.null(Subset$x) & !is.null(Subset$y), " & ", "")

    if (!is.null(Subset$x) | !is.null(Subset$y)) {
      Subset$gg <- paste0("\n# Subset of data\n",
                          "sub.data <- subset(", Code_Data$name, ", ", Subset$x, Subset$and, Subset$y, ")",
                          "\n\n# Graph\n",
                          "ggplot(data = sub.data", ", aes(x = ", input$x_input_sc, ", y = ",
                          input$y_input_sc, Code$AES, "))")
      return(
        paste0(
          sprintf(
            paste0("library(ggplot2)", Code$scales, "\n", Code$load_data, Code$seed, Subset$gg, Code$errbar, Code$points,
                   Code$gradient, Code$smooth, Code$grid, Code$wrap, Code$flip, Subset$logx,
                   Subset$logy, Code$leg, Code$labx, Code$laby,
                   Code$theme, Code$title, "\n\n\n\n\n\n# Alternatively: ----------------------------\n",

                   Code$seed, Code$gg, Code$points,
                   Code$gradient, Code$smooth, Code$errbar, Code$grid, Code$wrap, Code$flip, Code$logx, Code$range1,
                   Code$logy, Code$range2, Code$range_xfac, Code$range_yfac, Code$labx, Code$laby,
                   Code$theme, Code$leg, Code$title)))
      )
    }

    if (is.null(Subset$x) & is.null(Subset$y)) {

      paste0(sprintf(paste0("library(ggplot2)", Code$scales, "\n", Code$load_data, Code$seed, Code$gg, Code$errbar, Code$points,
                            Code$gradient, Code$smooth, Code$grid, Code$wrap, Code$flip, Code$logx, Code$range1,
                            Code$logy, Code$range2, Code$range_xfac, Code$range_yfac, Code$labx, Code$laby,
                            Code$theme, Code$leg, Code$title)))
    }

  })


  # ZOOM:
  sc_ranges <- reactiveValues(x = NULL, y = NULL)

  observeEvent(input$scatter_dblclick, {
    brush <- input$scatter_brush
    if (!is.null(brush)) {
        sc_ranges$x <- c(brush$xmin, brush$xmax)
        sc_ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      sc_ranges$x <- NULL
      sc_ranges$y <- NULL
    }
  })


  output$brush_info_sc <- renderUI({

    plotData <- plotData()
    dataScatter <- dataScatter()

    if (!is.null(plotData)) {

      X <- input$x_input_sc
      Y <- input$y_input_sc
      inS <- input$scatter_brush

      mX <- NA ; sX <- NA ; mY <- NA ; sY <- NA ; N <- NA

      if (!is.null(plotData) & !is.null(inS) ) {

        var_x <- plotData[ ,X]
        var_y <- plotData[ ,Y]

        is_var_x_dt <- is.datetime(var_x)
        is_var_y_dt <- is.datetime(var_y)

        is_var_x_date <- is.date(var_x)
        is_var_y_date <- is.date(var_y)

        var_x_scaling_fct <- ifelse(is_var_x_dt, 8640000, 1)
        var_y_scaling_fct <- ifelse(is_var_y_dt, 8640000, 1)
        var_x_mean_rounding <- ifelse(is_var_x_dt, "secs", 4)
        var_y_mean_rounding <- ifelse(is_var_y_dt, "secs", 4)

        brushed_pts_x <- brushedPoints(dataScatter, inS, xvar = X, yvar = Y)[ ,X]
        brushed_pts_y <- brushedPoints(dataScatter, inS, xvar = X, yvar = Y)[ ,Y]

        N <- length(brushed_pts_x)

        if (is.numeric(var_x) | is_var_x_date | is_var_x_dt) {
          mX <- as.character(round(mean(brushed_pts_x), var_x_mean_rounding))
          sX <- round(sd(brushed_pts_x) / var_x_scaling_fct, 4)
        }

        if (is.numeric(var_y) | is_var_y_date | is_var_y_dt) {
          mY <- as.character(round(mean(brushed_pts_y), var_y_mean_rounding))
          sY <- round(sd(brushed_pts_y) / var_y_scaling_fct, 4)
        }
      }

      if (length(inS) == 0 || is.null(inS)) return()
      if ((is.na(mX) || mX == "NaN") & (is.na(mY) || mY == "NaN")) return()

      list(
        tags$div( HTML("N:"), paste("  ", N)),
        br(),
        tags$div( HTML("X&#772;: "), paste("  ", mX)),
        tags$div( HTML("&sigma;<sub>x</sub>:"), paste(sX)),
        br(),
        tags$div( HTML("Y&#772;: "), paste0("  ", mY)),
        tags$div( HTML("&sigma;<sub>y</sub>:"), paste0(sY)),

        tags$style(type = "text/css", "#brush_info_sc {
                   background-color: rgba(140,135,130,0.05);
                   border-color: rgba(25,5,2,0.5);
                   padding: 12px;
                   border-radius: 5px; }"))
    } else {
      return()
    }
  })

  output$click_info_sc <- renderUI({
    if (length(names(plotData())) > 1) {
      inS <- input$scatter_click
      inX <- input$x_input_sc
      inY <- input$y_input_sc
      near <- nearPoints(dataScatter(), inS, xvar = inX, yvar = inY)[ ,c(inX, inY)]
      if (is.null(inS) | is.null(near)) return()
      else if (nrow(near) == 0) return()
      else verbatimTextOutput(outputId = "click_info_sc2")
    }
    else {
      return()
    }

  })

  output$click_info_sc2 <- renderPrint( {
    inS <- input$scatter_click
    inX <- input$x_input_sc
    inY <- input$y_input_sc
    near <- nearPoints(dataScatter(), inS, xvar = inX, yvar = inY)[ ,c(inX, inY)]
    colnames(near) <- c("x", "y")
    near

  })


  # Download plot handler
  downloadGraphObserverServer("scatterplot")
  downloadGraphHandlerServer("scatterplot", plots$pl)


  # ----------------------------- HISTOGRAM SECTION ----------------------------


  observe({

    x_vars <- names(plotData())[sapply(plotData(), is.numeric)]

    if (!is.null( plotData()) & length(x_vars != 0)) {
      updateSelectInput(session, inputId = "x_input_hi",
                        choices = x_vars)
    }
    else {
      updateSelectInput(session, inputId = "x_input_hi",
                        choices =  "none",
                        selected = "none")
    }
  })


  output$dynamic_range_x_hi <- renderUI({

    if (!is.null(plotData())) {

      inX <- input$x_input_hi
      min_x = min(plotData()[ ,inX ])
      max_x = max(plotData()[ ,inX ])

      return(sliderInput(inputId = "x_range_hi",
                         label = paste0("Range of '", inX, "':"),
                         min = min_x, max = max_x, value = c(min_x, max_x),
                         step = (max_x - min_x) / 100))
    }
    else {
      return(sliderInput(inputId = "x_range_hi",
                         label = paste0("Range of '", "none", "':"),
                         min = -1, max = 1, value = c(-1,1)))
    }
  })



  # TOGGLE SECTION -------------------------------------------------------------

  # Title
  observe({
    toggle(id = "show_title_widgets_hi", anim = TRUE,
           time = 0.3, condition = input$add_title_hi)
  })

  # Theme
  observe({
    toggle(id = "show_theme_widgets_hi", anim = TRUE,
           time = 0.3, condition = input$change_theme_hi)
  })

  # Plot size
  observe({
    toggle(id = "show_size_hi", anim = TRUE,
           time = 0.3, condition = input$ch_size_hi)
  })

  # ----------------------------------------------------------------------------

  observe({

    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "x_facet_hi",
                      choices = c("none" = ".", vars))

    updateSelectInput(session, inputId = "y_facet_hi",
                      choices = c("none" = ".", vars))

    if (input$faceting_hi == "none" | input$faceting_hi == "Wrap") {

      updateSelectInput(session, inputId = "x_facet_hi",
                        choices = c("none" = ".", vars),
                        selected = ".")

      updateSelectInput(session, inputId = "y_facet_hi",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })


  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "wrap_hi",
                      choices = c("none" = ".", vars))

    if (input$faceting_hi == "none" | input$faceting_hi == "Grid" ) {

      updateSelectInput(session, inputId = "wrap_hi",
                        choices = c("none" = ".", vars),
                        selected = ".")
      updateSelectInput(session, inputId = "wrap_hi2",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })

  observe ({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    if (input$wrap_sc == ".") {
      updateSelectInput(session, inputId = "wrap_hi2",
                        choices = c("none" = "."))
    }
    else {
      vars <- vars[!is.element(vars, input$wrap_hi)]
      updateSelectInput(session, inputId = "wrap_hi2",
                        choices = c("none" = ".", vars))
    }
  })


  # DYNAMIC LABELS:
  observe({
    updateTextInput(session, inputId = "label_x_hi",
                    value = input$x_input_hi)
  })

  # FILL BY A VARIABLE
  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]
    updateSelectInput(session, inputId = "fill_by_hi",
                      choices = c(vars) )
  })

  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]
    updateSelectInput(session, inputId = "dens_fill_by_hi",
                      choices = vars)
  })

  # WEIGHT
  observe({
    vars <- names(plotData())[sapply(plotData(), is.numeric)]
    updateSelectInput(session, inputId = "weight_hi",
                      choices = c("none", vars), selected = "none")
  })

  # RESET BUTTONS
  observeEvent(input$reset_count_hi, {
    updateColourInput(session, inputId = "low_hi" , value = "#132B43")
    updateColourInput(session, inputId = "high_hi", value = "#56B1F7")
  })

  observeEvent(input$reset_colours_hi, {
    updateColourInput(session, inputId = "color_hi", value = "white")
    updateColourInput(session, inputId = "fill_hi", value = "black")
  })

  observeEvent(input$reset_dens_hi, {
    updateColourInput(session, inputId = "dens_color_hi", value = "#BD1515")
    updateColourInput(session, inputId = "dens_fill_hi", value = "#BD1515")
    updateSliderInput(session, inputId = "dens_opacity_hi", value = 0.2)
  })



  # IF ONLY DENSITY THEN DISABLE FILL OPTIONS
  observe({
    if (input$change_density_hi == "Density") {
      updateRadioButtons(session, inputId = "change_fill_hi",
                         choices = "none",
                         selected = "none", inline = TRUE)
    }
    else {
      updateRadioButtons(session, inputId = "change_fill_hi",
                         choices = c("none", "Count", "Fill by"),
                         inline = TRUE)
    }
  })



  Subset_hi <- reactiveValues()

  dataHist <- reactive({

    if (!is.null(plotData() )) {

      if (sum(sapply(plotData(), is.numeric)) > 0) {
        closeAlert(session, "no_numeric_hi")

        if (input$show_range_hi) {
          data_hi <- plotData()
          inX <- input$x_input_hi

          rangeX <- input$x_range_hi

          if (!is.null(rangeX[1]) & !is.null(rangeX[2])) {

            if (!input$x_log_hi) {
              Code_hi$scales <- NULL
              Subset_hi$logx <- NULL

              if (rangeX[1] != min(data_hi[ ,inX]) | rangeX[2] != max(data_hi[ ,inX])) {
                Subset_hi$x <-  paste0(inX, " >= ", rangeX[1], " & ", inX, " <= ", rangeX[2])
                Code_hi$range <- paste0(" + \n  scale_x_continuous(limits = c(", rangeX[1], ", ", rangeX[2], "))")
              }
              else {
                Subset_hi$x <- NULL
                Code_hi$range <- NULL
                Code_hi$d <- NULL
              }
            }
            if (input$x_log_hi) {
              Subset_hi$logx <-  paste0(" + \n  scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                         ", labels = trans_format('log10', math_format(10^.x)))")
              if (rangeX[1] != min(data_hi[ ,inX]) | rangeX[2] != max(data_hi[ ,inX])) {
                Subset_hi$x <-  paste0(inX, " >= ", rangeX[1], " & ", inX, " <= ", rangeX[2])
                Code_hi$d <- NULL
                Code_hi$scales <- "\nlibrary(scales)"
                Code_hi$range <- paste0(" + \n  scale_x_continuous(limits = c(", rangeX[1], ", ", rangeX[2], ")",
                                         ", breaks = trans_breaks('log10', function(x) 10^x),\n",
                                         "                     ",
                                         "labels = trans_format('log10', math_format(10^.x))) ")
              }
              else {
                Subset_hi$x <- NULL
                Code_hi$range <- NULL
                Code_hi$d <- paste0(" + \n  scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                     ", labels = trans_format('log10', math_format(10^.x)))")
              }
            }
          }
          data_hi <- data_hi[data_hi[ ,inX] >= rangeX[1] & data_hi[ ,inX] <= rangeX[2], ]

          if (nrow(data_hi) > 1) {
            return(data_hi)
          }
        }
        else {
          Subset_hi$x <- NULL  ; Code_hi$range <- NULL
          Code_hi$d <- NULL
          if (input$x_log_hi) {
            Code_hi$d <- paste0(" + \n  scale_x_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                 ", labels = trans_format('log10', math_format(10^.x)))")
          }
          return(plotData())
        }
      }
      else {
        createAlert(session,
                    anchorId = "alert_hi",
                    alertId = "no_numeric_hi",
                    title = "No numeric variable available",
                    content = "",
                    dismiss = FALSE)
        return(NULL)
      }
    }
    else {
      return(NULL)
    }
  })


  # PLOT

  output$Histogram_ui <- renderUI ({
    plotOutput(outputId = "Histogram",
               height = input$height_hi,
               width = input$width_hi)
  })

  output$Histogram <- renderPlot({

    if ( !is.null(dataHist() ) ) {

      Density <- input$change_density_hi

      Var <- dataHist()[ ,input$x_input_hi]

      Bins <- diff(range(Var)) / (input$breaks_hi)

      Bins2 <- ifelse(input$breaks_hi != 30, paste0(", binwidth = ", round(Bins,5)), "")

      Alpha <- ifelse(input$opacity_hi != 1, paste0(", alpha = ", input$opacity_hi),"")

      Count <- ifelse(input$change_fill_hi == "Count", ", fill = after_stat(count)", "")

      Fill_by <- ifelse(input$change_fill_hi == "Fill by", paste0(", fill = ", input$fill_by_hi), "")

      Fill <- ifelse(input$fill_hi == '#000000' | input$fill_hi == 'black', "", paste0(", fill = ", "'",input$fill_hi,"'"))

      Colour <- ifelse(input$color_hi == '#000000' | input$fill_hi == 'black', "", paste0(", colour = ", "'", input$color_hi,"'"))

      Dens1 <- ifelse(input$change_density_hi2 == "Colour" & (Density == "Both" | Density == "Density"), ", y = after_stat(density)", "")

      Dens2 <- ifelse(input$change_density_hi2 == "Colour by" & (Density == "Both" | Density == "Density"), ", y = after_stat(density)", "")

      Weight <- ifelse(input$weight_hi != "none", paste0(", weight = ", input$weight_hi), "") # " + ylab(", "'", input$weight_hi, "'", ")")

      Code_hi$aes <- paste0(Dens1, Dens2, Count, Fill_by, Weight)


      if (input$change_fill_hi == "Count" & input$change_density_hi2 == "Colour by") {
        validate(paste0("\n This combination is infeasible. \n\n",
                 " If density is colored by some discrete variable (discrete y-axis),\n",
                 " then filling the histogram by counts is not possible (continuous y-axis)."))
      }

      pl_hi <- ggplot(dataHist(), aes(x = .data[[input$x_input_hi]]))

      Code_hi$gg <- paste0("\n# Graph\n",
                           "ggplot(data = ", Code_Data$name,
                           ", aes(x = ", input$x_input_hi, Code_hi$aes,"))")

      if (Density == "Density") {
        Code_hi$a <- NULL
      }

      if (Density != "Density") {

        Code_hi$b <- NULL

        if (input$change_fill_hi == "none") {
          pl_hi <- pl_hi + geom_histogram(color = input$color_hi,
                                          fill =  input$fill_hi,
                                          alpha = input$opacity_hi,
                                          binwidth = Bins)
          Code_hi$a <- paste0(" + \n  geom_histogram(", syntax(c(Fill, Colour, Bins2, Alpha)), ")")
        }

        if (input$change_fill_hi == "Fill by") {


          pl_hi <- pl_hi + geom_histogram(aes(fill = .data[[input$fill_by_hi]]),
                                          alpha = input$opacity_hi,
                                          position = input$position_hi,
                                          color = "white",
                                          binwidth = Bins)

          Position <- ifelse(input$position_hi != "stack", paste0(", position = ", "'",input$position_hi,"'"), "")

          Code_hi$a <- paste0(" + \n  geom_histogram(colour = 'white'", Bins2, Alpha, Position, ")")

        }

        if (input$change_fill_hi == "Count") {
          pl_hi <- pl_hi + geom_histogram(aes(fill = after_stat(count)),
                                          alpha = input$opacity_hi,
                                          binwidth = Bins) +
            scale_fill_gradient("Count", low = input$low_hi, high = input$high_hi)


          Gradient <- ifelse(input$low_hi != '#132B43' | input$high_hi != '#56B1F7',
                             paste0(" + \n  scale_fill_gradient('Count', low = ", "'",input$low_hi,"'",
                                    ", high = ", "'",input$high_hi,"'", ")"), "")

          Code_hi$a <- paste0(" + \n  geom_histogram(", syntax(c(Bins2, Alpha)), ")", Gradient)

        }
      }



      # DENSITY COLOUR
      if (input$change_density_hi2 == "Colour" & (Density == "Both" | Density == "Density")) {

        pl_hi <- pl_hi + geom_density(color = input$dens_color_hi,
                                      fill = input$dens_fill_hi,
                                      alpha = input$dens_opacity_hi) +
          aes(y = after_stat(density)) +
          ylab("density")



        Code_hi$b <- paste0(" + \n  geom_density(colour = ", "'", input$dens_color_hi, "'",
                             ", fill = ", "'",input$dens_fill_hi,"'", ", alpha = ", input$dens_opacity_hi, ")")


      }


      # DENSITY COLOUR BY
      if (input$change_density_hi2 == "Colour by" & (Density == "Both" |
                                                     Density == "Density")) {

        # updateTextInput(session, inputId = "label_y_hi", value = "density")

        pl_hi <- pl_hi + geom_density(aes(fill = .data[[input$dens_fill_by_hi]]),
                                      alpha = input$dens_fill_by_opacity_hi,
                                      position = input$dens_position_hi) +
          aes(y = after_stat(density)) +
          ylab("density")


        Position2 <- ifelse(input$dens_position_hi != "identity", paste0(", position = ", "'", input$dens_position_hi,"'"), "")

        Code_hi$b <- paste0(" + \n  geom_density(aes(fill = ", input$dens_fill_by_hi, ")",
                                                 ", alpha = ", input$dens_fill_by_opacity_hi, Position2, ")")

      }

      Code_hi$pct_y_axis <- ""
      if (input$position_hi == "fill" | input$dens_position_hi == "fill") {
        pl_hi <- pl_hi + scale_y_continuous(labels = label_percent())
        Code_hi$pct_y_axis <- paste0(" + \n  scale_y_continuous(labels = label_percent())")
      }

      if (input$change_density_hi == "no") {
        pl_hi <- pl_hi + ylab("count")
      }

      # LABELS
      if (input$label_x_hi != input$x_input_hi) {
        pl_hi <- pl_hi + xlab(input$label_x_hi)

        Code_hi$c <- paste0(" + \n  xlab(", "'", input$label_x_hi, "'", ")")
      }

      if (input$label_x_hi == input$x_input_hi) {
        Code_hi$c <- NULL
      }

      if (input$set_label_y_hi) {
        pl_hi <- pl_hi + ylab(input$label_y_hi)
        Code_hi$ylab <- paste0(" + \n  ylab(", "'", input$label_y_hi, "'", ")")
      }
      if (!input$set_label_y_hi) {
        Code_hi$ylab <- NULL
      }


      # FACETS

      facets_hi <- paste(input$x_facet_hi, "~", input$y_facet_hi)

      if (input$x_facet_hi != "." & input$y_facet_hi != ".") {
        validate(
          need(input$x_facet_hi != input$y_facet_hi,
               "Faceting variables can only appear in `rows` or `cols` not both.")
        )
      }

      Scales_hi <- input$scales_hi

      print_scales_hi  <- paste0(", scales = ", "'", Scales_hi , "'")
      if (Scales_hi == "fixed") {
        print_scales_hi <- ""
      }


      if (facets_hi == ". ~ .") {
        Code_hi$e <- NULL
      }

      if (facets_hi != ". ~ .") {
        pl_hi <- pl_hi + facet_grid(facets_hi, scales = Scales_hi)

        Code_hi$e <- paste0(" + \n  facet_grid(", facets_hi, print_scales_hi, ")")
      }


      # Wrap

      if (input$wrap_hi == "." & input$wrap_hi2 == ".") {
        Code_hi$f <- NULL
      }

      if (input$wrap_hi != "." & input$wrap_hi2 == ".") {
        pl_hi <- pl_hi + facet_wrap(as.formula(paste("~", input$wrap_hi)), scales = Scales_hi)


        Code_hi$f <- paste0(" + \n  facet_wrap(~ ", input$wrap_hi, print_scales_hi, ")")

      }

      if (input$wrap_hi != "." & input$wrap_hi2 != ".") {
        pl_hi <- pl_hi + facet_wrap(as.formula(paste("~", input$wrap_hi, "+", input$wrap_hi2)),
                                    scales = Scales_hi)


        Code_hi$f <- paste0(" + \n  facet_wrap(~ ", input$wrap_hi, " + ", input$wrap_hi2,
                             print_scales_hi, ")")
      }

      if (input$weight_hi != "none") {
          pl_hi <- pl_hi + aes(weight = .data[[input$weight_hi]]) # + ylab(input$weight_hi)
      }


      # LOG10-TRANSFORM
      if (input$x_log_hi) {

        pl_hi <- pl_hi + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x)))
      }


      # THEME
      Theme_hi <- input$theme_hi
      Size_hi <- input$theme_size_hi
      Font_hi <- input$theme_font_hi

      if (Theme_hi == "Grey") {
        pl_hi <- pl_hi + theme_gray(Size_hi, Font_hi )
        Code_hi$h <- paste0(" + \n  theme_grey(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "White") {
        pl_hi <- pl_hi + theme_bw(Size_hi, Font_hi)
        Code_hi$g <- paste0(" + \n  theme_bw(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "Linedraw") {
        pl_hi <- pl_hi + theme_linedraw(Size_hi, Font_hi)
        Code_hi$g <- paste0(" + \n  theme_linedraw(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "Light") {
        pl_hi <- pl_hi + theme_light(Size_hi, Font_hi)
        Code_hi$g <- paste0(" + \n  theme_light(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "Minimal") {
        pl_hi <- pl_hi + theme_minimal(Size_hi, Font_hi)
        Code_hi$g <- paste0(" + \n  theme_minimal(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "Classic") {
        pl_hi <- pl_hi + theme_classic(Size_hi, Font_hi)
        Code_hi$g <- paste0(" + \n  theme_classic(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "Dark") {
          pl_hi <- pl_hi + theme_dark(Size_hi, Font_hi)
          Code_hi$g <- paste0(" + \n  theme_dark(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }
      if (Theme_hi == "Void") {
          pl_hi <- pl_hi + theme_void(Size_hi, Font_hi)
          Code_hi$g <- paste0(" + \n  theme_void(", Size_hi, ", ", "'",Font_hi,"'", ")")
      }


      if (Size_hi == 11 & Font_hi == "sans" & Theme_hi == "Grey") {
        Code_hi$g <- NULL
      }


      # LEGEND
      pl_hi <- pl_hi + theme(legend.position = input$legend_hi)

      if (input$legend_hi == "right") {
        Code_hi$h <- NULL
      }
      if (input$legend_hi != "right") {
        Code_hi$h <- paste0(" + \n  theme(legend.position = ", "'", input$legend_hi, "'", ")")
      }

      # TITLE
      if (input$title_hi != "") {
        pl_hi <- pl_hi + labs(title = input$title_hi) +
          theme(plot.title = element_text(colour = input$title_color_hi,
                                          size = input$title_size_hi, vjust = 1.5))

        Code_hi$i <- paste0(" + \n  labs(title = ", "'", input$title_hi, "'", ")",
                            " + \n  theme(plot.title = element_text(colour = ", "'",
                            input$title_color_hi, "'", ", size = ", input$title_size_hi,
                            ", vjust = 1.5", "))")
      }

      if (input$title_hi == "") {
        Code_hi$i <- NULL
      }

      updateAceEditor(session, editorId = "print_code_hi", value = plot_code_hi() )

      plots$pl_hi <- pl_hi
      print(pl_hi)
    }

  })


  # PRINT THE CODE
  Code_hi <- reactiveValues()

  plot_code_hi <- reactive({

    if (!is.null(Subset_hi$x)) {
      Subset_hi$gg <- paste0("\n# Subset of data\n",
                              "sub.data <- subset(", Code_Data$name, ", ", Subset_hi$x, ")",
                              "\n\n# Graph\n",
                              "ggplot(data = sub.data", ", aes(x = ", input$x_input_hi, Code_hi$aes,"))")
      return(
        paste0(
          sprintf(paste0("library(ggplot2)", Code_hi$scales, "\n", Code_hi$load_data, Subset_hi$gg, Code_hi$a,
                         Code_hi$b, Code_hi$c, Code_hi$pct_y_axis, Code_hi$ylab, Code_hi$e, Code_hi$f, Subset_hi$logx, Code_hi$g, Code_hi$h, Code_hi$i,
                         "\n\n\n\n\n\n# Alternatively: ----------------------------\n",
                         Code_hi$gg, Code_hi$a, Code_hi$b, Code_hi$c, Code_hi$pct_y_axis, Code_hi$ylab, Code_hi$e,
                         Code_hi$f, Code_hi$d, Code_hi$g, Code_hi$h, Code_hi$i, Code_hi$range)))
      )
    }
    if (is.null(Subset_hi$x)) {
      return(
        paste0(
          sprintf(paste0("library(ggplot2)", Code_hi$scales, "\n", Code_hi$load_data, Code_hi$gg, Code_hi$a,
                         Code_hi$b, Code_hi$c, Code_hi$pct_y_axis, Code_hi$ylab, Code_hi$e, Code_hi$f, Code_hi$d, Code_hi$g, Code_hi$h,
                         Code_hi$i, Code_hi$range))))
    }
  })


  # Download plot handler
  downloadGraphObserverServer("histogram")
  downloadGraphHandlerServer("histogram", plots$pl_hi)


  # BAR CHART: -------------------------------------------------------------------

  # PLOT VARIABLE
  observe({

    x_vars <- names(plotData() )[sapply(plotData(), is.factor)]

    if (!is.null( plotData() ) & length(x_vars) != 0 ) {

      updateSelectInput(session, inputId = "x_input_ba",
                        choices = x_vars)
    }
    else {
      updateSelectInput(session, inputId = "x_input_ba",
                        choices =  ".none.",
                        selected = ".none.")
    }
  })

  # DYNAMIC FACTORS:
  output$dynamic_factors_ba <- renderUI({

    if (length(names(plotData())) > 1) {
      inX <- NULL

      if (input$x_input_ba != ".none.") {
        inX <- input$x_input_ba
      }

      levelss <- levels(plotData()[ ,inX])


      if (length(levelss) < 7 & length(levelss > 0)) {
        checkboxGroupInput(inputId = "x_range_ba_factor", label = "Include levels:",
                           choices = levelss, selected = levelss)
      } else if (is.null(levelss)) {
        selectInput(inputId = "x_range_ba_factor", label = "Include levels:",
                    choices = ".none.", selected = ".none.")
      } else {
        selectInput(inputId = "x_range_ba_factor", label = "Include levels:",
                    choices = levelss, selected = levelss, multiple = TRUE)
      }

    } else {
      return(selectInput(inputId = "x_range_ba_factor", label = "Include levels:",
                         choices = ".none.", selected = ".none."))
    }
  })

  # DYNAMIC RANGES:
  data_ba <- reactive({
    if (length(names(plotData())) > 1) {

      if (sum(sapply(plotData(), is.factor)) > 0) {

        closeAlert(session, alertId = "no_factor_ba")
        data_ba <- plotData()
        inX <- NULL
        if (input$x_input_ba != ".none.") {
          inX <- input$x_input_ba
        }
        levelss <- input$x_range_ba_factor
        data <- data_ba[data_ba[ ,inX] %in% levelss, ]
        if (nrow(data) > 0 ) {
          return(data)
        }
      }
      else {
        createAlert(session,
                    anchorId = "alert_ba",
                    alertId = "no_factor_ba",
                    title = "No factor variable available",
                    content = "",
                    dismiss = FALSE)
        return(NULL)
      }
    }
    else {
      return(NULL)
    }
  })


  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "x_facet_ba",
                      choices = c("none" = ".", vars))

    updateSelectInput(session, inputId = "y_facet_ba",
                      choices = c("none" = ".", vars))

    if (input$faceting_ba == "none" | input$faceting_ba == "Wrap") {
      updateSelectInput(session, inputId = "x_facet_ba",
                        choices = c("none" = ".", vars),
                        selected = ".")

      updateSelectInput(session, inputId = "y_facet_ba",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })


  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "wrap_ba",
                      choices = c("none" = ".", vars))

    if (input$faceting_ba == "none" | input$faceting_ba == "Grid" ) {
      updateSelectInput(session, inputId = "wrap_ba",
                        choices = c("none" = ".", vars),
                        selected = ".")
      updateSelectInput(session, inputId = "wrap_ba2",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })

  # WEIGHT
  observe({
    vars <- names(plotData())[sapply(plotData(), is.numeric)]
    updateSelectInput(session, inputId = "weight_ba",
                      choices = c("none", vars), selected = "none")
  })

  # DYNAMIC LABELS:
  observe({
    updateTextInput(session, inputId = "label_x_ba",
                    value = input$x_input_ba)
  })

  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]
    updateSelectInput(session, inputId = "fill_by_ba",
                      choices = vars)
  })


  observe({
    toggle(id = "show_title_widgets_ba", anim = TRUE,
           time = 0.3, condition = input$add_title_ba)
  })


  observe({
    toggle(id = "show_theme_widgets_ba", anim = TRUE,
           time = 0.3, condition = input$change_theme_ba)
  })


  observe({
    toggle(id = "show_size_ba", anim = TRUE,
           time = 0.3, condition = input$ch_size_ba)
  })


  observeEvent(input$reset_ba, {
    updateSliderInput(session, "width_ba" , "Plot Width (px)", value = 700)
    updateSliderInput(session, "height_ba", "Plot Height (px)", value = 500)
  })

  observeEvent(input$reset_colours_ba, {
    updateColourInput(session, inputId = "color_ba", value = "black")
    updateColourInput(session, inputId = "fill_ba", value = "black")
    updateSliderInput(session, inputId = "colour_size_ba", value = 0.5)
  })

  observe({
    if (input$fill_by_col_ba == "qual") {
      updateSliderInput(session, inputId = "palette_ba",
                        min = 1, max = 8, value = 1, step = 1)
    }
    if (input$fill_by_col_ba == "div") {
      updateSliderInput(session, inputId = "palette_ba",
                        min = 1, max = 9, value = 1, step = 1)
    }
    if (input$fill_by_col_ba == "seq") {
      updateSliderInput(session, inputId = "palette_ba",
                        min = 1, max = 18, value = 1, step = 1)
    }
  })

  # PRINT THE CODE
  Code_ba <- reactiveValues()
  Subset_ba <- reactiveValues()

  plot_code_ba <- reactive({

    if (!is.null(Subset_ba$x)) {
      Subset_ba$gg <- paste0("\n# Subset of data\n",
                             "sub.data <- subset(", Code_Data$name, ", ", Subset_ba$x, ")",
                             "\n\n# Graph\n",
                             "ggplot(data = sub.data", ", aes(x = ", input$x_input_ba, Code_ba$Fill_by,
                             Code_ba$weight, "))")
      return(
        paste0(
          sprintf(paste0("library(ggplot2)", "\n", Code_ba$load_data, Subset_ba$gg, Code_ba$bar, Code_ba$pct_y_axis,
                         Code_ba$col_by, Code_ba$facet1, Code_ba$facet2, Code_ba$theme,
                         Code_ba$legend, Code_ba$title, Code_ba$lab_x, Code_ba$lab_y, Code_ba$flip,
                         "\n\n\n\n\n\n# Alternatively: --------------------------------------\n",
                         Code_ba$load_data, Code_ba$gg, Code_ba$bar, Code_ba$pct_y_axis, Code_ba$col_by,
                         Code_ba$facet1, Code_ba$facet2, Code_ba$range_fac, Code_ba$theme,
                         Code_ba$legend, Code_ba$title, Code_ba$lab_x, Code_ba$lab_y, Code_ba$flip)))
      )
    }
    if (is.null(Subset_ba$x)) {
      return(
        paste0(
          sprintf(paste0("library(ggplot2)", "\n", Code_ba$load_data, Code_ba$gg, Code_ba$bar, Code_ba$pct_y_axis, Code_ba$col_by,
                         Code_ba$facet1, Code_ba$facet2, Code_ba$range_fac, Code_ba$theme,
                         Code_ba$legend, Code_ba$title, Code_ba$lab_x, Code_ba$lab_y, Code_ba$flip))))
    }



  })

  # PLOT -----------------------------------------------------------------------
  output$bar_plot <- renderUI ({
    plotOutput(outputId = "Barchart",
               height = input$height_ba,
               width = input$width_ba
    )
  })

  output$Barchart <- renderPlot({

    if (!is.null(data_ba() ) & input$x_input_ba != ".none.") {


      Code_ba$Fill_by <- ifelse(input$change_fill_ba == "Colour by", paste0(", fill = ", input$fill_by_ba), "")

      pl_ba <- ggplot(data = data_ba(), aes(.data[[input$x_input_ba]]))

      Position <- ifelse(input$position_ba != "stack", paste0(", position = ", "'",input$position_ba, "'"), "")

      Opacity <- ifelse(input$opacity_ba != 1, paste0(", alpha = ", input$opacity_ba), "")

      Width <- ifelse(input$bar_width != 0.9, paste0(", width = ", input$bar_width), "")

      Fill <- ifelse(input$fill_ba == '#000000' | input$fill_ba == 'black', "", paste0(", fill = ", "'",input$fill_ba, "'"))

      Colour <- ifelse(input$color_ba == '#000000' | input$fill_ba == 'black', "", paste0(", colour = ", "'",input$color_ba,"'"))

      Size <- ifelse(input$colour_size_ba != 0.5, paste0(", linewidth = ", input$colour_size_ba), "")

      Code_ba$weight <- ifelse(input$weight_ba != "none", paste0(", weight = ", input$weight_ba), "")


      Code_ba$gg <- paste0("\n# Graph\n",
                           "ggplot(data = ", Code_Data$name, ", aes(x = ",
                           input$x_input_ba, Code_ba$Fill_by, Code_ba$weight, "))")


      if (input$change_fill_ba == "Colours") {

          pl_ba <- pl_ba + geom_bar(position = input$position_ba,
                                    width = input$bar_width,
                                    colour = input$color_ba,
                                    linewidth = input$colour_size_ba,
                                    fill = input$fill_ba,
                                    alpha = input$opacity_ba)

          Code_ba$bar <- paste0(" + \n  geom_bar(", syntax(c(Fill, Colour, Size, Width, Opacity, Position)), ")")
      }

      if (input$change_fill_ba == "Colour by") {

        pl_ba <- pl_ba + geom_bar(aes(fill = .data[[input$fill_by_ba]]),
                                  position = input$position_ba, width = input$bar_width,
                                  alpha = input$opacity_ba)

        Code_ba$bar <- paste0(" + \n  geom_bar(", syntax(c(Width, Opacity, Position )), ")")
      }

      if (input$position_ba == "fill") {
          pl_ba <- pl_ba + scale_y_continuous(labels = label_percent())
          Code_ba$pct_y_axis <- paste0(Code_hi$b, " + \n  scale_y_continuous(labels = label_percent())")
      }

      Code_ba$col_by <- NULL
      if (input$change_fill_ba == "Colour by" & input$fill_by_col_ba != "default") {
        pl_ba <- pl_ba + scale_fill_brewer(type = input$fill_by_col_ba, palette = input$palette_ba)

        Code_ba$col_by <- paste0(" + \n  scale_fill_brewer(type = ", "'", input$fill_by_col_ba, "'",
                                  ", palette = ", input$palette_ba, ")")
      }


      # FACETS

      facets_ba <- paste(input$x_facet_ba, "~", input$y_facet_ba)
      if (input$x_facet_ba != "." & input$y_facet_ba != ".") {
        validate(
          need(input$x_facet_ba != input$y_facet_ba,
               "Faceting variables can only appear in `rows` or `cols` not both.")
        )
      }

      Scales_ba <- input$scales_ba

      print_scales_ba  <- paste0(", scales = ", "'", Scales_ba , "'")
      if (Scales_ba == "fixed") {
        print_scales_ba <- ""
      }

      # Grid
      if (facets_ba == ". ~ .") {
        Code_ba$facet1 <- NULL
      }

      if (facets_ba != ". ~ .") {
        pl_ba <- pl_ba + facet_grid(facets_ba, scales = Scales_ba)


        Code_ba$facet1 <- paste0(" + \n  facet_grid(", facets_ba, print_scales_ba, ")")

      }

      # Wrap

      if (input$wrap_ba == "." & input$wrap_ba2 == ".") {
        Code_ba$facet2 <- NULL
      }

      if (input$wrap_ba != "." & input$wrap_ba2 == ".") {
        pl_ba <- pl_ba + facet_wrap(as.formula(paste("~", input$wrap_ba)), scales = Scales_ba)


        Code_ba$facet2 <- paste0(" + \n  facet_wrap(~ ", input$wrap_ba, print_scales_ba, ")")

      }

      if (input$wrap_ba != "." & input$wrap_ba2 != ".") {
        pl_ba <- pl_ba + facet_wrap(as.formula(paste("~", input$wrap_ba, "+", input$wrap_ba2)),
                                    scales = Scales_ba)


        Code_ba$facet2 <- paste0(" + \n  facet_wrap(~ ", input$wrap_ba, " + ", input$wrap_ba2,
                                  print_scales_ba, ")")
      }


      if (input$weight_ba != "none") {

          pl_ba <- pl_ba + aes(weight = .data[[input$weight_ba]])
      }


      # LABELS
      if (input$label_x_ba != input$x_input_ba) {
        pl_ba <- pl_ba + xlab(input$label_x_ba)

        Code_ba$lab_x <- paste0(" + \n  xlab(", "'", input$label_x_ba, "'", ")")
      }

      if (input$label_x_ba == input$x_input_ba) {
        Code_ba$lab_x <- NULL
      }

      if (input$set_label_y_ba) {
        pl_ba <- pl_ba + ylab(input$label_y_ba)

        Code_ba$lab_y <- paste0(" + \n  ylab(", "'", input$label_y_ba, "'", ")")
      }

      if (!input$set_label_y_ba) {
        Code_ba$lab_y <- NULL
      }

      # THEME
      Theme_ba <- input$theme_ba
      Size_ba <- input$theme_size_ba
      Font_ba <- input$theme_font_ba

      if (Theme_ba == "Grey") {
        pl_ba <- pl_ba + theme_gray(Size_ba, Font_ba)
        Code_ba$theme <- paste0(" + \n  theme_grey(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "White") {
        pl_ba <- pl_ba + theme_bw(Size_ba, Font_ba)
        Code_ba$theme <- paste0(" + \n  theme_bw(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "Linedraw") {
        pl_ba <- pl_ba + theme_linedraw(Size_ba, Font_ba)
        Code_ba$theme <- paste0(" + \n  theme_linedraw(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "Light") {
        pl_ba <- pl_ba + theme_light(Size_ba, Font_ba)
        Code_ba$theme <- paste0(" + \n  theme_light(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "Minimal") {
        pl_ba <- pl_ba + theme_minimal(Size_ba, Font_ba)
        Code_ba$theme <- paste0(" + \n  theme_minimal(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "Classic") {
        pl_ba <- pl_ba + theme_classic(Size_ba, Font_ba)
        Code_ba$theme <- paste0(" + \n  theme_classic(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "Dark") {
          pl_ba <- pl_ba + theme_dark(Size_ba, Font_ba)
          Code_ba$theme <- paste0(" + \n  theme_dark(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }
      if (Theme_ba == "Void") {
          pl_ba <- pl_ba + theme_void(Size_ba, Font_ba)
          Code_ba$theme <- paste0(" + \n  theme_void(", Size_ba, ", ", "'",Font_ba,"'", ")")
      }


      if (Size_ba == 11 & Font_ba == "sans" & Theme_ba == "Grey") {
        Code_ba$theme <- NULL
      }


      # LEGEND
      pl_ba <- pl_ba + theme(legend.position = input$legend_ba)

      if (input$legend_ba == "right") {
        Code_ba$legend <- NULL
      }
      if (input$legend_ba != "right") {
        Code_ba$legend <- paste0(" + \n  theme(legend.position = ", "'",input$legend_ba, "'", ")")
      }


      if (input$title_ba != "") {
        pl_ba <- pl_ba + labs(title = input$title_ba) +
          theme(plot.title = element_text(colour = input$title_color_ba,
                                          size = input$title_size_ba, vjust = 1.5))
        Code_ba$title <- paste0(" + \n  labs(title = ", "'", input$title_ba, "'", ")",
                                 " + \n  theme(plot.title = element_text(colour = ", "'",
                                 input$title_color_ba, "'", ", size = ", input$title_size_ba,
                                 ", vjust = 1.5",  "))")
      }

      if (input$title_ba == "") {
        Code_ba$title <- NULL
      }




      if (input$coord_flip_ba) {
        validate(
          need(!(input$faceting_ba == "Wrap" & input$scales_ba != "fixed"),
               "ggplot2 does not currently support free scales with
               a non-cartesian coord or coord_flip.")
          )
        pl_ba <- pl_ba + coord_flip()

        Code_ba$flip <- paste0(" + \n  coord_flip()")
      }

      if (!input$coord_flip_ba) {
        Code_ba$flip <- NULL
      }


      # RANGE

      lev <- input$x_range_ba_factor

      if (length(lev) != length(levels(plotData()[ ,input$x_input_ba] ))) {

        if (length(lev) > 1) {
          Subset_ba$x <- paste0(input$x_input_ba, " %%in%% ",
                                 "c('", paste(lev, collapse = "', '"), "')")

          Code_ba$range_fac <- paste0(" + \n  scale_x_discrete(limits = c(", "'",
                                       paste(lev, collapse = "', '"), "'", "))")
        }
        if (length(lev) == 1) {
          Subset_ba$x <- paste0(input$x_input_ba, " == ", "'", paste(lev), "'")
          Code_ba$range_fac <- paste0(" + \n  scale_x_discrete(limits = ", "'",
                                       paste(lev), "'", ")")
        }

      }

      if (length(lev) == length(levels(plotData()[ ,input$x_input_ba] ))) {
        Subset_ba$x <- NULL
        Code_ba$range_fac <- NULL

      }

      # update the code for the histogram
      # shinyjs::text(id = "print_code_ba", text = plot_code_ba() )
      updateAceEditor(session, editorId = "print_code_ba", value = plot_code_ba())
      plots$pl_ba <- pl_ba
      pl_ba
    }

  })


  # Download plot handler
  downloadGraphObserverServer("bargraph")
  downloadGraphHandlerServer("bargraph", plots$pl_ba)


  # BOXPLOTS: --------------------------------------------------------------------


  observe({

    x_vars <- names(plotData())[sapply(plotData(), is.factor)]
    if (!is.null( plotData() ) & length(x_vars) != 0 ) {
      closeAlert(session, alertId = "no_factor_box1")

      updateSelectInput(session, inputId = "x_input_box",
                        choices = x_vars)
    }
    else {
      if (!is.null(plotData())) {
        createAlert(session,
                    anchorId = "alert_box1",
                    alertId = "no_factor_box1",
                    title = "No factor variable available",
                    content = "",
                    dismiss = FALSE)
      }
      updateSelectInput(session, inputId = "x_input_box",
                        choices =  ".none.",
                        selected = ".none.")
    }
  })


  observe({
    y_vars <- names(plotData())[sapply(plotData(), is.numeric)]
    if (!is.null( plotData() ) & length(y_vars) != 0) {
      closeAlert(session, alertId = "no_factor_box2")
      updateSelectInput(session, inputId = "y_input_box",
                        choices = y_vars )
    }
    else {
      if (!is.null(plotData())) {
        createAlert(session,
                    anchorId = "alert_box2",
                    alertId = "no_factor_box2",
                    title = "No numeric variable available",
                    content = "",
                    dismiss = FALSE)
      }
      updateSelectInput(session, inputId = "y_input_box",
                        choices =  ".none.",
                        selected = ".none.")
    }
  })


  # DYNAMIC FACTORS:
  output$dynamic_factors_box <- renderUI({

    if (length(names(plotData())) > 1) {
      inX <- NULL

      if (input$x_input_box != ".none.") {
        inX <- input$x_input_box
      }
      levelss <- levels(plotData()[ ,inX])

      if (length(levelss) < 7 & length(levelss > 0)) {
        checkboxGroupInput(inputId = "x_range_box_factor", label = "Include levels:",
                           choices = levelss, selected = levelss)
      }

      else if (is.null(levelss)) {
        selectInput(inputId = "x_range_box_factor", label = "Include levels:",
                    choices = "none", selected = "none")
      }
      else {
        selectInput(inputId = "x_range_box_factor", label = "Include levels:",
                    choices = levelss, selected = levelss, multiple = TRUE)
      }
    }
    else {
      return(selectInput(inputId = "x_range_box_factor", label = "Include levels:",
                         choices = "none", selected = "none"))
    }
  })

  # dynamic range for Y
  output$dynamic_range_y_box <- renderUI({

    if (!is.null(plotData()) ) {
      inY <- input$y_input_box

      if (is.numeric(plotData()[ ,inY]) ) {

          min_y = min(plotData()[ ,inY ])
          max_y = max(plotData()[ ,inY ])

        return(sliderInput(inputId = "y_range_box",
                           label = paste0("Range of '", input$y_input_box,"':"),
                           min = min_y, max = max_y, value = c(min_y, max_y),
                           step = (max_y - min_y) / 100))
      }
    }

    else {
      return(sliderInput(inputId = "y_range_box",
                         label = paste0("Range of '", "none" ,"':"),
                         min = -1, max = 1, value = c(-1, 1)))
    }
  })

  Subset_box <- reactiveValues()

  data_box <- reactive({

    if (!is.null(plotData() )) {
      test <- sum(sapply(plotData(), is.numeric)) > 0 & sum(sapply(plotData(), is.factor)) > 0
      if (test) {
        data_box <- plotData()
        inX <- input$x_input_box
        inY <- input$y_input_box

        if (inX != ".none." & inY != ".none.") {
          if (input$show_range_box) {

            if (is.numeric(data_box[ ,inY]) & nrow(data_box) > 1) {
              rangeY <- input$y_range_box
              if (!is.null(rangeY[1]) & !is.null(rangeY[2])) {

                if (!input$y_log_box) {
                  Subset_box$logy <- NULL
                  Code_box$scales <- NULL
                  if (rangeY[1] != min(data_box[ ,inY]) | rangeY[2] != max(data_box[ ,inY])) {
                    Subset_box$y <-  paste0(inY, " >= ", rangeY[1], " & ", inY, " <= ", rangeY[2])

                    Code_box$range_y <- paste0(" + \n  scale_y_continuous(limits = c(", rangeY[1], ", ", rangeY[2], "))")
                  }
                  else {
                    Subset_box$y <- NULL
                    Code_box$range_y <- NULL
                    Code_box$logy <- NULL
                  }
                }
                if (input$y_log_box) {
                  Subset_box$logy <- paste0(" + \n  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                             ", labels = trans_format('log10', math_format(10^.x)))")
                  if (rangeY[1] != min(data_box[ ,inY]) | rangeY[2] != max(data_box[ ,inY])) {

                    Subset_box$y <- paste0(inY, " >= ", rangeY[1], " & ", inY, " <= ", rangeY[2])

                    Code_box$logy <- NULL
                    Code_box$scales <- "\nlibrary(scales)"
                    Code_box$range_y <- paste0(" + \n  scale_y_continuous(limits = c(", rangeY[1], ", ", rangeY[2], ")",
                                                ", breaks = trans_breaks('log10', function(x) 10^x),\n",
                                                "                     ",
                                                "labels = trans_format('log10', math_format(10^.x))) ")
                  }
                  else {
                    Subset_box$y <- NULL
                    Code_box$range_y <- NULL
                    Code_box$logy <- paste0(" + \n  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                             ", labels = trans_format('log10', math_format(10^.x)))")


                  }
                }
              }
              data_box <- data_box[data_box[ ,inY] >= rangeY[1] & data_box[ ,inY] <= rangeY[2], ]
            }

            # Dynamic levels for factor variables
            if (is.factor(data_box[ ,inX])) {

              lev_x <- input$x_range_box_factor
              cond1 <- length(lev_x) != length(levels(plotData()[ ,input$x_input_box] ))
              if (cond1) {

                if (length(lev_x) > 1) {
                  Subset_box$x <- paste0(inX, " %%in%% ", "c('", paste(lev_x, collapse = "', '"), "')")

                  Code_box$range_x <- paste0(" + \n  scale_x_discrete(limits = c(", "'",
                                              paste(lev_x, collapse = "', '"), "'", "))")
                }
                if (length(lev_x) == 1) {
                  Subset_box$x <- paste0(inX, " == ", "'", paste(lev_x), "'")

                  Code_box$range_x <- paste0(" + \n  scale_x_discrete(limits = ", "'",
                                              paste(lev_x), "'", ")")
                }
              }
              if (!cond1) {
                Subset_box$x <- NULL
                Code_box$range_x <- NULL
              }
              data_box <- data_box[data_box[ ,inX] %in% lev_x, ]
            }
            return(data_box)
          }

          else {
            Subset_box$x <- NULL  ; Subset_box$y <- NULL
            Code_box$range_x <- NULL ; Code_box$range_y <- NULL
            Code_box$logy <- NULL

            if (input$y_log_box & !is.factor(plotData()[ ,input$y_input_box])) {
              Code_box$logy <- paste0(" + \n  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x)",
                                       ", labels = trans_format('log10', math_format(10^.x)))")
            }
            return(plotData())
          }
        }
      }
    }
  })


  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "x_facet_box",
                      choices = c("none" = ".", vars))

    updateSelectInput(session, inputId = "y_facet_box",
                      choices = c("none" = ".", vars))

    if (input$faceting_box == "none" | input$faceting_box == "Wrap") {
      updateSelectInput(session, inputId = "x_facet_box",
                        choices = c("none" = ".", vars),
                        selected = ".")

      updateSelectInput(session, inputId = "y_facet_box",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })

  # Wrap
  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]

    updateSelectInput(session, inputId = "wrap_box",
                      choices = c("none" = ".", vars))

    if (input$faceting_box == "none" | input$faceting_box == "Grid" ) {
      updateSelectInput(session, inputId = "wrap_box",
                        choices = c("none" = ".", vars),
                        selected = ".")
      updateSelectInput(session, inputId = "wrap_box2",
                        choices = c("none" = ".", vars),
                        selected = ".")
    }
  })

  # WEIGHT
  observe({
    vars <- names(plotData())[sapply(plotData(), is.numeric)]
    updateSelectInput(session, inputId = "weight_box",
                      choices = c("none", vars), selected = "none")
  })

  # DYNAMIC LABELS:
  observe({
    updateTextInput(session, inputId = "label_x_box",
                    value = input$x_input_box)
  })

  observe({
    updateTextInput(session, inputId = "label_y_box",
                    value = input$y_input_box)
  })

  observe({
    vars <- names(plotData())[sapply(plotData(), is.factor)]
    updateSelectInput(session, inputId = "fill_by_box",
                      choices = vars)
  })


  # TOGGLE SECTION -------------------------------------------------------------

  # Title
  observe({
    toggle(id = "show_title_widgets_box", anim = TRUE,
           time = 0.3, condition = input$add_title_box)
  })

  # Theme
  observe({
    toggle(id = "show_theme_widgets_box", anim = TRUE,
           time = 0.3, condition = input$change_theme_box)
  })

  # Plot size
  observe({
    toggle(id = "show_size_box", anim = TRUE,
           time = 0.3, condition = input$ch_size_box)
  })


  # RESET BUTTONS
  observeEvent(input$reset_box, {
    updateSliderInput(session, "width_box" , "Plot Width (px)", value = 700)
    updateSliderInput(session, "height_box", "Plot Height (px)", value = 500)
  })

  observeEvent(input$reset_colours_box, {
    updateColourInput(session, inputId = "color_box", value = "black")
    updateColourInput(session, inputId = "fill_box", value = "white")
    updateColourInput(session, inputId = "out_color", value = "black")
    # updateSliderInput(session, inputId = "colour_size_box", value = 0.5)
  })


  observe({
    if (input$fill_by_col_box == "qual") {
      updateSliderInput(session, inputId = "palette_box",
                        min = 1, max = 8, value = 1, step = 1)
    }
    if (input$fill_by_col_box == "div") {
      updateSliderInput(session, inputId = "palette_box",
                        min = 1, max = 9, value = 1, step = 1)
    }
    if (input$fill_by_col_box == "seq") {
      updateSliderInput(session, inputId = "palette_box",
                        min = 1, max = 18, value = 1, step = 1)
    }
  })


  observe({
    if (input$box_jitter) {
      updateSelectInput(session, inputId = "out_shape",
                        selected = c("No outlier" = -1))
    } else {
        updateSelectInput(session, inputId = "out_shape",
                          selected = c("Dot" = 19))
    }
  })


  # PRINT THE CODE
  Code_box <- reactiveValues()

  plot_code_box <- reactive({
    Subset_box$and <- ifelse(!is.null(Subset_box$x) & !is.null(Subset_box$y), " & ", "")

    if (!is.null(Subset_box$x) | !is.null(Subset_box$y)) {
      Subset_box$gg <- paste0("\n# Subset of data\n",
                              "sub.data <- subset(", Code_Data$name, ", ", Subset_box$x, Subset_box$and, Subset_box$y, ")",
                              "\n\n# Graph\n",
                              "ggplot(data = sub.data", ", aes(x = ", input$x_input_box, ", y = ",
                              input$y_input_box, Code_box$Fill_by_box, "))")
      return(
        paste0(
          sprintf(
            paste0("library(ggplot2)", Code_box$scales, "\n", Code_ba$load_data, Code_box$seed, Subset_box$gg, Code_box$boxplot,
                   Code_box$col_by, Code_box$flip, Code_box$grid, Code_box$wrap,
                   Subset_box$logy, Code_box$labx, Code_box$laby,
                   Code_box$theme, Code_box$leg, Code_box$title, Code_box$jitter,
                   "\n\n\n\n\n\n# Alternatively: ----------------------------\n",

                   Code_ba$load_data, Code_box$seed, Code_box$gg, Code_box$boxplot,
                   Code_box$col_by, Code_box$flip, Code_box$grid, Code_box$wrap,
                   Code_box$range_x, Code_box$logy, Code_box$range_y, Code_box$labx, Code_box$laby,
                   Code_box$theme, Code_box$leg, Code_box$title, Code_box$jitter)))
      )
    }

    if (is.null(Subset_box$x) & is.null(Subset_box$y)) {

      paste0(sprintf(paste0("library(ggplot2)", Code_box$scales, "\n", Code_ba$load_data, Code_box$seed, Code_box$gg, Code_box$boxplot,
                            Code_box$col_by, Code_box$flip, Code_box$grid, Code_box$wrap,
                            Code_box$range_x, Code_box$logy, Code_box$range_y, Code_box$labx, Code_box$laby,
                            Code_box$theme, Code_box$leg, Code_box$title, Code_box$jitter)))
    }

  })





  # PLOT -----------------------------------------------------------------------
  output$box_plot <- renderUI ({

    plotOutput(outputId = "Boxplot",
               height = input$height_box,
               width = input$width_box
    )
  })

  output$Boxplot <- renderPlot({

    if ( !is.null(data_box()) ) {


      Code_box$Fill_by_box <- ifelse(input$change_fill_box == "Fill by", paste0(", fill = ", input$fill_by_box), "")

      Code_box$Colour_by_box <- ifelse(input$change_fill_box == "Colour by", paste0(", colour = ", input$fill_by_box), "")

      Out_alpha_jitter <- ifelse(input$box_jitter_opacity != 1,
                                 paste0(", alpha = ", input$box_jitter_opacity), "")

      Code_box$jitter <- ""

      Fill <- ifelse(input$fill_box != "#FFFFFF", paste0(", fill = ", "'",input$fill_box, "'"), "")

      Colour <- ifelse(input$color_box != "#000000", paste0(", colour = ", "'", input$color_box, "'"), "")

      Width <- ifelse(input$box_width != 0.75, paste0(", width = ", input$box_width), "")

      Alpha <- ifelse(input$opacity_box != 1,
                      paste0(", alpha = ", input$opacity_box),"")

      Size <- ifelse(input$colour_size_box != 0.5,
                     paste0(", size = ", input$colour_size_box) ,"")

      Out_col <- ifelse(input$out_color != "#000000",
                        paste0(", outlier.colour = ", "'", input$out_color, "'"), "")

      Out_size <- ifelse(input$out_size != 1.5,
                         paste0(", outlier.size = ", input$out_size), "")

      Out_shape <- ifelse(input$out_shape != 19,
                          paste0(", outlier.shape = ", input$out_shape), "")

      Code_box_seed <- ""

      if (input$box_jitter) {
        set.seed(1)
        Code_box$seed <- paste0("\n#Set seed for reproducibility",
                                "\n",
                                "set.seed(1)\n")
      } else {
        Code_box$seed <- NULL
      }

      Code_box$gg <- paste0("\n# Graph\n",
                            "ggplot(data = ", Code_Data$name, ", aes(x = ", input$x_input_box,
                            ", y = ", input$y_input_box, Code_box$Fill_by_box,
                            Code_box$Colour_by_box, "))")


      pl_box <- ggplot(data = data_box(), aes(x = .data[[input$x_input_box]], y = .data[[input$y_input_box]]))



      if (input$change_fill_box == "Colours") {

        pl_box <- pl_box + geom_boxplot(fill = input$fill_box,
                                        colour = input$color_box,
                                        width = input$box_width,
                                        size = input$colour_size_box,
                                        alpha = input$opacity_box,
                                        outlier.colour = input$out_color,
                                        outlier.size = input$out_size,
                                        outlier.shape = as.numeric(input$out_shape))

        Code_box$boxplot <- paste0(" + \n  geom_boxplot(",
                                   syntax(c(Fill, Colour, Width, Size, Alpha, Out_col, Out_size, Out_shape)),
                                   ")")

        if (input$box_jitter) {
          pl_box <- pl_box + geom_jitter(alpha = input$box_jitter_opacity)
          Code_box$jitter <- paste0(" + \n  geom_jitter(", syntax(c(Out_alpha_jitter)), ")")


        }
      }

      if (input$change_fill_box == "Fill by") {

        pl_box <- pl_box + geom_boxplot(aes(fill = .data[[input$fill_by_box]]),
                                        size = input$colour_size_box,
                                        width = input$box_width,
                                        alpha = input$opacity_box,
                                        outlier.colour = input$out_color,
                                        outlier.size = input$out_size,
                                        outlier.shape = as.numeric(input$out_shape)
                                        )

        Code_box$boxplot <- paste0(" + \n  geom_boxplot(", syntax(c(Size, Width, Alpha, Out_col, Out_size, Out_shape)), ")")

        if (input$box_jitter) {
          pl_box <- pl_box + geom_jitter(alpha = input$box_jitter_opacity)
          Code_box$jitter <- paste0(" + \n  geom_jitter(", syntax(c(Out_alpha_jitter)), ")")
        }
      }

      if (input$change_fill_box == "Colour by") {

        pl_box <- pl_box + geom_boxplot(aes(colour = .data[[input$fill_by_box]]),
                                        size = input$colour_size_box,
                                        width = input$box_width,
                                        alpha = input$opacity_box,
                                        outlier.colour = input$out_color,
                                        outlier.size = input$out_size,
                                        outlier.shape = as.numeric(input$out_shape)
        )

        Code_box$boxplot <- paste0(" + \n  geom_boxplot(", syntax(c(Size, Width, Alpha, Out_col, Out_size, Out_shape)), ")")

        if (input$box_jitter) {
          pl_box <- pl_box + geom_jitter(aes(colour = .data[[input$fill_by_box]]),
                                         alpha = input$box_jitter_opacity)
          Code_box$jitter <- paste0(" + \n  geom_jitter(", syntax(c(Out_alpha_jitter)), ")")
        }
      }

      Code_box$col_by <- NULL
      if ((input$change_fill_box == "Fill by") & input$fill_by_col_box != "default") {
        pl_box <- pl_box + scale_fill_brewer(type = input$fill_by_col_box, palette = input$palette_box)

        Code_box$col_by <- paste0(" + \n  scale_fill_brewer(type = ", "'",input$fill_by_col_box, "'",
                                   ", palette = ", input$palette_box, ")")
      }

      if ((input$change_fill_box == "Colour by") & input$fill_by_col_box != "default") {
        pl_box <- pl_box + scale_colour_brewer(type = input$fill_by_col_box, palette = input$palette_box)

        Code_box$col_by <- paste0(" + \n  scale_colour_brewer(type = ", "'",input$fill_by_col_box, "'",
                                  ", palette = ", input$palette_box, ")")
      }

      # Grid
      facets_box <- paste(input$x_facet_box, "~", input$y_facet_box)
      if (input$x_facet_box != "." & input$y_facet_box != ".") {
        validate(
          need(input$x_facet_box != input$y_facet_box,
               "Faceting variables can only appear in `rows` or `cols` not both.")
        )
      }

      Scales_box <- input$scales_box

      print_scales_box  <- paste0(", scales = ", "'", Scales_box , "'")
      if (Scales_box == "fixed") {
        print_scales_box <- ""
      }


      if (facets_box == ". ~ .") {
        Code_box$grid <- NULL
      }

      if (facets_box != ". ~ .") {
        pl_box <- pl_box + facet_grid(facets_box, scales = Scales_box)

        Code_box$grid <- paste0(" + \n  facet_grid(", facets_box, print_scales_box, ")")
      }



      if (input$wrap_box == "." & input$wrap_box2 == ".") {
        Code_box$wrap <- NULL
      }


      if (input$wrap_box != "." & input$wrap_box2 == ".") {
        pl_box <- pl_box + facet_wrap(as.formula(paste("~", input$wrap_box)), scales = Scales_box)

        Code_box$wrap <- paste0(" + \n  facet_wrap(~ ", input$wrap_box, print_scales_box, ")")

      }

      if (input$wrap_box != "." & input$wrap_box2 != ".") {
        pl_box <- pl_box + facet_wrap(as.formula(paste("~", input$wrap_box, "+", input$wrap_box2)),
                                      scales = Scales_box)

        Code_box$wrap <- paste0(" + \n  facet_wrap(~ ", input$wrap_box, " + ", input$wrap_box2,
                                 print_scales_box, ")")
      }


      if (input$y_log_box) {

        pl_box <- pl_box + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                         labels = trans_format("log10", math_format(10^.x)))

      }


      # LABELS
      if (input$label_x_box == input$x_input_box) {
        Code_box$labx <- NULL
      }

      if (input$label_x_box != input$x_input_box) {
        pl_box <- pl_box + xlab(input$label_x_box)

        Code_box$labx <- paste0(" + \n  xlab(", "'", input$label_x_box, "'", ")")
      }


      if (input$label_y_box == input$y_input_box) {
        Code_box$laby <- NULL
      }
      if (input$label_y_box != input$y_input_box) {
        pl_box <- pl_box + ylab(input$label_y_box)

        Code_box$laby <- paste0(" + \n  ylab(", "'", input$label_y_box, "'", ")")
      }


      if (!input$coord_flip_box) {
        Code_box$flip <- NULL
      }


      if (input$coord_flip_box) {
        validate(
          need(!(input$faceting_box == "Wrap" & input$scales_box != "fixed"),
               "ggplot2 does not currently support free scales with
               a non-cartesian coord or coord_flip.")
          )
        Code_box$flip <- paste0(" + \n  coord_flip()")
        pl_box <- pl_box + coord_flip()
      }


      # THEME
      Theme_box <- input$theme_box
      Size_box <- input$theme_size_box
      Font_box <- input$theme_font_box

      if (Theme_box == "Grey") {
        pl_box <- pl_box + theme_gray(Size_box, Font_box)
        Code_box$theme <- paste0(" + \n  theme_grey(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "White") {
        pl_box <- pl_box + theme_bw(Size_box, Font_box)
        Code_box$theme <- paste0(" + \n  theme_bw(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "Linedraw") {
        pl_box <- pl_box + theme_linedraw(Size_box, Font_box)
        Code_box$theme <- paste0(" + \n  theme_linedraw(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "Light") {
        pl_box <- pl_box + theme_light(Size_box, Font_box)
        Code_box$theme <- paste0(" + \n  theme_light(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "Minimal") {
        pl_box <- pl_box + theme_minimal(Size_box, Font_box)
        Code_box$theme <- paste0(" + \n  theme_minimal(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "Classic") {
        pl_box <- pl_box + theme_classic(Size_box, Font_box)
        Code_box$theme <- paste0(" + \n  theme_classic(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "Dark") {
          pl_box <- pl_box + theme_dark(Size_box, Font_box)
          Code_box$theme <- paste0(" + \n  theme_dark(", Size_box, ", ", "'",Font_box,"'", ")")
      }
      if (Theme_box == "Void") {
          pl_box <- pl_box + theme_void(Size_box, Font_box)
          Code_box$theme <- paste0(" + \n  theme_void(", Size_box, ", ", "'",Font_box,"'", ")")
      }


      if (Size_box == 11 & Font_box == "sans" & Theme_box == "Grey") {
        Code_box$theme <- NULL
      }


      # LEGEND
      pl_box <- pl_box + theme(legend.position = input$legend_box)

      if (input$legend_box == "right") {
        Code_box$leg <- NULL
      }
      if (input$legend_box != "right") {
        Code_box$leg <- paste0(" + \n  theme(legend.position = ", "'", input$legend_box, "'", ")")
      }


      if (input$title_box != "") {
        pl_box <- pl_box + labs(title = input$title_box) +
          theme(plot.title = element_text(colour = input$title_color_box,
                                          size = input$title_size_box, vjust = 1.5))
        Code_box$title <- paste0(" + \n  labs(title = ", "'", input$title_box, "'", ")",
                                 " + \n  theme(plot.title = element_text(colour = ", "'",
                                 input$title_color_box, "'", ", size = ", input$title_size_box,
                                 ", vjust = 1.5", "))")
      }

      if (input$title_box == "") {
        Code_box$title <- NULL
      }

      #shinyjs::text(id = "print_code_box", text = plot_code_box() )
      updateAceEditor(session, editorId = "print_code_box", value = plot_code_box() )

      plots$pl_box <- pl_box
      pl_box
    }
  })


  # Download plot handler
  downloadGraphObserverServer("boxplot")
  downloadGraphHandlerServer("boxplot", plots$pl_box)


  onStop(function() {
      options("easyPlot.shiny.data" = NULL)
      options("easyPlot.shiny.name" = NULL)
  })

})

