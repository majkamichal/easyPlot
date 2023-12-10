recode_class_render_UI <- function(cID) {
  uiOutput(NS(cID, "recode_class_dynamic"))
}


recode_class_render_UI2 <- function(cID) {
  uiOutput(NS(cID, "recode_class_dynamic2"))
}


recode_class_render_UI3 <- function(cID) {
  uiOutput(NS(cID, "recode_class_dynamic3"))
}


recode_class_render_UI4 <- function(cID) {
  uiOutput(NS(cID, "recode_class_dynamic4"))
}


recode_class_render_UI5 <- function(cID) {
  uiOutput(NS(cID, "recode_class_dynamic5"))
}


recode_class_server <- function(cID, input_exampleData, input_upload, input_uploaded, input_my_data) {

  moduleServer(cID, function(input, output, session) {

    output$recode_class_dynamic <- renderUI({

      # widget <- checkboxInput(inputId = NS(cID, "recode_class"),
      #                         label = "Recode variables",
      #                         value = FALSE)
      widget <- prettySwitch(inputId = NS(cID, "recode_class"),
                             label = "Recode variables",
                             status = "primary",
                             fill = TRUE)

      if (input_my_data()) {

        return(widget)

      } else if (input_exampleData()) {

        return(widget)

      } else if (input_upload()) {

        upFile <- input_uploaded()
        req(upFile)
        csv_ <- any(endsWith(upFile$name, c(".csv", ".txt")))
        xlsx_ <- any(endsWith(upFile$name, c(".xlsx", ".xls")))

        if (csv_ | xlsx_) {
          return(widget)
        }

      } else {

        return(NULL)

      }
    })
  })
}


recode_target_class_server <- function(cID) {

  moduleServer(cID, function(input, output, session) {

    output$recode_class_dynamic2 <- renderUI({

      req(input$recode_class)

      if (input$recode_class == TRUE) {

          selectInput(inputId = NS(cID, "recode_target_class"),
                      label = "Select new class",
                      choices = c("Factor" = "factor",
                                  "Date" = "Date",
                                  "Datetime" = "POSIXct",
                                  "Numeric" = "numeric",
                                  "Character" = "character"))

      } else {
        NULL
      }
    })
  })
}


recode_class_vars_server <- function(cID, x) {

  moduleServer(cID, function(input, output, session) {

    output$recode_class_dynamic3 <- renderUI({
      req(x)
      req(input$recode_class)
      req(input$recode_target_class)

      if (input$recode_class == TRUE) {

        data <- head(x())
        new_choices <- fun_find_vars_to_recode(data, input$recode_target_class)

        selectInput(inputId = NS(cID, "recode_class_vars"),
                    label = "Select variables to recode:",
                    multiple = TRUE,
                    choices = new_choices)

      } else {
        NULL
      }

    })
  })
}


recode_class_convert_server <- function(cID) {

  moduleServer(cID, function(input, output, session) {

    output$recode_class_dynamic4 <- renderUI({

      req(input$recode_class)

      if (input$recode_class == TRUE) {
        actionButton(inputId = NS(cID, "recode_class_convert"),
                     label = "Convert",
                     class = "btn-primary") # add icon

      } else {
        NULL
      }

    })
  })
}


recode_class_reset_server <- function(cID) {

  moduleServer(cID, function(input, output, session) {

    output$recode_class_dynamic5 <- renderUI({

      req(input$recode_class)

      if (input$recode_class == TRUE) {
        actionButton(inputId = NS(cID, "recode_class_reset"),
                     label = "Reset",
                     class = "btn-link") # add icon

      } else {
        NULL
      }

    })
  })
}



recode_class_conversion_server <- function(cID, data) {

  moduleServer(cID, function(input, output, session) {

    observeEvent(input$recode_class_convert, {

      selected_vars <- input$recode_class_vars
      target_class <- input$recode_target_class

      req(data())
      req(target_class)
      req(selected_vars)

      data_new <- data()
      data_new <- fun_selected_vars_to_class(data_new, selected_vars, target_class)
      data(data_new)
      showNotification("Dataset has been updated",
                       duration = 2.5,
                       closeButton = TRUE
      )
    })
  })
}


recode_class_reset_server2 <- function(cID, data, backup) {

  moduleServer(cID, function(input, output, session) {

    observeEvent(input$recode_class_reset, {
      req(data())
      req(backup())
      data(backup())
      showNotification("Dataset has been reset",
        duration = 2.5,
        closeButton = TRUE
      )
    })

  })
}


fun_var_to_class <- function(x, target_class) {

  fun <- switch(target_class,
           "factor" = function(x) as.factor(x),
           "Date" = function(x) as.Date(x, origin = "1970-01-01"),
           "POSIXct" = function(x) as.POSIXct(x, origin = "1970-01-01"),
           "numeric" = function(x) as.numeric(x),
           "character" = function(x) as.character(x))

  tryCatch(fun(x), error = function(e) NA)
}


fun_selected_vars_to_class <- function(df, selected_vars, target_class) {

  fun <- function(x) fun_var_to_class(x, target_class)
  df[ ,selected_vars] <- lapply(df[selected_vars], fun)
  df
}


fun_find_vars_to_recode <- function(data, target_class) {

  # Convert all variables into a new class
  converted_data <-
    suppressWarnings(as.data.frame(sapply(data, function(x)
      fun_var_to_class(x, target_class))))

  # Find IDs of variables that could be converted (not NA)
  selected_columns <- sapply(converted_data, function(x) !all(is.na(x)))

  # Find classes of variables that can be converted to the target class
  column_classes <- sapply(data[selected_columns], function(x) class(x)[1])

  # Build a list with variable names for a given class
  choices <- list()

  for (class_name in unique(column_classes)) {
    class_variables <- names(column_classes[column_classes == class_name])
    choices[class_name] <- list(as.list(class_variables))
  }

  # Remove target class from choices
  choices[target_class] <- NULL

  # Recode names
  names_ <- tools::toTitleCase(as.character(names(choices)))
  names_[names_ == "POSIXct"] <- "Datetime"
  names(choices) <- names_

  # Return choices
  choices
}
