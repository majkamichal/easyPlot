# -----------------------------------------------------------------------------------------------------------

# This EasyPlot module allows storing created graph in multiple formats
# with multiple settings such as unit, height, width and resolution in ppi.

# -----------------------------------------------------------------------------------------------------------

downloadGraphButtonUI <- function(id) {
  actionButton(inputId = NS(id, "save_graph"), label = "  Download", icon = icon("download"))
}


downloadGraphModalUI <- function(id, size = "m") {

  ns <- NS(id)

  tagList(

    modalDialog(title = "Download the plot",

        selectInput(inputId = ns("download_type"),
                    label = "Image format",
                    choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                    selected = "png"),

          selectInput(inputId = ns("download_unit"),
                      label = "Choose plot unit",
                      choices = c("px", "in", "cm", "mm")),

          numericInput(inputId = ns("download_width"), label = "Width",
                       value = 1024, min = 1, max = 3000),

          numericInput(inputId = ns("download_height"), label = "Height",
                       value = 768, min = 1, max = 3000),

          numericInput(inputId = ns("download_resolution"),
                       label = "Plot resolution (in ppi)",
                       value = 72,
                       min = 1,
                       max = 1000),

          textInput(inputId = ns("download_name"),
                    label = "File name",
                    value = "GGplot"),

          downloadButton(outputId = ns("download_plot"),
                         label = "Download"),

        footer = modalButton("Dismiss"),
        size = size,
        easyClose = FALSE,
        fade = TRUE,
      )
   )
}


downloadGraphHandlerServer <- function(id, plot) {

    moduleServer(id, function(input, output, session) {

      # PNG/JPEG/TIFF
      download_type <- reactive({
        input$download_type
      })

      # File name
      filename <- function() {
        paste0(input$download_name, ".", input$download_type)
      }

      # Content (graph)
      content <- function(file) {
        fun <- match.fun(download_type())
        fun(file,
            height = input$download_height,
            width = input$download_width,
            units = input$download_unit,
            res = input$download_resolution)
        print(plot)
        dev.off()
      }

      # Download plot
      output$download_plot <- downloadHandler(filename = filename,
                                              content = content)

    })
}

downloadGraphObserverServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    observeEvent(input$save_graph, {
      showModal(
        downloadGraphModalUI(id)
      )
    })

  })
}
