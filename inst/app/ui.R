
# Libraries: -------------------------------------------------------------------
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(colourpicker))
library(shinyBS)
library(shinythemes)
library(scales)
library(Cairo)
library(shinyAce)


# ============================= User Interface: ================================


shinyUI(
  tagList(
    shinyjs::useShinyjs(),
    includeCSS("css/easyPlot.css"),
    div(
      id = "loader",
      h2("Loading..."),
      tags$style(type="text/css",
                 ".recalculating {opacity: 1.0;}" ) # No blinking
    ),
    navbarPage(title = "easyPlot", id = "navbar", fluid = TRUE, theme = shinytheme("flatly"),

               tabPanel(title = "Data", icon = icon("table"),

                        sidebarLayout(
                          sidebarPanel(

                            checkboxInput(inputId = "my_data",
                                          label = "My data",
                                          value = TRUE),
                            br(),
                            checkboxInput(inputId = "exampleData",
                                          label = "Use example data",
                                          value = FALSE
                            ),

                            conditionalPanel(
                              condition = "input.exampleData",
                              selectInput(inputId = "Data", label = "Example Datasets:",
                                          choices = c("iris", "mtcars", "faithful", "attitude", "diamonds")),

                              verbatimTextOutput(outputId = "description")
                            ),

                            tags$hr(),

                            checkboxInput(inputId = "upload_data",
                                          label = "Upload a file",
                                          value = FALSE
                            ),
                            conditionalPanel(
                              condition = "input.upload_data",
                              fileInput(inputId = "uploaded",
                                        label = "Upload file (.csv/.xlsx/.txt)",
                                        accept = c("text/csv",
                                                   ".csv",
                                                   "text/comma-separated-values",
                                                   "text/plain",
                                                   ".xlsx",
                                                   ".xls")),

                              bsTooltip(id = "upload_data",
                                        title = "If your data is uploaded and ready to visualize - press the red button `ready`",
                                        placement = "right",
                                        options = list(container = "body")),

                              bsTooltip(id = "readyButton",
                                        title = "If your data is uploaded and ready to visualize then press me",
                                        placement = "right",
                                        options = list(container = "body")),

                              tags$hr(),

                              # Dynamic UI for CSV datasets
                              conditionalPanel(
                                  condition = "input.upload_data == true && output.data_format == 'csvtxt'",
                                  source("UI_widgets_csvtxt.R", local = TRUE)$value
                              ),


                              # Dynamic UI for XLSX datasets
                              conditionalPanel(
                                condition = "input.upload_data == true && output.data_format == 'xlsx'",
                                source("UI_widgets_xlsx.R", local = TRUE)$value
                              ),

                              # Recoding of variables

                              # Data Ready button
                              conditionalPanel(
                                condition = "input.upload_data == true && (output.data_format == 'csvtxt' || output.data_format == 'xlsx')",
                                tags$hr(),
                                bsButton(inputId = "readyButton",
                                         label = "   Ready",
                                         type = "toggle",
                                         value = FALSE)
                              )
                            ) # end conditionalPanel("uploaded")
                          ), # end sidebarPanel
                          mainPanel(
                            bsAlert("dataAlert1"),
                            DT::dataTableOutput(outputId = "table")
                          )
                        ),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
               ),
# ----------------------------- SCATTERPLOT SECTION ----------------------------

     tabPanel(title = "Scatterplot", icon = icon("spinner"),

        bsAlert("dataAlert2"),
        bsAlert("dataAlert2_upload"),
        fluidRow(
          column(2,
               br(),
               br(),
               br(),
               uiOutput(outputId = "brush_info_sc"),
               br(),
               uiOutput(outputId = "click_info_sc")

            ),
        column(6,
               uiOutput(outputId = "scatter_ui")
        )
    ),
    tabsetPanel(
      tabPanel("MAIN",
               fluidRow(
                 # widgets for X-axis -------------------------------------------------
                 column(2,
                        selectInput(inputId = "x_input_sc", label = "X-axis",
                                    choices = ""),
                        checkboxInput(inputId = "show_range_sc", label = "Modify ranges:",
                                      value = FALSE),
                        conditionalPanel(condition = "input.show_range_sc",
                                         uiOutput(outputId = "dynamic_range_x")
                        ),

                        checkboxInput(inputId = "x_log_sc", label = "X-log:",
                                      value = FALSE),
                        textInput(inputId = "label_x_sc", label = "X-label",
                                  value = ""),
                        # COORD FLIP
                        checkboxInput(inputId = "coord_flip_sc", label = "Coord flip",
                                      value = FALSE)

                 ),
                 # widgets for Y-axis -------------------------------------------------
                 column(2,
                        selectInput(inputId = "y_input_sc", label = "Y-axis",
                                    choices = ""),
                        br(),

                        conditionalPanel(condition = "input.show_range_sc",

                                         uiOutput(outputId = "dynamic_range_y")
                        ),
                        checkboxInput(inputId = "y_log_sc", label = "Y-Log:",
                                      value = FALSE),
                        textInput(inputId = "label_y_sc", label = "Y-label",
                                  value = "")

                 ),
                 column(2,
                        #LOESS DYNAMIC SECTION
                        uiOutput(outputId = "loess_out_sc"),

                        conditionalPanel(
                          condition = "input.loess_sc !== 'none'",
                          checkboxInput(inputId = "customize_l_sc",label = "Customize:",
                                        value = FALSE)
                        ),

                        conditionalPanel(
                          condition = "input.customize_l_sc  && input.loess_sc === 'Loess'",

                          colourInput(inputId = "loess_col1_sc", label = "Colour:",
                                      showColour = "background",
                                      value = "blue",
                                      allowTransparent = FALSE),

                          sliderInput(inputId = "loess_size1_sc", label = "Linewidth:",
                                      min = 0.1, max = 5, value = 0.5, step = 0.1),
                          sliderInput(inputId = "loess_span1_sc", label = "Span:",
                                      min = 0, max = 1, value = 0.75, step = 0.05)


                        ),

                        conditionalPanel(
                          condition = "input.customize_l_sc && input.loess_sc === 'Loess + SE'",

                          colourInput(inputId = "loess_col2_sc", label = "Colour:",
                                      showColour = "background",
                                      value = "blue",
                                      allowTransparent = FALSE),

                          colourInput(inputId = "loess_fill2_sc", label = "Colour:",
                                      showColour = "background",
                                      value = "grey60",
                                      allowTransparent = FALSE),

                          sliderInput(inputId = "loess_size2_sc", label = "Linewidth:",
                                      min = 0.1, max = 5, value = 0.5, step = 0.1),
                          sliderInput(inputId = "loess_span2_sc", label = "Span:",
                                      min = 0, max = 1, value = 0.75, step = 0.05)

                        ),

                        uiOutput(outputId = "errbar_out_sc"),

                        conditionalPanel( condition = "input.errbar_sc !== 'none'",
                          checkboxInput(inputId = "customize_err_sc",label = "Customize:",
                                        value = FALSE)
                        ),

                        conditionalPanel(condition = "input.customize_err_sc && input.errbar_sc !== 'none'",

                                         sliderInput(inputId = "err_width_sc", label = "Width:",
                                                     min = 0, max = 1, value = 0.1),

                                         colourInput(inputId = "err_col_sc", label = "Colour:",
                                                     showColour = "background",
                                                     value = "red",
                                                     allowTransparent = FALSE),

                                         sliderInput(inputId = "err_size_sc", label = "Size:",
                                                     min = 0.5, max = 5, value = 2)
                        )),
                 column(2,
                        # FACETS:
                        radioButtons(inputId = "faceting_sc", label = "Facet Type:",
                                     choices = c("none", "Grid", "Wrap"),
                                     selected = "none"),

                        # Grid
                        conditionalPanel(condition = "input.faceting_sc === 'Grid'",

                                         selectInput(inputId = "x_facet_sc", label = "Row split:",
                                                     choices = ""),

                                         selectInput(inputId = "y_facet_sc", label = "Column split:",
                                                     choices = "")
                        ),

                        # Wrap
                        conditionalPanel(condition = "input.faceting_sc === 'Wrap'",

                                         selectInput(inputId = "wrap_sc", label = "Split by:",
                                                     choices = "")
                        ),
                        conditionalPanel(condition = "input.wrap_sc !== '.'",

                                         selectInput(inputId = "wrap_sc2", label = "and",
                                                     choices = "")
                        ),
                        conditionalPanel(condition = "input.faceting_sc !== 'none'",
                                         hr(),
                                         selectInput(inputId = "scales_sc", label = "Scales:",
                                                     choices = c("Default" = "fixed", "Free" = "free",
                                                                 "Free x" = "free_x", "Free y" = "free_y"))
                        )
                 ),
                 column(2,

                        radioButtons(inputId = "point_app_sc", label = "Reduce overplotting",
                                     choices = c("none", "Jitter", "Remove duplicates"))

                 ),
                 column(2,
                        # ADD TITLE
                        checkboxInput(inputId = "add_title_sc", label = "Add the title",
                                      value = FALSE),

                        div(id = "show_title_widgets_sc",

                            textInput(inputId = "title_sc", label = "Title",
                                      value = ""),

                            numericInput(inputId = "title_size_sc", label = "Size",
                                         value = 30, min = 1, max = 50, step = 0.5),

                            colourInput(inputId = "title_color_sc", label = "Title colour:",
                                        showColour = "background",
                                        value = "black",
                                        allowTransparent = FALSE)
                        ),
                        # CHANGE THEME
                        checkboxInput(inputId = "change_theme_sc",
                                      label = "Change the theme",
                                      value = FALSE),

                        div(id = "show_theme_widgets_sc",

                            radioButtons(inputId = "theme_sc", label = "Theme:",
                                         choices = c("Grey", "White", "Linedraw", "Light",
                                                     "Minimal", "Classic", "Dark", "Void")),

                            radioButtons(inputId = "legend_sc", label = "Legend",
                                         choices = c("right", "bottom", "left", "none"),
                                         selected = "right"),

                            numericInput(inputId = "theme_size_sc", label = "Font size",
                                         value = 11, min = 2, max = 50, step = 0.5),

                            selectInput(inputId = "theme_font_sc", label = "Font",
                                        choices = c("sans", "Times", "Courier"))
                        ),
                        # CHANGE SIZE
                        checkboxInput(inputId = "ch_size_sc",label = "Size of the plot:",
                                      value = FALSE),
                        div(id = "show_size_sc",

                            sliderInput("width_sc" , "Plot Width (px)",
                                        min = 400, max = 1500, value = 700,
                                        step = 50),

                            sliderInput("height_sc", "Plot Height (px)",
                                        min = 400, max = 1500, value = 500,
                                        step = 50),

                            actionButton(inputId = "reset_sc", "Reset")
                        ),
                        br(),
                        # SAVE THE PLOT
                        actionButton(inputId = "save_sc", label = "  Download", icon = icon("download")),
                        bsModal(id= "modal_sc", title = "Download the plot", trigger = "save_sc",
                                size = "medium",

                                selectInput(inputId = "download_type_sc", label = "Image format",
                                            choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                            selected = "png"),

                                numericInput(inputId = "download_width_sc", label = "Width (px)",
                                             value = 480, min = 300, max = 10000),

                                numericInput(inputId = "download_height_sc", label = "Height (px)",
                                             value = 480, min = 300, max = 10000),


                                textInput(inputId = "download_name_sc", label = "File name",
                                          value = "GGplot"),

                                downloadButton(outputId = "download_plot_sc", label = "Download")
                        )
                      )
                 )),
      tabPanel("POINTS",
               fluidRow(
                 column(2
                 ),
                 column(2,
                        radioButtons(inputId = "change_point_size_sc", label = "Point size:",
                                     choices = c("Size", "Size by"),
                                     selected = "Size", inline = TRUE),

                        conditionalPanel(condition = "input.change_point_size_sc === 'Size'",

                                         sliderInput(inputId = "point_size_sc", label = "Size:",
                                                     min = 0.5, max = 10, value = 2, step = 0.01)),

                        conditionalPanel(condition = "input.change_point_size_sc === 'Size by'",

                                         selectInput(inputId = "point_size_by_sc", label = "Size by:",
                                                     choices = ""))
                 ),
                 column(2,
                        radioButtons(inputId = "change_color_sc", label = "Colour:",
                                     choices = c("Colour", "Colour by"),
                                     selected = "Colour", inline = TRUE),

                        conditionalPanel(condition = "input.change_color_sc === 'Colour'",
                                         colourInput(inputId = "color_sc", label = "Colour:",
                                                     showColour = "background",
                                                     value = "black",
                                                     allowTransparent = FALSE)
                        ),
                        conditionalPanel(condition = "input.change_color_sc === 'Colour by'",

                                         selectInput(inputId = "color_by_sc", label = "Colour by:",
                                                     choices = "")
                        ),

                        uiOutput(outputId = "dyn_palette_sc"),

                        uiOutput(outputId = "dyn_palette_sc2")
                 ),


                 column(2,
                        selectInput(inputId = "shape_sc", label = "Shape by:",
                                    choices = "")
                 ),
                 column(2,
                        sliderInput(inputId = "opacity_sc", label = "Opacity:",
                                    min = 0, max = 1, value = 1,
                                    step = 0.01)
                 ),
                 column(2
                 )
               ),
               br(),
               br(),
               br(),
               br(),
               br()

      )),

    br(),
    aceEditor(outputId = "print_code_sc", value = "", mode = "r", theme = "texmate", readOnly = FALSE),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()

    ),

    # ----------------------------- HISTOGRAM SECTION --------------------------

    tabPanel(title = "Histogram", icon = icon("chart-column"),

             bsAlert(anchorId = "dataAlert3"),
             bsAlert(anchorId = "dataAlert3_upload"),
             bsAlert(anchorId = "alert_hi"),
             fluidRow(
               column(2),

               column(6,
                      uiOutput(outputId = "Histogram_ui")
               )
             ),

             br(),
             br(),

             fluidRow(
               column(2,
                      selectInput(inputId = "x_input_hi", label = "Variable:",
                                  choices = ""),
                      checkboxInput(inputId = "show_range_hi", label = "Modify ranges:",
                                    value = FALSE),
                      conditionalPanel(condition = "input.show_range_hi",
                                       uiOutput(outputId = "dynamic_range_x_hi")
                      ),

                      checkboxInput(inputId = "x_log_hi", label = "Log10-Transformation:",
                                    value = FALSE),

                      selectInput(inputId = "weight_hi", label = "Weight by:",
                                  choices = "none"),

                      textInput(inputId = "label_x_hi", label = "X-label:",
                                value = ""),

                      checkboxInput(inputId = "set_label_y_hi", "Y-Label", value = FALSE),

                      conditionalPanel(condition = "input.set_label_y_hi",
                          textInput(inputId = "label_y_hi", label = "", value = "")
                      )
               ),
               column(2,
                      # NUMBER OF BREAKS
                      sliderInput(inputId = "breaks_hi", label = "Number of bins:",
                                  min = 2, max = 100, step = 1, value = 30),

                      # OPACITY
                      sliderInput(inputId = "opacity_hi", label = "Opacity:",
                                  min = 0, max = 1, value = 1)
               ),

               column(2,
                      # FILL
                      radioButtons(inputId = "change_fill_hi", label = "Fill:",
                                   choices = c("none", "Count", "Fill by"),
                                   selected = "none"),

                      conditionalPanel(condition = "input.change_fill_hi === 'none' &&
                                       input.change_density_hi !== 'Density'",
                                       colourInput(inputId = "color_hi", label = "Colour:",
                                                   showColour = "background",
                                                   value = '#000000',
                                                   allowTransparent = FALSE),
                                       colourInput(inputId = "fill_hi", label = "Fill:",
                                                   showColour = "background",
                                                   value = "black",
                                                   allowTransparent = FALSE),
                                       actionButton(inputId = "reset_colours_hi", label = "Reset:")
                      ),

                      conditionalPanel(condition = "input.change_fill_hi === 'Fill by'",

                                       selectInput(inputId = "fill_by_hi", label = "Fill by:",
                                                   choices = ""),
                                       radioButtons(inputId = "position_hi", label = "Position:",
                                                    choices = c("Identity" = "identity","Stack" = "stack",
                                                                "Dodge" = "dodge", "Fill" = "fill"),
                                                    selected = "stack")


                      ),
                      conditionalPanel(condition = "input.change_fill_hi === 'Count'",

                                       colourInput(inputId = "high_hi", label = "Gradient high:",
                                                   showColour = "background",
                                                   value = "#56B1F7"),
                                       colourInput(inputId = "low_hi", label = "Gradient low:",
                                                   showColour = "background",
                                                   value = "#132B43"),

                                       actionButton(inputId = "reset_count_hi", "Reset:")

                      )
             ),
             column(2,
                    # FACETS:
                    radioButtons(inputId = "faceting_hi", label = "Facet Type:",
                                 choices = c("none", "Grid", "Wrap"),
                                 selected = "none"),

                    # Grid
                    conditionalPanel(condition = "input.faceting_hi === 'Grid'",

                                     selectInput(inputId = "x_facet_hi", label = "Row split:",
                                                 choices = ""),

                                     selectInput(inputId = "y_facet_hi", label = "Column split:",
                                                 choices = "")
                    ),

                    # Wrap
                    conditionalPanel(condition = "input.faceting_hi === 'Wrap'",

                                     selectInput(inputId = "wrap_hi", label = "Split by:",
                                                 choices = "")
                    ),
                    conditionalPanel(condition = "input.wrap_hi !== '.'",

                                     selectInput(inputId = "wrap_hi2", label = "and:",
                                                 choices = "")
                    ),
                    conditionalPanel(condition = "input.faceting_hi !== 'none'",
                                     hr(),
                                     selectInput(inputId = "scales_hi", label = "Scales:",
                                                 choices = c("Default" = "fixed", "Free" = "free",
                                                             "Free x" = "free_x", "Free y" = "free_y"))
                    )
             ),
             column(2,
                    # DENSITY
                    radioButtons(inputId = "change_density_hi", label = "Density:",
                                 choices = c("no", "Both", "Density"),
                                 selected = "no"),

                    conditionalPanel(condition = "input.change_density_hi !== 'no'",
                                     radioButtons(inputId = "change_density_hi2", label = "Colour:",
                                                  choices = c("Colour", "Colour by"),
                                                  selected = "Colour")
                    ),

                    conditionalPanel(condition = "input.change_density_hi !== 'no' &&
                                     input.change_density_hi2 === 'Colour'",

                                     colourInput(inputId = "dens_color_hi", label = "Colour:",
                                                 showColour = "background",
                                                 value = "#BD1515"),

                                     colourInput(inputId = "dens_fill_hi", label = "Fill:",
                                                 showColour = "background",
                                                 value = "#BD1515"),

                                     sliderInput(inputId = "dens_opacity_hi", label = "Opacity:",
                                                 min = 0, max = 1, value = 0.2),
                                     actionButton(inputId = "reset_dens_hi", label = "Reset:")
                    ),

                    conditionalPanel(condition = "input.change_density_hi !== 'no' &&
                                     input.change_density_hi2 === 'Colour by'",
                                     selectInput(inputId = "dens_fill_by_hi", label = "Colour by:",
                                                 choices = ""),
                                     sliderInput(inputId = "dens_fill_by_opacity_hi", label = "Opacity",
                                                 min = 0, max = 1, value = 0.2),

                                     radioButtons(inputId = "dens_position_hi", label = "Position:",
                                                  choices = c("Identity" = "identity", "Stack" = "stack", "Dodge" = "dodge",
                                                              "Fill" = "fill"),
                                                  selected = "identity")
                    )
             ),
             column(2,
                    # ADD TITLE
                    checkboxInput(inputId = "add_title_hi", label = "Add the title:",
                                  value = FALSE),
                    div(id = "show_title_widgets_hi",

                        textInput(inputId = "title_hi", label = "Title:",
                                  value = ""),

                        numericInput(inputId = "title_size_hi", label = "Size:",
                                     value = 30, min = 1, max = 50, step = 0.5),

                        colourInput(inputId = "title_color_hi", label = "Title colour:",
                                    showColour = "background",
                                    value = "black",
                                    allowTransparent = FALSE)
                    ),
                    # CHANGE THE THEME
                    checkboxInput(inputId = "change_theme_hi", label = "Change the theme:",
                                  value = FALSE),

                    div(id = "show_theme_widgets_hi",

                        radioButtons(inputId = "theme_hi", label = "Theme:",
                                     choices = c("Grey", "White","Linedraw", "Light",
                                                 "Minimal", "Classic", "Dark", "Void")),

                        radioButtons(inputId = "legend_hi", label = "Legend:",
                                     choices = c("right", "bottom", "left"),
                                     selected = "right"),
                        numericInput(inputId = "theme_size_hi", label = "Font size:",
                                     value = 11, min = 2, max = 50, step = 0.5),

                        selectInput(inputId = "theme_font_hi", label = "Font:",
                                    choices = c("sans", "Times", "Courier"))
                    ),
                    # CHANGE THE SIZE OF THE PLOT
                    checkboxInput(inputId = "ch_size_hi",label = "Size of the plot:",
                                  value = FALSE),

                    div(id = "show_size_hi",

                        sliderInput("width_hi" , "Plot Width (px):",
                                    min = 400, max = 1500, value = 700,
                                    step = 50),

                        sliderInput("height_hi", "Plot Height (px):",
                                    min = 400, max = 1500, value = 500,
                                    step = 50),
                        actionButton(inputId = "reset_hi", "Reset:")
                    ),
                    br(),
                    # DOWNLOAD PLOT
                    actionButton(inputId = "save_hi", label = "  Download", icon = icon("download")),
                    bsModal(id= "modal_hi", title = "Download the plot", trigger = "save_hi",
                            size = "medium",

                            selectInput(inputId = "download_type_hi", label = "Image format",
                                        choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                        selected = "png"),

                            numericInput(inputId = "download_width_hi", label = "Width (px)",
                                         value = 480, min = 300, max = 10000),

                            numericInput(inputId = "download_height_hi", label = "Height (px)",
                                         value = 480, min = 300, max = 10000),

                            textInput(inputId = "download_name_hi", label = "File name",
                                      value = "GGplot"),

                            downloadButton(outputId = "download_plot_hi", label = "Download ")
                    )



             )

             ),
             br(),
             aceEditor(outputId = "print_code_hi", value = "", mode = "r", theme = "texmate", readOnly = TRUE),
             br(),
             br(),
             br(),
             br(),
             br(),
             br()

    ),
    # ----------------------------- BOXPLOT SECTION -------------------------------
    tabPanel(title = "Boxplot", icon = icon("sliders", class = "fa-rotate-90"),
             bsAlert("dataAlert4"),
             bsAlert("dataAlert4_upload"),
             bsAlert("alert_box1"),
             bsAlert("alert_box2"),
             fluidRow(
               column(2),
               column(6,
                      uiOutput(outputId = "box_plot")
               )
             ),
             br(),
             fluidRow(
               column(2,
                      selectInput(inputId = "x_input_box", label = "X-Axis:",
                                  choices = ""),

                      checkboxInput(inputId = "show_range_box", label = "Modify ranges:",
                                    value = FALSE),
                      conditionalPanel(condition = "input.show_range_box",

                                       uiOutput(outputId = "dynamic_factors_box")
                      ),

                      textInput(inputId = "label_x_box", label = "X-label:",
                                value = ""),

                      checkboxInput(inputId = "coord_flip_box", label = "Coord flip:",
                                    value = FALSE)

               ),
               column(2,
                      selectInput(inputId = "y_input_box", label = "Y-Axis:",
                                  choices = ""),

                      conditionalPanel(condition = "input.show_range_box",
                                       uiOutput(outputId = "dynamic_range_y_box")
                      ),
                      br(),
                      br(),

                      textInput(inputId = "label_y_box", label = "Y-label:",
                                value = ""),
                      checkboxInput(inputId = "y_log_box", label = "Y-Log:",
                                    value = FALSE)

               ),
               column(2,
                      radioButtons(inputId = "change_fill_box", label = "Colours:",
                                   choices = c("Colours", "Colour by"),
                                   selected = "Colours", inline = FALSE),

                      conditionalPanel(condition = "input.change_fill_box === 'Colours'",

                                       colourInput(inputId = "fill_box", label = "Fill:",
                                                   showColour = "background",
                                                   value = "white",
                                                   allowTransparent = FALSE),

                                       colourInput(inputId = "color_box", label = "Colour:",
                                                   showColour = "background",
                                                   value = "black",
                                                   allowTransparent = FALSE),

                                       sliderInput(inputId = "colour_size_box", "Box size:",
                                                   min = 0.1, max = 2.5,
                                                   value = 0.5, step = 0.1),

                                       actionButton(inputId = "reset_colours_box", label = "Reset")
                      ),

                      conditionalPanel(condition = "input.change_fill_box === 'Colour by'",

                                       selectInput(inputId = "fill_by_box", label = "Colour by:",
                                                   choices = ""),

                                       selectInput(inputId = "fill_by_col_box", label = "Type:",
                                                   choices = c("Default" = "default", "Qualitative" = "qual",
                                                               "Sequential" = "seq", "Diverging" = "div"))
                      ),
                      conditionalPanel(condition = "input.change_fill_box === 'Colour by' &&
                                       input.fill_by_col_box !== 'default'",
                                       sliderInput(inputId = "palette_box", label = "Palette:",
                                                   min = 1, max = 8, value = 1, step = 1)
                      )
             ),
             column(2,
                    # FACETS:
                    radioButtons(inputId = "faceting_box", label = "Facet Type:",
                                 choices = c("none", "Grid", "Wrap"),
                                 selected = "none"),

                    # Grid
                    conditionalPanel(condition = "input.faceting_box === 'Grid'",

                                     selectInput(inputId = "x_facet_box", label = "Row split:",
                                                 choices = ""),

                                     selectInput(inputId = "y_facet_box", label = "Column split:",
                                                 choices = "")
                    ),

                    # Wrap
                    conditionalPanel(condition = "input.faceting_box === 'Wrap'",

                                     selectInput(inputId = "wrap_box", label = "Split by:",
                                                 choices = "")
                    ),
                    conditionalPanel(condition = "input.wrap_box !== '.'",

                                     selectInput(inputId = "wrap_box2", label = "and:",
                                                 choices = "")
                    ),

                    conditionalPanel(condition = "input.faceting_box !== 'none'",
                                     hr(),
                                     selectInput(inputId = "scales_box", label = "Scale:",
                                                 choices = c("Default" = "fixed", "Free" = "free",
                                                             "Free x" = "free_x", "Free y" = "free_y"))
                    )
             ),
             column(2,
                    sliderInput(inputId = "box_width", label = "Box width:",
                                min = 0.05, max = 1, value = 1),

                    sliderInput(inputId = "opacity_box", label = "Opacity:",
                                min = 0, max = 1, value = 1,
                                step = 0.01),
                    hr(),
                    sliderInput(inputId = "out_size", label = "Outlier size:",
                                min = 1, max = 5, value = 2, step = 0.25),

                    selectInput(inputId = "out_shape", label = "Outlier shape",
                                choices = c("Dot" = 16, "Solid square" = 15, "Solid triangle" = 17,
                                            "Solid rhombus" = 18, "Circle" = 1, "Square" = 0,
                                            "Triangle" = 2, "Rhombus" = 5)),

                    colourInput(inputId = "out_color", label = "Colour:", showColour = "background",
                                value = "black", allowTransparent = FALSE)
             ),


             column(2,

                    # ADD TITLE
                    checkboxInput(inputId = "add_title_box", label = "Add the title:",
                                  value = FALSE),

                    div(id = "show_title_widgets_box",

                        textInput(inputId = "title_box", label = "Title:",
                                  value = ""),

                        numericInput(inputId = "title_size_box", label = "Size:",
                                     value = 30, min = 1, max = 50, step = 0.5),

                        colourInput(inputId = "title_color_box", label = "Title colour:",
                                    showColour = "background",
                                    value = "black",
                                    allowTransparent = FALSE)
                    ),
                    # CHANGE THEME
                    checkboxInput(inputId = "change_theme_box",
                                  label = "Change the theme:",
                                  value = FALSE),

                    div(id = "show_theme_widgets_box",

                        radioButtons(inputId = "theme_box", label = "Theme:",
                                     choices = c("Grey", "White","Linedraw", "Light",
                                                 "Minimal", "Classic", "Dark", "Void")),

                        radioButtons(inputId = "legend_box", label = "Legend:",
                                     choices = c("right", "bottom", "left", "none"),
                                     selected = "right"),

                        numericInput(inputId = "theme_size_box", label = "Font size:",
                                     value = 11, min = 2, max = 50, step = 0.5),

                        selectInput(inputId = "theme_font_box", label = "Font:",
                                    choices = c("sans", "Times", "Courier"))
                    ),
                    # CHANGE SIZE
                    checkboxInput(inputId = "ch_size_box",label = "Size of the plot:",
                                  value = FALSE),
                    div(id = "show_size_box",

                        sliderInput("width_box" , "Plot Width (px):",
                                    min = 400, max = 1500, value = 700,
                                    step = 50),

                        sliderInput("height_box", "Plot Height (px):",
                                    min = 400, max = 1500, value = 500,
                                    step = 50),

                        actionButton(inputId = "reset_box", "Reset")
                    ),
                    # DOWNLOAD PLOT
                    br(),
                    actionButton(inputId = "save_box", label = "  Download", icon = icon("download")),
                    bsModal(id= "modal_box", title = "Download the plot", trigger = "save_box",
                            size = "medium",

                            selectInput(inputId = "download_type_box", label = "Image format",
                                        choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                        selected = "png"),

                            numericInput(inputId = "download_width_box", label = "Width (px)",
                                         value = 480, min = 300, max = 10000),

                            numericInput(inputId = "download_height_box", label = "Height (px)",
                                         value = 480, min = 300, max = 10000),

                            textInput(inputId = "download_name_box", label = "File name",
                                      value = "GGplot"),

                            downloadButton(outputId = "download_plot_box", label = "Download ")
                    )
             )
               ),
             br(),
             aceEditor(outputId = "print_code_box", value = "", mode = "r", theme = "texmate", readOnly = TRUE),
             br(),
             br(),
             br(),
             br(),
             br(),
             br()
             ),
    # ----------------------------- BAR GRAPH SECTION ------------------------------
    tabPanel(title = "Bar graph", icon = icon("align-left", class = "fa-rotate-270"),
             bsAlert(anchorId = "dataAlert5"),
             bsAlert(anchorId = "dataAlert5_upload"),
             bsAlert(anchorId = "alert_ba"),
             fluidRow(
               column(2),
               column(6,
                      uiOutput(outputId = "bar_plot")
               )
             ),
             br(),
             fluidRow(
               column(2,
                      selectInput(inputId = "x_input_ba", label = "Variable:",
                                  choices = ""),

                      uiOutput(outputId = "dynamic_factors_ba"),

                      selectInput(inputId = "weight_ba", label = "Weight by:",
                                  choices = ""),

                      textInput(inputId = "label_x_ba", label = "X-label:",
                                value = ""),

                      checkboxInput(inputId = "set_label_y_ba", "Y-Label", value = FALSE),

                      conditionalPanel(condition = "input.set_label_y_ba",
                                       textInput(inputId = "label_y_ba", label = "", value = "")
                      ),
                      checkboxInput(inputId = "coord_flip_ba", label = "Coord flip:",
                                    value = FALSE)

               ),
               column(2,

                      radioButtons(inputId = "change_fill_ba", label = "Colours:",
                                   choices = c("Colours", "Colour by"),
                                   selected = "Colours"),

                      conditionalPanel(condition = "input.change_fill_ba === 'Colours'",

                                       colourInput(inputId = "fill_ba", label = "Fill:",
                                                   showColour = "background",
                                                   value = "black",
                                                   allowTransparent = FALSE),
                                       colourInput(inputId = "color_ba", label = "Colour:",
                                                   showColour = "background",
                                                   value = "black",
                                                   allowTransparent = FALSE),
                                       sliderInput(inputId = "colour_size_ba", "Box Linewidth:",
                                                   min = 0.1, max = 5, value = 0.5),
                                       actionButton(inputId = "reset_colours_ba", label = "Reset")
                      ),

                      conditionalPanel(condition = "input.change_fill_ba === 'Colour by'",

                                       selectInput(inputId = "fill_by_ba", label = "Colour by:",
                                                   choices = ""),

                                       selectInput(inputId = "fill_by_col_ba", label = "Type:",
                                                   choices = c("Default" = "default", "Qualitative" = "qual",
                                                               "Sequential" = "seq", "Diverging" = "div"))
                      ),
                      conditionalPanel(condition = "input.change_fill_ba === 'Colour by' &&
                                       input.fill_by_col_ba !== 'default'",
                                       sliderInput(inputId = "palette_ba", label = "Palette:",
                                                   min = 1, max = 8, value = 1, step = 1)
                      )
             ),
             column(2,
                    sliderInput(inputId = "bar_width", label = "Bar width:",
                                min = 0.01, max = 1, value = 0.9),

                    sliderInput(inputId = "opacity_ba", label = "Opacity:",
                                min = 0, max = 1, value = 1,
                                step = 0.01)
             ),
             column(2,
                    # FACETS:
                    radioButtons(inputId = "faceting_ba", label = "Facet Type:",
                                 choices = c("none", "Grid", "Wrap"),
                                 selected = "none"),

                    # Grid
                    conditionalPanel(condition = "input.faceting_ba === 'Grid'",

                                     selectInput(inputId = "x_facet_ba", label = "Row split:",
                                                 choices = ""),

                                     selectInput(inputId = "y_facet_ba", label = "Column split:",
                                                 choices = "")
                    ),

                    # Wrap
                    conditionalPanel(condition = "input.faceting_ba === 'Wrap'",

                                     selectInput(inputId = "wrap_ba", label = "Split by:",
                                                 choices = "")
                    ),
                    conditionalPanel(condition = "input.wrap_ba !== '.'",

                                     selectInput(inputId = "wrap_ba2", label = "and:",
                                                 choices = "")
                    ),

                    conditionalPanel(condition = "input.faceting_ba !== 'none'",
                                     hr(),
                                     selectInput(inputId = "scales_ba", label = "Scale:",
                                                 choices = c("Default" = "fixed", "Free" = "free",
                                                             "Free x" = "free_x", "Free y" = "free_y"))
                    )
             ),
             column(2,
                    radioButtons(inputId = "position_ba", label = "Position:",
                                 choices = c("Stack" = "stack", "Dodge" = "dodge",
                                             "Fill" = "fill"),
                                 selected = "stack")
             ),


             column(2,

                    # ADD TITLE
                    checkboxInput(inputId = "add_title_ba", label = "Add the title:",
                                  value = FALSE),

                    div(id = "show_title_widgets_ba",

                        textInput(inputId = "title_ba", label = "Title:",
                                  value = ""),

                        numericInput(inputId = "title_size_ba", label = "Size:",
                                     value = 30, min = 1, max = 50, step = 0.5),

                        colourInput(inputId = "title_color_ba", label = "Title colour:",
                                    showColour = "background",
                                    value = "black",
                                    allowTransparent = FALSE)
                    ),
                    # CHANGE THEME
                    checkboxInput(inputId = "change_theme_ba",
                                  label = "Change the theme:",
                                  value = FALSE),

                    div(id = "show_theme_widgets_ba",

                        radioButtons(inputId = "theme_ba", label = "Theme:",
                                     choices = c("Grey", "White","Linedraw", "Light",
                                                 "Minimal", "Classic", "Dark", "Void")),

                        radioButtons(inputId = "legend_ba", label = "Legend:",
                                     choices = c("right", "bottom", "left", "none"),
                                     selected = "right"),

                        numericInput(inputId = "theme_size_ba", label = "Font size:",
                                     value = 11, min = 2, max = 50, step = 0.5),

                        selectInput(inputId = "theme_font_ba", label = "Font:",
                                    choices = c("sans", "Times", "Courier"))
                    ),
                    # CHANGE SIZE
                    checkboxInput(inputId = "ch_size_ba",label = "Size of the plot:",
                                  value = FALSE),
                    div(id = "show_size_ba",

                        sliderInput("width_ba" , "Plot Width (px):",
                                    min = 400, max = 1500, value = 700,
                                    step = 50),

                        sliderInput("height_ba", "Plot Height (px):",
                                    min = 400, max = 1500, value = 500,
                                    step = 50),

                        actionButton(inputId = "reset_ba", "Reset")
                    ),
                    # DOWNLOAD PLOT
                    br(),
                    actionButton(inputId = "save_ba", label = "  Download", icon = icon("download")),
                    bsModal(id = "modal_ba", title = "Download the plot", trigger = "save_ba",
                            size = "medium",

                            selectInput(inputId = "download_type_ba", label = "Image format",
                                        choices = c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                        selected = "png"),

                            numericInput(inputId = "download_width_ba", label = "Width (px)",
                                         value = 480, min = 300, max = 10000),

                            numericInput(inputId = "download_height_ba", label = "Height (px)",
                                         value = 480, min = 300, max = 10000),

                            textInput(inputId = "download_name_ba", label = "File name",
                                      value = "GGplot"),

                            downloadButton(outputId = "download_plot_ba", label = "Download "))
               )
             ),
             br(),
             aceEditor(outputId = "print_code_ba", value = "", mode = "r", theme = "chrome", readOnly = TRUE),
             br(),
             br(),
             br(),
             br(),
             br(),
             br()

             ),
    tabPanel(title = "", value = "quit", icon = icon("power-off"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             fluidRow(
               column(3),
               column(6,
                      h2("Thank you for using easyPlot!")
               )
             )
   )
  )
 )
)
