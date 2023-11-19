list(
  fluidRow(
    column(5,
           radioButtons(
             inputId = "sep",
             label =  'Separator',
             choices = c(Comma = ",", Semicolon = ";", Tab = '\t', Whitespace = " "),
             selected = ";")

    ),
    column(5,
           radioButtons(
             inputId = "quote",
             label = "Quote",
             choices = c("None" = '', 'Double Quote' = '"', 'Single Quote' = "'"),
             selected = '"')
    )
  ),
  fluidRow(
      column(5,
             radioButtons(
               inputId = "dec",
               label = "Decimal",
               choices = c(Dot = ".", Comma = ","),
               selected = ".")
             ),
      column(5,
             numericInput(
               inputId = "skip",
               label =  "Start row",
               value = 1,
               step = 1,
               min = 1)
      )
    ),
    fluidRow(
      column(5,
             checkboxInput(inputId = "header",
                           label =  "Header",
                           value = TRUE)
      ),
      column(5,
             list(
               checkboxInput(inputId = "strings_as_factor",
                             label = "Strings as factors",
                             value = TRUE),

               checkboxInput(inputId = "logicals_as_factor",
                             label = "Logicals as factors",
                             value = TRUE)
             )
      )
   )
)
