list(

  selectInput(inputId = "select_xlsx_sheet",
              label = "Select worksheet",
              choices = ""),

  selectInput(inputId = "select_xlsx_named_region",
              label = "Select named region",
              choices = "None"),

  checkboxInput(inputId = "strings_as_factor_xlsx",
                label = "Strings as factors",
                value = TRUE),

  checkboxInput(inputId = "logicals_as_factor_xlsx",
                label = "Logicals as factors",
                value = TRUE),

  numericInput(
    inputId = "skip_xlsx",
    label =  "Start row",
    value = 1,
    step = 1,
    min = 1)
)
