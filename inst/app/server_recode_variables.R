recode_class_reset_server("uploaded")
recode_class_reset_server2("uploaded", dataForTable, dataForTableBackup)

recode_class_reset_server("example_data")
recode_class_reset_server2("example_data", dataForTable, dataForTableBackup)

recode_class_reset_server("my_data")
recode_class_reset_server2("my_data", dataForTable, dataForTableBackup)

recode_class_conversion_server("uploaded", dataForTable)
recode_class_server("uploaded",
                    reactive(input$exampleData),
                    reactive(input$upload_data),
                    reactive(input$uploaded),
                    reactive(input$my_data))
recode_target_class_server("uploaded")
recode_class_vars_server("uploaded", dataForTable)
recode_class_convert_server("uploaded")

recode_class_conversion_server("example_data", dataForTable)
recode_class_server("example_data",
                    reactive(input$exampleData),
                    reactive(input$upload_data),
                    reactive(input$uploaded),
                    reactive(input$my_data))
recode_target_class_server("example_data")
recode_class_vars_server("example_data", dataForTable)
recode_class_convert_server("example_data")


recode_class_conversion_server("my_data", dataForTable)
recode_class_server("my_data",
                    reactive(input$exampleData),
                    reactive(input$upload_data),
                    reactive(input$uploaded),
                    reactive(input$my_data))
recode_target_class_server("my_data")
recode_class_vars_server("my_data", dataForTable)
recode_class_convert_server("my_data")
