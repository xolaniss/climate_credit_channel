Sheets_import <-
function(path, skip = 0, range = NULL){
  data<- path %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel,  
        col_names = TRUE,
        path=path, 
        skip = skip,
        range = range,
        progress = readxl_progress(),
       .name_repair = "universal")
}
