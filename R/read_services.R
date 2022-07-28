#' Read service trade data
#'
#' @param ... unused
#'
#' @return
#' @export

read_services <- function(...){

  # Read in service table URLs
  urls <- purrr::map_df(
    c(
      "international-trade-supplementary-information-calendar-year",
      "international-trade-supplementary-information-financial-year"
    ),
    readabs::get_available_files
  ) %>%
    dplyr::filter(
      stringr::str_detect(label, "services|Services"),
      stringr::str_detect(label, "State|state")
      ) %>%
    dplyr::pull(url)


  # Create destinations
  temp_dir <- tempdir()
  dest_file <- file.path(temp_dir, basename(urls))


  # Download all
  mapply(
    FUN      = download.file,
    url      = urls,
    destfile = dest_file,
    quiet    = TRUE,
    mode     = "wb"
  )


  # Ensure file cleanup
  on.exit(unlink(temp_dir))


  # Find all applicable sheets
  table_index <- data.frame(file = dest_file) %>%
    dplyr::group_by(file) %>%
    dplyr::summarise(
      sheet = readxl::excel_sheets(file) %>%
        stringr::str_subset("Table")
      ) %>%
    dplyr::ungroup()


  # Get tables
  table_index <- table_index %>%
    dplyr::mutate(
      title = mapply(
        FUN       = readxl::read_excel,
        path      = file,
        sheet     = sheet,
        range     = "A4",
        col_names = FALSE
      ) %>%
        unlist()
    )


  # Extract information from table titles
  table_index <- table_index %>%
    dplyr::mutate(
      flow = stringr::str_extract(title, "Credit|Debit") %>%
        dplyr::recode(Credit = "Export", Debit = "Import"),
      period = stringr::str_extract(title, "Financial Year|Calendar Year"),
      state = stringr::str_extract(
        title,
        "NSW|Vic.|Qld|SA|WA|Tas.|NT|ACT|Aust."
        )
    )


  # Parse data


  table_index <- table_index %>%
    dplyr::group_by(dplyr::across()) %>%
    dplyr::summarise(
      readxl::read_excel(file, sheet, "A6:W53") %>%
        dplyr::select(-1) %>%
        dplyr::bind_cols(service_hierarchy) %>%
        tidyr::pivot_longer(
          -dplyr::all_of(names(service_hierarchy)),
          names_to = "date"
          )
    ) %>%
    dplyr::ungroup()


  # Clean data classes
  table_index <- table_index %>%
    dplyr::mutate(
      value = abs(as.numeric(value)) * 1000000,
      value = ifelse(is.na(value), 0, value),
      date  = ifelse(
        period == "Financial Year",
        stringr::str_sub(date, 1, 4) %>%
          as.integer() %>%
          `+`(1L) %>%
          as.character(),
        date
        ),
      date = paste0(
        date,
        ifelse(period == "Financial Year", "-06-30", "-12-31")
        ),
      date = as.Date(date)
    )


  # # Clean service names
  # table_index <- table_index %>%
  #   dplyr::left_join(service_hierarchy)


  # Remove superfluous cols
  table_index <- table_index %>%
    dplyr::select(-file, -sheet, -title)


  # Return
  return(table_index)

}


service_hierarchy <- data.frame(
  stringsAsFactors = FALSE,
  # original_service = c(
  #   "Manufacturing services on physical inputs owned by others",
  #   "Maintenance and repair services n.i.e","Transport","Passenger (b)",
  #   "Freight","Other",
  #   "Postal and courier services (c)","Travel",
  #   "Business","Personal","Education-related",
  #   "Other","Construction",
  #   "Insurance and Pension services",
  #   "Direct insurance","Reinsurance",
  #   "Auxiliary services","Pension services",
  #   "Standardised guarantee services",
  #   "Financial Services",
  #   "Charges for the use of intellectual property n.i.e",
  #   "Licences to reproduce and/or distribute computer services",
  #   "Licences to reproduce and/or distribute audiovisual and related services",
  #   "Outcomes of research and development",
  #   "Franchise and trademarks licensing fees",
  #   "Other charges for the use of intellectual property",
  #   "Telecommunications, computer and information services","Telecommunication services",
  #   "Computer and Information services",
  #   "Computer services",
  #   "Information services","Other services",
  #   "Other business services",
  #   "Research and development services",
  #   "Professional and management consulting services",
  #   "Legal, accounting, management consulting, public relations and other professional services",
  #   "Advertising, market research and public opinion polling",
  #   "Technical, trade-related and other business services",
  #   "Architectural, engineering, scientific and other technical services",
  #   "Waste treatment and de-pollution, agricultural and mining services",
  #   "Operational leasing services",
  #   "Trade-related commission services",
  #   "Other business services n.i.e",
  #   "Personal, cultural, and recreational services",
  #   "Audiovisual and related services",
  #   "Other personal, cultural and recreational services",
  #   "Government goods and services n.i.e"),
  service = c(
    "Manufacturing services",
    "Maintenance","Transport",
    "Passenger transport","Freight transport",
    "Other transport","Post & courier",
    "Travel","Business travel",
    "Personal travel","Education travel",
    "Other travel","Construction",
    "Insurance","Direct insurance",
    "Reinsurance","Auxiliary insurance services",
    "Pension services",
    "Standardised guarantee services","Finance",
    "Intelectual property","Software IP",
    "Media IP","R&D IP",
    "Trademarks & franchising","Other IP","ICT",
    "Telecommunications","IT",
    "Computer services","Information services",
    "Other IT services",
    "Other business services","R&D",
    "Professional consulting",
    "Legal, accounting & professional services","Advertising",
    "Technical & trade services",
    "Architecture, engineering, scientific & other technical services",
    "Waste, agricultural & mining services",
    "Operational leasing services",
    "Trade-related commission services",
    "Other business services",
    "Personal & recreational services","Audiovisual services",
    "Other personal, cultural & recreational services",
    "Government services"),
  level = c(
    1L,1L,1L,2L,2L,2L,
    2L,1L,2L,2L,3L,3L,1L,1L,2L,
    2L,2L,2L,2L,1L,1L,2L,2L,2L,
    2L,2L,1L,2L,2L,3L,3L,3L,1L,
    2L,2L,3L,3L,2L,3L,3L,3L,3L,
    3L,1L,2L,2L,1L),
  level_1 = c(
    "Manufacturing services",
    "Maintenance","Transport",
    "Transport","Transport","Transport",
    "Transport","Travel","Travel",
    "Travel","Travel","Travel",
    "Construction","Insurance","Insurance",
    "Insurance","Insurance","Insurance",
    "Insurance","Finance",
    "Intelectual property","Intelectual property",
    "Intelectual property",
    "Intelectual property","Intelectual property",
    "Intelectual property","ICT","ICT",
    "ICT","ICT","ICT","ICT",
    "Other business services",
    "Other business services","Other business services",
    "Other business services",
    "Other business services",
    "Other business services","Other business services",
    "Other business services",
    "Other business services",
    "Other business services","Other business services",
    "Personal & recreational services",
    "Personal & recreational services",
    "Personal & recreational services",
    "Government services"),
  level_2 = c(
    NA,NA,NA,
    "Passenger transport","Freight transport",
    "Other transport","Post & courier",NA,
    "Business travel","Personal travel",
    "Personal travel",
    "Personal travel",NA,NA,"Direct insurance",
    "Reinsurance",
    "Auxiliary insurance services","Pension services",
    "Standardised guarantee services",NA,NA,
    "Software IP","Media IP","R&D IP",
    "Trademarks & franchising",
    "Other IP",NA,"Telecommunications","IT",
    "IT","IT","IT",NA,"R&D",
    "Professional consulting",
    "Professional consulting","Professional consulting",
    "Technical & trade services",
    "Technical & trade services",
    "Technical & trade services",
    "Technical & trade services",
    "Technical & trade services","Technical & trade services",
    NA,"Audiovisual services",
    "Other personal, cultural & recreational services",NA)
)
