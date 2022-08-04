

# List of Goods Exports

viz_exportlist <- function(
  data    = merch,
	country = "Total",
	date    = "max"
  ){

  filtered <- data %>%
    dplyr::filter(country_dest %in% country, origin == "Victoria")

  if (!any(class(filtered) == 'data.frame')) {
    filtered <- filtered %>%
      dplyr::collect() %>%
      dplyr::mutate(date = lubridate::ymd(date))
  }


	if(class(date) == "character") {
		ref_date <- max(filtered$date)
	} else {
			ref_date <- lubridate::floor_date(filtered$date, unit = "months")
		}

	df_month <- format(max(filtered$date), format = "%Y-%b")

	df_2020 <- filtered %>%
	  dplyr::filter(
	    date %in% c(
	      ref_date,
	      seq.Date(ref_date, by = "-1 year", length.out = 2)[2])
	    ) %>%
	  dplyr::arrange(dplyr::desc(date), dplyr::desc(value)) %>%
	  dplyr::select(date, sitc, sitc_code, value) %>%
	  dplyr::filter(date < lubridate::floor_date(ref_date, unit = "years"))

	df_2021 <- filtered %>%
	  dplyr::filter(
	    date %in% c(
	      ref_date,
	      seq.Date(ref_date, by = "-1 year", length.out = 2)[2]
	      )
	    ) %>%
	  dplyr::arrange(dplyr::desc(date), dplyr::desc(value)) %>%
	  dplyr::select(date, sitc, sitc_code, value) %>%
	  dplyr::filter(
	    date >= lubridate::floor_date(ref_date, unit = "months")
	    )

	dplyr::left_join(df_2021, df_2020, by = c("sitc", "sitc_code")) %>%
	  dplyr::transmute(
	    SITC                             = sitc,
			`SITC Code`                      = sitc_code,
			`Export Value (000s)`            = value.x,
			`Change since 12 months ago (%)` = round(
			  100 * (`Export Value (000s)` / value.y - 1), 2
			  )
			) %>%
	DT::datatable(
	  options = list(
	    paging = TRUE,
			pageLength = 10,
			scrollX = TRUE,
			scrollY = TRUE,
			autoWidth = TRUE,
			server = FALSE,
			dom = "Bfrtip",
			buttons = c('csv', 'excel'),
			columnDefs = list(list(width = '200px', targets = c(1)))
			),
	  extensions = "Buttons",
		selection = "Single",
		filter = "bottom"
		)
}

# Top 10 Markets for Goods Exports

viz_launchpad_marketlist <- function(data = merch) {

  filtered <- data %>%
    dplyr::filter(
      country_dest != "Total",
      sitc == "Total",
      origin == "Victoria"
      )

  if (!any(class(filtered) == 'data.frame')) {
    filtered <- filtered %>%
      dplyr::collect() %>%
      dplyr::mutate(date = lubridate::ymd(date))
  }

	country_list <- filtered %>%
	  dplyr::arrange(dplyr::desc(date), dplyr::desc(value)) %>%
		utils::head(10) %>%
	  dplyr::select(country_dest) %>%
		as.matrix()
}



# Services Headline
clean_dates <- function(data){

  datelist = data %>%
    dplyr::select(date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::slice(1,2,4,13)

  latest_date = datelist$date[1]
  prev_date = datelist$date[2]
  prev_qtr = datelist$date[3]
  prev_year = datelist$date[4]
  nice_latest_date = format(latest_date, "%b %Y")
  nice_prev_date = format(prev_date, "%b %Y")
  nice_prev_qtr = format(prev_qtr, "%b %Y")
  nice_prev_year = format(prev_year, "%b %Y")
  since_prev_date = paste0("Since ", nice_prev_date)
  since_prev_qtr = paste0("Since ", nice_prev_qtr)
  since_prev_year = paste0("Since ", nice_prev_year)

  list(
    datelist = datelist,
    latest_date = latest_date,
    prev_date = prev_date,
    prev_qtr = prev_qtr,
    prev_year = prev_year,
    nice_latest_date = nice_latest_date,
    nice_prev_date = nice_prev_date,
    nice_prev_qtr = nice_prev_qtr,
    nice_prev_year = nice_prev_year,
    since_prev_date = since_prev_date,
    since_prev_qtr = since_prev_qtr,
    since_prev_year = since_prev_year
  )

}



# Tables - Country

tab_launchpad_country_imp_exp <- function(
  direction = c('import', 'export'),
  data,
  rows      = 5,
  smooth    = TRUE
  ){

  direction = match.arg(direction)

  imp_ext_fields <- list(
    import = list(partner = 'country_origin', vic = 'dest'),
    export = list(vic = 'origin', partner = 'country_dest')
  )

  dir_fields <- imp_ext_fields[[direction]]

  list2env(
    clean_dates(data), envir = environment()
  )

  #generate table

  #country_list <- data %>%
    #filter(#date %in% !!datelist$date,  ############  IMPORTANT)

  # imports
  if (direction == 'import') {
    country_list <- data %>%
      dplyr::filter(
        sitc == "Total",
        country_origin != "Total",
        dest == "Victoria"
        )
  # exports
  } else if (direction == 'export') {
    country_list <- data %>%
      dplyr::filter(
        sitc == "Total",
        country_dest != "Total"
        )
  }


  country_list <- country_list %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(date))


  if(smooth) {

    all_dates <- country_list %>%
      dplyr::select(date) %>%
      unique()

    country_list <- country_list %>%
      tidyr::expand(
        .data[[dir_fields$vic]],
        .data[[dir_fields$partner]],
        all_dates
        ) %>%
      dplyr::left_join(
        country_list,
        by = c(dir_fields$vic, dir_fields$partner, "date")
        ) %>%
      dplyr::group_by(.data[[dir_fields$vic]], .data[[dir_fields$partner]]) %>%
      dplyr::mutate(
        value = tidyr::replace_na(value, 0),
        value = slider::slide_mean(value, before = 11L)
        ) %>%
      dplyr::ungroup()

  } else {

    country_list <- country_list %>%
      dplyr::group_by(.data[[dir_fields$partner]]) %>%
      dplyr::ungroup()

  }


  country_list <- country_list %>%
    dplyr::filter(date %in% datelist$date) %>%
    dplyr::group_by(.data[[dir_fields$partner]], date) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::select(dplyr::one_of(dir_fields$partner), date, value) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::arrange(.data[[dir_fields$partner]], date)
    #ungroup()%>%
    #ungroup()%>%
    #group_by(.data[[direction]]) %>%




  country_list <- country_list %>%
    dplyr::group_by(.data[[dir_fields$partner]]) %>%
    dplyr::mutate(
      prev_month_change = paste0(
        scales::comma(value-dplyr::lag(value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (value-dplyr::lag(value,1)) / dplyr::lag(value,1)
          ),
        ")"
        ),
      prev_qtr_change = paste0(
        scales::comma(value-dplyr::lag(value,2), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value, 2)) / dplyr::lag(value, 2)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(value - dplyr::lag(value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,3)) / dplyr::lag(value, 3)
          ),
        ")"
        )
    )%>%
    dplyr::filter(date==datelist$date[1])%>%
    dplyr::arrange(dplyr::desc(value))%>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      value = paste0(
        scales::comma(value, accuracy = 1),
        "\n(",
        scales::percent(
          value / sum(value, na.rm = TRUE),
          accuracy = 1.1
          ),
        ")"
        )
      )%>%
    dplyr::ungroup()%>%
    dplyr::select(-c(date)) %>%
    dplyr::rename(
      ` `                        := .data[[dir_fields$partner]],
      !!paste0(nice_latest_date) := value,
      !!paste0(nice_prev_date)   := prev_month_change,
      !!paste0(nice_prev_qtr)    := prev_qtr_change,
      !!paste0(nice_prev_year)   := prev_year_change
    )%>%
    utils::head(rows)

  return(country_list)
}





# Tables - Product

tab_launchpad_product_imp_exp <- function(direction = c('import', 'export'), data, sitc_level = 1, rows = 5, smooth = TRUE) {


  direction = match.arg(direction)

  country_field <- switch(
    direction,
    'import' = 'country_origin',
    'export' = 'country_dest'
  )
  source_field <- switch(
    direction,
    'import' = 'dest',
    'export' = 'origin'
  )

  list2env(
    clean_dates(data), envir = environment()
  )

  #generate table

  country_filter <- glue::glue('"{country_field}"')
  source_filter <- glue::glue('"{source_field}"')

  product_list <- data %>%
    dplyr::group_by(sitc)%>%
    dplyr::filter(
      date %in% !!datelist$date,
      sitc != "Total",
      !!dplyr::sql(country_filter) == "Total",
      !!dplyr::sql(source_filter) == "Victoria",
      nchar(sitc_code) == !!sitc_level
      ) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc = paste0(
        sitc_code,
        ": ",
        stringr::str_trunc(sitc, 30, "right")
        )
      )


  if(smooth) {

    all_dates <- product_list %>%
      dplyr::select(date) %>%
      unique()

    product_list <- product_list %>%
      tidyr::expand(sitc, .data[[source_field]], all_dates) %>%
      dplyr::left_join(product_list, by = c("sitc", source_field, "date")) %>%
      dplyr::group_by(sitc, .data[[source_field]]) %>%
      dplyr::mutate(
        value = tidyr::replace_na(value, 0),
        value = slider::slide_mean(value, before = 11L)
        ) %>%
      dplyr::ungroup()

  }

  product_list <- product_list %>%
    dplyr::filter(date %in% !!datelist$date) %>%
    dplyr::select(sitc, date, value)%>%
    dplyr::arrange(sitc, date) %>%
    dplyr::group_by(sitc) %>%
    dplyr::mutate(
      prev_month_change = paste0(
        scales::comma(value-dplyr::lag(value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value, 1)) / dplyr::lag(value, 1)
          ),
        ")"
        ),
      prev_qtr_change = paste0(
        scales::comma(value-dplyr::lag(value,2), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,2)) / dplyr::lag(value, 2)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(value - dplyr::lag(value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,3)) / dplyr::lag(value, 3)
          ),
        ")"
        )
    )%>%
    dplyr::ungroup()%>%
    dplyr::filter(date==datelist$date[1])%>%
    dplyr::arrange(dplyr::desc(value))%>%
    dplyr::group_by(date) %>%
    dplyr::mutate()%>%
    dplyr::mutate(
      value = paste0(
        scales::comma(value, accuracy = 1),
        "\n(",
        scales::percent(
          value / sum(value, na.rm = TRUE),
          accuracy = 1.1
          ),
        ")"
        )
      )%>%
    dplyr::ungroup() %>%
    dplyr::select(-c(date)) %>%
    dplyr::rename(
      ` ` = "sitc",
      !!paste0(nice_latest_date) := value,
      !!paste0(nice_prev_date)   := prev_month_change,
      !!paste0(nice_prev_qtr)    := prev_qtr_change,
      !!paste0(nice_prev_year)   := prev_year_change
    ) %>%
    utils::head(rows)

  return(product_list)
}




# The table that shows the change in exports and imports of goods and services
launchpad_table_export_import <- function(data = bop) {

  if ('tbl_lazy' %in% class(data)) {
    data <- data %>%
      dplyr::collect() %>%
      dplyr::mutate(date = as.Date(date))
  }


  df <- data %>%
    dplyr::select(-series_id, -unit) %>%
    dplyr::filter(
      indicator == "Chain Volume Measures",
      state == "Victoria"
      ) %>%
    dplyr::mutate(value = abs(value))

  datelist <- df %>%
    dplyr::filter(date %in% c(
      max(date),
      max(date) - months(3),
      max(date) - months(12),
      as.Date("2019-12-01")
      )) %>%
    dplyr::select(date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(date))

  #clean table headings
  latest_date <- datelist$date[1]
  prev_qtr <- datelist$date[2]
  prev_year <- datelist$date[3]
  since_covid <- datelist$date[4]

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_since_covid  <- format(since_covid, "%b %Y")
  nice_prev_qtr <- format(prev_qtr, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")

  since_since_covid <- paste0("Since ", nice_since_covid)
  since_prev_qtr <- paste0("Since ", nice_prev_qtr)
  since_prev_year <- paste0("Since ", nice_prev_year)

  df %>%
    dplyr::filter(date %in% datelist$date) %>%
    dplyr::group_by(exports_imports, goods_services) %>%
    dplyr::arrange(exports_imports, goods_services) %>%
    dplyr::mutate(
      prev_qtr_change = paste0(
        scales::comma(value - dplyr::lag(value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,1)) / dplyr::lag(value,1)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(value - dplyr::lag(value, 2), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value, 2)) / dplyr::lag(value, 2)
          ),
        ")"
        ),
      covid_change = paste0(
        scales::comma(value - dplyr::lag(value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(value,3)) / dplyr::lag(value,3)
          ),
        ")"
        )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(date == latest_date) %>%
    dplyr::arrange(exports_imports, dplyr::desc(value)) %>%
    dplyr::mutate(
      ` `   = paste(goods_services, exports_imports),
      value = scales::comma(value, accuracy = 1)
      ) %>%
    dplyr::select(
      -c(
        exports_imports,
        indicator,
        goods_services,
        state,
        date
      )
    ) %>%
    dplyr::rename(
      !!paste0(nice_latest_date) := value,
      !!paste0(nice_prev_qtr)    := prev_qtr_change,
      !!paste0(nice_prev_year)   := prev_year_change,
      !!paste0(nice_since_covid) := covid_change
      ) %>%
    dplyr::relocate(` `)
}

