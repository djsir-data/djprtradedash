# Top countries goods line chart

viz_launchpad_countries <- function(
  data   = merch,
  dates  = c(merch_dates$min, merch_dates$max),
  region = c("Victoria"),
  top    = 5,
  smooth = TRUE
  ) {

  filtered <- data %>%
    dplyr::filter(
      .data$origin %in% .env$region,
      .data$sitc == "Total",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
      )

  # TODO: Need to finalise and improve method for calculating top N. Current does not account for smoothed numbers
  ## could use window functions in DB to calc top N separately https://duckdb.org/docs/sql/window_functions

  top_5_country <- filtered %>%
    dplyr::filter(
      .data$country_dest != "Total",
      .data$origin %in% .env$region,
      .data$sitc == "Total"
      ) %>%
    dplyr::arrange(dplyr::desc(date), dplyr::desc(value)) %>%
    dplyr::select(country_dest) %>%
    dplyr::collect() %>%
    unique() %>%
    utils::head(top) %>%
    as.matrix()


  df <- filtered %>%
    dplyr::filter(.data$country_dest %in% top_5_country) %>%
    dplyr::select(country_dest, origin, date, value)


  if ('tbl_lazy' %in% class(df)) {
    df <- df %>%
      dplyr::collect()
  }

  df <- df %>%
    dplyr::mutate(date = as.Date(date))


  if(smooth) {

    all_dates <- df %>%
      dplyr::select(date) %>%
      unique()

    df <- df %>%
      tidyr::expand(country_dest, origin, all_dates) %>%
      dplyr::left_join(df, by = c("country_dest", "origin", "date")) %>%
      dplyr::group_by(country_dest, origin) %>%
      dplyr::mutate(
        value = tidyr::replace_na(value, 0),
        value = slider::slide_mean(value, before = 11L)
        ) %>%
      dplyr::ungroup()
  }



  #assertthat::assert_that(class(df$date) == 'Date', msg = 'incorrect date type')

  latest_month <- format(max(df$date), "%B %Y")

  caption <- paste0(
    "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
    latest_month,
    ")."
    )

  if(smooth) {
    caption <- paste0(
      caption,
      " Data has been smoothed using 12-month rolling averages."
      )
  }

  df <- df %>%
    dplyr::mutate(
      value = value / 1000,
      tooltip = paste0(
        country_dest,
        "\n",
        format(.data$date, "%b %Y"),
        "\n",
        format(round(.data$value, 1), big.mark=",")
        )
      )

  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$country_dest,
      y_labels = function(x) format(x, big.mark=","),
      #label_wrap_length = 15,
      x_expand_mult = c(0, 0.30)

    ) +
    ggplot2::labs(
      title = paste("Top", top, "Destinations for Exports from", region),
      subtitle = paste(
        "Total Exports from",
        region,
        "across All SITC Classifications ($m)"
        ),
      caption = caption
    )
}

# Top goods exports line chart

viz_launchpad_chart <- function(
  data       = merch,
  dates      = c(merch_dates$min, merch_dates$max),
	country    = "Total",
	region     = "Victoria",
  code_level = 3,
  top        = 5,
  smooth     = TRUE
  ){

  filtered <- data %>%
    dplyr::filter(
      .data$country_dest %in% !!country,
      .data$origin %in% region,
      nchar(.data$sitc_code) == .env$code_level,
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
      )

  top_5_code <- filtered %>%
    dplyr::filter(.data$date == max(.data$date), .data$sitc != "Total") %>%
    dplyr::slice_max(.data$value, n = top) %>%
    dplyr::select(.data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::pull() %>%
    unique() %>%
    as.matrix()

  df <- filtered %>%
    dplyr::filter(.data$sitc_code %in% !!top_5_code) %>%
    dplyr::select(.data$sitc, .data$date, .data$value, .data$sitc_code) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      sitc_shrink = paste0(
        .data$sitc_code,
        ": ",
        stringr::str_trunc(.data$sitc, 20, "right")
        )
      )



  latest_month <- format(max(df$date, na.rm = TRUE), "%B %Y")

  if(smooth) {

    all_dates <- df %>%
      dplyr::select(.data$date) %>%
      unique()

    df <- df %>%
      tidyr::expand(.data$sitc, .env$all_dates) %>%
      dplyr::left_join(df, by = c("sitc", "date")) %>%
      dplyr::group_by(.data$sitc) %>%
      dplyr::mutate(value = tidyr::replace_na(.data$value, 0),
                    value = slider::slide_mean(.data$value, before = 11L)) %>%
      dplyr::ungroup()
  }


  caption <- paste0(
    "Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ",
    latest_month,
    ")."
    )

  if(smooth) {
    caption <- paste0(
      caption,
      "Data has been smoothed using 12-month rolling averages."
      )
  }

  df <- df %>%
    dplyr::mutate(
      value   = .data$value / 1000,
      tooltip = paste0(
        "SITC: ",
        .data$sitc_code,
        "\n",
        format(.data$date, "%b %Y"),
        "\n",
        format(djprshiny::round2(.data$value, 1), big.mark=",")
        )
      )

  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$sitc_shrink,
      y_labels = function(x) format(x, big.mark=","),
      #label_wrap_length = 25,
      x_expand_mult = c(0, 0.25)
    ) +
    ggplot2::labs(
      title = paste("Top", top, "Exports from", region),
      subtitle = paste("Exports at SITC Level", code_level, "from", region," ($m)"),
      caption = caption
    )
}

# Annual growth of Victoria's imports and exports of goods & services
viz_goods_export_import_launchpad <- function(
  data  = bop,
  dates = c(merch_dates$min, merch_dates$max)
  ) {

  filtered <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$exports_imports == "Exports",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )

  if (!any(class(filtered) == 'data.frame')) {
    filtered <- filtered %>%
      dplyr::collect() %>%
      dplyr::mutate(date = lubridate::ymd(.data$date))
  }

  df <- filtered %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(
      goods_services = dplyr::if_else(
        .data$goods_services == "Goods and Services",
        "Total",
        .data$goods_services
        )
      ) %>%
    dplyr::mutate(value = abs(.data$value))


  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$goods_services, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1)
    ))

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - dplyr::lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::filter(.data$date == max(.data$date))


  title <-
    dplyr::case_when(
      latest_change$change > 0 ~ paste0(
        "Victoria's total exports rose by ",
        scales::comma(latest_change$change),
        " million dollars over the past quarter"
        ),
      latest_change$change < 0 ~ paste0(
        "Victoria's total exports fell by ",
        scales::comma(abs(latest_change$change)),
        " million dollars over the past quarter"
        ),
      latest_change$change == 0 ~
        "Victoria's total exports the same as over the past quarter ",
      TRUE ~ "Victoria's total exports over the past quarter"
    )


  caption <- paste0(
    "Source: ABS Balance of Payment quarterly (latest data is from ",
    latest_month,
    ". Note: Data seasonally Adjusted & Chain Volume Measures"
    )


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(djprshiny::round2(.data$value, 1))),
      y_labels = function(x) format(x, big.mark=",")) +
    ggplot2::labs(
      title = title,
      subtitle = "Victoria's exports of goods and services ($m)",
      caption = caption
    )
}

# Victoria's historical imports of goods and services
viz_good_services_export_chart <- function(
  data  = bop,
  dates = c(merch_dates$min, merch_dates$max)
) {


  filtered <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
      .data$exports_imports == "Exports",
      .data$indicator == "Chain Volume Measures",
      .data$date >= !!dates[1],
      .data$date <= !!dates[2]
    )


  if ('tbl_lazy' %in% class(filtered)) {
    filtered <- filtered %>%
      dplyr::collect() %>%
      dplyr::mutate(date = lubridate::ymd(.data$date))
  }


  df <- filtered %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::mutate(
      goods_services = dplyr::if_else(
        .data$goods_services == "Goods and Services",
        "Total",
        .data$goods_services
        ),
      value = abs(.data$value),
      date = as.Date(.data$date)
      )

  #assertthat::assert_that(class(df$date) == 'Date', msg = 'incorrect date type')

  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$goods_services, "\n",
      format(.data$date, "%b %Y"), "\n",
      djprshiny::round2(.data$value, 1)
    ))

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(
      annual_change = .data$value - dplyr::lag(.data$value, 4),
      annual_pchange = .data$annual_change / dplyr::lag(.data$value, 4)
      ) %>%
    dplyr::filter(!is.na(.data$annual_pchange), .data$date == max(.data$date))


  title <-
    dplyr::case_when(
      latest_change$annual_pchange > 0 ~ paste0(
        "Victoria's total exports rose by ",
        paste0(signif(latest_change$annual_pchange * 100, 2), "%"),
        " over the year"
        ),
      latest_change$annual_pchange < 0 ~ paste0(
        "Victoria's total exports fell by ",
        paste0(signif(latest_change$annual_pchange * 100, 2), "%"),
        " over the year"
        ),
      latest_change$annual_pchange == 0 ~
        "Victoria's total exports the same as over the past quarter ",
      TRUE ~ "Victoria's total exports over the past quarter"
    )


  caption <- paste0(
    "Source: ABS Balance of Payment quarterly (latest data is from ",
    latest_month,
    ". Note: Data seasonally Adjusted & Chain Volume Measures"
    )


  df %>%
    djprshiny::djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(djprshiny::round2(.data$value, 1))),
      y_labels = function(x) format(x, big.mark=",")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Victoria's exports of goods and services ($m)",
      caption = caption
    )
}

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
      dplyr::mutate(date = lubridate::ymd(.data$date))
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
	  dplyr::select(.data$date, .data$sitc, .data$sitc_code, .data$value) %>%
	  dplyr::filter(.data$date < lubridate::floor_date(ref_date, unit = "years"))

	df_2021 <- filtered %>%
	  dplyr::filter(
	    date %in% c(
	      ref_date,
	      seq.Date(ref_date, by = "-1 year", length.out = 2)[2]
	      )
	    ) %>%
	  dplyr::arrange(dplyr::desc(date), dplyr::desc(value)) %>%
	  dplyr::select(.data$date, .data$sitc, .data$sitc_code, .data$value) %>%
	  dplyr::filter(
	    .data$date >= lubridate::floor_date(ref_date, unit = "months")
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
      dplyr::mutate(date = lubridate::ymd(.data$date))
  }

	country_list <- filtered %>%
	  dplyr::arrange(dplyr::desc(.data$date), dplyr::desc(.data$value)) %>%
		utils::head(10) %>%
	  dplyr::select(.data$country_dest) %>%
		as.matrix()
}

# Export Headline

viz_launchpad_headline1 <- function(data = bop) {

}

# Services Headline

viz_launchpad_headline2 <- function(data = supp_cy) {

}

clean_dates <- function(data){

  datelist = data %>%
    dplyr::select(.data$date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$date)) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
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
        .data$sitc == "Total",
        .data$country_origin != "Total",
        .data$dest == "Victoria"
        )
  # exports
  } else if (direction == 'export') {
    country_list <- data %>%
      dplyr::filter(
        .data$sitc == "Total",
        .data$country_dest != "Total"
        )
  }


  country_list <- country_list %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(.data$date))


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
        value = tidyr::replace_na(.data$value, 0),
        value = slider::slide_mean(.data$value, before = 11L)
        ) %>%
      dplyr::ungroup()

  } else {

    country_list <- country_list %>%
      dplyr::group_by(.data[[dir_fields$partner]]) %>%
      dplyr::ungroup()

  }


  country_list <- country_list %>%
    dplyr::filter(.data$date %in% datelist$date) %>%
    dplyr::group_by(.data[[dir_fields$partner]], date) %>%
    dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
    dplyr::select(dplyr::one_of(dir_fields$partner), .data$date, .data$value) %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    dplyr::arrange(.data[[dir_fields$partner]], .data$date)
    #ungroup()%>%
    #ungroup()%>%
    #group_by(.data[[direction]]) %>%




  country_list <- country_list %>%
    dplyr::group_by(.data[[dir_fields$partner]]) %>%
    dplyr::mutate(
      prev_month_change = paste0(
        scales::comma(.data$value-dplyr::lag(.data$value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value-dplyr::lag(.data$value,1)) / dplyr::lag(.data$value,1)
          ),
        ")"
        ),
      prev_qtr_change = paste0(
        scales::comma(value-dplyr::lag(.data$value,2), accuracy = 1),
        "\n(",
        scales::percent(
          (value - dplyr::lag(.data$value, 2)) / dplyr::lag(.data$value, 2)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(.data$value - dplyr::lag(.data$value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value,3)) / dplyr::lag(.data$value, 3)
          ),
        ")"
        )
    )%>%
    dplyr::filter(.data$date==datelist$date[1])%>%
    dplyr::arrange(dplyr::desc(.data$value))%>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      value = paste0(
        scales::comma(.data$value, accuracy = 1),
        "\n(",
        scales::percent(
          .data$value / sum(.data$value, na.rm = TRUE),
          accuracy = 1.1
          ),
        ")"
        )
      )%>%
    dplyr::ungroup()%>%
    dplyr::select(-c(.data$date)) %>%
    dplyr::rename(
      ` `                        := .data[[dir_fields$partner]],
      !!paste0(nice_latest_date) := .data$value,
      !!paste0(nice_prev_date)   := .data$prev_month_change,
      !!paste0(nice_prev_qtr)    := .data$prev_qtr_change,
      !!paste0(nice_prev_year)   := .data$prev_year_change
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
    dplyr::group_by(.data$sitc)%>%
    dplyr::filter(
      .data$date %in% !!datelist$date,
      .data$sitc != "Total",
      !!dplyr::sql(country_filter) == "Total",
      !!dplyr::sql(source_filter) == "Victoria",
      nchar(sitc_code) == !!sitc_level
      ) %>%
    dplyr::collect() %>%
    dplyr::mutate(date = lubridate::ymd(.data$date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      sitc = paste0(
        .data$sitc_code,
        ": ",
        stringr::str_trunc(.data$sitc, 30, "right")
        )
      )


  if(smooth) {

    all_dates <- product_list %>%
      dplyr::select(.data$date) %>%
      unique()

    product_list <- product_list %>%
      tidyr::expand(.data$sitc, .data[[source_field]], all_dates) %>%
      dplyr::left_join(product_list, by = c("sitc", source_field, "date")) %>%
      dplyr::group_by(.data$sitc, .data[[source_field]]) %>%
      dplyr::mutate(
        value = tidyr::replace_na(.data$value, 0),
        value = slider::slide_mean(.data$value, before = 11L)
        ) %>%
      dplyr::ungroup()

  }

  product_list <- product_list %>%
    dplyr::filter(.data$date %in% !!datelist$date) %>%
    dplyr::select(.data$sitc, .data$date, .data$value)%>%
    dplyr::arrange(.data$sitc, .data$date) %>%
    dplyr::group_by(.data$sitc) %>%
    dplyr::mutate(
      prev_month_change = paste0(
        scales::comma(value-dplyr::lag(.data$value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value, 1)) / dplyr::lag(.data$value, 1)
          ),
        ")"
        ),
      prev_qtr_change = paste0(
        scales::comma(value-dplyr::lag(value,2), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value,2)) / dplyr::lag(.data$value, 2)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(.data$value - dplyr::lag(.data$value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value,3)) / dplyr::lag(.data$value, 3)
          ),
        ")"
        )
    )%>%
    dplyr::ungroup()%>%
    dplyr::filter(.data$date==datelist$date[1])%>%
    dplyr::arrange(dplyr::desc(.data$value))%>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate()%>%
    dplyr::mutate(
      value = paste0(
        scales::comma(.data$value, accuracy = 1),
        "\n(",
        scales::percent(
          .data$value / sum(.data$value, na.rm = TRUE),
          accuracy = 1.1
          ),
        ")"
        )
      )%>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$date)) %>%
    dplyr::rename(
      ` ` = "sitc",
      !!paste0(nice_latest_date) := .data$value,
      !!paste0(nice_prev_date)   := .data$prev_month_change,
      !!paste0(nice_prev_qtr)    := .data$prev_qtr_change,
      !!paste0(nice_prev_year)   := .data$prev_year_change
    ) %>%
    utils::head(rows)

  return(product_list)
}




# The table that shows the change in exports and imports of goods and services
launchpad_table_export_import <- function(data = bop) {

  if ('tbl_lazy' %in% class(data)) {
    data <- data %>%
      dplyr::collect() %>%
      dplyr::mutate(date = as.Date(.data$date))
  }


  df <- data %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(
      .data$indicator == "Chain Volume Measures",
      .data$state == "Victoria"
      ) %>%
    dplyr::mutate(value = abs(.data$value))

  datelist <- df %>%
    dplyr::filter(date %in% c(
      max(.data$date),
      max(.data$date) - months(3),
      max(.data$date) - months(12),
      as.Date("2019-12-01")
      )) %>%
    dplyr::select(.data$date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$date))

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
    dplyr::filter(.data$date %in% datelist$date) %>%
    dplyr::group_by(.data$exports_imports, .data$goods_services) %>%
    dplyr::arrange(.data$exports_imports, .data$goods_services) %>%
    dplyr::mutate(
      prev_qtr_change = paste0(
        scales::comma(.data$value - dplyr::lag(.data$value,1), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value,1)) / dplyr::lag(.data$value,1)
          ),
        ")"
        ),
      prev_year_change = paste0(
        scales::comma(.data$value - dplyr::lag(value, 2), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value, 2)) / dplyr::lag(.data$value, 2)
          ),
        ")"
        ),
      covid_change = paste0(
        scales::comma(.data$value - dplyr::lag(.data$value,3), accuracy = 1),
        "\n(",
        scales::percent(
          (.data$value - dplyr::lag(.data$value,3)) / dplyr::lag(.data$value,3)
          ),
        ")"
        )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$date == latest_date) %>%
    dplyr::arrange(.data$exports_imports, dplyr::desc(.data$value)) %>%
    dplyr::mutate(
      ` `   = paste(goods_services, exports_imports),
      value = scales::comma(value, accuracy = 1)
      ) %>%
    dplyr::select(
      -c(
        .data$exports_imports,
        .data$indicator,
        .data$goods_services,
        .data$state,
        .data$date
      )
    ) %>%
    dplyr::rename(
      !!paste0(nice_latest_date) := .data$value,
      !!paste0(nice_prev_qtr)    := .data$prev_qtr_change,
      !!paste0(nice_prev_year)   := .data$prev_year_change,
      !!paste0(nice_since_covid) := .data$covid_change
      ) %>%
    dplyr::relocate(` `)
}

