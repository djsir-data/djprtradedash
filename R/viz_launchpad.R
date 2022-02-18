# Top countries goods line chart

viz_launchpad_countries <- function(data = merch,
                                    region = c("Victoria"),
                                    top = 5) {
  top_5_country <- data %>%
    filter(.data$origin %in% .env$region,
           .data$sitc == "Total",
           .data$country_dest != "Total") %>%
    arrange(desc(date), desc(value)) %>%
    select(country_dest) %>%
    unique() %>%
    head(top) %>%
    as.matrix()

  df <- data %>%
    filter(.data$origin %in% .env$region,
           .data$sitc == "Total",
           .data$country_dest %in% top_5_country) %>%
    select(country_dest, origin, date, value)

  latest_month <- format(max(df$date), "%B %Y")

  caption <- paste0("Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ", latest_month, ").")

  df <- df %>%
    mutate(value = value/1000,
           tooltip = paste0(
            country_dest, "\n",
            format(.data$date, "%b %Y"), "\n",
            format(round2(.data$value, 1), big.mark=",")
            ))

  df %>%
    djpr_ts_linechart(
      col_var = .data$country_dest,
      y_labels = function(x) format(x, big.mark=","),
      label_wrap_length = 15,
      x_expand_mult = c(0, 0.30)

    ) +
    labs(
      title = paste("Top", top, "Exports Destinations from", region, "by Value of Exports"),
      subtitle = paste("Total Exports from", region, "across All SITC Classifications ($m)"),
      caption = caption
    )
}

# Top goods exports line chart

viz_launchpad_chart <- function(data = merch,
								country = c("Total"),
								region = c("Victoria"),
                code_level = 3,
                top = 5) {
	top_5_code <- data %>%
		filter(country_dest %in% country,
			   origin %in% region,
         sitc != "Total",
         nchar(sitc_code) == code_level) %>%
		arrange(desc(date), desc(value)) %>%
    select(sitc_code) %>%
    unique() %>%
    head(top) %>%
    as.matrix()

  df <- data %>%
    filter(country_dest %in% country,
         origin %in% region,
         sitc_code %in% top_5_code) %>%
    select(sitc, date, value, sitc_code)%>%
    group_by(sitc)%>%
    mutate(sitc_shrink = strsplit(sitc, "[(]")[[1]][1])

  latest_month <- format(max(df$date), "%B %Y")

  caption <- paste0("Source: ABS.Stat Merchandise Exports by Commodity (latest data is from ", latest_month, ").")

  df <- df %>%
    mutate(value = value/1000,
           tooltip = paste0(
            "SITC: ", sitc_code, "\n",
            format(.data$date, "%b %Y"), "\n",
            format(round2(.data$value, 1),big.mark=",")
            ))

  df %>%
    djpr_ts_linechart(
      col_var = .data$sitc_shrink,
      y_labels = function(x) format(x, big.mark=","),
      label_wrap_length = 25,
      x_expand_mult = c(0, 0.25)
    ) +
    labs(
      title = paste("Top", top, "Exports by Value of Exports"),
      subtitle = paste("Exports at SITC Level", code_level, "from", region," ($m)"),
      caption = caption
    )
}

# Annual growth of Victoria's imports and exports of goods & services
viz_goods_export_import_launchpad <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$exports_imports == "Exports", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value))


  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$goods_services, "\n",
      format(.data$date, "%b %Y"), "\n",
      round2(.data$value, 1)
    ))

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(change = .data$value - lag(.data$value, 1)) %>%
    dplyr::filter(!is.na(.data$change)) %>%
    dplyr::filter(.data$date == max(.data$date))


  title <-
    dplyr::case_when(
      latest_change$change > 0 ~ paste0("Victoria's total exports rose by ", scales::comma(latest_change$change), " million dollars over the past quarter"),
      latest_change$change < 0 ~ paste0("Victoria's total exports fell by ", scales::comma(abs(latest_change$change)), " million dollars over the past quarter"),
      latest_change$change == 0 ~ "Victoria's total exports the same as over the past quarter ",
      TRUE ~ "Victoria's total exports over the past quarter"
    )


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df %>%
    djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(round2(.data$value, 1))),
      y_labels = function(x) format(x, big.mark=",")) +
    labs(
      title = title,
      subtitle = "Victoria's exports of goods and services ($m)",
      caption = caption
    )
}

# Victoria's historical imports of goods and services
viz_good_services_import_chart <- function(data = bop) {
  df <- data %>%
    dplyr::filter(
      .data$state == "Victoria",
    ) %>%
    dplyr::select(-.data$series_id, -.data$unit) %>%
    dplyr::filter(.data$exports_imports == "Exports", .data$indicator == "Chain Volume Measures") %>%
    dplyr::mutate(goods_services = dplyr::if_else(.data$goods_services == "Goods and Services", "Total", .data$goods_services)) %>%
    dplyr::mutate(value = abs(.data$value))


  latest_month <- format(max(df$date), "%B %Y")


  df <- df %>%
    dplyr::mutate(tooltip = paste0(
      .data$goods_services, "\n",
      format(.data$date, "%b %Y"), "\n",
      round2(.data$value, 1)
    ))

  latest_change <- df %>%
    dplyr::filter(.data$goods_services == "Total") %>%
    dplyr::mutate(annual_change = .data$value - dplyr::lag(.data$value, 4),
                  annual_pchange = .data$annual_change/dplyr::lag(.data$value, 4)) %>%
    dplyr::filter(!is.na(.data$annual_pchange)) %>%
    dplyr::filter(.data$date == max(.data$date))


  title <-
    dplyr::case_when(
      latest_change$annual_pchange > 0 ~ paste0("Victoria's total exports rose by ", paste0(round(latest_change$annual_pchange*100,1),"%"), " over the year"),
      latest_change$annual_pchange < 0 ~ paste0("Victoria's total exports fell by ", paste0(round(latest_change$annual_pchange*100,1),"%"), " over the year"),
      latest_change$annual_pchange == 0 ~ "Victoria's total exports the same as over the past quarter ",
      TRUE ~ "Victoria's total exports over the past quarter"
    )


  caption <- paste0("Source: ABS Balance of Payment quarterly (latest data is from ", latest_month, ". Note: Data seasonally Adjusted & Chain Volume Measures")


  df %>%
    djpr_ts_linechart(
      col_var = .data$goods_services,
      label_num = paste0(scales::comma(round2(.data$value, 1))),
      y_labels = function(x) format(x, big.mark=",")
    ) +
    labs(
      title = title,
      subtitle = "Victoria's exports of goods and services ($m)",
      caption = caption
    )
}

# List of Goods Exports

viz_exportlist <- function(data = merch,
						   country = c("Total"),
						   date = "max") {

	if(class(date) == "character") {
		ref_date <- max(data$date)
	} else {
			ref_date <- lubridate::floor_date(date, unit = "months")
		}

	df_month <- format(max(data$date), format = "%Y-%b")

	df_2020 <- data %>%
		filter(country_dest %in% country,
			   origin == "Victoria") %>%
		filter(date %in% c(ref_date, seq.Date(ref_date, by = "-1 year", length.out = 2)[2])) %>%
		arrange(desc(date), desc(value)) %>%
		select(date, sitc, sitc_code, value) %>%
		filter(date < lubridate::floor_date(ref_date, unit = "years"))

	df_2021 <- data %>%
		filter(country_dest %in% country,
			   origin == "Victoria") %>%
		filter(date %in% c(ref_date, seq.Date(ref_date, by = "-1 year", length.out = 2)[2])) %>%
		arrange(desc(date), desc(value)) %>%
		select(date, sitc, sitc_code, value) %>%
		filter(date >= lubridate::floor_date(ref_date, unit = "months"))

	left_join(df_2021, df_2020, by = c("sitc", "sitc_code")) %>%
	transmute(SITC = sitc,
			  `SITC Code` = sitc_code,
			  `Export Value (000s)` = value.x,
			  `Change since 12 months ago (%)` = round(100*(`Export Value (000s)`/value.y-1), 2)) %>%
	DT::datatable(options = list(paging = TRUE,
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
				  filter = "bottom")
}

# Top 10 Markets for Goods Exports

viz_launchpad_marketlist <- function(data = merch) {
	country_list <- data %>%
		filter(country_dest != "Total",
			   sitc == "Total",
			   origin == "Victoria") %>%
		arrange(desc(date), desc(value)) %>%
		head(10) %>%
		select(country_dest) %>%
		as.matrix()
}

# Export Headline

viz_launchpad_headline1 <- function(data = bop) {

}

# Services Headline

viz_launchpad_headline2 <- function(data = supp_cy) {

}

# Tables - Country

tab_launchpad_country_exp <- function(data = merch, rows = 5) {

  datelist<-data%>%
    select(date)%>%
    distinct()%>%
    arrange(desc(date))%>%
    slice(1,2,4,13)

  #clean table headings
  latest_date <- datelist$date[1]
  prev_date <- datelist$date[2]
  prev_qtr <- datelist$date[3]
  prev_year <- datelist$date[4]

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_prev_date <- format(prev_date, "%b %Y")
  nice_prev_qtr <- format(prev_qtr, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")

  since_prev_date <- paste0("Since ", nice_prev_date)
  since_prev_qtr <- paste0("Since ", nice_prev_qtr)
  since_prev_year <- paste0("Since ", nice_prev_year)

  #generate table
  country_exp_list <- data %>%
    group_by(country_dest)%>%
    filter(date %in% datelist$date,
           country_dest != "Total",
           sitc == "Total",
           origin == "Victoria") %>%
    select(country_dest,date,value)%>%
    arrange(country_dest,date) %>%
    ungroup()%>%
    ungroup()%>%
    group_by(country_dest) %>%
    mutate(prev_month_change    = paste0(scales::comma(value-dplyr::lag(value,1), accuracy = 1),
                                        "\n(",
                                        scales::percent((value-dplyr::lag(value,1))/dplyr::lag(value,1)),
                                        ")"),
           prev_qtr_change      = paste0(scales::comma(value-dplyr::lag(value,2), accuracy = 1),
                                        "\n(",
                                        scales::percent((value-dplyr::lag(value,2))/dplyr::lag(value,2)),
                                        ")"),
           prev_year_change     = paste0(scales::comma(value-dplyr::lag(value,3), accuracy = 1),
                                        "\n(",
                                        scales::percent((value-dplyr::lag(value,3))/dplyr::lag(value,3)),
                                        ")")
           )%>%
    ungroup()%>%
    filter(date==datelist$date[1])%>%
    arrange(desc(value))%>%
    group_by(date) %>%
    mutate()%>%
    mutate(value = paste0(scales::comma(value, accuracy = 1),
                          "\n(",
                          scales::percent(value/sum(value[!is.na(value)]), accuracy = 1.1),
                          ")"))%>%
    ungroup()%>%
    select(-c(date))%>%
    rename(` ` = "country_dest",
           !!paste0(nice_latest_date) := .data$value,
           !!paste0(nice_prev_date) := .data$prev_month_change,
           !!paste0(nice_prev_qtr) := .data$prev_qtr_change,
           !!paste0(nice_prev_year) := .data$prev_year_change
           )%>%
    head(rows)

  return(country_exp_list)
}

tab_launchpad_country_imp <- function(data = merch_imp, rows = 5) {

  datelist<-data%>%
    select(date)%>%
    distinct()%>%
    arrange(desc(date))%>%
    slice(1,2,4,13)

  #clean table headings
  latest_date <- datelist$date[1]
  prev_date <- datelist$date[2]
  prev_qtr <- datelist$date[3]
  prev_year <- datelist$date[4]

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_prev_date <- format(prev_date, "%b %Y")
  nice_prev_qtr <- format(prev_qtr, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")

  since_prev_date <- paste0("Since ", nice_prev_date)
  since_prev_qtr <- paste0("Since ", nice_prev_qtr)
  since_prev_year <- paste0("Since ", nice_prev_year)

  #generate table
  country_imp_list <- data %>%
    group_by(country_origin)%>%
    filter(date %in% datelist$date,
           country_origin != "Total",
           sitc == "Total",
           dest == "Victoria") %>%
    select(country_origin,date,value)%>%
    arrange(country_origin,date) %>%
    ungroup()%>%
    ungroup()%>%
    group_by(country_origin) %>%
    mutate(prev_month_change    = paste0(scales::comma(value-dplyr::lag(value,1), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,1))/dplyr::lag(value,1)),
                                         ")"),
           prev_qtr_change      = paste0(scales::comma(value-dplyr::lag(value,2), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,2))/dplyr::lag(value,2)),
                                         ")"),
           prev_year_change     = paste0(scales::comma(value-dplyr::lag(value,3), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,3))/dplyr::lag(value,3)),
                                         ")")
    )%>%
    ungroup()%>%
    filter(date==datelist$date[1])%>%
    arrange(desc(value))%>%
    group_by(date) %>%
    mutate()%>%
    mutate(value = paste0(scales::comma(value, accuracy = 1),
                          "\n(",
                          scales::percent(value/sum(value[!is.na(value)]), accuracy = 1.1),
                          ")"))%>%
    ungroup()%>%
    select(-c(date))%>%
    rename(` ` = "country_origin",
           !!paste0(nice_latest_date) := .data$value,
           !!paste0(nice_prev_date) := .data$prev_month_change,
           !!paste0(nice_prev_qtr) := .data$prev_qtr_change,
           !!paste0(nice_prev_year) := .data$prev_year_change
    )%>%
    head(rows)


  return(country_imp_list)
}

# Tables - Product

tab_launchpad_product_exp <- function(data = merch, sitc_level = 1, rows = 5) {

  datelist<-data%>%
    select(date)%>%
    distinct()%>%
    arrange(desc(date))%>%
    slice(1,2,4,13)

  #clean table headings
  latest_date <- datelist$date[1]
  prev_date <- datelist$date[2]
  prev_qtr <- datelist$date[3]
  prev_year <- datelist$date[4]

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_prev_date <- format(prev_date, "%b %Y")
  nice_prev_qtr <- format(prev_qtr, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")

  since_prev_date <- paste0("Since ", nice_prev_date)
  since_prev_qtr <- paste0("Since ", nice_prev_qtr)
  since_prev_year <- paste0("Since ", nice_prev_year)

  #generate table
  product_exp_list <- data %>%
    group_by(sitc)%>%
    filter(date %in% datelist$date,
           sitc != "Total",
           country_dest == "Total",
           origin == "Victoria",
           nchar(sitc_code) == sitc_level) %>%
    select(sitc,date,value)%>%
    arrange(sitc,date) %>%
    ungroup()%>%
    ungroup()%>%
    group_by(sitc) %>%
    mutate(prev_month_change    = paste0(scales::comma(value-dplyr::lag(value,1), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,1))/dplyr::lag(value,1)),
                                         ")"),
           prev_qtr_change      = paste0(scales::comma(value-dplyr::lag(value,2), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,2))/dplyr::lag(value,2)),
                                         ")"),
           prev_year_change     = paste0(scales::comma(value-dplyr::lag(value,3), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,3))/dplyr::lag(value,3)),
                                         ")")
    )%>%
    ungroup()%>%
    filter(date==datelist$date[1])%>%
    arrange(desc(value))%>%
    group_by(date) %>%
    mutate()%>%
    mutate(value = paste0(scales::comma(value, accuracy = 1),
                          "\n(",
                          scales::percent(value/sum(value[!is.na(value)]), accuracy = 1.1),
                          ")"))%>%
    ungroup()%>%
    select(-c(date))%>%
    rename(` ` = "sitc",
           !!paste0(nice_latest_date) := .data$value,
           !!paste0(nice_prev_date) := .data$prev_month_change,
           !!paste0(nice_prev_qtr) := .data$prev_qtr_change,
           !!paste0(nice_prev_year) := .data$prev_year_change
    )%>%
    head(rows)

  return(product_exp_list)
}

tab_launchpad_product_imp <- function(data = merch_imp, sitc_level = 1, rows = 5) {


  datelist<-data%>%
    select(date)%>%
    distinct()%>%
    arrange(desc(date))%>%
    slice(1,2,4,13)

  #clean table headings
  latest_date <- datelist$date[1]
  prev_date <- datelist$date[2]
  prev_qtr <- datelist$date[3]
  prev_year <- datelist$date[4]

  nice_latest_date <- format(latest_date, "%b %Y")
  nice_prev_date <- format(prev_date, "%b %Y")
  nice_prev_qtr <- format(prev_qtr, "%b %Y")
  nice_prev_year <- format(prev_year, "%b %Y")

  since_prev_date <- paste0("Since ", nice_prev_date)
  since_prev_qtr <- paste0("Since ", nice_prev_qtr)
  since_prev_year <- paste0("Since ", nice_prev_year)

  #generate table
  product_imp_list <- data %>%
    group_by(sitc)%>%
    filter(date %in% datelist$date,
           sitc != "Total",
           country_origin == "Total",
           dest == "Victoria",
           nchar(sitc_code) == sitc_level) %>%
    select(sitc,date,value)%>%
    arrange(sitc,date) %>%
    ungroup()%>%
    ungroup()%>%
    group_by(sitc) %>%
    mutate(prev_month_change    = paste0(scales::comma(value-dplyr::lag(value,1), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,1))/dplyr::lag(value,1)),
                                         ")"),
           prev_qtr_change      = paste0(scales::comma(value-dplyr::lag(value,2), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,2))/dplyr::lag(value,2)),
                                         ")"),
           prev_year_change     = paste0(scales::comma(value-dplyr::lag(value,3), accuracy = 1),
                                         "\n(",
                                         scales::percent((value-dplyr::lag(value,3))/dplyr::lag(value,3)),
                                         ")")
    )%>%
    ungroup()%>%
    filter(date==datelist$date[1])%>%
    arrange(desc(value))%>%
    group_by(date) %>%
    mutate()%>%
    mutate(value = paste0(scales::comma(value, accuracy = 1),
                          "\n(",
                          scales::percent(value/sum(value[!is.na(value)]), accuracy = 1.1),
                          ")"))%>%
    ungroup()%>%
    select(-c(date))%>%
    rename(` ` = "sitc",
           !!paste0(nice_latest_date) := .data$value,
           !!paste0(nice_prev_date) := .data$prev_month_change,
           !!paste0(nice_prev_qtr) := .data$prev_qtr_change,
           !!paste0(nice_prev_year) := .data$prev_year_change
    )%>%
    head(rows)

  return(product_imp_list)
}

##### Testings new plot function, please delete #####


