# Country synopsis chart

viz_launchpad_chart <- function(data = merch,
								country = c("Total"),
								region = c("Victoria")) {
	data %>%
		filter(country_dest %in% country,
			   origin %in% region) %>%
		arrange(desc(date), desc(value))
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

launchpad_country <- function(data = merch) {

  datelist<-merch%>%
    select(`TIME_PERIOD: Time Period`)%>%
    distinct()%>%
    arrange(desc(`TIME_PERIOD: Time Period`))%>%
    slice(1,2,4,13)

  country_list <- merch %>%
    group_by(`COUNTRY_DEST: Country of Destination`)%>%
    filter(`TIME_PERIOD: Time Period` %in% datelist$`TIME_PERIOD: Time Period`,
           `COUNTRY_DEST: Country of Destination` != "TOT: Total",
           `COMMODITY_SITC: Commodity by SITC` == "TOT: Total",
           `STATE_ORIGIN: State of Origin` == "2: Victoria") %>%
    select(`COUNTRY_DEST: Country of Destination`,`TIME_PERIOD: Time Period`,OBS_VALUE)%>%
    arrange(`COUNTRY_DEST: Country of Destination`,`TIME_PERIOD: Time Period`) %>%
    group_by(`COUNTRY_DEST: Country of Destination`) %>%
    mutate(Country    = sub(".*: ", "", `COUNTRY_DEST: Country of Destination`),
           value      = format(OBS_VALUE,big.mark=",",scientific=FALSE),
           prev_month = paste0(format(dplyr::lag(OBS_VALUE,1),big.mark=",",scientific=FALSE),
                              " (",
                              paste0(round(100*(OBS_VALUE-dplyr::lag(OBS_VALUE,1))/dplyr::lag(OBS_VALUE,1),2),"%"),
                              ")"),
           prev_qtr   = paste0(format(dplyr::lag(OBS_VALUE,2),big.mark=",",scientific=FALSE),
                               " (",
                               paste0(round(100*(OBS_VALUE-dplyr::lag(OBS_VALUE,2))/dplyr::lag(OBS_VALUE,2),2),"%"),
                               ")"),
           prev_yr    = paste0(format(dplyr::lag(OBS_VALUE,3),big.mark=",",scientific=FALSE),
                               " (",
                               paste0(round(100*(OBS_VALUE-dplyr::lag(OBS_VALUE,3))/dplyr::lag(OBS_VALUE,3),2),"%"),
                               ")"))%>%
    ungroup()%>%
    filter(`TIME_PERIOD: Time Period`==datelist$`TIME_PERIOD: Time Period`[1])%>%
    arrange(desc(OBS_VALUE))%>%
    select(-c(`COUNTRY_DEST: Country of Destination`,OBS_VALUE,`TIME_PERIOD: Time Period`))%>%
    rename(!!paste0(datelist$`TIME_PERIOD: Time Period`[1]) := value,
           !!paste0(datelist$`TIME_PERIOD: Time Period`[2]," (%-change)") := prev_month,
           !!paste0(datelist$`TIME_PERIOD: Time Period`[3]," (%-change)") := prev_qtr,
           !!paste0(datelist$`TIME_PERIOD: Time Period`[4]," (%-change)") := prev_yr
           )%>%
    DT::datatable(options = list(paging = TRUE,
                                 pageLength = 10,
                                 scrollX = TRUE,
                                 scrollY = TRUE,
                                 autoWidth = TRUE,
                                 server = FALSE,
                                 dom = "Bfrtip",
                                 buttons = FALSE,
                                 columnDefs = list(list(width = '200px', targets = c(1)))
    ),
    extensions = "Buttons",
    selection = "Single")
}

# Tables - Product


##### Analysis - anything after this line, please delete #####

