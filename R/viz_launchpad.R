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

# 



##### Analysis - anything after this line, please delete #####

