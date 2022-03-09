page_methodology <- function(...) {
	djpr_tab_panel(
		title = "Notes",
	    value = "tab-notes",
	    br(),
		shiny::includeMarkdown("R/methodology.md"),
		centred_row(
			div(
				HTML(paste0(
					"The data methodology notes for the DJPR Trade Dashboard has been prepared ",
					"by the <b>Strategy and Priority Projects - Economic Policy Unit</b> ",
					"at the Victorian Department of Jobs, Precincts and Regions."
					)),
				style = "color: #828282; font-size: 0.75rem"
				)
			),
		h2("SITC Information and Explorer"),
		DT::dataTableOutput("sitc_table"),
	    div(
	    	HTML(paste0(
	    		"Source: ABS.Stat Merchandise Exports data per commodity. Latest data is from ",
	    		format(max(merch$date), "%B %Y"), "."
	    		)
	    		),
	    	style = "font-size: 0.75rem"
	    	),
		br(),
	    centred_row(htmlOutput("methodology_footnote")),
	    br()
		)
}