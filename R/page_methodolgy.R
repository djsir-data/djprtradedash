page_methodology <- function(...) {
  shiny::fluidRow(
	# 	title = "Notes",
	#     value = "tab-notes",
		shiny::br(),
		shiny::includeMarkdown("R/methodology.md"),
		djprshiny::centred_row(
		  shiny::div(
		    shiny::HTML(paste0(
					"The data methodology notes for the DJPR Trade Dashboard has been prepared ",
					"by the <b>Strategy and Priority Projects - Economic Policy Unit</b> ",
					"at the Victorian Department of Jobs, Precincts and Regions."
					)),
				style = "color: #828282; font-size: 0.75rem"
				)
			),
		shiny::h2("SITC Information and Explorer"),
		DT::dataTableOutput("sitc_table"),
		shiny::div(
		  shiny::HTML(paste0(
	    		"Source: ABS.Stat Merchandise Exports data per commodity. Latest data is from ",
	    		format(merch_dates$max, "%B %Y"), "."
	    		)
	    		),
	    	style = "font-size: 0.75rem"
	    	),
		shiny::br(),

		shiny::includeMarkdown("R/glossary.md"),

		shiny::br(),

		shiny::includeMarkdown("R/note.md"),

		shiny::br(),

		djprshiny::centred_row(shiny::htmlOutput("methodology_footnote")),
		shiny::br()
		)
}
