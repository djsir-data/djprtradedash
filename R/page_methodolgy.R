page_methodology <- function(...) {
	djpr_tab_panel(
		title = "Notes",
	    value = "tab-notes",
	    br(),
		shiny::includeMarkdown("R/methodology.md"),
		br(),
	    centred_row(htmlOutput("methodology_footnote")),
	    br()
		)
}
