page_methodology <- function(...) {
	djpr_tab_panel(
		title = "Notes",
	    value = "tab-notes",
	    br(),
		shiny::includeMarkdown("methodology.md"),
	    centred_row(htmlOutput("methodology_footnote")),
	    br()
		)
}