#' Create a table for the dashboard or a briefing document
#' @param data A data frame containing data to summarise
#' @param destination "dashboard" or "briefing"
#' @param notes Optional notes to add to caption. Source will be inferred
#' automatically based on the data using `caption_auto()`.
#' @param title Character vector to use as the table title. Will only be used
#' when `destination` is "briefing".
#' @param header_row Top row labels
#' @examples
#' # dash_data <- load_dash_data()
#' \dontrun{
#' make_table(
#'   data = filter_dash_data(series_ids = c(
#'     "A84423354L",
#'     "A84423242V",
#'     "A84423466F"
#'   )),
#'   row_order = c(
#'     "A84423354L",
#'     "A84423242V",
#'     "A84423466F"
#'   ),
#'   highlight_rows = c("A84426256L")
#' )
#' }
#
make_table_launchpad <- function(
  data,
  destination = Sys.getenv("R_DJPRLABOURDASH_TABLEDEST", unset = "dashboard"),
  notes = NULL,
  title = "",
  header_row = c(
    "",
    "Current figure & share (%)",
    "Change in latest period",
    "Change in three months",
    "Change in past year")
  ){

  stopifnot(destination %in% c("dashboard", "briefing"))
  stopifnot(inherits(data, "data.frame"))
  stopifnot(nrow(data) >= 1)

  # Create a basic flextable using the supplied dataframe
  latest_date <- names(data)[2] %>%
    lubridate::my() %>%
    format("%B %Y")

  if (is.element("Dec 2019", names(data))){
    caption <- paste0("ABS Balance of Payment quarterly data (latest data is from ", latest_date, ").  Note: Data seasonally Adjusted & Chain Volume Measures")
  } else {
    caption <- paste0("ABS.Stat Merchandise Exports by Commodity (latest data is from ", latest_date, "). Data has been smoothed using 12-month rolling averages." )
  }



  flex <- data %>%
    flextable::flextable() %>%
    flextable::add_footer(` ` = caption) %>%
    flextable::fontsize(size = 7, part = "footer") %>%
    flextable::merge_at(j = 1:5, part = "footer")

  if (destination == "dashboard") {
    # Define cell colours ----
    # Create a summary table that will be used for conditional formatting
    # Note we do not use the dashboard-level ts_summ for this, as we want to ensure
    # any data pre-processing (such as rolling averages) are captured

    # Full palette for table
    full_pal <- grDevices::colorRampPalette(c("#E95A6A", "white", "#62BB46"))(100)



  # Set lineheight
  flex <- flex %>%
    flextable::line_spacing(space = 1)

  # Ensure the flextable fits the container (eg. Word doc) it is placed in
  flex <- flex %>%
    flextable::autofit(add_w = 0, add_h = 0, part = "all")

  # Centre content
  flex <- flex %>%
    flextable::align(
      j = 3:flextable::ncol_keys(flex),
      i = 1,
      align = "justify"
    ) %>%
    flextable::valign()

  # Add an extra header row

  flex <- flex %>%
    flextable::add_header_row(values = header_row)

  # Add borders
  flex <- flex %>%
    flextable::border_remove()

  if (destination == "dashboard") {
    flex <- flex %>%
      flextable::border(border.top = flextable::fp_border_default(
        color = "grey90", width = 0.25
      ))
  }

  flex <- flex %>%
    flextable::border(
      i = 1,
      border.top = flextable::fp_border_default()
    ) %>%
    flextable::border(i = nrow(data), border.bottom = flextable::fp_border_default())

  # Ensure font, font size, and bolding is correct
  if (destination == "dashboard") {
    font_family <- "VIC-Regular"
    font_size_main <- 10.5
    font_size_secondary <- 9
  } else if (destination == "briefing") {
    font_family <- "Arial"
    font_size_main <- 9
    font_size_secondary <- 8
  }

  flex <- flex %>%
    flextable::font(fontname = font_family) %>%
    flextable::font(fontname = font_family, part = "header") %>%
    flextable::fontsize(size = font_size_main) %>%
    flextable::fontsize(size = font_size_main, i = 1, part = "header") %>%
    flextable::fontsize(size = font_size_secondary, i = 2, part = "header") %>%
    flextable::bold(i = 1, part = "header")

  # Right align columns other than the first one (row label/indicator)
  flex <- flex %>%
    flextable::align(j = -1, align = "right") %>%
    flextable::align(j = -1, align = "right", part = "header")

  # Bold highlight rows, indent non-highlight rows
  flex <- flex %>%
      flextable::bold(j = 1)

  # all_rows <- seq_len(nrow(df))

  flex <- flex %>%
      flextable::padding(j = 1, padding.left = 20)

  }

  # Add caption / footer
  if (destination == "dashboard") {
    caption_notes <- paste0(
      notes,
      "Information to be printed."
    )
  } else {
    if (is.null(notes)) {
      caption_notes <- NULL
    } else {
      caption_notes <- notes
    }
  }

  # table_caption <- caption_auto(data,
                                # notes = caption_notes
  # )

  # Add footer caption
  # flex <- flex %>%
  #   flextable::add_footer(` ` = table_caption) %>%
  #   flextable::merge_at(
  #     # j = 1:flextable::ncol_keys(flex),
  #     # part = "footer"
  #   ) %>%
  #   flextable::italic(part = "footer") %>%
  #   flextable::font(fontname = font_family) %>%
  #   flextable::fontsize(
  #     size = font_size_secondary * 0.85,
  #     part = "footer"
  #   ) %>%
  #   flextable::color(
  #     part = "footer",
  #     color = "#343a40"
  #   ) %>%
  #   flextable::line_spacing(
  #     part = "footer",
  #     space = 0.8
  #   ) %>%
  #   flextable::font(
  #     fontname = font_family,
  #     part = "footer"
  #   ) %>%
  #   flextable::italic(part = "footer")



  # Add title to briefing tables and resize columns
  if (destination == "briefing") {
    flex <- flex %>%
      flextable::set_caption(caption = title)

    flex <- flex %>%
      flextable::width(
        j = c(3:flextable::ncol_keys(flex)),
        width = 0.88
      ) %>%
      flextable::width(
        j = 1,
        width = 2
      )
  }

  flex
}

# make_table_launchpad(data = tab_launchpad_country_exp(), country_or_product = "Country")
