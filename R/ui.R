
ui <- function() {
  djprshiny::djpr_page(col_widths = c(1,10,1),
                       tags$style(HTML("@font-face {
                                          font-family: vic-bold;
                                          src: url(https://raw.githubusercontent.com/djpr-data/blank_rmd_project/main/fonts/VIC-Bold.woff2);
                                        }")),
                       tags$style(HTML("@font-face {
                                          font-family: vic-semibold;
                                          src: url(https://raw.githubusercontent.com/djpr-data/blank_rmd_project/main/fonts/VIC-SemiBold.woff2);
                                        }")),
                       tags$style(HTML("@font-face {
                                          font-family: vic-medium;
                                          src: url(https://raw.githubusercontent.com/djpr-data/blank_rmd_project/main/fonts/VIC-Medium.woff2);
                                        }")),
                       tags$style(HTML("@font-face {
                                          font-family: vic-regular;
                                          src: url(https://raw.githubusercontent.com/djpr-data/blank_rmd_project/main/fonts/VIC-Regular.woff2);
                                        }")),
                       tags$style(HTML("@font-face {
                                          font-family: vic-light;
                                          src: url(https://raw.githubusercontent.com/djpr-data/blank_rmd_project/main/fonts/VIC-Light.woff2);
                                        }")),
    title = shiny::HTML("DJPR Trade<br>Dashboard"),
    page_launchpadUI('lp'),
    page_merchUI(),
    page_bopUI(),#,
    #page_country_profile()
    page_methodology()#,
    # page_country_profile()
  )
}
