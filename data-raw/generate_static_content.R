
# Load funs
pkgload::load_all()


# load data
load_tabs()


# Set highcharter options
set_hcharts_options()


# List funs to eval and save content
# Follows do.call syntax (what = fun, args = args)
to_eval <- list(
  "launchpad_goods" = list(
    what = highcharts_launchpad_goods,
    args = list(
      dates = c(merch_dates$max - months(48), merch_dates$max),
      top  = 10,
      sitc_level = 1
      )
    ),
  "launchpad_services" = list(
    what = highcharts_launchpad_services,
    args = list(period  = "Calendar Year")
  ),
  "launchpad_top_countries" = list(
    what = table_countries,
    args = list()
  ),
  "launchpad_bop" = list(
    what = highcharts_bop_export_chart,
    args = list()
  ),
  "launchpad_rising_goods" = list(
    what = highcharts_rising_goods,
    args = list()
  ),
  "launchpad_imp_country_table" = list(
    what = flextable::htmltools_value,
    args = list(
      tab_launchpad_country_imp_exp(
      direction = 'import',
      data      = merch_imp,
      rows      = 5
      ) %>%
        make_table_launchpad() %>%
        flextable::autofit()
    )
  ),
  "launchpad_exp_country_table" = list(
    what = flextable::htmltools_value,
    args = list(
      tab_launchpad_country_imp_exp(
        direction = 'export',
        data      = merch,
        rows      = 5
      ) %>%
        make_table_launchpad() %>%
        flextable::autofit()
    )
  ),
  "launchpad_product_exp_table" = list(
    what = flextable::htmltools_value,
    args = list(
      tab_launchpad_product_imp_exp(
        direction  = 'export',
        data       = merch,
        rows       = 5,
        sitc_level = 3
      ) %>%
        make_table_launchpad() %>%
        flextable::autofit()
    )
  ),
  "launchpad_product_imp_table" = list(
    what = flextable::htmltools_value,
    args = list(
      tab_launchpad_product_imp_exp(
        direction  = 'import',
        data       = merch_imp,
        rows       = 5,
        sitc_level = 3
      ) %>%
        make_table_launchpad() %>%
        flextable::autofit()
    )
  ),
  "launchpad_bop_table" = list(
    what = flextable::htmltools_value,
    args = list(
      make_table_launchpad(
        data = launchpad_table_export_import(),
        header_row = c(
          "",
          "Current figure ($m)",
          "Change since last quarter",
          "Change in past year",
          "Change since COVID"
        )
      ) %>%
        flextable::autofit()
    )
  ),
  "services_composition" = list(
    what = highcarts_service_compositon,
    args = list()
  ),
  "service_category_list" = list(
    what = service_category_list,
    args = list()
  )

)




# Eval and save
evaluated <- lapply(
  to_eval,
  function(x) do.call(do.call, x)
)


# Save
out_path <- file.path("inst", paste0(names(to_eval), ".rds"))
mapply(saveRDS, evaluated, out_path)
