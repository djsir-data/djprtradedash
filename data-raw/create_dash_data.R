# Updates data from source and recompiles database

future::plan("multisession")
fs <- list(
  # Merchandise trade data
  future::future({
    pkgload::load_all()
    read_merch(series = "export") |> data_save("merch")
    NULL
  })
  ,future::future({
    pkgload::load_all()
    read_merch(series = "import") |> data_save("merch_imp")
    NULL
  })

  # Service data
  ,future::future({
    pkgload::load_all()
    read_services() |> data_save("service_trade")
    NULL
  })

  # ABS Balance of Payments
  ,future::future({
    pkgload::load_all()
    read_bop() |> data_save("bop")
    NULL
  })
)

invisible(future::resolve(fs))
