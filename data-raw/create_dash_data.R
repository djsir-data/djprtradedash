# Updates data from source and recompiles database

# Load required packages & set options
pkgload::load_all()
options(timeout = 60 * 10)

# Merchandise trade data
read_merch(series = "export") |> data_save("merch")
read_merch(series = "import") |> data_save("merch_imp")

# Service data
read_services() |> data_save("service_trade")

# ABS Balance of Payments
read_bop() |> data_save("bop")
