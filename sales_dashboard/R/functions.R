clean_data <- function(df) {

  df <- df %>%
    janitor::clean_names() %>%
    mutate(order_date = dmy(order_date),
           ship_date = dmy(ship_date),
           postal_code = as.factor(postal_code),
           year = year(order_date),
           month = floor_date(order_date, "month"),
           ship_days = as.numeric(ship_date - order_date))

}