tar_load(data_clean)
superstore <- data_clean

plot_df <- superstore %>% group_by(country) %>%
    summarise(
      sales = sum(sales, na.rm = TRUE),
      profit = sum(profit, na.rm = TRUE),
      orders = n_distinct(order_id),
      .groups = "drop"
    )

plot_ly(
    data = plot_df,
    x = ~sales,
    y = ~country,
    type = "bar",
    orientation = "h",
    text = ~paste0(
      "Country: ", country,
      "<br>Sales: ", dollar(sales),
      "<br>Profit: ", dollar(profit),
      "<br>Orders: ", comma(orders)
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = "Sales", tickprefix = "$"),
      yaxis = list(title = ""),
      margin = list(l = 120)
    )
