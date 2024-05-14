# define vizualization theme ----------------------------------------------
astho_theme <-
  highcharter::hc_theme(
    colors = main_colors,
    chart = list(
      backgroundColor = NULL),
    style = list(
      fontFamily = "Jost"),
    title = list(
      style = list(
        color = dark_blue,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "20px")
      ),
    subtitle = list(
      style = list(
        color = dark_blue,
        fontFamily = "Jost",
        fontSize = "14px")
      ),
    caption = list(
      style = list(
        color = "#666",
        fontFamily = "Jost",
        fontSize = "13px")
      ),
    xAxis = list(
      labels = list(
        style = list(
          fontFamily = "Jost",
          fontSize = "15px",
          fontWeight = "normal",
          textOverflow = 'none',
          color = "#666")
        ),
      title = list(
        style = list(
          color = dark_blue,
          fontFamily = "Jost",
          fontWeight = "500",
          fontSize = "15px")
        )
      ),
    yAxis = list(
      labels = list(
        style = list(
          fontFamily = "Jost",
          fontSize = "20px",
          fontWeight = "normal",
          color = "#666")
        ),
      title = list(
        style = list(
          color = dark_blue,
          fontFamily = "Jost",
          fontWeight = "500",
          fontSize = "15px")
        )
      ),
    legend = list(
      itemStyle = list(
        fontFamily = "Jost",
        color = dark_blue,
        fontSize = "17px",
        fontWeight = "normal",
        color = "#666"),
      title = list(
        style = list(
          textDecoration = "none",
          fontFamily = "Jost",
          fontSize = "16px")
        )
      ),
    tooltip = list(
      padding = 10,
      borderRadius = 20,
      backgroundColor = "#fff",
      style = list(
        fontFamily = "Jost",
        fontSize = "16px",
        lineHeight = "20px")
      ),
    itemHoverStyle = list(
      color = light_accent)
  )
