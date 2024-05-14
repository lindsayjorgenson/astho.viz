globalVariables(c("map_data", "data_classes", "map_data", "map_value", "value", "from", "map_type"))

#' Create an accessible categorical map using the ASTHO theme in {highcharter}
#'
#' @param data the data frame to create the map.
#' @param fips_column column of a dataframe with the Federal Information Processing System (FIPS) Codes for States and Counties, in quotes.
#' @param selected_value column of a dataframe with the values to visualize in the map, in quotes.
#' @param include_islands boolean, TRUE or FALSE, where FALSE removes island areas. Defaults to TRUE.
#' @param my_colors vector; colors to supply to the map, as hex values.
#' @param my_click JS function; use to add a click event, requires a JS function, JS(), Defaults to NA.
#' @param title_text character string; title of the map.
#' @param subtitle_text character string; subtitle to appear underneath the title, typically a date or year.
#' @param caption_text character string; source line and data notes to appear under the map.
#' @param legend_text character string; title of the legend. Defaults to NA.
#' @param accessible_desc character string; alternative text description of the figure for screen readers.
#' @param my_tooltip character string; text and values to appear in the tooltip, use {point.xx} to call a value from the data frame, where xx is a specific column.
#' @param my_reverse boolean, TRUE or FALSE, where TRUE reverses the legend. Defaults to FALSE.
#' @param enable_legend boolean, TRUE or FALSE, where TRUE enables the legend. Defaults to TRUE.
#'
#' @return A categorical map
#' @export
#'
#' @examples
#'
#' categorical_map(data = astho_governance,
#'       fips_column = "fips",
#'        selected_value = "governance",
#'        title_text = "Map of U.S. State and Island Area Public Health Agency Governance", # title
#'        subtitle_text = "2022", # content under the title
#'        caption_text =
#'        "<b> Source: </b> Association of State and Territorial Health Officials.
#'        ASTHO Profile of State and Territorial Public Health, Volume Six. Arlington,
#'        VA: Association of State and Territorial Health Officials. 2023.",
#'       accessible_desc =
#'       "This is a map of governance classification, with most
#'       jurisdictions classified as decentralized followed by
#'       centralized. Few jurisdictions have mixed or shared
#'       governance. Enter the map to review classification at the
#'       jurisdiction level.",
#'       my_tooltip =
#'       "<b>{point.JurisdictionName}</b><br>
#'       <b>Agency Governance:</b>   {point.governance}<br>
#'       <b>Agency Structure:</b>   {point.structure}<br>
#'       <b>HHS Region:</b>   {point.HHSRegion}<br>
#'       <b>Population Size:</b>   {point.size}<br>",
#'      )
#'
#'
categorical_map <- function(data,
                            fips_column,
                            selected_value,
                            include_islands = TRUE,
                            my_colors = main_colors,
                            my_click = NA,
                            title_text,
                            subtitle_text,
                            caption_text,
                            legend_text = NA,
                            accessible_desc,
                            my_tooltip = "{point.value}",
                            my_reverse = FALSE,
                            enable_legend = TRUE
) {

  if (include_islands == TRUE) {
    map_type <- "c"
  } else {
    map_type <- "b"
  }

  map_data <- data %>%
    dplyr::rename(fips = fips_column) %>%
    dplyr::mutate(map_value = get(selected_value))


  data_classes <- map_data %>%
    dplyr::mutate(value = map_value) %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(map_value = unique(value), .groups = "drop") %>%
    dplyr::arrange(value) %>%
    dplyr::rename(name = map_value,
           from = value) %>%
    dplyr::mutate(to = from) %>%
    highcharter::list_parse()


  hc <- highcharter::highchart(type = "map") %>%
    highcharter::hc_add_dependency(
      name = "modules/accessibility.js"
    ) %>%
    highcharter::hc_add_dependency(
      name = "modules/exporting.js"
    ) %>%
    highcharter::hc_add_dependency(
      name = "modules/export-data.js"
    ) %>%
    highcharter::hc_add_dependency(
      name = "modules/pattern-fill.js"
    ) %>%
    custom.map::hc_add_custom_map_cat(
      data = map_data,
      value = selected_value,
      joinBy = "fips",
      mapType = map_type,
      borderColor = "#020202",
      borderWidth = .25,
      dataLabels = list(
        enabled = FALSE,
        format = "{point.state}"
      ),
      accessibility = list(
        enabled = TRUE,
        exposeAsGroupOnly = TRUE,
        keyboardNavigation = list(
          enabled = TRUE),
        linkedDescription = accessible_desc
      )
    ) %>%
    highcharter::hc_chart(
      height = 600
    ) %>%
    highcharter::hc_colors(
      colors = my_colors
    ) %>%
    highcharter::hc_colorAxis(
      dataClassColor = "category",
      dataClasses = data_classes
    ) %>%
    highcharter::hc_plotOptions(
      series = list(
        events = list(
          click = my_click
        ),
        accessibility = list(
          enabled = TRUE,
          description = accessible_desc,
          keyboardNavigation = list(
            enabled = TRUE
          )
        )
      )
    ) %>%
    highcharter::hc_title(
      text = title_text, # title text
      margin = 10
    ) %>%
    highcharter::hc_subtitle(
      text = subtitle_text, # text under title
      align = "center"
    ) %>%
    highcharter::hc_caption(
      text = caption_text, # source and data notes
      useHTML = TRUE,
      margin = 20,
      align = "center"
    ) %>%
    highcharter::hc_legend(
      reversed = my_reverse,
      enabled = enable_legend,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      itemMarginBottom = 3,
      itemMarginTop = 3,
      margin = 22,
      title = list(
        text = legend_text
      )
    ) %>%
    highcharter::hc_tooltip(
      headerFormat = "{point.JurisdictionName}",
      pointFormat = my_tooltip,
      useHTML = TRUE
    ) %>%
    highcharter::hc_add_theme(
      astho_theme
    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      accessibility = list(
        enabled = TRUE
      ),
      formAttributes = list(
        target = "_blank" # opens in new window
      ),
      sourceHeight = 700, # sets height of download
      sourceWidth = 1200, # sets width of download
      allowHTML = TRUE,
      url = "https://exporter.opifex.org", #serve owned by John Coene
      buttons = list(
        contextButton = list(
          symbol = "menu",
          symbolStrokeWidth = 2,
          symbolFill = main_green,
          symbolStroke = main_green,
          menuItems = c("printChart",
                        "separator",
                        "downloadPNG",
                        "downloadJPEG",
                        "downloadPDF",
                        "downloadSVG",
                        "separator",
                        "downloadCSV",
                        "downloadXLS",
                        "openInCloud")
        )
      )
    )
  return(hc)
}




