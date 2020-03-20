results = reactiveValues(
  dataframeTotal = NULL,
  dfDaily = NULL,
  newCases = NULL,
  dataframeTotalOldCases = NULL,
  newCasesDeath = NULL,
  dataframeFinal = NULL,
  newCasesRecovered = NULL
)


output$dashboard = renderUI({
  argonTabSet(
    id = "analysisSettingsTabs",
    card_wrapper = T,
    horizontal = TRUE,
    circle = F,
    size = "sm",
    width = 12,
    iconList = list(
      icon("tachometer-alt"), 
      icon("globe"), 
      icon("twitter")
    ),
    # analysis setting tab -----
    argonTab(
      tabName = "Dashboard",
      active = T,
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("cardUI") %>% withSpinner()
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("chartUI") %>% withSpinner()
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          dataTableOutput("dataTableCountryWise") %>% withSpinner()
          
        )
      )
    ),
  argonTab(
    tabName = "Country specific",
    active = F,
    uiOutput("countrySpecificCards") %>% withSpinner(),
    tags$hr(),
    uiOutput("countrySpecificChartsUI") %>% withSpinner()
    
  ),
  argonTab(
    tabName = "Sentiments",
    active = F,
    uiOutput("sentimentUI") %>% withSpinner()
  )
  
  )
})

observe({
  waiter_show(loader)
  dataframeFinal = coronavirus %>%
                        dplyr::mutate(country = dplyr::if_else(Country.Region == "US", "United States of America", Country.Region)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "UK", "United Kingdom", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Saint Barthelemy", "France", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Gibraltar", "United Kingdom", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Palestine", "Israel", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "Macedonia", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Iran (Islamic Republic of)", "Iran", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Republic of Korea", "South Korea", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Korea, South", "South Korea", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Taipei and environs", "Taiwan", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Taiwan*", "Taiwan", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Viet Nam", "Vietnam", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "occupied Palestinian territory", "Israel", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Russian Federation", "Russia", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "French Guiana", "France", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Hong Kong SAR", "China", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Macao SAR", "China", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Serbia", "Republic of Serbia", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Republic of Moldova", "Moldova", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Martinique", "France", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Czechia", "Czech Republic", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Reunion", "France", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Guadeloupe", "France", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Aruba", "Netherlands", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Congo (Kinshasa)", "Democratic Republic of the Congo", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Congo (Brazzaville)", "Republic of Congo", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Republic of the Congo", "Republic of Congo", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Tanzania", "United Republic of Tanzania", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "The Gambia", "Gambia", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Gambia, The", "Gambia", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Cote d'Ivoire", "Ivory Coast", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Eswatini", "Swaziland", country)) %>%
                        dplyr::mutate(country = dplyr::if_else(country == "Bahamas, The", "The Bahamas", country)) %>%
                        dplyr::mutate(country = trimws(country))
  results$dataframeFinal = dataframeFinal
  dataframeTotal <- dataframeFinal %>% 
    dplyr::group_by(Country.Region, type) %>%
    dplyr::summarise(total = sum(cases)) %>%
    tidyr::pivot_wider(names_from =  type, 
                       values_from = total) %>%
    dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
    dplyr::arrange(-confirmed) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(country = dplyr::if_else(Country.Region == "US", "United States of America", Country.Region)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "UK", "United Kingdom", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Saint Barthelemy", "France", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Gibraltar", "United Kingdom", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Palestine", "Israel", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "Macedonia", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Iran (Islamic Republic of)", "Iran", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Republic of Korea", "South Korea", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Korea, South", "South Korea", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Taipei and environs", "Taiwan", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Taiwan*", "Taiwan", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Viet Nam", "Vietnam", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "occupied Palestinian territory", "Israel", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Russian Federation", "Russia", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "French Guiana", "France", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Hong Kong SAR", "China", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Macao SAR", "China", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Serbia", "Republic of Serbia", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Republic of Moldova", "Moldova", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Martinique", "France", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Czechia", "Czech Republic", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Reunion", "France", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Guadeloupe", "France", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Aruba", "Netherlands", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Congo (Kinshasa)", "Democratic Republic of the Congo", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Congo (Brazzaville)", "Republic of Congo", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Republic of the Congo", "Republic of Congo", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Tanzania", "United Republic of Tanzania", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "The Gambia", "Gambia", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Gambia, The", "Gambia", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Cote d'Ivoire", "Ivory Coast", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Eswatini", "Swaziland", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Bahamas, The", "The Bahamas", country)) %>%
    dplyr::mutate(country = trimws(country)) %>%
    select(-Country.Region)
  dataframeTotal[,1:4] = lapply(dataframeTotal[,1:4], function(x) as.numeric(x))
  dataframeTotal = dataframeTotal %>%
    group_by(country) %>%
    summarise(totalConfirmed = sum(confirmed,na.rm = T),
              totalDeath = sum(death,na.rm = T),
              totalRecovered = sum(recovered,na.rm = T),
              totalUnrecovered = sum(unrecovered,na.rm = T)
    ) %>%
    as.data.frame()
  results$dataframeTotal = dataframeTotal
  df_daily <- coronavirus %>% 
    dplyr::group_by(date, type) %>%
    dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = total) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(active =  confirmed - death - recovered) %>%
    dplyr::mutate(confirmed_cum = cumsum(confirmed),
                  death_cum = cumsum(death),
                  recovered_cum = cumsum(recovered),
                  active_cum = cumsum(active))
  results$dfDaily = df_daily
  
  max_date <- max(coronavirus$date)
  newCases = coronavirus %>% 
              dplyr::filter(type == "confirmed", date == max_date) %>%
              dplyr::group_by(Country.Region) %>%
              dplyr::summarise(total_cases = sum(cases)) %>%
              dplyr::arrange(-total_cases) %>%
              dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
              dplyr::ungroup() %>%
              dplyr::top_n(n = 25, wt = total_cases)
  
  results$newCases = newCases
  newCasesDeaths = coronavirus %>% 
                    dplyr::filter(type == "death", date == max_date) %>%
                    dplyr::group_by(Country.Region) %>%
                    dplyr::summarise(total_cases = sum(cases)) %>%
                    dplyr::filter(total_cases > 0) %>%
                    dplyr::arrange(-total_cases) %>%
                    dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
                    dplyr::ungroup() 
  
  results$newCasesDeath = newCasesDeaths
  newCasesRecovered = coronavirus %>% 
                      dplyr::filter(type == "recovered", date == max_date) %>%
                      dplyr::group_by(Country.Region) %>%
                      dplyr::summarise(total_cases = sum(cases)) %>%
                      dplyr::filter(total_cases > 0) %>%
                      dplyr::arrange(-total_cases) %>%
                      dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
                      dplyr::ungroup() 
  
  results$newCasesRecovered = newCasesRecovered
  
  dataframeTotalOldCases = coronavirus %>% 
                            dplyr::filter(!(date == max_date)) %>% 
                            dplyr::group_by(type) %>%
                            dplyr::summarise(total = sum(cases)) %>%
                            tidyr::pivot_wider(names_from =  type, 
                                               values_from = total) %>%
                            dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
    summarise(totalConfirmed = sum(confirmed,na.rm = T),
              totalDeath = sum(death,na.rm = T),
              totalRecovered = sum(recovered,na.rm = T),
              totalUnrecovered = sum(unrecovered,na.rm = T)
    )
  results$dataframeTotalOldCases = dataframeTotalOldCases
  waiter_hide()
})

output$confirmedCount <- renderCountup({
  totalConfirmed = sum(results$dataframeTotal$totalConfirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Confirmed ")
  countup(
    totalConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$activeCount <- renderCountup({
  totalUnrecovered = sum(results$dataframeTotal$totalUnrecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$totalConfirmed,na.rm = T)
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Active ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalUnrecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$recoveredCount <- renderCountup({
  totalRecovered = sum(results$dataframeTotal$totalRecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$totalConfirmed,na.rm = T)
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Recovered ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
               )
  countup(
    totalRecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$deathCount <- renderCountup({
  totalDeath = sum(results$dataframeTotal$totalDeath,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$totalConfirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Deaths ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalDeath,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$countryCount <- renderCountup({
  x = results$dataframeTotal$country %>% 
      unique() %>%                                                                         length()
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Total countries affected: "
              )
  countup(
    x,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$cardUI = renderUI({
  
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        center = T,
        argonBadge(text = countupOutput("countryCount"), 
                   src = NULL, 
                   pill = T, 
                   status = "danger")
       )
      ),
      tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("confirmedCount"),
          icon = icon("users"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "warning",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalConfirmed,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("activeCount"),
          icon = icon("hospital"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "info",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalUnrecovered,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("recoveredCount"),
          icon = icon("smile"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "success",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalRecovered,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("deathCount"),
          icon = icon("heartbeat"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "danger",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalDeath,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      )
    )
  )
})

output$chartUI = renderUI({
  tagList(
  argonRow(
    argonColumn(
      width = 3,
      argonRow(
        argonColumn(
          width = 12,
          pickerInput(
            "populationSelect",
            label = strong("Countries showing in the geospatial map: "),
            choices = results$dataframeTotal$country,
            options = list(`actions-box` = TRUE,
                           `live-search` = TRUE
            ),
            multiple = T,
            selected = results$dataframeTotal$country,
            width = "100%",
            inline = F
          )
        ),
        tags$hr(),
        tags$br(),
        argonColumn(
          radioGroupButtons(inputId = "highchartOption", 
                            label = strong("Specify the outcome to be shown in map:"), 
                            choices = setNames(c(1:4),c("Total Cases","Recovered cases","Deaths","Active Cases")),
                            selected = "1",
                            justified = T,
                            width = "100%",
                            status = "primary",
                            direction = "vertical"
                            )
        )
      )
    ),
    argonColumn(
      width = 9,
      highchartOutput("worldMap",width = "100%") %>% withSpinner()
    )
  ),
  tags$hr(),
  argonRow(
    argonColumn(
      width = 12,
      argonTabSet(
        id = "dashboardTab",
        card_wrapper = F,
        horizontal = TRUE,
        circle = F,
        size = "sm",
        width = 12,
        # analysis setting tab -----
        argonTab(
          tabName = "Total cases",
          active = T,
          highchartOutput("totalCasesPlot",width = "100%") %>% withSpinner()
        ),
        argonTab(
          tabName = "Cumulative Cases",
          active = F,
          highchartOutput("cumulativePlot",width = "100%") %>% withSpinner()
        ),
        argonTab(
          tabName = paste0("Confirmed New Cases (",Sys.Date() - 1,")"),
          active = F,
          highchartOutput("newCasesPlot",width = "100%") %>% withSpinner()
        ),
        argonTab(
          tabName = paste0("Deaths (",Sys.Date() - 1,")"),
          active = F,
          highchartOutput("newCasesDeathsPlot",width = "100%") %>% withSpinner()
        ),
        argonTab(
          tabName = paste0("Recovered (",Sys.Date() - 1,")"),
          active = F,
          highchartOutput("newCasesRecoveredPlot",width = "100%") %>% withSpinner()
        )
        

      )

    )
  )
  )

})

output$worldMap <- renderHighchart({
  req(!is.null(results$dataframeTotal))
  x = input$highchartOption %>% as.numeric()
  y = input$populationSelect %>% tolower()
  data = results$dataframeTotal %>% 
         filter(str_detect(tolower(country), pattern = paste(y,collapse = "|"))) 
  value = switch(x,"totalConfirmed","totalRecovered","totalDeath","totalUnrecovered")
  colnames(data)[1] = "name"
  highchart(type = "map",width = "100%",height = "100%") %>%
    hc_add_series_map(map = worldgeojson, df = data, value = value, joinBy = "name") %>%
    hc_colorAxis(stops = color_stops()) %>%
    hc_tooltip(useHTML = TRUE,headerFormat = '',pointFormat = paste0('{point.name}: {point.',value,'} ')) %>%
    hc_exporting(enabled = TRUE,filename = value) %>% 
    hc_add_theme(hc_theme_ffx()) %>%
    hc_chart(zoomType = "xy") %>%
    hc_mapNavigation(enabled = TRUE) 
  
})

output$cumulativePlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily = results$dfDaily
  x = max(df_daily$confirmed_cum,df_daily$active_cum,df_daily$death_cum,df_daily$recovered_cum)
  y = nchar(x) - 1
  yLimit = x %>% round(-y)
  hc <- highchart() %>% 
    hc_subtitle(text = "Cumulative Cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases")) %>%
    hc_add_series(name = "Confirmed",data = df_daily$confirmed_cum) %>% 
    hc_add_series(name = "Active",data = df_daily$active_cum) %>% 
    hc_add_series(name = "Recovered", data = df_daily$recovered_cum) %>% 
    hc_add_series(name = "Death", data = df_daily$death_cum)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$totalCasesPlot = renderHighchart({
  x = results$dataframeTotal %>%
    arrange(desc(totalConfirmed)) %>%
    .[1:15,]
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Total  Cases (Top 15 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$country) %>%
    hc_yAxis(title = list(text = "Total Cases (Log scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Confirmed",data = x$totalConfirmed) %>% 
    hc_add_series(name = "Active",data = x$totalUnrecovered) %>% 
    hc_add_series(name = "Recovered", data = x$totalRecovered) %>% 
    hc_add_series(name = "Death", data = x$totalDeath)
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesPlot = renderHighchart({
  x = results$newCases
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "New  Cases (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$country) %>%
    hc_yAxis(title = list(text = "New Cases")) %>%
    hc_add_series(name = "Countries",data = x$total_cases) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})
output$newCasesDeathsPlot = renderHighchart({
  x = results$newCasesDeath
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "New Deaths (All countries having atleast 1 death yesterday)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$country) %>%
    hc_yAxis(title = list(text = "New Deaths")) %>%
    hc_add_series(name = "Countries",data = x$total_cases) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})
output$newCasesRecoveredPlot = renderHighchart({
  x = results$newCasesRecovered
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Newly Recovered (All countries having atleast 1 recovered case yesterday)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$country) %>%
    hc_yAxis(title = list(text = "New Recovered")) %>%
    hc_add_series(name = "Countries",data = x$total_cases) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$dataTableCountryWise = renderDataTable({
  x = results$dataframeTotal %>%
        arrange(desc(totalConfirmed)) %>%
        mutate(totalActivePer = totalUnrecovered/totalConfirmed) %>%
        mutate(totalRecoveredPer = totalRecovered/totalConfirmed) %>%
        mutate(totalDeathPer = totalDeath/totalConfirmed) %>%
    select(Country = country, Confirmed = totalConfirmed, Active = totalUnrecovered,Recovered = totalRecovered,Deaths = totalDeath,"Active (%)" = totalActivePer,"Recovered (%)" = totalRecoveredPer,"Deaths (%)" = totalDeathPer)
  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Download'
                  )
                )
              
            )
            
  ) %>%
  formatPercentage('Active (%)',2) %>%
  formatPercentage('Recovered (%)',2) %>%
  formatPercentage('Deaths (%)',2) %>%
  formatStyle(
      'Active (%)',
      background = styleColorBar(x$'Active (%)', '#31bed4'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Recovered (%)',
      background = styleColorBar(x$'Recovered (%)', '#8bd431'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Deaths (%)',
      background = styleColorBar(x$'Deaths (%)', '#ff5757'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})

#### Country Specific tab

output$countrySpecificCards = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        div(
          class = "inlineSubInput",
          pickerInput(
            "countrySelect",
            label = strong("Showing results from "),
            choices = results$dataframeTotal$country,
            options = list(
              `live-search` = TRUE
            ),
            multiple = F,
            selected = "China",
            width = "100%",
            inline = T
          )
        )
       )
      ),
    tags$br(),
      argonRow(
        argonColumn(
          width = 3,
          argonInfoCard(
            value = countupOutput("confirmedCountCountry"),
            icon = icon("users"),
            icon_background = "default",
            hover_lift = F,
            shadow = T,
            background_color = "warning",
            gradient = T,
            width = 12
          )
          # h6(paste0("Yesterday: ",
          #           prettyNum(results$dataframeTotalOldCases$totalConfirmed,big.mark = ",")
          # ), 
          # style = 'text-align:center;
          # font-size:15px;')
          ),
        argonColumn(
          width = 3,
          argonInfoCard(
            value = countupOutput("activeCountCountry"),
            icon = icon("hospital"),
            icon_background = "default",
            hover_lift = F,
            shadow = T,
            background_color = "info",
            gradient = T,
            width = 12
          )
          # h6(paste0("Yesterday: ",
          #           prettyNum(results$dataframeTotalOldCases$totalUnrecovered,big.mark = ",")
          # ), 
          # style = 'text-align:center;
          # font-size:15px;')
          ),
        argonColumn(
          width = 3,
          argonInfoCard(
            value = countupOutput("recoveredCountCountry"),
            icon = icon("smile"),
            icon_background = "default",
            hover_lift = F,
            shadow = T,
            background_color = "success",
            gradient = T,
            width = 12
          )
          # h6(paste0("Yesterday: ",
          #           prettyNum(results$dataframeTotalOldCases$totalRecovered,big.mark = ",")
          # ), 
          # style = 'text-align:center;
          # font-size:15px;')
          ),
        argonColumn(
          width = 3,
          argonInfoCard(
            value = countupOutput("deathCountCountry"),
            icon = icon("heartbeat"),
            icon_background = "default",
            hover_lift = F,
            shadow = T,
            background_color = "danger",
            gradient = T,
            width = 12
          )
          # h6(paste0("Yesterday: ",
          #           prettyNum(results$dataframeTotalOldCases$totalDeath,big.mark = ",")
          # ), 
          # style = 'text-align:center;
          # font-size:15px;')
          )
        )
      )
})

output$confirmedCountCountry <- renderCountup({
  x = results$dataframeTotal %>% 
        filter(country == input$countrySelect)
  totalConfirmed = sum(x$totalConfirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Confirmed ")
  countup(
    totalConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$activeCountCountry <- renderCountup({
  x = results$dataframeTotal %>% 
         filter(country == input$countrySelect)
  totalUnrecovered = sum(x$totalUnrecovered,na.rm = T)
  totalConfirmed = sum(x$totalConfirmed,na.rm = T)
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Active ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalUnrecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$recoveredCountCountry <- renderCountup({
  x = results$dataframeTotal %>% 
        filter(country == input$countrySelect)
  totalRecovered = sum(x$totalRecovered,na.rm = T)
  totalConfirmed = sum(x$totalConfirmed,na.rm = T)
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Recovered ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
  )
  countup(
    totalRecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$deathCountCountry <- renderCountup({
  x = results$dataframeTotal %>% 
        filter(country == input$countrySelect)
  totalDeath = sum(x$totalDeath,na.rm = T)
  totalConfirmed = sum(x$totalConfirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Deaths ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalDeath,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$countrySpecificChartsUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        argonTabSet(
          id = "dashboardCountryTab",
          card_wrapper = F,
          horizontal = TRUE,
          circle = F,
          size = "sm",
          width = 12,
          argonTab(
            tabName = "Cumulative Cases",
            active = T,
            highchartOutput("cumulativeCountryPlot",width = "100%") %>% withSpinner()
          ),
          argonTab(
            tabName = paste0("Confirmed New Cases (",Sys.Date() - 1,")"),
            active = F,
            highchartOutput("newCasesCountryPlot",width = "100%") %>% withSpinner()
          ),
          argonTab(
            tabName = paste0("Deaths (",Sys.Date() - 1,")"),
            active = F,
            highchartOutput("newCasesDeathCountryPlot",width = "100%") %>% withSpinner()
          ),
          argonTab(
            tabName = paste0("Recovered (",Sys.Date() - 1,")"),
            active = F,
            highchartOutput("newCasesRecoveredCountryPlot",width = "100%") %>% withSpinner()
          )
          
        )
        
      )
    )
  )
  
})

output$cumulativeCountryPlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily <- results$dataframeFinal %>% 
                filter(country == input$countrySelect) %>%
                dplyr::group_by(date, type) %>%
                dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
                tidyr::pivot_wider(names_from = type,
                                   values_from = total) %>%
                dplyr::arrange(date) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(active =  confirmed - death - recovered) %>%
                dplyr::mutate(confirmed_cum = cumsum(confirmed),
                              death_cum = cumsum(death),
                              recovered_cum = cumsum(recovered),
                              active_cum = cumsum(active))
  x = max(df_daily$confirmed_cum,df_daily$active_cum,df_daily$death_cum,df_daily$recovered_cum)
  y = nchar(x) - 1
  yLimit = x %>% round(-y)
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Cumulative Cases in ",input$countrySelect),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases")) %>%
    hc_add_series(name = "Confirmed",data = df_daily$confirmed_cum) %>% 
    hc_add_series(name = "Active",data = df_daily$active_cum) %>% 
    hc_add_series(name = "Recovered", data = df_daily$recovered_cum) %>% 
    hc_add_series(name = "Death", data = df_daily$death_cum)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
              dplyr::filter(country == input$countrySelect,type == "confirmed") %>%
              dplyr::group_by(date) %>%
              dplyr::summarise(total_cases = sum(cases)) %>%
              # dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
              dplyr::ungroup() 
  x = newCases
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "Confirmed cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "New Cases")) %>%
    hc_add_series(name = "Countries",data = x$total_cases) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesDeathCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
    dplyr::filter(country == input$countrySelect,type == "death") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(total_cases = sum(cases)) %>%
    # dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
    dplyr::ungroup() 
  x = newCases
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Deaths",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_add_series(name = "Countries",data = x$total_cases) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesRecoveredCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
    dplyr::filter(country == input$countrySelect,type == "recovered") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(total_cases = sum(cases)) %>%
    # dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
    dplyr::ungroup() 
  x = newCases
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Recovered",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Recovered")) %>%
    hc_add_series(name = "Countries",data = x$total_cases) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

#### Sentiment analysis ----

sentimentAnalResult = reactiveValues(
  run = 1,
  tweetText = NULL,
  tweetScores = NULL
)

output$sentimentUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        center = T,
        h1("Sentiment analysis using Twitter Data")
      )
    ),
    argonRow(
      argonColumn(
        width = 4,
        argonRow(
          argonColumn(
            width = 12,
            pickerInput(
              "twitterHashtag",
              label = strong("Please select the hashtags you want to include in sentiment analysis: "),
              choices = c("#coronavirus","#covid19","#Covid_19","#CoronaCrisis"),
              options = list(`actions-box` = TRUE,
                             `live-search` = TRUE
              ),
              multiple = T,
              selected = c("#coronavirus","#covid19","#Covid_19","#CoronaCrisis"),
              width = "100%",
              inline = F
            ),
            tags$br(),
            radioGroupButtons(inputId = "tweetsOption", 
                              label = strong("Specify the number of latest tweets use for analysis (time take to run the analysis):"), 
                              choices = setNames(c(1:5),c("500 (<1 Minutes)","1000 (1 Minutes)","2000 (2 Minutes)","5000 (3 Minutes)","10000 (5 Minutes)")),
                              selected = "1",
                              justified = T,
                              width = "100%",
                              status = "primary",
                              direction = "vertical"
            )
          )
        )
      ),
      argonColumn(
        width = 8,
        plotOutput("workcloud",width = "100%",height = "500px") %>% withSpinner()
      )
    ),
    tags$hr(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("sentimentPlot") %>% withSpinner()
      )
    )
    
    
  )
})

observeEvent(debounce(reactive(c(input$tweetsOption,input$twitterHashtag)), 1000)(),{
  time = switch(input$tweetsOption,
                "1" = "<1 minute",
                "2" = "1 minute",
                "3" = "2 minutes",
                "4" = "3 minutes",
                "5" = "5 minutes"
  )
  tweetsN = switch(input$tweetsOption,
                "1" = 500,
                "2" = 1000,
                "3" = 2000,
                "4" = 5000,
                "5" = 10000
  )
    progressSweetAlert(session, id = "twitterProgress", 
                       value = 10, 
                       total = NULL,
                       display_pct = T, 
                       size = NULL, 
                       status = "success", 
                       striped = T,
                       title = paste0("Fetching data from twitter...","(Take approx ",time," to run"))
    consumer_key <- 'lJPpmqW41pv6K6miqXmcXYemV'
    consumer_secret <- '01kHQaZqGhmboh7bAUBJIy1rqdqb7QLjan0RQgyNIyWwLOm2u1'
    access_token <- '1966387032-BDCHsUK08TbtmxOcFuFnKii1Jt94l72tn1Oz5lZ'
    access_secret <- 'Ry8n90UsUerTmcUnoNxBfVSnwR6fCmV4aC7j02JGQnPPq'
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    tweets <- searchTwitter(paste0(input$twitterHashtag, collapse = " OR "),
                            n = tweetsN,
                            lang = "en")
    updateProgressBar(
      session = session,
      id = "twitterProgress",
      value = 50,
      title = "Pre-processing data..."
    )
    tweetsDF <- twListToDF(tweets)
    tweetsText <- tweetsDF$text
    tweetsText <- tolower(tweetsText)
    tweetsText <- gsub("rt", "", tweetsText)
    tweetsText <- gsub("@\\w+", "", tweetsText)
    tweetsText <- gsub("[[:punct:]]", "", tweetsText)
    tweetsText <- gsub("http\\w+", "", tweetsText)
    tweetsText <- gsub("[ |\t]{2,}", "", tweetsText)
    tweetsText <- gsub("^ ", "", tweetsText)
    tweetsText <- gsub(" $", "", tweetsText)
    sentimentAnalResult$tweetText = tweetsText
    sample <- sample(tweetsText, (length(tweetsText)))
    corpus <- Corpus(VectorSource(list(sample)))
    corpus <- tm_map(corpus, removePunctuation) %>% suppressWarnings()
    corpus <- tm_map(corpus, content_transformer(tolower)) %>% suppressWarnings()
    corpus <- tm_map(corpus, removeNumbers) %>% suppressWarnings()
    corpus <- tm_map(corpus, stripWhitespace) %>% suppressWarnings()
    corpus <- tm_map(corpus, removeWords, stopwords('english')) %>% suppressWarnings()
    
    corpus <- tm_map(corpus, stemDocument)##obtain word stems
    sentimentAnalResult$corpus = corpus
    
    updateProgressBar(
      session = session,
      id = "twitterProgress",
      value = 90,
      title = "Generating scores.."
    )
    tweetsScores <- get_nrc_sentiment((sentimentAnalResult$tweetText))
    tweetsScores <- data.frame(colSums(tweetsScores[,]))
    names(tweetsScores) <- "Score"
    tweetsScores <- cbind("sentiment" = rownames(tweetsScores),tweetsScores)
    rownames(tweetsScores) <- NULL
    sentimentAnalResult$tweetScores = tweetsScores
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = " Analysis completed !!!",
      type = "success"
    )
  
})

output$workcloud = renderPlot({
  req(!is.null(sentimentAnalResult$corpus))
  wordcloud(sentimentAnalResult$corpus,
            min.freq = 10,
            scale = c(5,0.5),
            colors = brewer.pal(8, "Dark2"),
            random.color = TRUE)
})

output$sentimentPlot = renderHighchart({
  req(!is.null(sentimentAnalResult$tweetScores))
  tweetsScores = sentimentAnalResult$tweetScores
  proper = function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  hc <- highchart() %>% 
    hc_subtitle(text = "Sentiments of people behind the tweets on pandemic CORONAVIRUS",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = proper(tweetsScores$sentiment),title = list(text = "Sentiments")) %>%
    hc_yAxis(title = list(text = "Score")) %>%
    hc_add_series(name = "Score",data = tweetsScores$Score,showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

#### to check which countries are new
# 
# mapdata <- get_data_from_map(download_map_data("custom/world-palestine-highres"))
# 
# dataframeTotal$country[!(dataframeTotal$country %in% mapdata$name)]
# 
# [1] "Cruise Ship"
# [2] "Holy See