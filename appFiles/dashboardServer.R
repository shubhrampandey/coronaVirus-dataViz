results = reactiveValues(
  dataframeTotal = NULL,
  dfDaily = NULL,
  newCases = NULL,
  dataframeTotalOldCases = NULL
)


output$dashboard = renderUI({
  tagList(
    uiOutput("cardUI") %>% withSpinner() ,
    tags$hr(),
    uiOutput("chartUI") %>% withSpinner(),
    tags$hr(),
    h2("Data (in tabular format)"),
    dataTableOutput("dataTable") %>% withSpinner()
    
  )
})

observe({
  dataframeTotal <- coronavirus %>% 
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
    dplyr::mutate(country = dplyr::if_else(country == "Cote d'Ivoire", "Ivory Coast", country)) %>%
    dplyr::mutate(country = dplyr::if_else(country == "Eswatini", "Swaziland", country)) %>%
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
  dataframeTotal = dataframeTotal %>% as.data.frame()
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
              dplyr::top_n(n = 15, wt = total_cases)
  
  results$newCases = newCases
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
      
})

output$confirmedCount <- renderCountup({
  totalConfirmed = sum(results$dataframeTotal$totalConfirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Total confirmed cases: ")
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
               prefix = "Active Cases: ",
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
               prefix = "Recovered Cases: ",
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
               prefix = "Deaths (till today): ",
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


output$cardUI = renderUI({
  
  tagList(
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
      center = T,
      h4(paste0("Total countries affected: ",results$dataframeTotal$country %>% unique() %>% length()), 
              style = 'vertical-align: middle;
                       padding: 0px 0px;
                       font-size:25px;')
    ),
    argonRow(
      argonColumn(
        width = 5,
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
            tabName = paste0("New Cases (",Sys.Date() - 1,")"),
            active = F,
            highchartOutput("newCasesPlot",width = "100%") %>% withSpinner()
          )
          
        )
        
      ),
      argonColumn(
        width = 7,
        argonRow(
          argonColumn(
            width = 12,
            argonRow(
              tags$br(),
              argonColumn(
                width = 8,
                pickerInput(
                  "populationSelect",
                  label = NULL,
                  choices = results$dataframeTotal$country,
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE
                  ),
                  multiple = T,
                  selected = results$dataframeTotal$country,
                  width = "100%"
                )
              ),
              argonColumn(
                width = 3,
                pickerInput(
                  "highchartOption",
                  label = NULL,
                  choices = setNames(c(1:4),c("Total Cases","Recovered cases","Deaths","Active Cases")),
                  selected = "1",
                  width = "100%"
                )
              )
            ),
            highchartOutput("worldMap",width = "100%") %>% withSpinner()
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
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily = results$dfDaily
  x = max(df_daily$active_cum,df_daily$death_cum,df_daily$recovered_cum)
  y = nchar(x) - 1
  yLimit = x %>% round(-y)
  hc <- highchart() %>% 
    hc_subtitle(text = "Cumulative Cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases")) %>%
    hc_add_series(name = "Active",data = df_daily$active_cum) %>% 
    hc_add_series(name = "Recovered", data = df_daily$recovered_cum) %>% 
    hc_add_series(name = "Death", data = df_daily$death_cum)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$totalCasesPlot = renderHighchart({
  x = results$dataframeTotal %>%
    arrange(desc(totalConfirmed)) %>%
    .[1:15,]
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Total  Cases (Top 15)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$country) %>%
    hc_yAxis(title = list(text = "Total Cases (Log scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Active",data = x$totalUnrecovered) %>% 
    hc_add_series(name = "Recovered", data = x$totalRecovered) %>% 
    hc_add_series(name = "Death", data = x$totalDeath)
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesPlot = renderHighchart({
  x = results$newCases
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "New  Cases (Top 15)",
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

output$dataTable = renderDataTable({
  x = coronavirus %>% 
    select(Date = date, Province = Province.State, Country = Country.Region, `Case Type` = type, `Number of Cases` = cases)
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
            
  )
})
