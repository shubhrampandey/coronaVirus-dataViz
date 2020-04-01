results = reactiveValues(
  dataframeTotal = NULL,
  dfDaily = NULL,
  newCases = NULL,
  dataframeTotalOldCases = NULL,
  newCasesDeath = NULL,
  dataframeFinal = NULL,
  newCasesRecovered = NULL,
  dataframeOldCases = NULL
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
  results$dataframeFinal = coronavirus
  dataframeTotal <- coronavirus %>% 
                      dplyr::group_by(countryName) %>%
                      slice(n()) %>%
                      ungroup() %>%
                      dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>%
                      dplyr::arrange(-Confirmed) %>%
                      dplyr::ungroup() %>%
                      select(-c(date,region,lat,lon))
  # browser()
  results$dataframeTotal = dataframeTotal
  df_daily <- coronavirus %>% 
                dplyr::group_by(date) %>%
                dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                                 totalRecovered = sum(Recovered,na.rm = TRUE),
                                 totalDeaths = sum(Deaths,na.rm = T)
                                 ) %>%
                dplyr::arrange(date) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) 
  results$dfDaily = df_daily
  
  max_date <- as.Date(max(coronavirus$date)) 
  newCases = coronavirus %>% 
              dplyr::filter(date == max_date | date == max_date - 1) %>%
              dplyr::group_by(countryName) %>%
              mutate(ConfirmedNew = Confirmed - shift(Confirmed,1)) %>% 
              mutate(RecoveredNew = Recovered - shift(Recovered,1)) %>%
              mutate(DeathsNew = Deaths - shift(Deaths,1)) %>%
              slice(n()) %>%
              ungroup() %>%
              select(countryName,ConfirmedNew,RecoveredNew,DeathsNew)
  
  results$newCases = newCases
  dataframeTotalOldCases = coronavirus %>%
                            dplyr::filter(date == max_date - 1) %>%
                            dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths) %>%
    summarise(totalConfirmed = sum(Confirmed,na.rm = T),
              totalDeath = sum(Deaths,na.rm = T),
              totalRecovered = sum(Recovered,na.rm = T),
              totalUnrecovered = sum(Unrecovered,na.rm = T)
    )
  results$dataframeTotalOldCases = dataframeTotalOldCases
  dataframeOldCases = coronavirus %>%
                            dplyr::filter(date == max_date - 1) %>%
                            dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths)
  results$dataframeOldCases = dataframeOldCases
  waiter_hide()
})

output$confirmedCount <- renderCountup({
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
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
  totalUnrecovered = sum(results$dataframeTotal$Unrecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
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
  totalRecovered = sum(results$dataframeTotal$Recovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
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
  totalDeath = sum(results$dataframeTotal$Deaths,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
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
  x = results$dataframeTotal %>%
      filter(Confirmed > 0) %>%
      select(countryName) %>%
      unique() %>%                                                                              nrow()
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
            choices = results$dataframeTotal$countryName,
            options = list(`actions-box` = TRUE,
                           `live-search` = TRUE
            ),
            multiple = T,
            selected = results$dataframeTotal$countryName,
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
         filter(str_detect(tolower(countryName), pattern = paste(y,collapse = "|"))) 
  value = switch(x,"Confirmed","Recovered","Deaths","Unrecovered")
  colnames(data)[5] = "name"
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
  x = max(df_daily$totalConfirmed,df_daily$totalUnrecovered,df_daily$totalDeaths,df_daily$totalRecovered)
  y = nchar(x) - 1
  yLimit = x %>% round(-y)
  hc <- highchart() %>% 
    hc_subtitle(text = "Cumulative Cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases")) %>%
    hc_add_series(name = "Confirmed",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Active",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Recovered", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Death", data = df_daily$totalDeaths)
  
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
        select(-countryCode) %>%
        arrange(desc(Confirmed)) %>%
        .[1:15,]
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Total  Cases (Top 15 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "Total Cases (Log scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Confirmed",data = x$Confirmed) %>% 
    hc_add_series(name = "Active",data = x$Unrecovered) %>% 
    hc_add_series(name = "Recovered", data = x$Recovered) %>% 
    hc_add_series(name = "Death", data = x$Deaths)
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
  x = results$newCases %>%
        select(countryName,ConfirmedNew) %>%
        arrange(-ConfirmedNew) %>%
        top_n(n = 25,wt = ConfirmedNew)
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "New  Cases (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "New Cases")) %>%
    hc_add_series(name = "Countries",data = x$ConfirmedNew) 
  
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
  x = results$newCases %>%
        select(countryName,DeathsNew) %>%
        arrange(-DeathsNew) %>%
        top_n(n = 25,wt = DeathsNew)
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "New Deaths (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "New Deaths")) %>%
    hc_add_series(name = "Countries",data = x$DeathsNew) 
  
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
  x = results$newCases %>%
        select(countryName,RecoveredNew) %>%
        arrange(-RecoveredNew) %>%
        top_n(n = 25,wt = RecoveredNew)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Newly Recovered (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "New Recovered")) %>%
    hc_add_series(name = "Countries",data = x$RecoveredNew) 
  
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
        select(-countryCode) %>%
        arrange(desc(Confirmed)) %>%
        mutate(totalActivePer = Unrecovered/Confirmed) %>%
        mutate(totalRecoveredPer = Recovered/Confirmed) %>%
        mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(Country = countryName, Confirmed = Confirmed, Active = Unrecovered,Recovered = Recovered,Deaths = Deaths,"Active (%)" = totalActivePer,"Recovered (%)" = totalRecoveredPer,"Deaths (%)" = totalDeathPer)
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
            choices = results$dataframeTotal$countryName,
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
          ),
          argonRow(
            center = T,
            uiOutput("yesterdayConfirmedCountry")
           )
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
          ),
          argonRow(
            center = T,
            uiOutput("yesterdayActiveCountry")
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
          ),
          argonRow(
            center = T,
            uiOutput("yesterdayRecoveredCountry")
          )
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
          ),
          argonRow(
            center = T,
            uiOutput("yesterdayDeathsCountry")
          )
          )
        )
      )
})

output$yesterdayConfirmedCountry = renderUI({
  req(!is.null(input$countrySelect) )
  h6(paste0("Yesterday: ",
            prettyNum(results$dataframeOldCases %>%
                             filter(countryName == input$countrySelect) %>%
                             .["Confirmed"],
                      big.mark = ",")
  ),
  style = 'text-align:center;
  font-size:15px;')
})

output$yesterdayActiveCountry = renderUI({
  req(!is.null(input$countrySelect) )
  h6(paste0("Yesterday: ",
            prettyNum(results$dataframeOldCases %>%
                        filter(countryName == input$countrySelect) %>%
                        .["Unrecovered"],
                      big.mark = ",")
  ),
  style = 'text-align:center;
  font-size:15px;')
})
output$yesterdayRecoveredCountry = renderUI({
  req(!is.null(input$countrySelect) )
  h6(paste0("Yesterday: ",
            prettyNum(results$dataframeOldCases %>%
                        filter(countryName == input$countrySelect) %>%
                        .["Recovered"],
                      big.mark = ",")
  ),
  style = 'text-align:center;
  font-size:15px;')
})

output$yesterdayDeathsCountry = renderUI({
  req(!is.null(input$countrySelect) )
  h6(paste0("Yesterday: ",
            prettyNum(results$dataframeOldCases %>%
                        filter(countryName == input$countrySelect) %>%
                        .["Deaths"],
                      big.mark = ",")
  ),
  style = 'text-align:center;
  font-size:15px;')
})

output$confirmedCountCountry <- renderCountup({
  x = results$dataframeTotal %>% 
        filter(countryName == input$countrySelect)
  totalConfirmed = x$Confirmed
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
        filter(countryName == input$countrySelect)
  totalUnrecovered = x$Unrecovered
  totalConfirmed = x$Confirmed
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
          filter(countryName == input$countrySelect)
  totalRecovered = x$Recovered
  totalConfirmed = x$Confirmed
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
         filter(countryName == input$countrySelect)
  totalDeath = x$Deaths
  totalConfirmed = x$Confirmed
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
  df_daily <- coronavirus %>% 
                filter(countryName == input$countrySelect) %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                                 totalRecovered = sum(Recovered,na.rm = TRUE),
                                 totalDeaths = sum(Deaths,na.rm = T)
                ) %>%
                dplyr::arrange(date) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) 
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Cumulative Cases in ",input$countrySelect),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases")) %>%
    hc_add_series(name = "Confirmed",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Active",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Recovered", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Death", data = df_daily$totalDeaths)
  
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
              select(date,countryName,Confirmed) %>%
              dplyr::filter(countryName == input$countrySelect) %>%
              mutate(confirmedDaily = if_else(is.na(Confirmed - shift(Confirmed,1)),
                                      Confirmed,
                                      Confirmed - shift(Confirmed,1)
                                      )
                     
                     ) 
  x = newCases
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "Confirmed cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "New Cases")) %>%
    hc_add_series(name = "Countries",data = x$confirmedDaily) 
  
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
                select(date,countryName,Deaths) %>%
                dplyr::filter(countryName == input$countrySelect) %>%
                mutate(deathDaily = if_else(is.na(Deaths - shift(Deaths,1)),
                                            Deaths,
                                            Deaths - shift(Deaths,1)
                )
                
                )
  x = newCases
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Deaths",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_add_series(name = "Countries",data = x$deathDaily) 
  
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
                select(date,countryName,Recovered) %>%
                dplyr::filter(countryName == input$countrySelect) %>%
                mutate(recoveredDaily = if_else(is.na(Recovered - shift(Recovered,1)),
                                            Recovered,
                                            Recovered - shift(Recovered,1)
                )
                
                )
  x = newCases
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Recovered",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Recovered")) %>%
    hc_add_series(name = "Countries",data = x$recoveredDaily) 
  
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
            argonRow(
              center = T,
              actionBttn(
                inputId = "runSentiment",
                label = "Run Sentiment Analysis",
                color = "warning",
                block = T
              )
            ),
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

observeEvent(input$runSentiment,{
  req(c(!is.null(input$tweetsOption),!is.null(input$twitterHashtag)))
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
# mapdata <- get_data_from_map(download_map_data("custom/world"))
# # # 
# results$dataframeTotal$countryName[!(results$dataframeTotal$countryName %in% mapdata$name)]
# 
# [1] "Cruise Ship"
# [2] "Holy See