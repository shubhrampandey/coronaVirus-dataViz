##### Global: options #####
Production = F  
options(scipen = 1000, expressions = 10000)
appVersion = "v0.1"
appName = "CORONAVIRUS Data Visualization Platform"
appLongName = "CORONAVIRUS Data Visualization Platform"
lastUpdate = "2020-03-12 09:00:00 IST"

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)
##### User interface #####
ui <- tagList(
  
  ##### CSS and style functions #####
  CSS, #CSS.R
  
  argonDash::argonDashPage(
    title = appLongName,
    header = argonDash::argonDashHeader(
      gradient = T,
      color = NULL,
      top_padding = 2,
      bottom_padding = 0,
      background_img = "coronavirus.jpg",
      height = 100,
      argonRow(
        argonColumn(width = 9,
                    h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:45px;')
                    ),
        argonColumn(
          width = 3,
          argonRow(
            center = T,
            argonColumn(
              width = 12,
              center = T,
              h6(paste0("Last Updated: ",lastUpdate), style = 'color:white;font-size:13px;')
            ),
            argonColumn(
              width = 12,
              center = T,
              h5(HTML("Creator and maintainer: <a href='mailto:shubhram1992@gmail.com'>Shubhram Pandey</a>"), style = 'color:white;text-align:right;font-size:15px;')
            ),
            argonColumn(
              width = 12,
              argonNavMenu(
                side = "right",
                argonRow(
                  center = T,
                  
                  argonColumn(
                    width = 1,
                    argonNavItem(
                      name = "facebook",
                      src = "https://www.facebook.com/Shubhram1992",
                      icon = icon("facebook-square"),
                      tooltip = "Like us on Facebook"
                    )
                  ),
                  argonColumn(
                    width = 1,
                    offset = 1,
                    argonNavItem(
                      name = "linkedin",
                      src = "https://www.linkedin.com/in/shubhrampandey/",
                      icon = icon("linkedin"),
                      tooltip = "Follow us on Linkedin"
                    )
                  ),
                  argonColumn(
                    width = 1,
                    offset = 1,
                    argonNavItem(
                      name = "github",
                      src = "https://github.com/shubhrampandey",
                      icon = icon("github"),
                      tooltip = "Star us on Github"
                    )
                  )
                )
              )
            )
          )
        )
        
                    )
      
      
      ),
    sidebar = NULL,
    body = argonDashBody(
      tags$script( "$(document).on('click', function(event) {
                   Shiny.onInputChange('activeTab', $('.active').data().value);});"),
      tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
      tags$br(),
      dashboardUI
    )
  )
  )

##### server #####
server <- function(input, output, session) {
  
  printLogJs = function(x, ...) {
    logjs(x)
    T
  }
  # addHandler(printLogJs)
  if (!Production) options(shiny.error = recover)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
    # q("no")
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  ##### debug server logic #####
  output$runRCodeOutput = renderPrint({
    req(rcode())
    isolate({
      eval(parse(text = rcode()$text))
    })
  })
  rcode = reactiveVal()
  observeEvent(input$runRCodeButton, {
    req(!Production); rcode(list("text" = input$runRCode, "type" = "runRCode", "rand" = runif(1)))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
}

# Run the application
shinyApp(ui = ui, server = server)