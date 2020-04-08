##### Global: options #####
Production = T 
options(scipen = 1000, expressions = 10000)
appVersion = "v2.0"
appName = "COVID-19 Data Visualization Platform"
appLongName = "COVID-19 Data Visualization Platform"
lastUpdate = "2020-04-08"

loader <- tagList(
  waiter::spin_loaders(42),
  br(),
  h3("Loading data")
)

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)
##### User interface #####
ui <- tagList( # dependencies
  use_waiter(),
  useSweetAlert(),
  useShinyjs(),
  extendShinyjs(text = jsToggleFS),
  waiter::waiter_show_on_load(loader, color = "#000"),
# shows before anything else
  ##### CSS and style functions #####
  CSS, #CSS.R
  # Loading message
  argonDash::argonDashPage(
    title = appLongName,
    header = argonDash::argonDashHeader(
      gradient = T,
      color = NULL,
      top_padding = 2,
      bottom_padding = 0,
      background_img = "coronavirus.jpg",
      height = 70,
      argonRow(
        argonColumn(width = 8,
                    h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:40px;')
                    ),
        argonColumn(
          width = 4,
          h6(HTML(paste0("Creator & Maintainer: <a href='https://www.shubhrampandey.com' target = '_blank'>Shubhram Pandey</a>")), style = 'color:white;
                                  text-align: right;
                                  font-size:15px;
                                  margin-bottom: 0em'),
          h6(HTML(paste0("<a href='https://www.3ai.in' target = '_blank'> - 3AI Ambassdor</a>")), style = 'color:white;text-align: right;font-size:15px;')
        ),
        fixedPanel(
          div(
            actionBttn("fullScreen",
                       style = "material-circle",
                       icon = icon("arrows-alt"),
                       size = "xs",
                       color = "warning"),
            bsPopover("fullScreen", title = NULL, content = "Click to view in full screen", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "shinyjs.toggleFullScreen();"
          ),
          top = 55,
          right = 10
          
        ),
        fixedPanel(
          div(
            actionBttn("kofi",
                       style = "material-circle",
                       icon = icon("coffee"),
                       size = "xs",
                       color = "success"),
            bsPopover("kofi", title = NULL, content = "Buy me a coffee", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://ko-fi.com/shubhrampandey', '_blank')"
          ),
          top = 55,
          right = 40
          
        ),
        fixedPanel(
          div(
            actionBttn("userGuide",
                       style = "material-circle",
                       icon = icon("info"),
                       size = "xs",
                       color = "royal"),
            bsPopover("userGuide", title = NULL, content = "Go to app help page", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://sites.google.com/view/covid-19-userguide/home', '_blank')"
          ),
          top = 55,
          right = 70
          
        ),
        fixedPanel(
          div(
            actionBttn("webSite",
                       style = "material-circle",
                       icon = icon("address-card"),
                       size = "xs",
                       color = "primary"),
            bsPopover("webSite", title = NULL, content = "About developer", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://www.shubhrampandey.com', '_blank')"
          ),
          top = 55,
          right = 100
          
        )
                    )
      
      
      ),
    sidebar = NULL,
    body = argonDashBody(
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
  # Hide the loading message when the rest of the server function has executed
  waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)