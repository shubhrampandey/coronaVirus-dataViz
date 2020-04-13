##### Global: options #####
Production = T 
options(scipen = 1000, expressions = 10000)
appVersion = "v2.0"
appName = "3AI COVID-19 Data Monitor"
appLongName = "3AI COVID-19 Data Monitor"
lastUpdate = "2020-04-13"

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
source("appFiles/dataLoad.R",local = TRUE)
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
        argonColumn(width = 11,
                    h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:40px;')
                    ),
        argonColumn(
          width = 1,
          aiLogoL
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
          top = 60,
          right = 8
          
        )
        # fixedPanel(
        #   div(
        #     div(
        #       id = "logo",
        #       tags$img(src = 'covid.jpg',width = "90%",height = "45px")
        #     ),
        #     bsPopover("logo", title = NULL, content = "3AI", placement = "left", trigger = "hover",
        #               options = NULL),
        #     onclick = "window.open('https://www.3ai.in', '_blank')"
        #   ),
        #   top = 10,
        #   right = 8
        #   
        # )
      )
      ),
    sidebar = NULL,
    body = argonDashBody(
      tags$head(tags$meta(name = "viewport", content = "width=1400"),uiOutput("body")),
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
  if (!Production) options(shiny.error = recover)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  # Hide the loading message when the rest of the server function has executed
  waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)