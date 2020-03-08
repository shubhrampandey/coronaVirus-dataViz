dashboardUI = tagList(
  argonRow(
    argonColumn(
      width = 12,
      uiOutput("dashboard") %>% withSpinner()
    )
  )
)