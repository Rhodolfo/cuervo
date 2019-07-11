library(shinydashboard)


header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; padding-right: 15px; color: #66b3ff;"),
          tags$li(class = "dropdown", actionLink("boton_login_pre",textOutput('texto_link')))
          )
)

header$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
                                           tags$img(src='logo_arena.png'))


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu',
    menuItem(
      'Log in',tabName =  'entrada'
    )
  )
)


body <- dashboardBody(
  tags$head(tags$style(HTML('

.skin-blue .main-header .logo {
                              background-color: #66b3ff;
                              }
  .skin-blue .main-header .navbar {
    background-color: #001a33;
  }'))),  
  tabItems(
    tabItem(
      'entrada',
      uiOutput('page')
    )
  )
)

dashboardPage(header, sidebar, body)