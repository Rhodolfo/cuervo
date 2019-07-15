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
  conditionalPanel(condition = 'output.esta_logeado',
    sidebarMenu(
      id = 'menu',
      menuItem(
        'Log in',tabName =  'entrada'
      )
    )
  ),
  conditionalPanel(condition = 'output.es_administrador',
    sidebarMenu(
      id = 'menu_proceso',
      menuItem(
        'Proceso', tabName = 'proceso'
      )
    )
  ),
  h1('variables'),
  textOutput('variables_boton_login_pre'),
  textOutput('variables_boton_login'),
  textOutput('variables_user_prelog'),
  textOutput('variables_user_logged'),
  textOutput('variables_user_name'),
  textOutput('variables_user_role')
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
    ),
    tabItem(
      'proceso',
      box(
        title = 'Proceso',
        solidHeader = T,
        width = 9,
        status = 'warning',
        sankeyNetworkOutput('grafica_proceso')
      )
    )
  )
)

dashboardPage(header, sidebar, body)