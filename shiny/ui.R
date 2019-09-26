library(shinydashboard)


header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; padding-right: 15px; color: #66b3ff;"),
          tags$li(class = "dropdown", actionLink("boton_login_pre",textOutput('texto_link')))
          )
)

header$children[[2]]$children <-  tags$a(href='wiii',
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
  
  # carga de los datos -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  conditionalPanel(condition = 'output.activa_carga',    
    sidebarMenu(
      id = 'menu_preparacion',
      menuItem(
        'Preparación de los datos', tabName = 'preparacion'
      )
    )
  ),
  
  
  conditionalPanel(condition = 'output.activa_visualizacion1',    
                   sidebarMenu(
                     id = 'menu_visualizacion1',
                     menuItem(
                       'Visualización 1', tabName = 'visualizacion1'
                     )
                   )
  ),
  
  # filtros -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  conditionalPanel(condition = 'output.activa_visualizacion1',
    sidebarMenu(
      id = 'menu_filtros_1',
      pickerInput(                  # filtro región
        'input_filtro_zona',
        'Región',
        selected = NULL,
        multiple = FALSE,
        choices = c(
          'USA',
          'Doméstico',
          'Resto del mundo'
        )
      ),
      pickerInput(                   # filtro 1
        'input_filtro1',
        'filtro1',
        selected = 'ninguna',
        choices = 'ninguna',
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      conditionalPanel('output.activa_filtro2',
                       pickerInput(                   # filtro 2
                         'input_filtro2',
                         'filtro2',
                         selected = 'ninguna',
                         choices = 'ninguna',
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE)
                       )
      ),
      pickerInput(                                    # filtro fecha variable
        'filtro_fecha_variable',
        'Fecha para filtrar',
        choices = excel_parametros$usa_fechas[!is.na(excel_parametros$usa_fechas)],
        selected = excel_parametros$usa_fechas[!is.na(excel_parametros$usa_fechas)][1],
        multiple = FALSE
      ),
      dateRangeInput(                     # filtro fecha rango
        'filtro_fecha_rango',
        'Rango de fechas',
        start = '2019-01-01',
        end = '2019-12-31',
        min = '2019-01-01',
        max = '2019-12-31'
      )
    )
  ),
  
  # (sidebar) botón de filtro 1 ------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  conditionalPanel(condition = 'output.activa_visualizacion1',
                   sidebarMenu(
                     id = 'menu_boton_1',
                     actionButton(
                       inputId = 'boton_filtrar1',
                       label = 'Filtrar'
                     )
                   )
  ),
  
  # (sidebar) seguimiento de variables -----------------------------------------------------------------------------------------------------------------------------------------------------
  
  conditionalPanel(condition = 'output.es_administrador & output.',
    sidebarMenu(
      id = 'menu_variables',
      h1('variables'),
      textOutput('variables_boton_login_pre'),
      textOutput('variables_boton_login'),
      textOutput('variables_user_prelog'),
      textOutput('variables_user_logged'),
      textOutput('variables_user_name'),
      textOutput('variables_user_role'),
      textOutput('variables_status_carga'),
      textOutput('variables_filtro_region'),
      textOutput('variables_input_filtro1'),
      textOutput('variables_input_filtro2'),
      textOutput('variables_filtro_fecha_variable'),
      textOutput('variables_filtro_fecha_rango')
    )
  )
)


# body ------------------------------------------------------------------------------------------------------------------------------------------------


body <- dashboardBody(
  
  tags$script(HTML(                 # función para definir openTab
        "                                             
        var openTab = function(tabName){
                   $('a', $('.sidebar')).each(function() {
                   if(this.getAttribute('data-value') == tabName) {
                   this.click()
                   };
                   });
                   }
                   ")),
  
  tags$head(tags$style(HTML('
.skin-blue .main-header .logo {background-color: #66b3ff;}.skin-blue .main-header .navbar {background-color: #001a33;}'))),  
  tabItems(
    
    
  # (body) login -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    tabItem(                        # login
      'entrada',
      uiOutput('page')
    ),
  
  # (body) carga de los datos ------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    tabItem(                        # carga de los datos
      'preparacion',
        box(
          width = 12,
          title = 'Preparación de los datos',
          actionButton(
            inputId = 'boton_carga',
            label = 'Carga de los datos'
          ),
          textOutput('o_texto_carga_usa'),
          textOutput('o_texto_carga_row'),
          textOutput('o_texto_carga_domestico')
        ),
        actionButton(
          inputId = 'boton_siguiente_carga',
          label = 'Siguiente'
        )
    ),
    
  # (body) visualización 1  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    tabItem(                   
      'visualizacion1',
      tabBox(
        title = 'análisis',
        id = 'tabset1',
        width = 12,
        height = 800,
        tabPanel(
          'tiempo de los procesos',
          plotOutput('output_grafica_tiempo1',height = 700)
        )
      )
    )
  )
)

dashboardPage(header, sidebar, body)