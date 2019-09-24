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
  
  # visualización 1 sidebar -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  conditionalPanel(condition = 'output.activa_visualizacion1',    
                   sidebarMenu(
                     id = 'menu_visualizacion1',
                     menuItem(
                       'Visualización 1', tabName = 'visualizacion1'
                     )
                     # uiOutput('ui_filtros_fecha_variable_visualizacion1'),
                     # uiOutput('ui_filtros_fecha_rango_visualizacion1'),
                   )
  ),
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
      # uiOutput('ui_filtros_visualizacion1'),
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
  conditionalPanel(condition = 'output.activa_visualizacion1',
                   sidebarMenu(
                     id = 'menu_boton_1',
                     actionButton(
                       inputId = 'boton_filtrar',
                       label = 'Filtrar'
                     )
                   )
  ),
  
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
      textOutput('variables_filtro_fecha_variable')
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
    tabItem(                        # login
      'entrada',
      uiOutput('page')
    ),
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
        tabPanel(
          'tiempo de los procesos',
          plotOutput('output_grafica_tiempo1',height = 600)
        )
      )
    ),
    tabItem(                    # visualizacion 2
      'pedidos_abiertos',
      box(
        width = 12,
        title = 'Filtros',
        div(style="display:inline-block",
            pickerInput(width = 100,
                        'input_filtro_region_pa',
                        'Región',
                        selected = 'USA',
                        multiple = FALSE,
                        choices = c(
                          'USA',
                          'Doméstico',
                          'Resto del mundo'
                        )
            )
        ),
        div(style="display:inline-block",
              pickerInput(width = 200,
                          'input_fecha_final_pa_141',
                          'Fecha de evaluación',
                          selected = NULL,
                          multiple = FALSE,
                          choices = c(
                            'ninguno'
                          )
              )
        ),
        div(style="display:inline-block",
         dateInput(
            'input_filtro_fecha_1',
           'fecha_inicial',
           value = Sys.Date() -7
          )
        ),
        div(style="display:inline-block",
          dateInput(
            'input_filtro_fecha_1',
            'fecha_final',
            value = Sys.Date()
          )
        ),
        
        actionButton(
          inputId = 'boton_filtrar_pa',
          label = 'Filtrar'
        )
      ),
      tabBox(
        title = 'pedidos abiertos',
        id = 'tabset2',
        width = 12,
        tabPanel(
          'status_pedidos',
          'status',
          plotOutput('output$output_grafica_pa_total')
        )
      )
    )
  )
)

dashboardPage(header, sidebar, body)