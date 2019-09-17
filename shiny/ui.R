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
  conditionalPanel(condition = 'output.es_administrador',
    sidebarMenu(
      id = 'menu_proceso',
      menuItem(
        'Proceso', tabName = 'proceso'
      )
    )
  ),
  conditionalPanel(condition = 'output.es_servicio_cliente_nacional || output.es_administrador',
                   sidebarMenu(
                     id = 'menu_servicio_cliente_nacional',
                     menuItem(
                       'Pedidos', tabName = 'pedidos_nacional'
                     )
                   )
  ),
  conditionalPanel(condition = 'output.es_administrador',
    sidebarMenu(
      id = 'menu_kpi',
      menuItem(
        'KPIs', tabName = 'kpi'
      )
    )
  ),
  conditionalPanel(condition = 'output.es_administrador || output.es_tester',    # visualizador de tablas
    sidebarMenu(
      id = 'menu_visualizador',
      menuItem(
        'Visualizador', tabName = 'visualizador'
      ),
      menuItem(
        'Pedidos Abiertos', tabName = 'pedidos_abiertos'
      )
    )
  ),
  conditionalPanel(condition = 'output.es_administrador',
    sidebarMenu(
      id = 'menu_variables',
      h1('variables'),
      textOutput('variables_boton_login_pre'),
      textOutput('variables_boton_login'),
      textOutput('variables_user_prelog'),
      textOutput('variables_user_logged'),
      textOutput('variables_user_name'),
      textOutput('variables_user_role'),
      textOutput('variables_filtro_region'),
      textOutput('variables_filtro_ano_mes')
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
    ),
    tabItem(
      'kpi',
      box(
        width = 6,
        highchartOutput('grafica_fill_rate')
      ),
      box(
        width = 6,
        leafletOutput('mapa_fill_rate')
      )
    ),
    tabItem(                            # carga de los datos
      'visualizador',
      conditionalPanel('input.boton_siguiente_carga == 0',
        box(
          width = 12,
          title = 'Preparación de los datos',
          actionButton(
            inputId = 'boton_carga',
            label = 'Carga de los datos'
          ),
          textOutput('o_texto_carga_zsdr141'),
          textOutput('o_texto_carga_zsdr159')
        )
      ),
      conditionalPanel('input.boton_carga == 1 & input.boton_siguiente_carga == 0',
                       actionButton(
                         inputId = 'boton_siguiente_carga',
                         label = 'Siguiente'
                       )
      ),
      conditionalPanel('input.boton_siguiente_carga == 1',
        box(
          width = 12,
          title = 'Filtros',
          div(style="display:inline-block",
              pickerInput(width = 100,
                'input_filtro_region',
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
              pickerInput(
                'input_filtro_fecha_original',
                'Fecha Original de Entrega',
                selected = NULL,
                multiple = TRUE,
                choices = c(
                  'ninguno'
                )
              )
            ),
          actionButton(
            inputId = 'boton_filtrar',
            label = 'Filtrar'
          )
        ),
        tabBox(
          title = 'análisis',
          id = 'tabset1',
          width = 12,
          # tabPanel(
          #   'datos_completos',
          #   'datos completos',
          #   formattableOutput('output_tabla_completo')
          # ),
          tabPanel(
            'tiempo de los procesos',
            plotOutput('output_grafica_tiempo1')
          )
        )
      )
    ),
    tabItem(
      'pedidos_abiertos',
      box(
        width = 12,
        title = 'Filtros',
        div(style="display:inline-block",
            pickerInput(width = 100,
                        'input_filtro_region_pa',
                        'Región',
                        selected = NULL,
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
    ),
    tabItem(
      'pedidos_nacional'
    )
  )
)

dashboardPage(header, sidebar, body)