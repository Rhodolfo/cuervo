
library(shinydashboard)

shinyServer(function(input, session, output) {
  
  
  # definición de las variables reactivas ---------------------------------------------------------------------------------------------------------
  
  chat <- reactiveValues(     # variables para el chat
    chat = NULL,
    users = NULL
  )
  
  user <- reactiveValues(    # variables de usuario y status de logeo
    prelog = FALSE,
    logged = FALSE, 
    role = '',
    name = NULL
  ) 
  
  nube <- reactiveValues(                # datos de la nube
    usuarios = nube_cuervo_usuarios
  )
  
  tablas <- reactiveValues(       # tabla
    ano_mes = NULL,
    zsdr141 = NULL,
    total = NULL,
    vis = NULL,
    vis_pa = NULL,
    visualizacion1 = NULL,
    usa = NULL,
    row = NULL,
    domestico = NULL
  )
  
  status <- reactiveValues(      # banderas para supervisar procesos
    carga = FALSE
  )
  
  codigos <- reactiveValues(
    update_filtros = NULL
  )
  
  parametros <- reactiveValues(             # parámetros que se leen desde el excel
    domestico_fechas = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_fechas)) %>%
      dplyr::select(domestico_fechas) %>%
      unlist %>%
      as.character,
    
    usa_fechas = excel_parametros %>% 
      dplyr::filter(!is.na(usa_fechas)) %>%
      dplyr::select(usa_fechas) %>%
      unlist %>%
      as.character,
    
    row_fechas = excel_parametros %>% 
      dplyr::filter(!is.na(row_fechas)) %>%
      dplyr::select(row_fechas) %>%
      unlist %>%
      as.character,
    
    domestico_cantidades = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_cantidades)) %>%
      dplyr::select(domestico_cantidades) %>%
      unlist %>%
      as.character,
    
    usa_cantidades = excel_parametros %>% 
      dplyr::filter(!is.na(usa_cantidades)) %>%
      dplyr::select(usa_cantidades) %>%
      unlist %>%
      as.character,
    
    row_cantidades = excel_parametros %>% 
      dplyr::filter(!is.na(row_cantidades)) %>%
      dplyr::select(row_cantidades) %>%
      unlist %>%
      as.character,
    
    domestico_filtros = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_filtros)) %>%
      dplyr::select(domestico_filtros) %>%
      unlist %>%
      as.character,
    
    usa_filtros = excel_parametros %>% 
      dplyr::filter(!is.na(usa_filtros)) %>%
      dplyr::select(usa_filtros) %>%
      unlist %>%
      as.character,
    
    row_filtros = excel_parametros %>% 
      dplyr::filter(!is.na(row_filtros)) %>%
      dplyr::select(row_filtros) %>%
      unlist %>%
      as.character,
    
    usa_carpeta = excel_parametros %>% 
      dplyr::filter(!is.na(usa_carpeta)) %>%
      dplyr::select(usa_carpeta) %>%
      unlist %>%
      as.character,
    
    row_carpeta = excel_parametros %>% 
      dplyr::filter(!is.na(row_carpeta)) %>%
      dplyr::select(row_carpeta) %>%
      unlist %>%
      as.character,
    
    domestico_carpeta = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_carpeta)) %>%
      dplyr::select(domestico_carpeta) %>%
      unlist %>%
      as.character,
    
    usa_pedido = excel_parametros %>% 
      dplyr::filter(!is.na(usa_pedido)) %>%
      dplyr::select(usa_pedido) %>%
      unlist %>%
      as.character,
    
    row_pedido = excel_parametros %>% 
      dplyr::filter(!is.na(row_pedido)) %>%
      dplyr::select(row_pedido) %>%
      unlist %>%
      as.character,
    
    domestico_pedido = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_pedido)) %>%
      dplyr::select(domestico_pedido) %>%
      unlist %>%
      as.character
    
  )
  
  # prueba parámetros
  # 
  # parametros <- list()
  # 
  # parametros$domestico_fechas = excel_parametros %>%
  #   dplyr::filter(!is.na(domestico_fechas)) %>%
  #   dplyr::select(domestico_fechas) %>%
  #   unlist
  # 
  # parametros$usa_fechas = excel_parametros %>%
  #   dplyr::filter(!is.na(usa_fechas)) %>%
  #   dplyr::select(usa_fechas) %>%
  #   unlist
  # 
  # parametros$row_fechas = excel_parametros %>%
  #   dplyr::filter(!is.na(row_fechas)) %>%
  #   dplyr::select(row_fechas) %>%
  #   unlist
  # 
  # parametros$domestico_cantidades = excel_parametros %>%
  #   dplyr::filter(!is.na(domestico_cantidades)) %>%
  #   dplyr::select(domestico_cantidades) %>%
  #   unlist
  # 
  # parametros$usa_cantidades = excel_parametros %>%
  #   dplyr::filter(!is.na(usa_cantidades)) %>%
  #   dplyr::select(usa_cantidades) %>%
  #   unlist
  # 
  # parametros$row_cantidades = excel_parametros %>%
  #   dplyr::filter(!is.na(row_cantidades)) %>%
  #   dplyr::select(row_cantidades) %>%
  #   unlist
  # 
  # parametros$domestico_filtros = excel_parametros %>%
  #   dplyr::filter(!is.na(domestico_filtros)) %>%
  #   dplyr::select(domestico_filtros) %>%
  #   unlist
  # 
  # parametros$usa_filtros = excel_parametros %>%
  #   dplyr::filter(!is.na(usa_filtros)) %>%
  #   dplyr::select(usa_filtros) %>%
  #   unlist
  # 
  # parametros$row_filtros = excel_parametros %>%
  #   dplyr::filter(!is.na(row_filtros)) %>%
  #   dplyr::select(row_filtros) %>%
  #   unlist
  # 
  # parametros$usa_carpeta = excel_parametros %>%
  #   dplyr::filter(!is.na(usa_carpeta)) %>%
  #   dplyr::select(usa_carpeta) %>%
  #   unlist
  # 
  # parametros$row_carpeta = excel_parametros %>%
  #   dplyr::filter(!is.na(row_carpeta)) %>%
  #   dplyr::select(row_carpeta) %>%
  #   unlist
  # 
  # parametros$domestico_carpeta = excel_parametros %>%
  #   dplyr::filter(!is.na(domestico_carpeta)) %>%
  #   dplyr::select(domestico_carpeta) %>%
  #   unlist
  # 
  # parametros$usa_pedido = excel_parametros %>%
  #   dplyr::filter(!is.na(usa_pedido)) %>%
  #   dplyr::select(usa_pedido) %>%
  #   unlist
  # 
  # parametros$row_pedido = excel_parametros %>%
  #   dplyr::filter(!is.na(row_pedido)) %>%
  #   dplyr::select(row_pedido) %>%
  #   unlist
  # 
  # parametros$domestico_pedido = excel_parametros %>%
  #   dplyr::filter(!is.na(domestico_pedido)) %>%
  #   dplyr::select(domestico_pedido) %>%
  #   unlist

  
  # login ------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  
  ui1 <- function(){   # pantalla de logeo inicial
    tagList(
      div(id = "login",
          wellPanel(textInput("input_usuario", "Usuario"),
                    passwordInput("passwd", "Contraseña"),
                    br(),actionButton("boton_login", "Log in")))
      ,tags$style(type="text/css", "#login {font-size:10px;
                  text-align: left;
                  position:absolute;
                  top: 25%;
                  left: 50%;
                  margin-top: -10px
                  ;margin-left: -150px;}")
      )}
  
  ui2 <- function(){list(tabPanel(user$name,actionButton( # pantalla de log in existoso
    inputId = 'boton_siguiente_login',
    label = 'Siguiente'
  )),tabPanel(user$role))}    

  
  
  observeEvent(input$boton_login_pre,{
    user$logged <- FALSE
    user$role <- ''
    user$name <- ''
  })
  
  observeEvent(input$boton_login,{                           # observador que checa el log in
    if(user$prelog == FALSE){
      if (user$logged == FALSE) {
        if (!is.null(input$boton_login)) {
          if (input$boton_login > 0) {
            username <- isolate(input$input_usuario)
            Password <- isolate(input$passwd)
            Id.username <- which(nube$usuarios$usuario == username)
            Id.password <- which(nube$usuarios$contrasena == Password)
            if (length(Id.username) > 0 & length(Id.password) > 0) {
              if (Id.username == Id.password) {
                user$logged <- TRUE
                user$role = nube$usuarios$rol[Id.username] 
                user$name <- nube$usuarios$nombre[Id.username]
              }
            } 
          }
        }
      }
    }
    
  })
  observe({
    if (user$logged == FALSE) {
      output$page <- renderUI({
          div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (user$logged == TRUE)    {
      output$page <- renderUI({
        box(width = 12,
            div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Login exitoso",ui2())))
        )})
      Sys.sleep(.5)
      cat('\n')
      cat(parametros$domestico_fechas)
    }
  })
  
  
  observeEvent(input$boton_login_pre, {
    updateTabsetPanel(session, "menu",selected = "entrada")
  })
  
  
  output$logged_user <- renderText({
    if(user$logged == TRUE) return(paste0('Sesión iniciada como ', user$name))
    return("")
  })
  
  observeEvent(input$boton_siguiente_login, {
    updateTabsetPanel(session, "menu_preparacion",selected = "preparacion")
  })
  
  # variables de activación de menús ---------------------------------------------------------------------------------------------------------------------------------------------------
  
 
  output$texto_link <- renderText({
    if(user$logged == TRUE)return('log out')
    return('')
  })
  
  output$esta_logeado <- reactive({
    user$role == ''
  })
  outputOptions(output, "esta_logeado", suspendWhenHidden = FALSE)
  
  output$activa_carga <- reactive({
    user$logged & (input$boton_siguiente_carga == 0)
  })
  outputOptions(output, "activa_carga", suspendWhenHidden = FALSE)
  
  output$activa_visualizacion1 <- reactive({
    status$carga & (input$boton_siguiente_carga > 0)
  })
  outputOptions(output, "activa_visualizacion1", suspendWhenHidden = FALSE)
  
  
  # activación de los diferentes tipos de usuario ------------------------------------------------------------------------------------------------------------------------------------------
  
  output$es_administrador <- reactive({
    user$role == 'administrador'
  })
  outputOptions(output, "es_administrador", suspendWhenHidden = FALSE)
  
  output$es_tester <- reactive({
    user$role == 'tester'
  })
  outputOptions(output, "es_tester", suspendWhenHidden = FALSE)
  
  output$es_servicio_cliente_nacional <- reactive({
    user$role == 'servicio_cliente_nacional'
  })
  outputOptions(output, "es_servicio_cliente_nacional", suspendWhenHidden = FALSE)
  
  
  # seguimiento de las inputs ---------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$variables_boton_login_pre <- renderText({
    paste(
      'boton_login_pre',
      input$boton_login_pre
    )
  })
  
  output$variables_boton_login <- renderText({
    paste(
      'boton_login',
      input$boton_login
    )
  })
  
  # seguimiento de las variables reactivas ----------------------------------------------------------------------------------------------------------------------------------------------------
  
  output$variables_user_prelog <- renderText({
    paste(
      'user_prelog',
      user$prelog
    )
  })
  
  output$variables_user_logged <- renderText({
    paste(
      'user_logged',
      user$logged
    )
  })
  
  output$variables_user_role <- renderText({
    paste(
      'user_role',
      user$role
    )
  })
  
  output$variables_user_name <- renderText({
    paste(
      'user_name',
      user$name
    )
  })
  
  output$variables_filtro_region <- renderText({
    paste(
      'filtro_region',
      input$input_filtro_region
    )
  })
  
  output$variables_filtro_ano_mes <- renderText({
    paste(
      'ano_mes',
      input$input_filtro_fecha_original
    )
  })
  
  output$variables_status_carga <- renderText({
    paste(
      'status_carga',
      status$carga
    )
  })
  
  output$variables_filtro_region <- renderText({
    paste(
      'filtro_region',
      input$input_filtro_zona
    )
  })
  output$variables_filtro_fecha_variable <- renderText({
    paste(
      'filtro_fecha',
      input$input_filtro_fecha_variable
    )
  })

  
  
  # carga de los datos -----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # tablas <- list()
  
  observeEvent(input$boton_carga,{
    
    oldw <- getOption("warn")
    options(warn=-1)
    style <- isolate(input$style)
    progress <- shiny::Progress$new(style = style)
    
    
    progress$set(message = "Cargando USA ", value = 0)
    Sys.sleep(1)
    tablas$usa <- funcion_cargar_datos(parametros$usa_carpeta,parametros$usa_fechas,parametros$usa_cantidades,parametros$usa_filtros,parametros$usa_pedido) %>% 
      dplyr::filter(Zona_de_ventas != 'ninguno') %>%
      dplyr::filter(Nombre_Región == 'USA')


    progress$set(message = "Cargando Resto del Mundo ", value = 0.3)
    Sys.sleep(1)
    tablas$row <- funcion_cargar_datos(parametros$row_carpeta,parametros$row_fechas,parametros$row_cantidades,parametros$row_filtros,parametros$row_pedido) %>%
      dplyr::filter(Zona_de_ventas != 'ninguno') %>%
      dplyr::filter(Nombre_Región != 'USA')


    progress$set(message = "Doméstico ", value = 0.7)
    Sys.sleep(1)
    tablas$domestico <- funcion_cargar_datos(parametros$domestico_carpeta,parametros$domestico_fechas,parametros$domestico_cantidades,parametros$domestico_filtros,parametros$domestico_pedido)


    progress$set(message = "Carga finalizada ", value = 1)
    tablas$ano_mes <- base::intersect(unique(tablas$zsdr141$ano_mes),unique(tablas$zsdr159$ano_mes))
    Sys.sleep(1)
    progress$close()
    options(warn = oldw)
    
    status$carga <- TRUE
    
  })
  
  output$o_texto_carga_usa <- renderText({
    validate(need(tablas$usa,'nop'))
    'Carga de las transacciones usa finalizada exitosamente'
  })
  
  output$o_texto_carga_row <- renderText({
    validate(need(tablas$row,'nop'))
    'Carga de las transacciones row finalizada exitosamente'
  })
  
  output$o_texto_carga_domestico <- renderText({
    validate(need(tablas$domestico,'nop'))
    'Carga de las transacciones doméstico finalizada exitosamente'
  })
  
  observeEvent(input$boton_siguiente_carga, {
    updateTabsetPanel(session, "menu_visualizacion1",selected = "visualizacion1")
  })
  
  
  # visualizacion1 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#   ui_filtros_visualizacion1 <- function(){                   # función interfaz con los filtros dinámicos (toma los filtros de la tabla de excel para cada uno de los 3 canales)
#     f_region <- 'usa'
#     if(input$input_filtro_zona == 'USA')f_region <- 'usa'
#     if(input$input_filtro_zona == 'Resto del mundo')f_region <- 'row'
#     if(input$input_filtro_zona == 'Doméstico')f_region <- 'domestico'
#     
#     
#     cat('\n')
#     cat(f_region)
#     cat('\n')
#     
#     eval(parse(text = paste0('parametros$visualizacion1_filtros <- parametros$',f_region,'_filtros')))
#     
#     cat('\n')
#     cat(parametros$visualizacion1_filtros)
#     cat('\n')
#     
#     eval(parse(text = paste0('tablas$visualizacion1 <- tablas$',f_region)))
#     f_lista_opciones <- list()
#     f_t <-  paste0('f_lista_opciones[[',1:length(parametros$visualizacion1_filtros),']] <- unique(tablas$visualizacion1$',parametros$visualizacion1_filtros,')')
#     eval(parse(text = paste0(f_t,collapse = ';')))
#      
#     for(i in 1:length(f_lista_opciones)){
#       f_lista_opciones[[i]] <- f_lista_opciones[[i]][!is.na(f_lista_opciones[[1]])]
#     }
#     
#     f_vector_opciones <- NULL
#     for(i in 1:length(f_lista_opciones)){
#       f_lista_opciones[[i]] <- paste0('"',paste0(f_lista_opciones[[i]],collapse = '","'),'"')
#       f_vector_opciones <- c(f_vector_opciones,f_lista_opciones[[i]])
#     }
#     funcion1 <- 'tagList('
#     funcion2 <- paste0(
#                     'pickerInput(width = 150,
#                           "input_filtro_',1:length(parametros$visualizacion1_filtros),'",
#                           "',parametros$visualizacion1_filtros,'",
#                           selected = c(',f_vector_opciones,'),
#                           options = list(`actions-box` = TRUE),
#                           multiple = TRUE,
#                           choices = c(',f_vector_opciones,'))'
#                 , collapse = ',\n')
#     funcion2 <- paste0(funcion2,collapse = ',')   
#     funcion3 <- ')'
#     resultado <- eval(parse(text = paste0(
#       funcion1,
#       funcion2,
#       funcion3
#     )))
#     return(resultado)
#   }
#   
#   observe({                                                   # decido cuál de las interfaces se muestra, resultó ser un paso trivial pero lo dejo como referencia por si es necesario usar una no trivial
#       output$ui_filtros_visualizacion1 <- renderUI({
#         div(class="outer",do.call(bootstrapPage,c("",ui_filtros_visualizacion1())))
#       })
#   })
#   
#   observeEvent(input$input_filtro_1,{   # actualización dinámica de los filtros (hardcodedish porque las variables tienen siempre esta estructura de naming), soporta 3 variables por el momento y
#     eval(parse(text = paste(            # anida siguiendo la jerarquización:  filtro1 > filtro2 > filtro3
#       "a <- 'ninguno'
#       if(!is.null(parametros$visualizacion1_filtros)){
#       f_tabla <- tablas$visualizacion1 %>% dplyr::filter(",parametros$visualizacion1_filtros[1]," %in% input$input_filtro_1) %>% dplyr::select(",parametros$visualizacion1_filtros[2],")
#       a <- unique(as.character(f_tabla$",parametros$visualizacion1_filtros[2],"))
#       }
#       updatePickerInput(session = session, inputId = 'input_filtro_2', choices = a,selected = a)"
#     )))
# })
#   observeEvent(input$input_filtro_2,{                       # actualización dinámica de los filtros (hardcodedish porque las variables tienen siempre esta estructura de naming), soporta 3 variables por el momento y
#     if(length(parametros$visualizacion1_filtros) >= 3){     # anida siguiendo la jerarquización:  filtro1 > filtro2 > filtro3
#       eval(parse(text = paste(
#         "a <- 'ninguno'
#         if(!is.null(parametros$visualizacion1_filtros)){
#         f_tabla <- tablas$visualizacion1 %>% dplyr::filter(",parametros$visualizacion1_filtros[1]," %in% input$input_filtro_1);
#         f_tabla <- f_tabla %>% dplyr::filter(",parametros$visualizacion1_filtros[2]," %in% input$input_filtro_2) %>% dplyr::select(",parametros$visualizacion1_filtros[3],")
#         a <- unique(as.character(f_tabla$",parametros$visualizacion1_filtros[3],"))
#         }
#         updatePickerInput(session = session, inputId = 'input_filtro_3', choices = a, selected = a)"
#       )))
#     }
#   })
  
  
  observeEvent(input$input_filtro_zona,{                                                # actualización del filtro de fecha_variable dependiendo de la zona
    a <-excel_parametros$usa_fechas[!is.na(excel_parametros$usa_fechas)]
    b <- excel_parametros$usa_fechas[!is.na(excel_parametros$usa_fechas)][1]
    if(input$input_filtro_zona == 'Resto del mundo'){
      a <- parametros$row_fechas[!is.na(parametros$row_fechas)]
      b <- parametros$row_fechas[!is.na(parametros$row_fechas)][1]
    }
    if(input$input_filtro_zona == 'USA'){
      a <-excel_parametros$usa_fechas[!is.na(excel_parametros$usa_fechas)]
      b <- excel_parametros$usa_fechas[!is.na(excel_parametros$usa_fechas)][1]
    }
    if(input$input_filtro_zona == 'Doméstico'){
      a <- excel_parametros$domestico_fechas[!is.na(excel_parametros$domestico_fechas)]
      b <- excel_parametros$domestico_fechas[!is.na(excel_parametros$domestico_fechas)][1]
    }
    updatePickerInput(
      session,'filtro_fecha_variable', choices = a, selected = b
    )
  })
  
  observeEvent(input$input_filtro_fecha_variable,{                                                # actualización del filtro de fecha_rango dependiendo de fecha_variable
  
    eval(parse(paste0(
      'f_fecha <- tablas$visualizacion1$',input$filtro_fecha_variable
    )))
    
    f_start = min(f_fecha, na.rm = T)
    f_end = max(f_fecha, na.rm = T)
    f_min = max(f_fecha, na.rm = T) - 30
    f_max = max(f_fecha, na.rm = T)

    updatePickerInput(
      session,'filtro_fecha_rango', start = f_start, end = f_end, min = f_min, max = f_max
    )
  })
  
  
  
  
  output$output_grafica_tiempo1 <- renderPlot({
    
    
    g <- NULL
 
    
    
    
    
    p_compresion <- FALSE
    p_fecha_focal <- 'fecha_pedido'
    if(input$input_filtro_region == 'Doméstico'){
      p_fecha_focal = 'fecha_creacion_min'
      p_compresion <- TRUE
    }
    
    
    
    
    
    oldw <- getOption("warn")
    options(warn=-1)
    
    g <- tryCatch(funcion_grafica_tiempos_grande(tablas$vis, p_fecha_focal, p_compresion, 'días', 'tablas (con diferentes grados de información',3),error = function(e){return(NULL)})
    
    options(warn = oldw)
    
    validate(need(!is.null(g),'una vez seleccionados los filtros pulsa filtrar para ver la gráfica'))
    
    
    
    g
  })
  
  

  
  
})
    

