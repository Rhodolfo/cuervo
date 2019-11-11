
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
    domestico = NULL,
    sub = NULL
  )
  
  status <- reactiveValues(      # banderas para supervisar procesos
    carga = FALSE
  )
  
  codigos <- reactiveValues(
    update_filtros = NULL
  )
  
  parametros <- reactiveValues(             # parámetros que se leen desde el excel
  
    
    fecha_actual = as.Date('2019-08-17'),   # fecha contra la que se calcula el fill rate
    
    status_morado = -1,  # parametros del semaforo
    status_rojo = 0,
    status_amarillo = 2,
    
    domestico_procesos_tabla = excel_domestico_procesos_tabla,        # tablas de procesos
    domestico_procesos_incluir = excel_domestico_procesos_incluir,
    
    usa_procesos_tabla = excel_usa_procesos_tabla,
    usa_procesos_incluir = excel_usa_procesos_incluir,
    
    row_procesos_tabla = excel_row_procesos_tabla,
    row_procesos_incluir = excel_row_procesos_incluir,
    
    domestico_fechas = excel_parametros %>%        #fechas
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
    
    domestico_cantidades = excel_parametros %>%     # cantidades
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
    
    domestico_filtros = excel_parametros %>%         # filtros
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
    
    usa_carpeta = excel_parametros %>%             # carpeta
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
    
    usa_pedido = excel_parametros %>%                # pedido
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
      as.character,
    
    usa_fecha_inicio = excel_parametros %>%         # fecha inicio
      dplyr::filter(!is.na(usa_fechas)) %>%
      dplyr::select(usa_fechas) %>%
      unlist %>%
      as.character %>%
      head(.,n = 1),
    
    row_fecha_inicio = excel_parametros %>% 
      dplyr::filter(!is.na(row_fechas)) %>%
      dplyr::select(row_fechas) %>%
      unlist %>%
      as.character %>%
      head(.,n = 1),
    
    
    domestico_fecha_inicio = excel_parametros %>% 
      dplyr::filter(!is.na(row_fechas)) %>%
      dplyr::select(row_fechas) %>%
      unlist %>%
      as.character %>%
      head(.,n = 1),
    
    usa_fecha_fin = excel_parametros %>%     # fecha fin
      dplyr::filter(!is.na(usa_fin)) %>%
      dplyr::select(usa_fin) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    row_fecha_fin = excel_parametros %>% 
      dplyr::filter(!is.na(row_fin)) %>%
      dplyr::select(row_fin) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    domestico_fecha_fin = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_fin)) %>%
      dplyr::select(domestico_fin) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    usa_cantidad_inicio = excel_parametros %>%         # cantidades inicio
      dplyr::filter(!is.na(usa_cantidades)) %>%
      dplyr::select(usa_cantidades) %>%
      unlist %>%
      as.character %>%
      head(.,n = 1),
    
    row_cantidades_inicio = excel_parametros %>% 
      dplyr::filter(!is.na(row_cantidades)) %>%
      dplyr::select(row_cantidades) %>%
      unlist %>%
      as.character %>%
      head(.,n = 1),
    
    domestico_cantidades_inicio = excel_parametros %>% 
      dplyr::filter(!is.na(row_cantidades)) %>%
      dplyr::select(row_cantidades) %>%
      unlist %>%
      as.character %>%
      head(.,n = 1),
    
    usa_cantidades_cerrado = excel_parametros %>%     # cantidades cerrado
      dplyr::filter(!is.na(usa_cantidades)) %>%
      dplyr::select(usa_cantidades) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    row_cantidades_cerrado = excel_parametros %>% 
      dplyr::filter(!is.na(row_cantidades)) %>%
      dplyr::select(row_cantidades) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    domestico_cantidades_cerrado = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_cantidades)) %>%
      dplyr::select(domestico_cantidades) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    usa_fechas_benchmark = excel_parametros %>%                # fechas benchmark
      dplyr::filter(!is.na(usa_fechas_benchmark)) %>%
      dplyr::select(usa_fechas_benchmark) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),

    row_fechas_benchmark = excel_parametros %>%
      dplyr::filter(!is.na(row_fechas_benchmark)) %>%
      dplyr::select(row_fechas_benchmark) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),

    domestico_fechas_benchmark = excel_parametros %>%
      dplyr::filter(!is.na(domestico_fechas_benchmark)) %>%
      dplyr::select(domestico_fechas_benchmark) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    usa_cantidad_benchmark = excel_parametros %>%                    # cantidad benchmarck
      dplyr::filter(!is.na(usa_cantidad_benchmark)) %>%
      dplyr::select(usa_cantidad_benchmark) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    row_cantidad_benchmark = excel_parametros %>%
      dplyr::filter(!is.na(row_cantidad_benchmark)) %>%
      dplyr::select(row_cantidad_benchmark) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1),
    
    domestico_cantidad_benchmark = excel_parametros %>%
      dplyr::filter(!is.na(domestico_cantidad_benchmark)) %>%
      dplyr::select(domestico_cantidad_benchmark) %>%
      unlist %>%
      as.character %>%
      tail(.,n = 1), 

    domestico_aux_carpetas = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_aux_carpetas)) %>%
      dplyr::select(domestico_aux_carpetas) %>%
      unlist %>%
      as.character,
    domestico_aux_x = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_aux_x)) %>%
      dplyr::select(domestico_aux_x) %>%
      unlist %>%
      as.character,
    domestico_aux_y = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_aux_y)) %>%
      dplyr::select(domestico_aux_y) %>%
      unlist %>%
      as.character,
    row_aux_carpetas = excel_parametros %>% 
      dplyr::filter(!is.na(row_aux_carpetas)) %>%
      dplyr::select(row_aux_carpetas) %>%
      unlist %>%
      as.character,
    row_aux_x = excel_parametros %>% 
      dplyr::filter(!is.na(row_aux_x)) %>%
      dplyr::select(row_aux_x) %>%
      unlist %>%
      as.character,
    row_aux_y = excel_parametros %>% 
      dplyr::filter(!is.na(row_aux_y)) %>%
      dplyr::select(row_aux_y) %>%
      unlist %>%
      as.character,
    usa_aux_carpetas = excel_parametros %>% 
      dplyr::filter(!is.na(usa_aux_carpetas)) %>%
      dplyr::select(usa_aux_carpetas) %>%
      unlist %>%
      as.character,
    usa_aux_x = excel_parametros %>% 
      dplyr::filter(!is.na(usa_aux_x)) %>%
      dplyr::select(usa_aux_x) %>%
      unlist %>%
      as.character,
    usa_aux_y = excel_parametros %>% 
      dplyr::filter(!is.na(usa_aux_y)) %>%
      dplyr::select(usa_aux_y) %>%
      unlist %>%
      as.character

    
  )
  

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
  
  output$datos_ok <- reactive({
    status$carga & (input$boton_siguiente_carga > 0) 
  })
  outputOptions(output, "datos_ok", suspendWhenHidden = FALSE)
  
  output$activa_vista_ejecutiva <- reactive({
    status$carga & (input$boton_siguiente_carga > 0)
  })
  outputOptions(output, "activa_vista_ejecutiva", suspendWhenHidden = FALSE)
  
  
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
  
  # output$variables_boton_login_pre <- renderText({
  #   paste(
  #     'boton_login_pre',
  #     input$boton_login_pre
  #   )
  # })
  # 
  # output$variables_boton_login <- renderText({
  #   paste(
  #     'boton_login',
  #     input$boton_login
  #   )
  # })
  
  # seguimiento de las variables reactivas ----------------------------------------------------------------------------------------------------------------------------------------------------
  
  # output$variables_user_prelog <- renderText({
  #   paste(
  #     'user_prelog',
  #     user$prelog
  #   )
  # })
  # output$variables_user_logged <- renderText({
  #   paste(
  #     'user_logged',
  #     user$logged
  #   )
  # })
  # output$variables_user_role <- renderText({
  #   paste(
  #     'user_role',
  #     user$role
  #   )
  # })
  # output$variables_user_name <- renderText({
  #   paste(
  #     'user_name',
  #     user$name
  #   )
  # })
  # output$variables_filtro_region <- renderText({
  #   paste(
  #     'filtro_region',
  #     input$input_filtro_region
  #   )
  # })
  # output$variables_filtro_ano_mes <- renderText({
  #   paste(
  #     'ano_mes',
  #     input$input_filtro_fecha_original
  #   )
  # })
  # output$variables_status_carga <- renderText({
  #   paste(
  #     'status_carga',
  #     status$carga
  #   )
  # })
  # output$variables_filtro_region <- renderText({
  #   paste(
  #     'region',
  #     input$input_filtro_zona
  #   )
  # })
  # output$variables_input_filtro1 <- renderText({
  #   f_var <- paste0(input$input_filtro1,collapse = ',')
  #   paste(
  #     'filtro1',f_var
  #   )
  # })
  # output$variables_input_filtro2 <- renderText({
  #   f_var <- paste0(input$input_filtro2,collapse = ',')
  #   paste(
  #     'filtro2',f_var
  #   )
  # })
  # output$variables_filtro_fecha_variable <- renderText({
  #   f_var <- paste0(input$filtro_fecha_variable,collapse = ',')
  #   paste(
  #     'fecha_variable',f_var
  #   )
  # })
  # output$variables_filtro_fecha_rango <- renderText({
  #   f_var <- paste0(input$filtro_fecha_rango,collapse = ',')
  #   paste(
  #     'fecha_rango',f_var
  #   )
  # })
  

  
  
  # carga de los datos -----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # tablas <- list()
  
  observeEvent(input$boton_carga,{
    
    oldw <- getOption("warn")
    options(warn=-1)
    style <- isolate(input$style)
    progress <- shiny::Progress$new(style = style)
    
    
    progress$set(message = "Cargando USA ", value = 0)  # carga usa
    Sys.sleep(1)
    tablas$usa <- funcion_cargar_datos(parametros$usa_carpeta,parametros$usa_fechas,parametros$usa_cantidades,parametros$usa_filtros,parametros$usa_pedido, parametros$usa_fechas_benchmark,p_aux_carpetas=parametros$usa_aux_carpetas,p_aux_x=parametros$usa_aux_x,p_aux_y=parametros$usa_aux_y) %>% 
      dplyr::filter(Zona_de_ventas != 'ninguno') %>%
      dplyr::filter(Nombre_Región == 'USA')


    progress$set(message = "Cargando Resto del Mundo ", value = 0.3)   # carga resto del mundo
    Sys.sleep(1)
    tablas$row <- funcion_cargar_datos(parametros$row_carpeta,parametros$row_fechas,parametros$row_cantidades,parametros$row_filtros,parametros$row_pedido, parametros$row_fechas_benchmark, p_aux_carpetas=parametros$row_aux_carpetas,p_aux_x=parametros$row_aux_x,p_aux_y=parametros$row_aux_y) %>%
      dplyr::filter(Zona_de_ventas != 'ninguno') %>%
      dplyr::filter(Nombre_Región != 'USA')


    progress$set(message = "Cargando Doméstico ", value = 0.7)   # carga domestico
    Sys.sleep(1)
    tablas$domestico <- funcion_cargar_datos(parametros$domestico_carpeta,parametros$domestico_fechas,parametros$domestico_cantidades,parametros$domestico_filtros,parametros$domestico_pedido, parametros$domestico_fechas_benchmark,parametros$domestico_procesos_incluir,parametros$domestico_procesos_tabla,p_aux_carpetas=parametros$domestico_aux_carpetas,p_aux_x=parametros$domestico_aux_x,p_aux_y=parametros$domestico_aux_y)
    
      
    if(str_detect(excel_parametros$domestico_benchmark_formula,'formula')){   # viendo el pedo de una variable custom
      parametros$domestico_fechas_benchmark <- 'fecha_dom_bench_custom'
      eval(parse(text = paste0(
        'tablas$domestico <- tablas$domestico %>%
        mutate(
          fecha_dom_bench_custom = ',parametros$domestico_fechas[1],' + 5
        )'
      )))
    }

    progress$set(message = "Carga finalizada ", value = 1)
    Sys.sleep(1)
    progress$close()
    options(warn = oldw)
    
    status$carga <- TRUE
    
    eval(parse(text = paste0('f_opciones_filtro1 <- as.character(unique(tablas$usa$',parametros$usa_filtros[1],'))')))  # inicializo el filtro1
    updatePickerInput(
      session,
      inputId = 'input_filtro1',
      label = 'usa',
      choices = f_opciones_filtro1,
      selected = f_opciones_filtro1
    )
    
  })
  
  output$o_texto_carga_usa <- renderText({         # textos de terminacion
    validate(need(tablas$usa,'Carga de datos USA'))
    'Carga de las transacciones usa finalizada exitosamente'
  })
  output$o_texto_carga_row <- renderText({
    validate(need(tablas$row,'Carga de datos RoW'))
    'Carga de las transacciones row finalizada exitosamente'
  })
  output$o_texto_carga_domestico <- renderText({
    validate(need(tablas$domestico,'Carga de datos domésticos'))
    'Carga de las transacciones doméstico finalizada exitosamente'
  })
  observeEvent(input$boton_siguiente_carga, {
    updateTabsetPanel(session, "menu_visualizacion",selected = "vista_ejecutiva")
  })
  
  
  # filtros --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
  observeEvent(input$input_filtro_zona,{                # actualización dinámica de filtro 1
    f_region <- funcion_asigna_region(input$input_filtro_zona)
    f_variable <- eval(parse(text = paste0('parametros$',f_region,'_filtros[1]')))
    f_choices <- eval(parse(text = paste0('as.character(unique(tablas$',f_region,'$',f_variable,'))')))
    updatePickerInput(            
      session,
      inputId = 'input_filtro1',
      label = f_variable,
      choices = f_choices[order(f_choices)],
      selected = f_choices[order(f_choices)]
    )
  })

  output$activa_filtro2 <- reactive({               # condicionante para que aparezca el filtro 2
    f_region_parametros <- funcion_asigna_region_variables(input$input_filtro_zona, parametros, input)
    length(f_region_parametros$filtros) >= 2
  })
  outputOptions(output, "activa_filtro2", suspendWhenHidden = FALSE)

  observeEvent(input$input_filtro1,{                # actualización dinámica de filtro 2
    if(input$input_filtro_zona == 'USA')f_region <- 'usa'
    if(input$input_filtro_zona == 'Resto del mundo')f_region <- 'row'
    if(input$input_filtro_zona == 'Doméstico')f_region <- 'domestico'
    eval(parse(text = paste0('a <- (length(parametros$',f_region,'_filtros) >= 2)')))
    if(a){
      f_variable_1 <- eval(parse(text = paste0('parametros$',f_region,'_filtros[1]')))
      f_filtro1 <- paste0('c("',paste0(input$input_filtro1,collapse = '","'),'")')
      f_variable_2 <- eval(parse(text = paste0('parametros$',f_region,'_filtros[2]')))
      f_tabla <- eval(parse(text=paste0('tablas$',f_region,' %>% dplyr::filter(',f_variable_1,' %in% ',f_filtro1,')')))
      f_choices <- eval(parse(text = paste0('as.character(unique(f_tabla$',f_variable_2,'))')))
      updatePickerInput(            
        session,
        inputId = 'input_filtro2',
        label = f_variable_2,
        choices = f_choices[order(f_choices)],
        selected = f_choices[order(f_choices)]
      )
    }
  })

  observeEvent(input$input_filtro_zona,{                                       # actualización del filtro de fecha_variable dependiendo de la zona
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

  observeEvent(input$input_filtro_fecha_variable,{                           # actualización del filtro de fecha_rango dependiendo de
    f_fecha <- Sys.Date()
    f_start = '2019-08-01'
    f_end = '2019-08-31'
    f_min = f_fecha -30
    f_max = f_fecha
    updatePickerInput(
      session,'filtro_fecha_rango', start = f_start, end = f_end, min = f_min, max = f_max
    )
  })
  
  observeEvent({   # actualizando los pedidos disponibles de acuerdo a los filtros
    input$input_filtro_zona
    input$input_filtro1
    input$input_filtro2
    input$filtro_fecha_variable
    input$filtro_fecha_rango
  },{
    if(!is.null(tablas$usa)){
      f_region_parametros <- funcion_asigna_region_variables(input$input_filtro_zona, parametros, input)
      temp <- funcion_filtro_vista_ejecutiva(tablas,f_region_parametros,input)
      temp <- eval(parse(text = paste('unique(temp$',f_region_parametros$pedido,')')))
      temp <- temp[order(temp)]
      updatePickerInput(
        session,'filtro_pedido', choices = temp,selected = temp
      )
    }
    
    
  })
  
  
  # vista ejecutiva  -------------------------------------------------------------------------------------------------------------------
  
  
  # input <- list()
  # input$input_filtro_zona <- 'USA'
  # tablas$sub <- tablas$domestico
  
  observeEvent(input$ve_boton_filtro,{
    
    # variables de region
    
    f_region_original <- f_region <- funcion_asigna_region_variables(input$input_filtro_zona, parametros, input)

    # filtro
    
    tablas$sub <- funcion_filtro_vista_ejecutiva(tablas,f_region,input)
    
    # filtro cantidades 0
    
    tablas$sub <- eval(parse(text = paste0(
      'tablas$sub %>% dplyr::filter(',f_region$variables_cantidades[1],' > 0)'
    )))
    
    # compresión 
    
    f_res <- funcion_compresion_fecha_extremo(tablas$sub, f_region$fechas, f_region$pedido,f_region$variables_cantidades,f_region$fecha_benchmark)
    f_tabla <- f_res$tabla
    f_region$fechas <- f_res$fechas
    f_region$fecha_benchmark <- paste0(f_region$fecha_benchmark,'_max')
    
    f_res2 <- funcion_solo_variables_maximo(f_tabla, f_region$fechas)
    f_tabla <- f_res2$tabla
    f_region$fechas <- f_res2$variables
    f_region$fecha_fin <- paste0(f_region$fecha_fin,'_max')
    
    
    # variables extra en la tabla de subconjunto
    
    f_tabla <- funcion_variables_tabla_subconjunto_vista_ejecutiva(f_tabla,f_region)
    
    # variables para graficar
    
    f_extras <- funcion_variables_extra_vista_ejecutiva(tablas$sub,f_tabla,input,f_region)
    
    # cajas
    
    output$ve_caja_pedidos <- renderValueBox({     # caja pedidos
      valueBox(
        nrow(f_tabla),
        'pedidos',
        icon = icon("credit-card"),
        color = 'blue'
      )
    })
    
    output$ve_caja_entregas <- renderValueBox({     # caja entregas
      valueBox(
        nrow(tablas$sub),
        'entregas',
        icon = icon("bicycle",lib = 'font-awesome'),
        color = 'blue'
      )
    })

    output$ve_caja_cajas <- renderValueBox({     # caja cajas
      valueBox(
        paste0(eval(parse(text = paste0('round(sum(tablas$sub$',f_region$cantidad,', na.rm = T)/1000,1)'))),'k'),
        'volumen',
        icon = icon("archive",lib = 'font-awesome'),
        color = 'blue'
      )
    })
    
    output$ve_caja_beforetime <- renderValueBox({     # caja BT
      valueBox(
        f_extras$beforetime,
        'on time',
        icon = icon(f_extras$beforetime_icono,lib = 'font-awesome'),
        color = f_extras$beforetime_color
      )
    })
    
    output$ve_caja_fillrate <- renderValueBox({     # caja BT
      valueBox(
        f_extras$fillrate,
        'fillrate',
        icon = icon(f_extras$fillrate_icono,lib = 'font-awesome'),
        color = f_extras$fillrate_color
      )
    })
    
    output$grafica_tiempo_procesos <- renderPlot({
      funcion_revisar_fechas_coherentes(tablas$sub,f_region_original)
    })
    
    output$grafica_pedidos_cerrados_1 <- renderPlot({   # grafica pedidos cerrados 1
      oldw <- getOption("warn")
      options(warn=-1)
      
      g <- NULL
      f_region <- funcion_asigna_region(input$input_filtro_zona)
      f_p_variables_fecha <- eval(parse(text = paste0(
        'parametros$',f_region,'_fechas'
      )))
      f_p_variable_pedido <- eval(parse(text = paste0(
        'parametros$',f_region,'_pedido[1]'
      )))
      f_variables_cantidades <- eval(parse(text = paste0('parametros$',f_region,'_cantidades')))
      
      f_fechas_benchmark <- eval(parse(text = paste0('parametros$',f_region,'_fechas_benchmark')))
      
      f_fecha_fin <- eval(parse(text = paste0('parametros$',f_region,'_fecha_fin')))
      
      
      tabla_cerrados <- eval(parse(text = paste0(
        'tablas$sub %>%
          dplyr::filter(!is.na(',f_fecha_fin,'))'
      )))

      
      tryCatch(g <- funcion_main_grafica_1(tabla_cerrados, p_compresion = TRUE,'x','y',f_p_variables_fecha,f_p_variable_pedido,f_variables_cantidades,f_fechas_benchmark),error = function(e){})
      
      options(warn = oldw)
      
      validate(need(!is.null(g),'Esta tabla no tiene datos'))
      g
      
    })
    
    output$grafica_pedidos_abiertos_1 <- renderPlot({   # grafica pedidos abiertos 1
      oldw <- getOption("warn")
      options(warn=-1)
      
      g <- NULL
     
      
      f_region <- funcion_asigna_region_variables(input$input_filtro_zona, parametros, input)
      
      # f_p_variables_fecha <- eval(parse(text = paste0(
      #   'parametros$',f_region,'_fechas'
      # )))
      # f_p_variable_pedido <- eval(parse(text = paste0(
      #   'parametros$',f_region,'_pedido[1]'
      # )))
      # f_variables_cantidades <- eval(parse(text = paste0('parametros$',f_region,'_cantidades')))
      # 
      # f_fechas_benchmark <- eval(parse(text = paste0('parametros$',f_region,'_fechas_benchmark')))
      # 
      # f_fecha_fin <- eval(parse(text = paste0('parametros$',f_region,'_fecha_fin')))
      # 
      
      tabla_abiertos <- eval(parse(text = paste0(
        'tablas$sub %>%
          dplyr::filter(is.na(',f_region$fecha_fin,'))'
      )))
      
      
      # funcion_main_grafica_1(p_tabla, p_compresion = TRUE,p_texto_x,p_texto_y,p_variables_fecha,p_variable_pedido,p_variables_cantidades, p_fecha_benchmark,p_procesos_incluir = TRUE,p_procesos_tabla = parametros$domestico_procesos_tabla,p_colorear = TRUE, p_fecha_actual = p_parametros$fecha_actual,p_parametros)  
      # tryCatch(g <- funcion_main_grafica_1(tabla_abiertos, p_compresion = TRUE,'x','y',f_region$fechas,f_region$pedido,f_region$variables_cantidades,f_region$fecha_benchmark,p_procesos_incluir = TRUE,p_procesos_tabla = f_region$procesos_tabla,p_colorear = TRUE, p_fecha_actual = parametros$fecha_actual,p_parametros =parametros),error = function(e){})
      
      tryCatch(g <- funcion_main_grafica_1(tabla_abiertos, p_compresion = TRUE,'x','y',f_region$fechas,f_region$pedido,f_region$variables_cantidades,f_region$fecha_benchmark,p_procesos_incluir = TRUE,p_procesos_tabla = f_region$procesos_tabla,p_colorear = TRUE, p_fecha_actual = parametros$fecha_actual,p_parametros =parametros),error = function(e){})
      
      options(warn = oldw)
      
      validate(need(!is.null(g),'Esta tabla no tiene datos'))
      g
      
    })
    
    
    output$grafica_entregas_desagregadas <- renderPlot({    # gráfica de tiempos desagregadeos
      
      oldw <- getOption("warn")
      options(warn=-1)
      
      g <- NULL
      f_region <- funcion_asigna_region(input$input_filtro_zona)
      f_p_variables_fecha <- eval(parse(text = paste0(
        'parametros$',f_region,'_fechas'
      )))
      f_p_variable_pedido <- eval(parse(text = paste0(
        'parametros$',f_region,'_pedido[1]'
      )))
      f_variables_cantidades <- eval(parse(text = paste0('parametros$',f_region,'_cantidades')))
      
      f_fechas_benchmark <- eval(parse(text = paste0('parametros$',f_region,'_fechas_benchmark')))
      
      tryCatch(g <- funcion_main_grafica_1(tablas$sub, p_compresion = TRUE,'x','y',f_p_variables_fecha,f_p_variable_pedido,f_variables_cantidades,f_fechas_benchmark),error = function(e){})
      
      options(warn = oldw)
      
      validate(need(!is.null(g),'Esta tabla no tiene datos'))
      g
    })
    
    output$grafica_litros <- renderPlot({  # output de grafica de entregas por proceso
      
      oldw <- getOption("warn")
      options(warn=-1)
      
      g <- NULL
      f_region <- funcion_asigna_region(input$input_filtro_zona)
      f_variables_fecha <- eval(parse(text = paste0('parametros$',f_region,'_fechas')))
      f_variable_pedido <- eval(parse(text = paste0('parametros$',f_region,'_pedido[1]')))
      f_variables_cantidades <- eval(parse(text = paste0('parametros$',f_region,'_cantidades')))
      f_fechas_benchmark <- eval(parse(text = paste0('parametros$',f_region,'_fechas_benchmark')))
      
      g <- funcion_main_grafica_2(
        p_tabla <- tablas$sub,
        p_texto_x = 'prceso',
        p_texto_y = 'volumen',
        p_variables_fecha = f_variables_fecha,
        p_variable_pedido = f_variable_pedido,
        p_compresion = TRUE,
        p_variables_cantidades = f_variables_cantidades,
        p_texto_label = 'volumen',
        p_tipo_fgb = 'suma',
        p_fecha_benchmark = f_fechas_benchmark
      )
      
      options(warn = oldw)
      
      validate(need(!is.null(g),'una vez seleccionados los filtros pulsa filtrar para ver la gráfica'))
      g
    })
    
  })


  
  
  
})
    

