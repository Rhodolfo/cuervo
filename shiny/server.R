
library(shinydashboard)

shinyServer(function(input, session, output) {
  
  
  # definición de las variables reactivas ---------------------------------------------------------------------------------------------------------
  
  chat <- reactiveValues(     # variables para el chat
    chat = NULL,
    users = NULL
  )
  
  user <- reactiveValues(        # variables de usuario y status de logeo
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
    vis_pa = NULL
  )
  
  status <- reactiveValues(      # banderas para supervisar procesos
    carga = FALSE
  )
  
  parametros <- reactiveValues(             # parámetros que se leen desde el excel
    domestico_fechas = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_fechas)) %>%
      dplyr::select(domestico_fechas) %>%
      unlist,
    
    usa_fechas = excel_parametros %>% 
      dplyr::filter(!is.na(usa_fechas)) %>%
      dplyr::select(usa_fechas) %>%
      unlist,
    
    row_fechas = excel_parametros %>% 
      dplyr::filter(!is.na(row_fechas)) %>%
      dplyr::select(row_fechas) %>%
      unlist,
    
    domestico_cantidades = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_cantidades)) %>%
      dplyr::select(domestico_cantidades) %>%
      unlist,
    
    usa_cantidades = excel_parametros %>% 
      dplyr::filter(!is.na(usa_cantidades)) %>%
      dplyr::select(usa_cantidades) %>%
      unlist,
    
    row_cantidades = excel_parametros %>% 
      dplyr::filter(!is.na(row_cantidades)) %>%
      dplyr::select(row_cantidades) %>%
      unlist,
    
    domestico_filtros = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_filtros)) %>%
      dplyr::select(domestico_filtros) %>%
      unlist,
    
    usa_filtros = excel_parametros %>% 
      dplyr::filter(!is.na(usa_filtros)) %>%
      dplyr::select(usa_filtros) %>%
      unlist,
    
    row_filtros = excel_parametros %>% 
      dplyr::filter(!is.na(row_filtros)) %>%
      dplyr::select(row_filtros) %>%
      unlist,
    
    usa_carpeta = excel_parametros %>% 
      dplyr::filter(!is.na(usa_carpeta)) %>%
      dplyr::select(usa_carpeta) %>%
      unlist,
    
    row_carpeta = excel_parametros %>% 
      dplyr::filter(!is.na(row_carpeta)) %>%
      dplyr::select(row_carpeta) %>%
      unlist,
    
    domestico_carpeta = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_carpeta)) %>%
      dplyr::select(domestico_carpeta) %>%
      unlist,
    
    usa_pedido = excel_parametros %>% 
      dplyr::filter(!is.na(usa_pedido)) %>%
      dplyr::select(usa_pedido) %>%
      unlist,
    
    row_pedido = excel_parametros %>% 
      dplyr::filter(!is.na(row_pedido)) %>%
      dplyr::select(row_pedido) %>%
      unlist,
    
    domestico_pedido = excel_parametros %>% 
      dplyr::filter(!is.na(domestico_pedido)) %>%
      dplyr::select(domestico_pedido) %>%
      unlist
    
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
  
  
  
  
    # update de selector de  --------------------------------------------------------------------------------------------------------------------------------------------------

  
  
  observe({
    a <- 'ninguno'
    if(!is.null(tablas$ano_mes)){
      if(input$input_filtro_region_pa == 'Doméstico'){
        a <- funcion_extrae_fechas(tablas$zsdr159) %>% names
      }
      if(input$input_filtro_region_pa %in% c('USA','Resto del mundo')){
        a <- funcion_extrae_fechas(tablas$zsdr141) %>% names
      }
     
    }
   
    updatePickerInput(
      session,'input_fecha_final_pa_141',
      choices = unique(as.character(a)),
      selected = NULL
    )
  })

  
  # ui de login ------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  
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
  
  ui2 <- function(){list(tabPanel(user$name,actionButton(
    inputId = 'boton_siguiente_login',
    label = 'Siguiente'
  )),tabPanel(user$role))}    # información de log in existoso

  
  
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
      dplyr::filter(!is.na(Zona_de_ventas)) %>%
      dplyr::filter(Nombre_Región == 'USA')
    
   
    progress$set(message = "Cargando Resto del Mundo ", value = 0.3)
    Sys.sleep(1)
    tablas$row <- funcion_cargar_datos(parametros$row_carpeta,parametros$row_fechas,parametros$row_cantidades,parametros$row_filtros,parametros$row_pedido) %>% 
      dplyr::filter(!is.na(Zona_de_ventas)) %>%
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
  
  
  # visualizacion1 --------------------------------------------------------------------------------------------------------------------------------------------------------------

  
  ui_filtros_visualizador1 <- function(){          # interface con los filtros general
    
    
    if(input$input_filtro_zona == 'USA')f_region <- 'usa'
    if(input$input_filtro_zona == 'Resto del mundo')f_region <- 'row'
    if(input$input_filtro_zona == 'Doméstico')f_region <- 'domestico'
      
    
    
    lista_opciones <- list()
    
    eval(parse(text=paste0('a <- ')))
    
    eval(parse(text=paste0('lista_opciones[[',1:3,']] <- unique(tablas$',f_region,'$',,')',collapse = ';')))
    
    vector_opciones <- NULL
    for(i in 1:length(lista_opciones)){
      lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
      vector_opciones <- c(vector_opciones,lista_opciones[[i]])
    }
    funcion1 <- 'tagList('
    funcion2 <- paste0('div(style="display:inline-block",pickerInput(width = 150,"input_filtro_',eval(parse(text=paste0("parametros$",f_region,"_filtros"))),'",
                       "',eval(parse(text=paste0("parametros$",f_region,"_filtros"))),'",selected = NULL,options = list(`actions-box` = TRUE),multiple = TRUE,choices = c(',vector_opciones,')))')
    funcion2 <- paste0(funcion2,collapse = ',')    
    funcion3 <- ')'
    resultado <- eval(parse(text = paste0(
      funcion1,
      funcion2,
      funcion3
    )))
    return(resultado)
  }
  
  
  # ui_filtros_domestico <- function(){          # interface con los filtros de doméstico
  #   lista_opciones <- list()
  #   for(i in 1:length(parametros$domestico_filtros)){
  #     eval(parse(text=paste0('lista_opciones[[',i,']] <- unique(tablas$domestico$',parametros$domestico_filtros[i],')')))
  #   }
  #   vector_opciones <- NULL
  #   for(i in 1:length(lista_opciones)){
  #     lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
  #     vector_opciones <- c(vector_opciones,lista_opciones[[i]])
  #   }
  #   funcion1 <- 'tagList('
  #   funcion2 <- paste0('div(style="display:inline-block",pickerInput(width = 150,"input_filtro_',parametros$domestico_filtros,'",
  #                      "',parametros$domestico_filtros,'",selected = NULL,options = list(`actions-box` = TRUE),multiple = TRUE,choices = c(',vector_opciones,')))')
  #   funcion2 <- paste0(funcion2,collapse = ',')    
  #   funcion3 <- ')'
  #   resultado <- eval(parse(text = paste0(
  #     funcion1,
  #     funcion2,
  #     funcion3
  #   )))
  #   return(resultado)
  # }
  # 
  # 
  # ui_filtros_usa <- function(){         # interface con los filtros de usa
  #   lista_opciones <- list()
  #   for(i in 1:length(parametros$usa_filtros)){
  #     eval(parse(text=paste0('lista_opciones[[',i,']] <- unique(tablas$usa$',parametros$usa_filtros[i],')')))
  #   }
  #   vector_opciones <- NULL
  #   for(i in 1:length(lista_opciones)){
  #     lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
  #     vector_opciones <- c(vector_opciones,lista_opciones[[i]])
  #   }
  #   funcion1 <- 'tagList('
  #   funcion2 <- paste0('div(style="display:inline-block",pickerInput(width = 150,"input_filtro_',parametros$usa_filtros,'",
  #                      "',parametros$usa_filtros,'",selected = NULL,options = list(`actions-box` = TRUE),multiple = TRUE,choices = c(',vector_opciones,')))')
  #   funcion2 <- paste0(funcion2,collapse = ',')    
  #   funcion3 <- ')'
  #   resultado <- eval(parse(text = paste0(
  #     funcion1,
  #     funcion2,
  #     funcion3
  #   )))
  #   return(resultado)
  # }
  # 
  # 
  # ui_filtros_row <- function(){         # interface con los filtros de resto del mundo 
  #   lista_opciones <- list()
  #   for(i in 1:length(parametros$row_filtros)){
  #     eval(parse(text=paste0('lista_opciones[[',i,']] <- unique(tablas$row$',parametros$row_filtros[i],')')))
  #   }
  #   vector_opciones <- NULL
  #   for(i in 1:length(lista_opciones)){
  #     lista_opciones[[i]] <- paste0('"',paste0(lista_opciones[[i]],collapse = '","'),'"')
  #     vector_opciones <- c(vector_opciones,lista_opciones[[i]])
  #   }
  #   funcion1 <- 'tagList('
  #   funcion2 <- paste0('div(style="display:inline-block",pickerInput(width = 150,"input_filtro_',parametros$row_filtros,'",
  #                      "',parametros$row_filtros,'",selected = NULL,options = list(`actions-box` = TRUE),multiple = TRUE,choices = c(',vector_opciones,')))')
  #   funcion2 <- paste0(funcion2,collapse = ',')    
  #   funcion3 <- ')'
  #   resultado <- eval(parse(text = paste0(
  #     funcion1,
  #     funcion2,
  #     funcion3
  #   )))
  #   return(resultado)
  # }
  
  observe({                                              # decido cuál de las interfaces se muestra
      output$i_filtros_visualizacion1 <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui_filtros_visualizador1())))
      })
  })
  
  # observe({                                              # decido cuál de las interfaces se muestra
  #   if (input$input_filtro_zona == 'Doméstico') {
  #     output$i_filtros_visualizacion1 <- renderUI({
  #       div(class="outer",do.call(bootstrapPage,c("",ui_filtros_domestico())))
  #     })
  #   }
  #   if (input$input_filtro_zona == 'USA') {
  #     output$i_filtros_visualizacion1 <- renderUI({
  #       div(class="outer",do.call(bootstrapPage,c("",ui_filtros_usa())))
  #     })
  #   }
  #   if (input$input_filtro_zona == 'Resto del mundo') {
  #     output$i_filtros_visualizacion1 <- renderUI({
  #       div(class="outer",do.call(bootstrapPage,c("",ui_filtros_row())))
  #     })
  #   }
  # })

  
  observe
  
  observe({                           # actualización de los filtros
    a <- 'ninguno'
    if(!is.null(tablas$ano_mes)){
      if(input$input_filtro_region_pa == 'Doméstico'){
        a <- funcion_extrae_fechas(tablas$zsdr159) %>% names
      }
      if(input$input_filtro_region_pa %in% c('USA','Resto del mundo')){
        a <- funcion_extrae_fechas(tablas$zsdr141) %>% names
      }
      
    }
    
    updatePickerInput(
      session,'input_fecha_final_pa_141',
      choices = unique(as.character(a)),
      selected = NULL
    )
  })
  
  
  
  
  observeEvent(input$boton_filtrar,{                           # filtro de la información
    if(input$input_filtro_region == 'Doméstico'){
      tablas$vis <- tablas$zsdr159
    }
    if(input$input_filtro_region == 'USA'){
      tablas$vis <- tablas$zsdr141 %>%
        dplyr::filter(!is.na(region_nombre)) %>%
        dplyr::filter(region_nombre == 'USA')
    }
    if(input$input_filtro_region == 'Resto del mundo'){
      tablas$vis <- tablas$zsdr141 %>%
        dplyr::filter(!is.na(region_nombre)) %>%
        dplyr::filter(region_nombre %in% c('APAC','EMEA','LATAM','PUK','JCMD'))
    }
    tablas$vis <- tablas$vis %>%
    filter(ano_mes %in% input$input_filtro_fecha_original)
  })
  
  
  observeEvent(input$boton_filtrar_pa,{                      # filtro pedidos abiertos
    if(input$input_filtro_region == 'Doméstico'){
      tablas$vis_pa <- tablas$zsdr159
    }
    if(input$input_filtro_region == 'USA'){
      
      eval(parse(text = paste0(
        "tablas$vis_pa <- tablas$zsdr141 %>%
        dplyr::filter(!is.na(region_nombre)) %>%
        dplyr::filter(region_nombre == 'USA') %>%
        dplyr::filter(is.na(",input$input_fecha_final_pa_141,")) %>%
        dplyr::filter(fecha_original_preferente>= '",input$input_filtro_fecha_1,"') %>%
        dplyr::filter(fecha_original_preferente<= '",input$input_filtro_fecha_2,"')"
      )))
      
      
    }
    if(input$input_filtro_region == 'Resto del mundo'){
     
      eval(parse(text = paste0(
        "tablas$vis_pa <- tablas$zsdr141 %>%
        dplyr::filter(!is.na(region_nombre)) %>%
        dplyr::filter(region_nombre %in% c('APAC','EMEA','LATAM','PUK','JCMD')) %>%
        dplyr::filter(is.na(",input$input_fecha_final_pa_141,"))  %>%
        dplyr::filter(",input$input_fecha_final_pa_141,">= '",input$input_filtro_fecha_1,"') %>%
        dplyr::filter(",input$input_fecha_final_pa_141,"<= '",input$input_filtro_fecha_2,"')"
      )))
      
    }
  
    
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
  
  
  # outputs de la pestaña de pedidos abiertos

  output$output_grafica_pa_total <- renderPlot({
    
  })
  
  
  
})
    

