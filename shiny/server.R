
library(shinydashboard)

shinyServer(function(input, session, output) {
  
  
  # definición de las variables reactivas -------------------------------------------------------------------------------------------------
  
  
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
  
  proceso = reactiveValues(
    nacionales_moderno = 1,
    nacionales_mayoreo = 1,
    nacionales_consumo = 1,
    usa_wine = 1,
    usa_proximo = 1,
    row_region1 = 1,
    row_region2 = 1,
    row_region3 = 1,
    row_region4 = 1,
    row_region5 = 1
  )
  
  tablas <- reactiveValues(
    ano_mes = NULL,
    zsdr141 = NULL,
    total = NULL,
    vis = NULL
  )
  

    # update de selector de fechas --------------------------------------------------------------------------------------------------

  observe({
    a <- 'ninguno'
    if(!is.null(tablas$ano_mes)){
     
        a <- sort(unique(tablas$ano_mes))
      
    }
    updatePickerInput(
      session,'input_filtro_fecha_original',
      choices = unique(as.character(a)),
      selected = NULL
    )
  })

  
  # ui de login --------------------------------------------------------------
  
  
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
  
  ui2 <- function(){list(tabPanel(user$name),tabPanel(user$role))}    # información de log in existoso
  
  
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
    }
  })
  
  
  observeEvent(input$boton_login_pre, {
    updateTabsetPanel(session, "menu",selected = "entrada")
  })
  
  
  output$logged_user <- renderText({
    if(user$logged == TRUE) return(paste0('Sesión iniciada como ', user$name))
    return("")
  })
  
  
  
  output$texto_link <- renderText({
    if(user$logged == TRUE)return('log out')
    return('')
  })
  
  output$esta_logeado <- reactive({
    user$role == ''
  })
  
  outputOptions(output, "esta_logeado", suspendWhenHidden = FALSE)
  
  
  
  
  # activación de los diferentes tipos de usuario ----------------------------------------------
  
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
  
  
  # seguimiento de las inputs ------------------------------------------------------------------
  
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
  
  # seguimiento de las variables reactivas ------------------------------------------------------
  
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
  
  
  # gráfica de procedimiento -------------------------------------------------------------------------------------------------
  
  output$grafica_proceso <- renderSankeyNetwork({
    sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale=my_color, LinkGroup="group", fontSize = 15)
  })
  
  output$grafica_fill_rate <- renderHighchart({
    funcion_grafica_fill_rate(datos_fill_rate)
  })
  
  # mapa de fill_rate -----------------------------------------------------------------------------------------------------------------------
  
  output$mapa_fill_rate <- renderLeaflet({
    leaflet(data = mexico) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(fill_rate), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)
  })
  
  # carga de los datos -----------------------------------------------------------------------------------------------------
  
  # tablas <- list()
  
  observeEvent(input$boton_carga,{
    
    

    
    oldw <- getOption("warn")
    options(warn=-1)
    
    style <- isolate(input$style)
    progress <- shiny::Progress$new(style = style)
    progress$set(message = "Cargando zsdr141 ", value = 0)
    
    tablas$zsdr141 <- funcion_carga_datos('zsdr141','zsdr141')
    
    tablas$zsdr141 <- funcion_ano_mes(tablas$zsdr141)
    
    progress$set(message = "Cargando zsdr159 ", value = 0.5)
    
    tablas$zsdr159 <- funcion_carga_datos('zsdr159','zsdr159')
    
    tablas$zsdr159 <- funcion_ano_mes(tablas$zsdr159)
    
    progress$set(message = "Carga finalizada ", value = 1)
    
    tablas$ano_mes <- base::intersect(unique(tablas$zsdr141$ano_mes),unique(tablas$zsdr159$ano_mes))
    
    options(warn = oldw)
    
  })
  
  output$o_texto_carga_zsdr141 <- renderText({
    validate(need(tablas$zsdr141,'nop'))
    'Carga de las transacciones zsdr141 finalizada exitosamente'
  })
  
  output$o_texto_carga_zsdr159 <- renderText({
    validate(need(tablas$zsdr159,'nop'))
    'Carga de las transacciones zsdr151 finalizada exitosamente'
  })
  
  # filtro de los datos -------------------------------------------------------------------------------------------------------
  
  
  # input <- list()
  # input$input_filtro_fecha_original <- c('agosto','septiembre')
  # input$input_filtro_region <- c('USA')
  
  observeEvent(input$boton_filtrar,{
    
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
  
  
  # completo
  
  output$output_tabla_completo <- renderFormattable({
    
    nona <- function(a){
      return(!is.na(a))
    }
    
    f_completo <- tablas$vis %>%
      mutate_all(.funs = nona) %>%
      summarise_all(.funs = sum)
    
   f_completo <- f_completo / nrow(tablas$vis)  
   
  
   f_completo <- f_completo %>%
     select(-contains('ex_fecha_'),-contains('ex_liberacion_calidad'))
   
   f_completo_arreglado <- data.frame(variable = names(f_completo), porcentaje = unlist(f_completo[1,]))
   
   
   f_completo_arreglado <- f_completo_arreglado %>%
     filter(!(variable %in% c('ano','mes','ano_mes')))
    
   f_completo_arreglado %>%
     formattable
    
  })
  
  output$output_grafica_tiempo1 <- renderPlot({
    
    p_compresion <- FALSE
    p_fecha_focal <- 'fecha_pedido'
    if(input$input_filtro_region == 'Doméstico'){
      p_fecha_focal = 'fecha_creacion_min'
      p_compresion <- TRUE
    }
    
    oldw <- getOption("warn")
    options(warn=-1)
    
    g <- funcion_grafica_tiempos_grande(tablas$vis, p_fecha_focal, p_compresion, 'días', 'tablas (con diferentes grados de información')
    
    options(warn = oldw)
    
    
    g
  })
  
  
})
    

