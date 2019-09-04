
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
    zsdr141 = NULL,
    total = NULL,
    vis = NULL
  )
  

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
    tablas$zsdr141 <- funcion_carga_datos('zsdr141','zsdr141')
    tablas$total <- tablas$zsdr141
    
  })
  
  output$o_texto_carga_zsdr141 <- renderText({
    validate(need(tablas$zsdr141,'nop'))
    'Carga de las transacciones zsdr141 finalizada exitosamente'
  })
  
  # visualización de los datos -------------------------------------------------------------------------------------------------------
  
  
  # input <- list()
  # input$input_filtro_fecha_original <- c('agosto','septiembre')
  # input$input_filtro_region <- c('USA')
  
  observeEvent(input$boton_filtrar,{
    f_mes_fecha_original <- sapply(input$input_filtro_fecha_original, funcion_mes_a_numero)
    tablas$vis <- tablas$total %>%
      mutate(
        mes_fecha_original_preferente = month(fecha_original_preferente)
      ) %>%
      filter(mes_fecha_original_preferente %in% f_mes_fecha_original) %>%
      filter(region_nombre_2 %in% input$input_filtro_region)
  })
  
  
  # usa
  
  
})
    

