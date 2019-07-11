
library(shinydashboard)

shinyServer(function(input, session, output) {
  
  user <- reactiveValues(        # variables de usuario y status de logeo
    preloged = 'noaprovado',
    logged = FALSE, 
    role = NULL,
    name = NULL
  ) 
  
  nube <- reactiveValues(                # datos de la nube
    usuarios = nube_cuervo_usuarios
  )
  
  ui1 <- function(){   # pantalla de logeo inicial
    tagList(
      div(id = "login",
          wellPanel(textInput("input_usuario", "Usuario"),
                    passwordInput("passwd", "Contraseña"),
                    br(),actionButton("boton_login", "Log in")))
      ,tags$style(type="text/css", "#login {font-size:10px;
                  text-align: left;
                  position:absolute;
                  top: 10%;
                  left: 50%;
                  margin-top: -10px
                  ;margin-left: -150px;}")
    )}
  
  ui2 <- function(){list(tabPanel(user$name),tabPanel(user$role))}    # información de log in existoso
  
  observe({                           # observador que checa el log in
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
  
  output$logged_user <- renderText({
    if(user$logged == TRUE) return(paste0('Sesión iniciada como ', user$name))
    return("")
  })
  
  output$texto_login <- renderText({
    if(user$logged == TRUE) return("log out")
    return("log in")
  })
  
  observeEvent(input$boton_login_pre,{
    user$preloged <- 'aprovado'
    user$logged <- (user$logged + 1) %% 2
  })
  
  
  
})
    