library(tcltk)
library(readxl)

#VENTANA 1 -> VENTANA PRINCIPAL DE TODO (ACÁ SE MUESTRAN LOS OBJETIVOS)
ventana1 <- function(){
  #Crea una ventana, en donde todo se va a insertar ahi
  ventana <- tktoplevel()
  
  #Se cambia el titulo de la ventana
  tkwm.title(ventana, "Distribucion Chi-cuadrado")
  
  # Crear un label con el título centrado y en negrilla
  titulo <- tklabel(ventana, text = "Modulo Distribución Poisson", font = "Helvetica 40 bold")
  
  #Crear un label con el autor
  autor <- tklabel(ventana, text = "Laura Sofía Salamanca Barrera", font = "Helvetica 20")
  
  #Crear un label con los obejtivos
  objetivos <- tklabel(ventana, text = 
                "Objetivos:\n\n
                1. Evaluar la comprensión de la distribución chi-cuadrado,\n
                centrándose en sus propiedades, parámetros y características esenciales.\n\n
                2. Medir la capacidad de solución de problemas a partir de las\n
                características esenciales de la distribución chi - cuadrado\n\n
                3. Evaluar los conocimientos acerca de la relación de la chi-cuadrado\n
                con otras distribuciones de probabilidad. Además de identificar y\n
                diferenciar esta distribución de las demas\n\n",justify = "left",font = "Helvetica 14")

  #Acá se cargan todos los labels
  tkpack(titulo, side = "top", pady = 10)
  tkpack(autor, side = "top", pady = 5)
  tkpack(objetivos, side = "top", pady = 10)
  
  #tamaño de la ventana
  tkwm.geometry(ventana, "1000x700")
  
  #Interaccion del usuario
  #Creacion del boton como tal 
  frame_boton <- tkframe(ventana)
  tkpack(frame_boton, side = "bottom")
  
  # Agregar el boton. Si lo oprime, se va al comando instrucciones (contiene las instrucciones)
  boton_iniciar <- tkbutton(frame_boton, text = "Instrucciones",command = function() {
    tkdestroy(ventana)  # Destruir la ventana principal
    ventana2()   # Llamar a otra función
  })
  
  # configurar la posicion, pady es respecto a abajo. ipadx o padx es respecto a los lados
  tkpack(boton_iniciar, side = "top", pady = 40, ipadx = 10,ipady = 10)
  
  # Configurar las propiedades de la letra
  tkconfigure(boton_iniciar, font = "-size 15 -weight bold")
}


#VENTANA 2 -> VENTANA DONDE SE VAN A ESCRIBIR LAS INSTRUCCIONES GENERALES DE TODO.
ventana2 <- function(){
  #Crea una ventana, en donde todo se va a insertar ahi
  ventana <- tktoplevel()
  
  #Se cambia el titulo de la ventana
  tkwm.title(ventana, "Instrucciones")
  
  # Crear un label con el título centrado y en negrilla
  titulo <- tklabel(ventana, text = "Instrucciones", font = "Helvetica 40 bold")
  
  #Crear un label con el autor
  puntaje <- tklabel(ventana, text = "Puntaje minimo = 0\nPuntaje maximo = 20", font = "Helvetica 20")
  
  #Crear un label con los obejtivos
  instrucciones <- tklabel(ventana, text = 
                  "Hay 3 tipos de preguntas:\n\n
                  1. VERDADERO/FALSO: se mostrará una proposición y usted debe escoger
                  si es verdadera o falsa. Tenga en cuenta que (1) Falso, (2) Verdadero.
                  Estas preguntas tienen un peso de 1 punto. En total son 3 preguntas.\n\n
                  2. RESPUESTA NÚMERICA: se mostrará un ejercicio y usted debe solucionarlo
                  con sus conocimientos. La respuesta de este debe ser númerica. Estas
                  preguntas tienen un peso de 3 puntos. En total son 3 preguntas.\n\n
                  3. SELECCIÓN MULTIPLE: se mostrará una pregunta donde usted debe escoger
                  la opción que sea correcta. Recuerde que la respuesta es una única opción.
                  Estas preguntas tienen un peso de 2 puntos. En total son 4 preguntas\n\n"
                  ,justify = "left",font = "Helvetica 14")
  
  #Acá se cargan todos los labels
  tkpack(titulo, side = "top", pady = 10)
  tkpack(puntaje, side = "top", pady = 5)
  tkpack(instrucciones, side = "top", pady = 10)
  
  #tamaño de la ventana
  tkwm.geometry(ventana, "1000x700")
  
  #Interaccion del usuario
  #Creacion del boton como tal 
  frame_boton <- tkframe(ventana)
  tkpack(frame_boton, side = "bottom")
  
  # Agregar el boton. Si lo oprime, se va al comando preguntas (una funcion que contiene las preguntas)
  boton_iniciar <- tkbutton(frame_boton, text = "Iniciar Cuestionario",command = function() {
    tkdestroy(ventana)  # Destruir la ventana principal
    preguntas()   # Llamar a otra función
  })
  
  # configurar la posicion, pady es respecto a abajo. ipadx o padx es respecto a los lados
  tkpack(boton_iniciar, side = "top", pady = 40, ipadx = 10,ipady = 10)
  
  # Configurar las propiedades de la letra
  tkconfigure(boton_iniciar, font = "-size 15 -weight bold")
  
}

#VENTANA GENERERAL -> FUNCION GENERAL DONDE SE PASARÁN LAS PREGUNTAS Y SE MOSTRARAN EN UNA VENTANA
ventana_general <- function(obs,puntaje){
  #Crea una ventana, en donde todo se va a insertar ahi (general)
  ventana <- tktoplevel()
  #Crea un bool que nos indica si la ventana esta abierta o cerrada
  ventana_c = TRUE
  
  #Se cambia el titulo de la ventana (general)
  tkwm.title(ventana, paste("Pregunta ",obs$Numero))
  
  # Crear un label con el título centrado y en negrita (general)
  titulo <- tklabel(ventana, text = paste("PREGUNTA ",obs$Numero), font = "Helvetica 40 bold")
  
  #DEPENDE DEL TIPO DE PREGUNTA
  if(obs$`Tipo de pregunta`==1){
    #Crear un label con el autor
    tipo <- tklabel(ventana, text = paste("VERDADERO/FALSO\nPUNTAJE ACTUAL = ",puntaje), font = "Helvetica 20")
    
    #Crear las opciones
    opciones <- paste("1. Falso\n2. Verdadero\n\n")
    opc <- tklabel(ventana,text = opciones, justify = "left", font = "Helvetica 12")
    
    #Se asignan 30 segundos a las preguntas de falso - verdadero
    tiempo = 30  
    
    #Extraer la respuesta de la pregunta (esto solo es para imprimirla en caso de que sea incorrecta)
    res <- obs[3+obs$Respuesta]
    
  }else if(obs$`Tipo de pregunta` == 2){
    #Crear un label con el autor
    tipo <- tklabel(ventana, text = paste("RESPUESTA NUMERICA\nPUNTAJE ACTUAL = ",puntaje), font = "Helvetica 20")
    
    #Opciones en blanco (solo para que quede lindo)
    opciones <- paste("\n\n")
    opc <- tklabel(ventana,text = opciones, justify = "left", font = "Helvetica 12")
    
    #Se asignan 60 segundos a las preguntas de respuesta única
    tiempo = 60  
    
    #Extraer la respuesta de la pregunta (esto solo es para imprimirla en caso de que sea incorrecta)
    res <- obs$Respuesta
    
  }else{
    #Crear un label con el autor
    tipo <- tklabel(ventana, text = paste("RESPUESTA MÚLTIPLE\nPUNTAJE ACTUAL = ",puntaje), font = "Helvetica 20")
    
    #Listaar todas las posibles opciones
    opciones <- paste("1. ",obs$`Opcion 1`,"\n2. ",obs$`Opcion 2`,"\n3. ",
                      obs$`Opcion 3`,"\n4. ",obs$`Opcion 4`,"\n5. ",
                      obs$`Opcion 5`, "\n\n")
    opc <- tklabel(ventana,text = opciones, justify = "left", font = "Helvetica 12")
    
    #Se crean 60  segundos a las preguntas de selección multiple
    tiempo = 60
    
    #Extraer la respuesta de la pregunta (esto solo es para imprimirla en caso de que sea incorrecta)
    res <- obs[3+obs$Respuesta]
  }
  
  
  #Crear un label con la pregunta
  texto <- obs$Enunciado
  preg <- tklabel(ventana, text = texto, justify = "left",font = "Helvetica 14")
  
  #etiqueta de la ventana
  pb <- tkProgressBar("Temporizador")
  inicio <- Sys.time()
  
  #Agrego a la ventana el titulo, el tipo, la pregunta y las opciones
  tkpack(titulo, side = "top", pady = 10)
  tkpack(tipo, side = "top", pady = 5)
  tkpack(preg, side = "top", pady = 10)
  tkpack(opc,side="top",pady=10)
  
  
  #Cuadro para ingresar el texto
  #tclvar es para indicar el tipo de variable
  texto_ingresado <- tclVar("")
  #Declarar el cuadro de ingreso
  cuadro_texto <- tkentry(ventana, textvariable = texto_ingresado)
  #Se le agrega un mensaje a la ventana
  tkpack(tklabel(ventana, text = "Ingresa tu respuesta:"), side = "top")
  #Agregamos el cuadro de texto a la ventana
  tkpack(cuadro_texto, side = "top")
  
  #BOTON
  #Creamos el boton
  frame_boton <- tkframe(ventana)
  tkpack(frame_boton, side = "bottom")
  
  #Aca definimos donde va (va en el frame declarado anteriormente que ya esta en la ventana)
  #el texto es el que va dentro del boton
  #command es la función que va a seguir cuando se oprima el boton (en este caso destruye la ventana y cambia el bool a Falso)
  boton_confirmar <- tkbutton(frame_boton, text = "Enviar respuesta", command = function() {
    ventana_c <<- FALSE
    tkdestroy(ventana)
  })
  #Agregamos el boton
  tkpack(boton_confirmar, side = "top", pady = 40, ipadx = 10, ipady = 10)
  tkconfigure(boton_confirmar, font = "-size 15 -weight bold")
  
  
  #WHILE DEL TEMPORIZADOR (mientras que sea verdaero y la ventana este abierta)
  #Lo segundo es porque cuando se envia la respuesta o cuando se acaba el tiempo se destruye la ventana, entonces es una banderaa que nos indica si el temporizador debe seguir
  while (TRUE & ventana_c) {
    progreso <- as.numeric(difftime(Sys.time(), inicio, units = 'secs'))
    restante <- tiempo - progreso
    Sys.sleep(0.1)
    setTkProgressBar(pb, restante/tiempo, label = sprintf("Tiempo restante: %i segundos", round(restante)))
    if (restante < 0) {
      tkdestroy(ventana)
      print(round(progreso))
      break
    }
    
    #Este comando sirve para indicarle al programa que siga viendo los eventos de la interfaz (en cuanto al boton y al cuadro de ingreso)
    tclServiceMode(TRUE)
    
    if (ventana_c == FALSE) {
      tkdestroy(ventana)
      break
    }
  }
  #Se cierra la barra de progreso
  close(pb)
  
  #Si la ventana aun esta abierta significa que se acabo el tiempo
  if(ventana_c == TRUE){
    tkdestroy(ventana)
    ventana <- tktoplevel()
    titulo <- tklabel(ventana, text = paste("SE ACABO EL TIEMPO\nRESPUESTA INCORRECTA"), font = "Helvetica 40 bold")
    tkpack(titulo, side = "top", pady = 10)
    if(obs$Formula != "-1"){
      formula <- tklabel(ventana, text = obs$Formula, font = "Helvetica 20")
      tkpack(formula, side = "top", pady = 10)
    }
    solucion <- tklabel(ventana, text = obs$Solución, font = "Helvetica 20")
    tkpack(solucion, side = "top", pady = 10)
    respuesta <- tklabel(ventana, text = paste("Respuesta correcta: ",as.character(res)),font = "Helvetica 20 bold")
    tkpack(respuesta,side = "top", pady = 10)
    Sys.sleep(15)
    tkdestroy(ventana)
    
    #En este caso se retornara un arreglo (forma de retornar dos cosas)
    #El primer elemento es un binario que nos indica si la pregunta estuvo bien(1) o mal(2)
    #El segundo elemento es el tiempo que se demoro el usuario en contestar
    return(c(0,round(progreso)))
  } 
  
  #El tclvalue hace que el valor sea leido (no se puede leer directamente)
  #Ambos quedan como string y se comparan los dos strings. No difiere de la 
  #respuesta, pues debe ser exactamente la misma. 
  
  #Si la respuesta es igual al valor ingresado Y el progreso es distinto al tiempo (lo que indica que no se le acabo el tiempo)
  if(tclvalue(texto_ingresado) == as.character(obs$Respuesta) & progreso != tiempo & ventana_c == FALSE){
    ventana <- tktoplevel()
    titulo <- tklabel(ventana, text = paste("RESPUESTA CORRECTA"), font = "Helvetica 40 bold")
    tkpack(titulo, side = "top", pady = 10)
    Sys.sleep(5)
    tkdestroy(ventana)
    return(c(1,round(progreso)))
  }else if(tclvalue(texto_ingresado) != as.character(obs$Respuesta) & progreso != tiempo & ventana_c == FALSE){
    ventana <- tktoplevel()
    titulo <- tklabel(ventana, text = paste("RESPUESTA INCORRECTA"), font = "Helvetica 40 bold")
    tkpack(titulo, side = "top", pady = 10)
    if(obs$Formula != "-1"){
      formula <- tklabel(ventana, text = obs$Formula, font = "Helvetica 20")
      tkpack(formula, side = "top", pady = 10)
    }
    solucion <- tklabel(ventana, text = obs$Solución, font = "Helvetica 20")
    tkpack(solucion, side = "top", pady = 10)
    respuesta <- tklabel(ventana, text = paste("Respuesta correcta: ",as.character(res)),font = "Helvetica 20 bold")
    tkpack(respuesta,side = "top", pady = 10)
    Sys.sleep(15)
    tkdestroy(ventana)
    return(c(0,round(progreso)))
  }
}


preguntas <- function(){
  preguntas <- read_excel("C:/Users/laura/OneDrive - Pontificia Universidad Javeriana/Preguntas_chi.xlsx")
  preguntas <- as.data.frame(preguntas)
  puntaje <- 0
  respuestas <- c()
  tiempos <- c()

  #En este caso se recorrera cada observación
  for(i in 1:length(preguntas$Numero)){
    obs <- preguntas[i,]
    retorno<-ventana_general(obs,puntaje)
    respuesta <- retorno[1]
    print(respuesta)
    if(preguntas[i,]$`Tipo de pregunta` == 1 & respuesta == 1){
      puntaje <- puntaje + 1
    }else if(preguntas[i,]$`Tipo de pregunta` == 2 & respuesta == 1){
      puntaje <- puntaje + 3
    }else if(preguntas[i,]$`Tipo de pregunta` == 3 & respuesta == 1){
      puntaje <- puntaje + 2
    }
    respuestas <- append(respuestas,respuesta)
    tiempo <- retorno[2]
    tiempos <- append(tiempos,tiempo)
  }
}

ventana1()

