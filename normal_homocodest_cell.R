#funcion que prueba homocedasticidad y noramldiad. si lahomocedasticidad 
#no se cumple hacepruebaas por pares de niveles


#"Anything that can be automatically done for you can be automatically done to you.
#  Wyland's Law of Automation


#problemas que creo que habra
#no tiene defensacontra NA, NANS, etc
#no tiene defensa contra tipos de datos incorrectos
#no tienedefensa contra celdas de tamanio 1
#no tiene defensa contra variables ausentes
#no tiene control sobre tipo de pruebas


cell_tests<-function(variables,factors=NULL,data,conf=0.05){
  #defino de una vez la funcion de combinaciones
  combination <- function(n, r, v) {#funcion de gtools
    if (r == 0) 
      v0
    else if (r == 1) 
      matrix(v, n, 1)
    else if (r == n) 
      matrix(v, 1, n)
    else rbind(cbind(v[1], Recall(n - 1, r - 1, v[-1])), 
               Recall(n - 1, r, v[-1]))
  }

  if (length(factors)<1){
    return("Not implemtented")#esto seria lo mismo que tener una variable sin factores,
    #no tendria caso hacer todo esto
  }
  else{
  
    col_factors<-match(factors,colnames(data))# numero de columna con factor
    col_variables<-match(variables,colnames(data))# numero de columnas con variables
    global_factor<-droplevels(interaction(data[,col_factors]))# hacer nuevo factor combinando los otros

    results_n<-data.frame(Cell_normality=factor(levels = c("Yes","No")),
                          Cell_normality_p_value=numeric(),
                          Cell_normality_W_statistic=numeric())#inicializar espacio para resultados
    
    results_h<-data.frame(Homoscedasticity=factor(levels = c("Yes","No"," ")),
                        Homoscedasticity_p_value=numeric(),
                        Homoscedasticity_statistic=numeric())
    
    for (v in col_variables){#para cada variable
      #homocestads con bartlet
      h_test<-bartlett.test(data[,v]~global_factor)
      row2<-data.frame(Homoscedasticity=if(h_test$p.value<conf)"No"else("Yes"),
                       Homoscedasticity_p_value=round(h_test$p.value,3),
                       Homoscedasticity_statistic=h_test$statistic)
      rownames(row2)<-colnames(data)[v]
      results_h<-rbind(results_h, row2)
      
      if(h_test$p.value<conf){#haer eso de que pares no cumplen, pero con var.test
        #combinaciones de los factores:
        temp_results_h<-data.frame(Homoscedasticity=factor(levels = c("Yes","No"," ")),
                                  Homoscedasticity_p_value=numeric(),
                                  Homoscedasticity_statistic=numeric())
        
        perm_matrix<-combination(length(levels(global_factor)),2,levels(global_factor))

        for(fil in (1:dim(perm_matrix)[1])){
          var_test<-var.test(data[global_factor==perm_matrix[fil,1],v],
                             data[global_factor==perm_matrix[fil,2],v])
          temp_row_2<-data.frame(Homoscedasticity=if(var_test$p.value<conf)"No"else("Yes"),
                             Homoscedasticity_p_value=round(var_test$p.value,3),
                             Homoscedasticity_statistic=var_test$statistic)
          rownames(temp_row_2)<-paste(colnames(data)[v], paste(perm_matrix[fil,1],perm_matrix[fil,2],
                                                        sep = " Vs "),sep = " : ")
          temp_results_h<-rbind(temp_results_h,temp_row_2)}
        
        results_h<-rbind(results_h, temp_results_h[order(temp_results_h$Homoscedasticity_p_value,decreasing = T),])
        }
      
      for(f in levels(global_factor)){
      #normality, with shapiro
      n_test<-shapiro.test(data[global_factor==f,v])
      row<-data.frame(Cell_normality=if(n_test$p.value<conf)"No"else("Yes"),
                      Cell_normality_p_value=round(n_test$p.value,3),
                      Cell_normality_W_statistic=n_test$statistic)
      rownames(row)<-paste(colnames(data)[v], f)
      results_n<-rbind(results_n, row)

      }}
    warning("Faltan pruebas, no usar esta funcion para la vida real.")
    return(list(results_n,results_h))}}

#para probarla  

data('iris')
datos<-iris

#iristiene solo un factor, le voy a inventar otro
datos$season<-factor(sample(x = c("Verano","invierno","otonio"),size = length(iris$Sepal.Length),replace = T))
#la funcion  cell_tests necesita: 
#variables: vector de caracteresde los nombres de las columnas de variables
#variables: vector de caracteresde los nombres de las columnas de factores
#data:el data frame donde estan las columnas con los datos deinteres
#conf: la confianza para determinar la significancia de una prueba
#la funcion reregresa dos listas, la primera con la prueba de normalidad por variable por celda
#la segunda la prueba de homocedast por varible (con bartlet), si el valor p de la prueba 
#es menor a confianza, se incluye que pares de compraraciones de niveles de factor son
#tienen varianza diferente(con var.test)
str(datos)

resultados<-cell_tests(variables =c("Sepal.Length","Sepal.Width","Petal.Length"),
           factors = c("Species","season"),data = datos,conf = 0.05)

levels(datos$Species)

resultados[[1]]

View(resultados[[2]]) #como hay muchas comparaciones, se ve mejor en otra ventana

#algunas pruebas para que si es lo mismo
shapiro.test(datos$Petal.Length[datos$Species=="virginica"&datos$season=="Verano"])


bartlett.test(datos$Sepal.Width~droplevels(interaction(datos$Species,datos$season)))

var.test(datos$Sepal.Length[datos$Species=="virginica"&datos$season=="invierno"],
         datos$Sepal.Length[datos$Species=="virginica"&datos$season=="otonio"])

