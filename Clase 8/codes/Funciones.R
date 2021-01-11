
# Funcion que devueleve un vector con 'N' caracteres aleatorios de una palabra 
caracter_vector <- function(Palabra,Numero){
              
    # Generamos un vector tan largo como 'Numero' que contiene numeros aleatorios entre 1 y el numero maximo de caracteres de 'Palabra' 
    X <- round(runif(Numero, min=1, max=nchar(Palabra)),0)
    
    # Generamos un vector que va a alojar las palabra y es tan largo como el vector 'X'
    vector_final <- vector(mode="character", length=length(X))
    
    # Si la palabra tienes mas caracteres que el numero de caracteres que le pedimos 
    if (Numero < nchar(Palabra)) {
       for(i in 1:length(X)) {
             vector_final[i] <- substr(Palabra,X[i],X[i])
       } 
    return(vector_final)  
    }    
    
    # la palabra tienes el mismo numero de caracteres que el numero de caracteres que le pedimos  
    else if (Numero == nchar(Palabra)) {
            warning(paste0("Advertencia! El numero ", Numero," es igual al numero de caracteres de la palabra '",Palabra,"'"))
            for(j in 1:length(X)) {
                  vector_final[j] <- substr(Palabra,X[j],X[j])
           } 
    return(vector_final)
    }
    
     # la palabra tienes menos caracteres que el numero de caracteres que le pedimos          
     else {
          stop(paste0("Error! El numero ", Numero," es mayor al numero de caracteres de la palabra '",Palabra,"'")) 
     }
}


# Funcion para extraer los caracteres nonumericos de una variable
extrac_nonumeric_vector <- function(variable){
        print("Funcion para extraer un vector con los caracteres no numericos de una variable")
        X <- unique(variable) %>% as.data.frame(.)
        for(i in 0:9){
            X[,1] <- gsub(as.character(i),"",X[,1])  
        }
        Y <- unique(X[,1]) %>% .[!. %in% c("",NA)]
return(Y)
}

# Funcion para extraer informacion de las bases de datos
descriptive_function <- function(database){
                        print("Funcion para extraer las descriptivas de todas las variable de un dataframe")
                        counter = 1
                        list_variables <- list()
                        database <- database %>% as.data.frame(.)
                        for (i in colnames(database)){
                            variable <- as.character(i)
                            tipo <- as.character(class(database[,i]))
                            descriptive <- sort(unique(database[,i])) %>% gsub("--","",.) %>% gsub("  "," ",.)
                            descriptive <- descriptive[!descriptive %in% c("","-","."," ","NA")] %>% .[1:5] %>% 
                                           .[!. %in% NA] %>% paste(., collapse = " ; ")
                            list_variables[[counter]] <- data.frame(variable,tipo,descriptive)
                            counter = counter + 1
                        }
                        variables <- rbindlist(list_variables,fill=TRUE, idcol=NULL)
                        return(variables)  
}
