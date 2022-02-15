# LEER FICHERO OPTA F24
library(xml2)
library(tidyverse)
library(plyr)
library(ggrepel)
library(xslt)






leer_f24 <- function(xml.filename){
  
  #// Define functions to be used ---------------------------------------------------------//
      
      ## event parsing functions
    
    grab.the.qualifiers <- function(xlm.2.spread) {
    
      Value <- ifelse(is.na(Qualifiers.List["value"]), 1, Qualifiers.List["value"])
      temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
      colnames(temp) <- Qualifiers.List["qualifier_id"]
    
      return(bind_rows(results.temp, temp))
     
    }
    
    ## convert string to numeric 
    
    string_to_numeric <- function(x){as.numeric(as.character(x))}
    
    ## Pick the Maximum (non-NA) Values
    
    pick.out.the.maximum.values <- function(qualifier.values){
        
        max.values <- list()
        for (c in 1:NCOL(qualifier.values)) {
        col.2.test <- qualifier.values[,c]
        max.val <- col.2.test[!is.na(col.2.test)][1]
        max.values <- append(unlist(max.values), max.val)
        }
        results.Q <- t(as.data.frame(max.values))
        colnames(results.Q) <- colnames(qualifier.values)
        return(results.Q)
    }
    
    ## The Main Unpacking Function
    
    
    convert.event.node.row <- function(xml.2.spread){
      
        ## convert the info in the event node header into a dataframe 
        results <- as.data.frame(t(as.data.frame((xml.2.spread$attrs))))
        rownames(results) <- NULL
    
        ## find the number of qualifiers for this event 
        no.of.qualifiers <- lengths(xml.2.spread$value)
        
        if(no.of.qualifiers > 0){
        ## create a list of qualifiers 
        Qualifier.Unpacked.Step1 <- data.frame(stringsAsFactors = F)
      
        ## loop through each qualifer and pull out the info then bind it to the results .. above 
        for (Q in 1:no.of.qualifiers) {
        Qualifier.unpacked <- unlist(xml.2.spread$value[1][[1]][Q])
        Value <- ifelse(is.na(Qualifier.unpacked["value"]), 1, Qualifier.unpacked["value"])
        temp <- data.frame(Q = as.character(Value), stringsAsFactors = F)
        colnames(temp) <- Qualifier.unpacked["qualifier_id"]
        Qualifier.Unpacked.Step1 <- bind_rows(Qualifier.Unpacked.Step1, temp)
        }
        
        ## keep the maximum values in the dataframe (the only none NA values) return as a 
        ## dataframe for use 
         Qualifier.unpacked.df <- pick.out.the.maximum.values(Qualifier.Unpacked.Step1)
         rownames(Qualifier.unpacked.df) <- NULL  
        
        #Qualifier.Unpacked.Step1[1,] <- Qualifier.Unpacked.Step1[is.not.na(Qualifier.Unpacked.Step1)]
        #Qualifier.unpacked.df <- as.data.frame(Qualifier.Unpacked.Step1[1,], stringsAsFactors = F)
        results <- cbind(results, Qualifier.unpacked.df)}
        
        return(results)
    } # end of function 
    
      
  #// Read in the XML File ----------------------------------------------------------------//
  
  pbpParse <- read_xml(xml.filename, encoding = "", as_html = TRUE, options = "NOERROR")
  
  
  #// Spilt the XML File ------------------------------------------------------------------//

    all.event.nodes <- pbpParse %>% 
        xml_find_all('//event') %>% 
        map_df(~list(attrs = list(xml_attrs(.x)), value = list(map(xml_children(.x), xml_attrs))))

  #// Convert all evvents and store in a dataframe ----------------------------------------//

    events <- all.event.nodes %>% 
      split(1:nrow(.)) %>% 
      purrr::map(convert.event.node.row) %>% 
      dplyr::bind_rows()
      
    
  #// convert strings to numerics ---------------------------------------------------------//
    
    events$min     <- string_to_numeric(events$min)
    events$sec     <- string_to_numeric(events$sec)
    events$x       <- string_to_numeric(events$x)
    events$y       <- string_to_numeric(events$y)
	
	# Pass ends
    events$`140`   <- string_to_numeric(events$`140`) # pass end x
    events$`141`   <- string_to_numeric(events$`141`) # pass end y
	# Angles in radians and Lengths
    events$`213`   <- string_to_numeric(events$`213`) # angle in radians
    events$`212`   <- string_to_numeric(events$`212`) # length of ball travel
	
	# GK Coordinates X Y
	events$`230`   <- string_to_numeric(events$`230`) # GK X Coordinate
  events$`231`   <- string_to_numeric(events$`231`) # GK y Coordinate
	
	# GoalMouth Coordinates Y Z
	 events$`102`   <- string_to_numeric(events$`102`) # Goal mouth y co-ordinate
    events$`103`   <- string_to_numeric(events$`103`) # Goal mouth z co-ordinate

	
    events$outcome <- string_to_numeric(events$outcome)  # success yes or no 0 1
    
  #// Return the resulting dataframe -----------------------------------------------------//


    return(events)
    
} # end of parse.f24 function 
   

# Leer PLANTILLAS 2020-2021
x <- read_xml("srml-23-2020-squads.xml")

#nuevo codigo inicio
style <- read_xml("Style.xsl",package="xslt")

# TRANSFORM
# library("xslt") CARGAR LIBRERIA XSLT e instalar nueva librería
new_xml <- xml_xslt(x,style)



recs <-  xml_find_all(new_xml,"//Player")

df_list <- lapply(recs,function(r)
    data.frame(rbind(setNames(xml_text(xml_children(r)),
                              xml_name(xml_children(r)))),
               stringsAsFactors = FALSE))
                                  

jugadores <- bind_rows(df_list)

jugadores <- select(jugadores,uID,Name,Position, country,loan)
                    


# reemplazar p por blanco 
jugadores$uID <- gsub("p", "", jugadores$uID)

# cambiar el nombre de la columna
names(jugadores)[names(jugadores) == 'uID'] <- 'player_id'



##  FICHERO F24

eventos_38 <- leer_f24("f24-23-2020-2136571-eventdetails.xml")

# CRUZAR LOS EVENTOS Y LOS JUGADORES PARA INCLUIR LAS COLUMNAS DE JUGADOR
eventos_38 <-merge(eventos_38,jugadores, all.x=TRUE)

# CREAR NUEVA COLUMNA RESULTADO
eventos_38$resultado <- if_else(eventos_38$outcome== 0, "No Completado","Completado")

# EXPORTAR EL FICHERO DE EVENTOS
write.table(eventos_38,file='J38.csv',sep=';',row.names = F)

