library(readxl)
library(tidyverse)
library(magrittr) 

Tasas<-function(estados,anio){
  
  indicadores<-read_xlsx("Indicadores demográficos (1).xlsx")
  
  edad<-c("00-05 años","06-11 años","12-14 años","15-17 años",
          "17-19 años","20-24 años","25-29 años","30-49 años",
          "50-64 años","64 años y mas")
  
  RepublicaPoblacion<-indicadores %>% filter(AÑO==anio,
                                             ENTIDAD=="República Mexicana") %>% 
    select(starts_with("P")) %>% t() %>% 
    as.data.frame()%>%cbind(edad) 
  colnames(RepublicaPoblacion)=c("Poblacion","Edades")
  
  RepublicaDefuncion<-indicadores %>% filter(AÑO==anio,
                                             ENTIDAD=="República Mexicana") %>% 
    select(starts_with("D")) %>% t() %>% 
    as.data.frame()%>%cbind(edad) 
  colnames(RepublicaDefuncion)=c("Defunciones","Edades")
  
  Republica<-Reduce(merge,list(RepublicaPoblacion,RepublicaDefuncion))
  Republica %<>%mutate(TasaEstandar=Defunciones/Poblacion*1000)  
 
  Tablas<-list()
  Tasas<-data.frame()
 
   for (i in 1:length(estados)) {
    edo<-indicadores %>% filter(AÑO==anio,ENTIDAD==estados[i])
    
    mujeres<-edo %>% select(starts_with("M")) %>%t() %>% 
      as.data.frame()%>%cbind(edad) 
    colnames(mujeres)=c("Mujeres","Edades")
    
    hombres<-edo %>% select(starts_with("H")) %>%t() %>% 
      as.data.frame()%>%cbind(edad) 
    colnames(hombres)=c("Hombres","Edades")
    
    poblacion<-edo %>% select(starts_with("P")) %>%t() %>% 
      as.data.frame()%>%cbind(edad) 
    colnames(poblacion)=c("Poblacion","Edades")
    
    defunciones<-edo %>% select(starts_with("D")) %>%t() %>% 
      as.data.frame()%>%cbind(edad) 
    colnames(defunciones)=c("Defunciones","Edades")
    
    Tablas[[estados[i]]]=Reduce(merge,
                                list(mujeres,hombres,poblacion,defunciones))
    Tablas[[estados[i]]] %<>% mutate(Defunciones_Esperadas=
                                       Poblacion*Republica$TasaEstandar/1000)
    Tasabruta<-(sum(Tablas[[estados[i]]]$Defunciones)/sum(
      Tablas[[estados[i]]]$Poblacion))*1000
    
    FactordeAjuste=sum(Tablas[[estados[i]]]$Defunciones)/sum(
      Tablas[[estados[i]]]$Defunciones_Esperadas)
    
  TasaEstandarizada=1000*sum(Republica$Defunciones)/sum(Republica$Poblacion)*FactordeAjuste
  Tasas %<>% rbind(c(Tasabruta,FactordeAjuste,TasaEstandarizada)) 
   }
  
  colnames(Tasas)=c("Tasa bruta","Factor de Ajuste","Tasa Estandarizada")
  rownames(Tasas)=estados
  
  Piramide<-list()
  for (i in 1:length(estados)) {
    ClasporSexo<-Tablas[[estados[i]]] %>% select(Edades,Mujeres,Hombres) %>% 
      pivot_longer(cols = c("Hombres","Mujeres"), 
                   names_to = "Sexo",
                   values_to = "Poblacion por Sexo")
    Piramide[[estados[i]]]=as.data.frame(ClasporSexo)
  }
  
  for (i in 1:length(estados)) {
    x=Piramide[[estados[i]]]
    plt <- ggplot(x, aes(x = `Edades`,
                         y = `Poblacion por Sexo`,
                         fill = Sexo))+
      # Seccion de HOMBRES
      geom_col(data = subset(x, Sexo == "Hombres") %>% 
                 # Convertimos los datos de los Hombres en negativos
                 mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
               width = 0.5, fill = "blue") +
      # Seccion de MUJERES
      geom_col(data = subset(x, Sexo == "Mujeres"),
               width = 0.5, fill = "pink") + 
      # Cambio de ejes de coordenadas
      coord_flip() + 
      scale_y_continuous(
        breaks = c(seq(-(max(x$`Poblacion por Sexo`)+10000), 
                       -50000, by = max(x$`Poblacion por Sexo`)/5), 
                   seq(0, max(x$`Poblacion por Sexo`)+10000, 
                       by = max(x$`Poblacion por Sexo`)/5)),
        labels = c(seq(-(max(x$`Poblacion por Sexo`)+10000), -50000, 
                       by = max(x$`Poblacion por Sexo`)/5) * -1, 
                   seq(0,max(x$`Poblacion por Sexo`)+10000, 
                       by =max(x$`Poblacion por Sexo`)/5)))+
      ggtitle(str_c(estados[i]," en el año ",anio))
    print(plt)
  }
  Tasas %<>%t() %>% as.data.frame() 
  return(Tasas)
}

estado<-c("Jalisco","México","Sinaloa","Tlaxcala")

Tasas(estado,2017)

