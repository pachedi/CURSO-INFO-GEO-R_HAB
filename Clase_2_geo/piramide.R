


piramide <- function(base, bpop=NULL){

sexo_edad <- base %>% 
  select("P02","P03")

sexo_edad <- sexo_edad %>% 
  mutate(P02 = case_when(P02 == 1~ "M",
                         P02 == 2~ "F" ) )


ciudad_barrio <- c("en la ciudad formal", "en los barrios populares")

if(bpop == "si"){
  
  ciudad_barrio = ciudad_barrio[2]
} else if ( bpop == "no"){
  
  ciudad_barrio = ciudad_barrio[1]
  
} else{
  
  ciudad_barrio == ""
  
}


sexo_edad <- sexo_edad %>% 
  rename(Sexo = 1, Edad = 2)

pir <- sexo_edad %>% 
  group_by(Sexo) %>% 
  count(Edad)

pir <- pir  %>% 
  rename(Poblacion = n)


ggplot(pir, aes(x = Edad, color=Sexo,fill=Sexo,
                y = ifelse(test = Sexo == "M",
                           yes = -Poblacion, no = Poblacion))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(pir$Poblacion) * c(-1,1)) +
  scale_x_continuous(breaks = seq(0,110, by=20))+
  labs(x="Edad", y = "Frecuencia",
       title = paste("Pirámide poblacional de Quilmes","\n",ciudad_barrio),
       subtitle = "Provincia de Buenos Aires, Argentina",
       caption= "Fuente: Elaboración propia en base a datos\n del Censo Nacional de hogares y viviendas Argentina 2010")+
  scale_color_manual(name= "Género",
                     values= c("F" = "red",
                               "M" = "blue"))+
  scale_fill_manual(name= "Género",
                     values= c("F" = "red",
                               "M" = "blue"))+
  theme_minimal()+
  coord_flip()

}




