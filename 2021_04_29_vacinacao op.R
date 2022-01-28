setwd("D:/Desktop/Consultorias/covid")
library(tidyverse)
library(scales)
library(data.table)
library(ggfx)
library(ggthemes)
library(gridExtra)
library(grid)


vacinacao <- fread("vacinacao.csv")
vacinacao <- vacinacao %>%
  select(3,5,7,10,19,26,29)

pop_op <- readxl::read_xlsx("POP_OP_ESTIMADA_2020.xlsx")
pop_op$Populacao<- round(pop_op$Populacao, 0)

vacinacao <-vacinacao%>%
  dplyr::rename("cidade" = estabelecimento_municipio_nome,
                "sexo" = "paciente_enumSexoBiologico",
                "dose" = "vacina_descricao_dose",
                "vacina" = "vacina_fabricante_nome",
                "raca" = "paciente_racaCor_valor")

vacina_op<-vacinacao%>%
  filter(cidade == "OURO PRETO")

vacina_op$dose <- iconv(vacina_op$dose,from="UTF-8",to="ASCII//TRANSLIT")




vacina_op<-vacina_op%>%
  mutate(Faixa_Etaria = ifelse(paciente_idade>80, "80+", ifelse(paciente_idade>75, "75-79", ifelse(paciente_idade>70, "70-74", ifelse(paciente_idade>65, "65-69", ifelse(paciente_idade>60, "60-64", ifelse(paciente_idade>55, "55-59",ifelse(paciente_idade>50, "50-54", ifelse(paciente_idade>45, "45-49", ifelse(paciente_idade>40, "40-44", ifelse(paciente_idade>35, "35-39", ifelse(paciente_idade>30, "30-34", ifelse(paciente_idade>25, "25-29", ifelse(paciente_idade>20, "20-24", ">20"))))))))))))))%>%
  select(-paciente_idade)


pop_op <- pop_op%>%
  group_by(sexo)



vacinacao_op_clean <- vacina_op %>%
  group_by(cidade, sexo, Faixa_Etaria, dose)%>%
  count()%>%
  spread(dose, n)%>%
  full_join(pop_op)%>%
  gather(Individuo, quantidade, 4,5,6)%>%
  filter(Faixa_Etaria != ">20")

 vacinacao_op_clean <-vacinacao_op_clean %>%
  group_by(sexo, Individuo, Faixa_Etaria)%>%
   filter(!is.na(quantidade))%>%
   summarise(quantidade = sum(quantidade))%>%
   mutate(porcentagem = round(quantidade/sum(quantidade)*100,2))

plot <- ggplot()+
  as_group(
    geom_col(data = vacinacao_op_clean%>%
               filter(sexo == "M"), aes( x = Faixa_Etaria, y = quantidade, fill = sexo  , alpha = Individuo), position ="identity", width = 0.9835, colour = F ),
    geom_col(data = vacinacao_op_clean%>%
               filter(sexo == "F"), aes( x = Faixa_Etaria, y = -quantidade, fill = sexo, alpha = Individuo),  position ="identity", width = 0.9835, colour = F)
  )+
  coord_flip()+
  scale_fill_manual(values = c("red", "blue"), labels= c("Feminino", "Masculino"))+
  scale_alpha_manual(values=c(0.4,1,0.35),guide=F)+
  scale_y_continuous(breaks=seq(-3500, 3500, 500), labels=abs(seq(-3500,3500,by=500)))+
  labs( title = "Vacinação por faixa etária em Ouro Preto - Minas Gerais",
        subtitle = "Cor escura 2ª Dose; Intermediaria 1ª Dose; Clara População Geral ",
        x = "Faixa Etária",
        y = "")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank())
  grid.arrange(plot + labs(caption="Fontes: DataSUS e IBGE"),
                                                 bottom = textGrob("Autor:João Lúcio Resende", x = 1,
                                                                   hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))
