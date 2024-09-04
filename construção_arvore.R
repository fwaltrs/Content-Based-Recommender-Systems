library(rpart)
library(dplyr)
library(rpart.plot)

movies=read.csv("D:/One Drive - Ativo/OneDrive - StepWise/Estudos/FB_recomendação/movies.csv",sep=",")
ratings=read.csv("D:/One Drive - Ativo/OneDrive - StepWise/Estudos/FB_recomendação/ratings.csv",sep=",")
tags=read.csv("D:/One Drive - Ativo/OneDrive - StepWise/Estudos/FB_recomendação/tags.csv",sep=",")


movies_tags=merge(movies,tags,by=c("movieId"))
movies_tags_rating2=merge(movies_tags,ratings,by=c("userId","movieId","timestamp"))


library(tidyr)
df_separado <- movies_tags_rating2 %>% 
  mutate(id = row_number()) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(userId,movieId) %>%
  mutate(coluna_number = row_number()) %>%
  pivot_wider(names_from = coluna_number, values_from = genres, names_prefix = "Genero")


df_separado <- df_separado %>% 
  mutate(id = row_number()) %>%
  separate_rows(tag, sep = "\\|") %>%
  group_by(userId,movieId) %>%
  mutate(coluna_number = row_number()) %>%
  pivot_wider(names_from = coluna_number, values_from = tag, names_prefix = "Tag")



df_separado = df_separado %>% select(rating,Genero1,Genero2,Genero3,Genero4,
                                     tag)


divisao=sample(c(rep('Treino',60),rep('Teste',23)))
df_separado$amostra = divisao

df_separado = df_separado %>% mutate(Genero1 = as.factor(Genero1),
                                     Genero2 = as.factor(Genero2),
                                     Genero3 = as.factor(Genero3),
                                     Genero4 = as.factor(Genero4),
                                     tag = as.factor(tag))

treino = df_separado[df_separado$amostra=="Treino", -c(1,2,9)]
teste = df_separado[df_separado$amostra=="Teste",-c(1,2,9)]



set.seed(10)
arvore=rpart(formula = rating ~ ., 
                       data = treino, 
                       method = "anova")


melhor_cp <- arvore$cptable[which.min(arvore$cptable[, "xerror"]),
                         "CP"]
pfit <- rpart::prune(arvore, cp = melhor_cp)
pred_arvore <- predict(pfit,
                       teste)

erro = mean((teste$rating - as.numeric(pred_arvore))^2) #0.02470238
rpart.plot(pfit,cex=0.7)



