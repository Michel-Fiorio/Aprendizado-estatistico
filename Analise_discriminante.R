# Análise discriminante

##Analise dos dados Iris

require(MASS)
data(iris)
head(iris, 3)

##Validacao Cruzada (leave one out). Treina-se o modelo com n-1 dados e o avalia utilizando a informação restante.

# Funcao Discriminante Linear (onde considera a variância dos dados homogênea)
r <- lda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3, CV=TRUE)   # CV=TRUE representa a ativação da função validação cruzada 'leave one out'
                                        # prior: define as probabilidades para as populações. Nesse caso 1/3 p/ cada população.
#Obter classificacao
classificacao <- r$class

# MAtriz de confusão
cvl <- table(iris$Species,classificacao)
cvl

#Taxa de Erro Aparente
TEA <- 1 - (sum(diag(cvl))/sum(cvl))
TEA

# Funcao discrimante Quadratica (onde considera a variância dos dados heterogênea)

q <- qda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3, CV=TRUE)

classificacao <- q$class

# MAtriz de confusão
cvq <- table(iris$Species,q$class)
cvq


#Taxa de Erro Aparente
TEA <- 1 - (sum(diag(cvq))/sum(cvq))
TEA

