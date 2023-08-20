# Análise do conjunto de dados BreastCancer

#**Tarefa 1**. Utilize o conjunto **BreastCancer** para realizar análises de reconhecimento de padrões. 
#*Para tanto suponha que a variável **Class** (última variável do conjunto) não é conhecida. 
#*Faça um relatório. Tal relatório deve ser entregue de maneira organizada discutindo os resultados encontrados.

#install.packages("mlbench")
library(mlbench)

# Obter o arquivo de dados
data(BreastCancer)

bc = BreastCancer

# Avaliar o conteúdo do arquivo.
summary(bc)
head(bc)
tail(bc)

# Há dados faltantes?
anyNA(bc)

# Remover dados NA
bc = na.omit(bc)
summary(bc)

# Tipo de dados no data frame
str(bc)

# Converter de factor p/ numeric
bc$Cl.thickness = as.numeric(bc$Cl.thickness)
bc$Cell.size = as.numeric(bc$Cell.size)
bc$Cell.shape = as.numeric(bc$Cell.shape)
bc$Marg.adhesion = as.numeric(bc$Marg.adhesion)
bc$Epith.c.size = as.numeric(bc$Epith.c.size)
bc$Bare.nuclei = as.numeric(bc$Bare.nuclei)
bc$Bl.cromatin = as.numeric(bc$Bl.cromatin)
bc$Normal.nucleoli = as.numeric(bc$Normal.nucleoli)
bc$Mitoses = as.numeric(bc$Mitoses)

str(bc)
  
# Análise PCA com função prcomp
bc.pca = prcomp(bc[,2:10], center=TRUE, scale=FALSE)
summary(bc.pca)
names(bc.pca)
str(bc.pca)

# Plotando resultado do PCA
biplot(bc.pca)
abline(a=0,b=0)
abline(h=0,v=0)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(bc.pca)
ggbiplot(bc.pca, groups = bc$Class)
ggbiplot(bc.pca, groups = bc$Class, ellipse = TRUE)

# Análise PCA com função princomp
bc.pca1 = princomp(bc[,2:10])
summary(bc.pca1)
names(bc.pca1)
str(bc.pca1)

# Plotando resultado do PCA
biplot(bc.pca1)
abline(a=0,b=0)
abline(h=0,v=0)

# Análise PCA com função PCA da biblioteca FactoMineR e gráfico da biblioteca 

library(FactoMineR)

bc.pca2 = PCA(bc[,2:10])
summary(bc.pca2)
names(bc.pca2)
str(bc.pca2)

library("factoextra")

fviz_pca(bc.pca2, geom.ind = "point", title = "Componentes Principais do conjunto Breast Cancer")

fviz_eig(bc.pca2, addlabels = TRUE, xlab = "Componentes principais", ylab = "Porcentagem" ,title = "Porcentagem dos dados explicados por cada CP.")



# Análise de agrupamento considerando a distância euclidiana quadrática

# Distância euclidiana quadrática
dist.bc = dist(bc[,2:10], method = "euclidean")^2

# Criando o dendrograma - agrupamento hierárquico
agrupamento.bc = hclust(dist.bc, method = "average")
summary(agrupamento.bc)
names(bc.pca2)

dendograma.bc <- as.dendrogram(agrupamento.bc)
plot(dendograma.bc,xlab="",ylab="Distância Euclidiana Quadrática",main="Método do vizinho mais próximo")

rect.hclust(agrupamento.bc, k = 2)


# Tarefa 2. Utilizando o mesmo conjunto de dados BreastCancer realize análises de classificação. 
# Para tanto, a variável Class (última variável do conjunto) é nossa variável dependente. 
# Utilize validação cruzada para avaliar os modelos ajustados. Faça um relatório. 
# Tal relatório deve ser entregue de maneira organizada discutindo os resultados encontrados.

# Realizando análise do banco dados Breast Cancer com Análise Discriminante Linear

library(MASS)

discrilinear.bc = lda(Class ~ ., data = bc[,2:11], prior = c(1,1)/2 , CV = TRUE)
str(discrilinear.bc)
discrilinear.bc
summary(discrilinear.bc)

# Obter a classificação resultante do modelo para o conjunto de dados

Classif.linear = discrilinear.bc$class
summary(Classif.linear)

# Gerar a matriz de confusão do modelo

confusao.linear = table(bc$Class, Classif.linear)
confusao.linear

# Calculando taxa de erro aparente do modelo

tea.linear = 1 - (sum(diag(confusao.linear))/sum(confusao.linear))
tea.linear


# Realizando análise do banco dados Breast Cancer com Análise Discriminante Quadratica

discriquad.bc = qda(Class ~ ., data = bc[,2:11], CV = TRUE)
str(discriquad.bc)
discriquad.bc
summary(discriquad.bc)

# Obter a classificação resultante do modelo para o conjunto de dados

Classif.quad = discriquad.bc$class
summary(Classif.quad)

# Gerar a matriz de confusão do modelo

confusao.quad = table(bc$Class, Classif.quad)
confusao.quad

# Calculando taxa de erro aparente do modelo

tea.quad = 1 - (sum(diag(confusao.quad))/sum(confusao.quad))
tea.quad



# Tarefa 3. Utilize o conjunto Boston Housing Dataset predizer o valor médio das casas ocupadas 
# pelo proprietário (medv) por meio de técnicas de predição. Faça um relatório. 
# Tal relatório deve ser estregue de maneira organizada discutindo os resultados encontrados.

# Carregando pacotes
library(tree)
library(MASS)
library(randomForest)

# Obtendo e analisando as características do banco de dados
?Boston
dados.boston = Boston
summary(dados.boston)
head(dados.boston)

# Há dados faltantes?
anyNA(dados.boston)

#Particionar o conjunto de dados (Treinamento e validacao)
set.seed(1)
treinamento.boston = sample (1:nrow(dados.boston), nrow(dados.boston)/2)

#Ajuste da árvore de regressão usando random Forest com os dados de treinamento
arvore.boston = randomForest(medv ~ .,data=dados.boston , subset=treinamento.boston ,
                      mtry=4, importance =TRUE)
arvore.boston

# Medida de qualidade do modelo com dados de teste. Cálculo da raiz do erro quadrático médio.
predicao.boston = predict(arvore.boston,newdata=dados.boston[-treinamento.boston,])
teste.boston=dados.boston[-treinamento.boston ,"medv"]
REQM_arvore.boston <- sqrt(mean((predicao.boston-teste.boston)^2))
REQM_arvore.boston

# Ajuste da árvore de regressão usando todo o conjunto dos dados
arvore.bostonf = randomForest(medv ~ .,data=dados.boston ,
                             mtry=4, importance =TRUE)
arvore.bostonf

# Medida de qualidade do modelo usando todo o conjunto dos dados. Cálculo da raiz do erro quadrático médio.
predicao.bostonf = predict(arvore.bostonf,newdata=dados.boston)
teste.bostonf=dados.boston[,"medv"]
REQM_arvore.bostonf <- sqrt(mean((predicao.bostonf-teste.bostonf)^2))
REQM_arvore.bostonf


# Criação do modelo usando a técnica de regressão linear múltipla. Usando dados de treinamento.
regressão.boston = lm(medv ~ ., data = dados.boston, subset = treinamento.boston)
summary(regressão.boston)

# Medida de qualidade do modelo com dados de teste. Cálculo da raiz do erro quadrático médio.
predicao.regressão.boston = predict(regressão.boston,newdata=dados.boston[-treinamento.boston,])
REQM_regressão.boston <- sqrt(mean((predicao.regressão.boston-teste.boston)^2))
REQM_regressão.boston

# Criação do modelo usando a técnica de regressão linear múltipla. Usando dados completos.
regressão.bostonf = lm(medv ~ ., data = dados.boston)
summary(regressão.bostonf)

# Medida de qualidade do modelo com dados de teste. Cálculo da raiz do erro quadrático médio.
predicao.regressão.bostonf = predict(regressão.bostonf,newdata=dados.boston)
REQM_regressão.bostonf <- sqrt(mean((predicao.regressão.bostonf-teste.boston)^2))
REQM_regressão.bostonf

