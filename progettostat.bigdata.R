library(haven)
library(pdftools)
library(ggplot2) 
library(plotly)
library(corrplot)
library(factoextra)
library(ISLR2)
library(precrec)
library(PRROC)
library(ROCR)
library(xtable)
library(lmtest) 
library(regclass)
library(lattice)  
library(cluster)
library(broom)
library(relaimpo)
library(Hmisc)
library(scales)
library(reshape2)

write.csv(m4, "DocumentazioniDati.csv")
summary(m4) 

data.EX <- na.omit(m4[, c(  "Y.x","carta","bancomat","cartapre","spesecon")])
colnames(data.EX) 
dim(data.EX) #mi dice le dimensioni dell'oggetto 
str(data.EX) #mostra la struttura interna dell'oggetto


                             # REGRESSIONE LINEARE SEMPLICE
model_EX = lm(formula = Y.x ~ carta, data = data.EX )
model_EX2 = lm(formula = Y.x ~ bancomat, data = data.EX )
model_EX3 = lm(formula = Y.x ~ cartapre, data = data.EX )
model_EX4 = lm(formula = Y.x ~ spesecon, data = data.EX )

ggplot(data.EX, aes(x = factor(carta), y = Y.x)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  labs(x = "Possesso Carta", y = "Reddito Netto (€)", 
    title = "Reddito netto per possesso carta di credito") +
  scale_x_discrete(labels = c("1" = "Sì", "2" = "No")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

ggplot(data.EX, aes(x = factor(carta), y = Y.x)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "darkred") +
  labs(x = "Possesso Carta", y = "Reddito Netto", title = "Modello di regressione: reddito vs carta") +
  scale_x_discrete(labels = c("1" = "Sì", "2" = "No")) +
  theme_minimal()

ggplot(data.EX, aes(x = factor(bancomat), y = Y.x)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(x = "Possesso Bancomat", y = "Reddito Netto", title = "Reddito netto per possesso di bancomat") +
  scale_x_discrete(labels = c("1" = "Sì", "2" = "No")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

ggplot(data.EX, aes(x = factor(bancomat), y = Y.x)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "darkred") +
  labs(x = "Possesso Bancomat", y = "Reddito Netto", title = "Modello di regressione: reddito vs bancomat") +
  scale_x_discrete(labels = c("1" = "Sì", "2" = "No")) +
  theme_minimal()

ggplot(data.EX, aes(x = factor(cartapre), y = Y.x)) +
  geom_boxplot(fill = "darkorange", alpha = 0.7) +
  labs(x = "Possesso Carta Prepagata", y = "Reddito Netto", title = "Reddito netto per possesso di carta prepagata") +
  scale_x_discrete(labels = c("1" = "Sì", "2" = "No")) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

ggplot(data.EX, aes(x = factor(cartapre), y = Y.x)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "blue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "darkred") +
  labs(x = "Possesso Carta Prepagata", y = "Reddito Netto", title = "Modello di regressione: reddito vs carta prepagata") +
  scale_x_discrete(labels = c("1" = "Sì", "2" = "No")) +
  theme_minimal()

breaks <- c(-50000, 0, 100000, 200000, 300000, 500000)
labels <- c("-50K–0", "0–100K", "100K–200K", "200K–300K", ">300K")
data.EX$reddito_cat <- cut(data.EX$Y.x,  breaks = breaks,
                           labels = labels,)
ggplot(data.EX, aes(x = reddito_cat, y = spesecon)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Fasce di Reddito Netto", y = "Spese in Contanti",
       title = "Spese in contanti per fascia di reddito") +
  theme_minimal()

summary(model_EX)
summary(model_EX2)
summary(model_EX3)
summary(model_EX4)

n = nrow(data.EX) 
SSTOT = var(data.EX$Y.x) * (n-1) 
SSR = sum((model_EX$fitted.values - mean (data.EX$Y.x))^2)
SSE = sum(model_EX$residuals^2) 
r.squared = SSR/SSTOT 

                                #REGRESSIONE MULTIPLA
data.RM <- na.omit(m4[,c("Y.x","bancomat","cartapre","carta")])
mod_multiple <- lm(Y.x ~ carta + bancomat + cartapre  , data= data.RM)
summary(mod_multiple)
summary(mod_multiple)$adj.r.squared
par(mfrow = c(2, 2))
plot(mod_multiple)

rel_imp <- calc.relimp(mod_multiple, type = "lmg", rela = TRUE)

rel_df <- as.data.frame(rel_imp@lmg)
rel_df$Variabile <- rownames(rel_df)
colnames(rel_df)[1] <- "Importanza"
rel_df$Importanza <- rel_df$Importanza * 100

ggplot(rel_df, aes(x = reorder(Variabile, Importanza), y = Importanza)) +
  geom_col(fill = "darkcyan") +
  geom_text(aes(label = paste0(round(Importanza, 1), "%")),
            hjust = -0.1, color = "black", size = 3.5) +
  coord_flip() +
  labs(x = "Variabili", y = "Importanza relativa (%)",
       title = "Contributo percentuale alla spiegazione del reddito") +
  theme_minimal() +
  ylim(0, max(rel_df$Importanza) * 1.15)

                                     # REGRESSIONE LOGISTICA
m4$carta <- factor(m4$carta, levels = c(1, 2), labels = c("possiede", "non possiede"))
carta_binaria <- as.numeric(m4$carta == "possiede")  
table(carta_binaria, m4$carta) 
mod1=glm(carta_binaria~Y.x + C,data=m4,family = binomial) 
summary(mod1)
mod1$coefficients[1] 
exp(mod1$coefficients[1]) 

reddito_seq <- seq(min(m4$Y.x), max(m4$Y.x), length.out = 100)
consumo_medio <- mean(m4$C, na.rm = TRUE)
df_pred <- data.frame(Y.x = reddito_seq, C = consumo_medio)
df_pred$prob <- predict(mod1, newdata = df_pred, type = "response")
ggplot(df_pred, aes(x = Y.x, y = prob)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  labs(x = "Reddito Netto", y = "Probabilità di possedere una carta",
       title = "Curva logistica: possesso carta in funzione del reddito",
       subtitle = paste("Consumo fissato a", round(consumo_medio, 0))) +
  scale_x_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()

predict.probs = predict(mod1,type="response") 
head(predict.probs)
mod1$fitted.values 
p.soglia    = 0.3
predict.class = ifelse(predict.probs>=p.soglia,1,0) 
conf.mat = table(Predetti=predict.class,
                 Osservati = carta_binaria) 
TN = conf.mat[1,1]
TP = conf.mat[2,2]
FN = conf.mat[1,2]
FP = conf.mat[2,1]
conf.mat
sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
table(m4$carta)

predict.class <- factor(predict.class, levels = c(0, 1), labels = c("non possiede", "possiede"))
boxplot(predict.probs~predict.class);abline(h = 0.3, col = "red")

ROCRpred = prediction(predict.probs,carta_binaria) 
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr") 
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1)) 
performance(ROCRpred, measure = "auc")@y.values[[1]]

                                     #CLUSTER
#ricaricare il i dati del progetto
load("/Users/Ricca/Downloads/DatiProgetto.rda")
m4$carta    <- ifelse(m4$carta == 1, 1, 0)
m4$bancomat <- ifelse(m4$bancomat == 1, 1, 0)
m4$cartapre <- ifelse(m4$cartapre == 1, 1, 0)

data.CL <- na.omit(m4[, c("ireg","carta", "bancomat", "cartapre")])
colnames(data.CL) 
dim(data.CL) 
str(data.CL) 
class(data.CL)
summary(data.CL)
colSums(is.na(data.CL))
data.CL$area_geografica <- cut(data.CL$ireg, breaks = c(0, 8, 14, 18, 20), 
                               labels = c("Nord", "Centro", "Sud", "Isole") 
)
CL_scaled <- scale(data.CL[, c("carta", "bancomat", "cartapre")])

fviz_nbclust(CL_scaled, kmeans, method = "silhouette",k.max = 6)
fviz_nbclust(CL_scaled, kmeans, method = "wss", k.max = 6)

CL_cluster <- kmeans(CL_scaled, centers = 3, iter.max = 100, nstart = 10)
names(CL_cluster)
data.CL$cluster <- CL_cluster$cluster
distances = get_dist(CL_scaled,method="euclidean")
sil_k_means = silhouette(CL_cluster$cluster,distances)
cbind(area = as.character(data.CL$area_geografica), sil_k_means)
fviz_silhouette(sil_k_means)
table(data.CL$area_geografica, data.CL$cluster)
risultato <- aggregate(. ~ cluster, data = data.CL[, c("carta", "bancomat", "cartapre", "cluster")], mean)
print(risultato, row.names = FALSE)
data.CL$etichetta_cluster <- factor(data.CL$cluster,
                                    levels = c(1, 2, 3),
                                    labels = c("Multi-servizio", "Bancomat-centrici", "Accesso ridotto")
)
ggplot(data.CL, aes(x = factor(area_geografica))) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ etichetta_cluster) +
  labs(x = "Area geografica", y = "N. osservazioni", title = "Distribuzione regionale per tipologia di utente") +
  theme_minimal()
prop.table(table(data.CL$etichetta_cluster))
dati_long <- melt(risultato, id.vars = "cluster")
ggplot(dati_long, aes(x = variable, y = value, fill = factor(cluster))) +
  geom_col(position = "dodge") +
  labs( x = "Tipologia di carta", y = "Valore medio", fill = "Cluster",
    title = "Comportamenti di possesso per tipo di carta nei diversi gruppi"
  ) +
  theme_minimal()
