prst1 <- read.csv("https://goo.gl/z5P8ce")

#Q1
summary(prst1)
#summary(prst1)
#   Adaptable       BestValue      CuttingEdge     Delightful       Exciting        Friendly        Generous        Helpful        Intuitive        Brand          
# Min.   :1.000   Min.   :1.000   Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000   Length:3050       
# 1st Qu.:4.000   1st Qu.:3.000   1st Qu.:3.00   1st Qu.:3.000   1st Qu.:3.000   1st Qu.:4.000   1st Qu.:3.000   1st Qu.:3.000   1st Qu.:3.000   Class :character  
# Median :4.000   Median :4.000   Median :4.00   Median :4.000   Median :4.000   Median :4.000   Median :4.000   Median :4.000   Median :4.000   Mode  :character  
# Mean   :4.255   Mean   :3.849   Mean   :4.07   Mean   :3.983   Mean   :3.892   Mean   :4.246   Mean   :4.031   Mean   :3.894   Mean   :3.724                     
# 3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.00   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:5.000   3rd Qu.:4.000                     
# Max.   :7.000   Max.   :7.000   Max.   :7.00   Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000   Max.   :7.000        
#since all the variables are on the same 1-7 scaling so it is not necessary to scale them

#Q2
prst1_1 <- prst1
prst1_1[, 1:9] <- scale(prst1[, 1:9])
library(psych)
describe(prst1_1[, 1:9])
#summary(prst1_1[, 1:9])
#   Adaptable         BestValue        CuttingEdge         Delightful          Exciting          Friendly          Generous           Helpful           Intuitive      
# Min.   :-3.3414   Min.   :-2.7323   Min.   :-2.88159   Min.   :-3.04151   Min.   :-2.8785   Min.   :-3.2141   Min.   :-3.20078   Min.   :-2.67745   Min.   :-2.8282  
# 1st Qu.:-0.2622   1st Qu.:-0.8139   1st Qu.:-1.00463   1st Qu.:-1.00202   1st Qu.:-0.8881   1st Qu.:-0.2435   1st Qu.:-1.08908   1st Qu.:-0.82696   1st Qu.:-0.7519  
 #Median :-0.2622   Median : 0.1453   Median :-0.06616   Median : 0.01772   Median : 0.1070   Median :-0.2435   Median :-0.03323   Median : 0.09829   Median : 0.2863  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000  
# 3rd Qu.: 0.7643   3rd Qu.: 1.1045   3rd Qu.: 0.87232   3rd Qu.: 1.03746   3rd Qu.: 1.1022   3rd Qu.: 0.7467   3rd Qu.: 1.02262   3rd Qu.: 1.02353   3rd Qu.: 0.2863  
 #Max.   : 2.8171   Max.   : 3.0228   Max.   : 2.74928   Max.   : 3.07695   Max.   : 3.0925   Max.   : 2.7271   Max.   : 3.13431   Max.   : 2.87403   Max.   : 3.4007  
# the rescalling does not change so much the distributions so that rescalling is optional but in my opinion it is not necessary to rescale

#Q3
library(coorplot)
corrplot(cor(prst1[ , 1:9]))
#it suggest there are 3 factors:
#adaptable, cutting edge, exciting
#generous, helpful, friendly
#intuitive, bestvalue

#Q4
prst1.mean <- aggregate(. ~ Brand, data=prst1, mean)
row.names(prst1.mean) <- prst1.mean$Brand
prst1.mean$Brand <- NULL
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(prst1.mean), 
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          main="\n\n\n\n\nBrand attributes", margins=c(8, 6))

#Q5
rst1.pc <- prcomp(prst1[, 1:9], center = TRUE, scale. = TRUE)
summary(prst1.pc)
plot(summary(prst1.pc)$importance[2, ], type = "b", 
     xlab = "Principal component", 
     ylab = "Proportion of variance ", 
     main = "Scree plot")

#Q6   
library(ggplot2)
library(dplyr) 
prst1.mean <- aggregate(. ~ Brand, data = prst1, mean)
row.names(prst1.mean) <- prst1.mean$Brand
prst1.mean$Brand <- NULL
prst1.mu.pc <- prcomp(prst1.mean, center = TRUE, scale. = TRUE)
biplot(prst1.mu.pc, main = "Biplot of brands against first two components")
biplot(prst1.mu.pc, choices = 2:3, main = "Biplot of brands against second and third components")
#interpret:
#x-axis captures the maximum variance in the dataset
# y-axis captures the second-highest variance
#brands which are close together are considered similar in terms of their ratings across the adjectives
#the longer arrow suggests that the corresponding adjective has a more influence on that principal component

# plot for second and third components
biplot(prst1.mu.pc, choices = 2:3, main = "Biplot of brands against second and third components")

#not all components have equal importance
#the first few components explain the majority of the variance
#but subsequent components still provide valuable insights
# it is crucial to examine the eigenvalues or variance explained by each component to determine their relevance

#Q８
library(nFactors)
nfactors(prst1[ , 1:9])
#there are 3 factors

#Q９
factanal(prst1[ , 1:9], factors=2)
factanal(prst1[ , 1:9], factors=3)
factanal(prst1[ , 1:9], factors=4)
library(psych)
fa(prst1[, 1:9], nfactors = 3, rotate = "varimax")
library(GPArotation)
factanal(prst1[ , 1:9], factors=3, rotation="oblimin")
#varimax is an appropriate choice since it provides clearer interpretation of each factor
#if the factors are expected to be correlated
#oblimin might be more suitable as it allows for correlations between factors

#Q１０
library(gplots)
library(RColorBrewer)
heatmap.2(prst1.fa$loadings, 
          col=brewer.pal(9, "Blues"), 
          trace="none", 
          key=FALSE, 
          dend="none",
          Colv=FALSE, 
          cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")
library(semPlot)
semPaths(prst1.fa, 
         what="est", 
         residuals=FALSE,
         cut=0.3, 
         posCol=c("white", "darkblue"), 
         negCol=c("white", "red"),
         edge.label.cex=0.75, 
         nCharNodes=7)

＃Q11
library(gplots)
library(RColorBrewer)
factor_scores <- factor.scores(prst1[, 1:9], prst1.fa)$scores
brand_scores <- data.frame(Brand = prst1$Brand, factor_scores)
mean_brand_scores <- aggregate(. ~ Brand, data=brand_scores, FUN=mean)
mean_brand_scores_matrix <- as.matrix(mean_brand_scores[, -1])
rownames(mean_brand_scores_matrix) <- mean_brand_scores$Brand
heatmap.2(mean_brand_scores_matrix, 
          col=brewer.pal(9, "GnBu"), 
          trace="none", 
          key=TRUE, 
          dend="none",
          main="Mean Factor Scores for Each Brand", 
          margins=c(10, 8))

#Q12
prst1.fa <- factanal(prst1[ , 1:9], factors=3, scores="Bartlett", rotation="varimax")
factor_scores <- factor.scores(prst1[, 1:9], prst1.fa)$scores
brand_scores <- data.frame(Brand = prst1$Brand, factor_scores)
mean_brand_scores <- aggregate(. ~ Brand, data=brand_scores, FUN=mean)
print(mean_brand_scores)
#mean factor are:
#Brand    Factor1    Factor2     Factor3
#1   Papa  0.5902862 -0.6880622 -0.12768387
#2  Romeo  0.1706279 -0.1600457 -0.02973335
#3 Sierra -0.2488066  0.2508970  0.02044974
#4  Tango -0.6108510  0.7060844  0.22632390

library(gplots)
library(RColorBrewer)
mean_brand_scores_matrix <- as.matrix(mean_brand_scores[, -1])
rownames(mean_brand_scores_matrix) <- mean_brand_scores$Brand
heatmap.2(mean_brand_scores_matrix, col=brewer.pal(9, "GnBu"), trace="none", 
          key=TRUE, dend="none",main="Mean Factor Scores for Each Brand", margins=c(10, 8))

#Q13
plot(prst1.mds, type="n")
text(prst1.mds, rownames(prst1.mds), cex=1.2)
