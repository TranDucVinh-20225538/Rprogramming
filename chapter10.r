prst2 <- read.csv("https://goo.gl/BTxyFB")
#Q1
library(corrplot)
cor_matrix <- cor(prst2)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

#Q2
prst1 <- read.csv("https://goo.gl/z5P8ce")
factanal(prst1[ , 1:9], factors = 3)
prst.3factor <- "EaseOfUse  =~ Adaptable + Friendly + Helpful + 1.0 * Intuitive
                 Appealing  =~ 1.0* CuttingEdge + Delightful + Exciting + Generous
                 Functional =~ Adaptable + 1.0 * BestValue"
library(lavaan)
prst.3f.fit <- cfa(prst.3factor, data = prst1[ , 1:9], std.lv = TRUE)
summary(prst.3f.fit)
#Q3
library(semPlot)
semPaths(prst.3f.fit, what = "est", fade = FALSE, residuals = FALSE, 
         layout = "tree", structural = FALSE, nCharNodes = 7, edge.label.cex = 1)

#Q4
factanal(prst1[ , 1:9], factors = 2)
prst.2factor <- "EaseOfUse  =~ Adaptable + Friendly + Helpful + Intuitive
                Appealing  =~ CuttingEdge + Delightful + Exciting + Generous"
prst.2f.fit <- cfa(prst.2factor, data = prst2[ , 1:9], std.lv = TRUE)
summary(prst.2f.fit, fit.measures = TRUE)
semPaths(prst.2f.fit, what = "est", fade = FALSE, residuals = FALSE, 
          layout = "tree", structural = FALSE, nCharNodes = 7, edge.label.cex = 1)

#Q5
fit.measures.2f <- fitMeasures(prst.2f.fit)
fit.measures.3f <- fitMeasures(prst.3f.fit)
fit_comparison <- data.frame(
  Fit_Value = c("Chi-squared", "df", "p-value", "CFI", "TLI", "RMSEA"),
  Two_Factor = c(fit.measures.2f["chisq"], fit.measures.2f["df"], fit.measures.2f["pvalue"], fit.measures.2f["cfi"], fit.measures.2f["tli"], fit.measures.2f["rmsea"]),
  Three_Factor = c(fit.measures.3f["chisq"], fit.measures.3f["df"], fit.measures.3f["pvalue"], fit.measures.3f["cfi"], fit.measures.3f["tli"], fit.measures.3f["rmsea"])
)
fit.copmparison
#fit_comparison
#         Fit_Value Two_Factor Three_Factor
#chisq  Chi-squared 15.8274155   25.3977405
#df              df 19.0000000   26.0000000
#pvalue     p-value  0.6687602    0.4965608
#cfi            CFI  1.0000000    1.0000000
#tli            TLI  1.0126757    1.0020558
#rmsea        RMSEA  0.0000000    0.0000000

#2 factor model has lower chi-squared, df and higher p-value and tli 
#show that the 2 factor model is simpler and more suitable for the dataset than the 3 factor model


intent.df <- read.csv("https://goo.gl/6U5aYr")
#Q6
library(lavaan)
sem_model <- "
  # Biến tiềm ẩn
  ProductRating =~ iCuttingEdge + iEaseOfUse + iBestValue
  PurchaseInterest =~ ProductRating + iPreviousModelRating
  PurchaseIntent =~ PurchaseInterest + iCost + iPurchaseIntent
"
fit <- sem(sem_model, data = intent.df)
summary(fit, fit.measures=TRUE, standardized=TRUE)

#Q7
library(lavaan)
simple_sem_model <- "
  ProductRating =~ iBestValue + iEaseOfUse
  PurchaseIntent =~ ProductRating + iCost + iPurchaseIntent
"
simple_fit <- sem(simple_sem_model, data = intent.df)
summary(simple_fit, fit.measures=TRUE, standardized=TRUE)
library(semPlot)
semPaths(simple_fit, what="est", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=FALSE, nCharNodes=7, edge.label.cex=1)

#Q8
#ProductRating and PurchaseIntent with iBestValue and iCost
model_1 <- "
  ProductRating =~ iBestValue
  PurchaseIntent =~ ProductRating + iCost + iPurchaseIntent
"
fit_model_1 <- sem(model_1, data = intent.df)
summary(fit_model_1, fit.measures=TRUE, standardized=TRUE)

# without ProductRating
model_2 <- "
            PurchaseIntent =~ iCost + iPurchaseIntent + iBestValue + iEaseOfUse"

fit_model_2 <- sem(model_2, data = intent.df)
summary(fit_model_2, fit.measures=TRUE, standardized=TRUE)


#Q9
set.seed(123) 
sample_size <- 30
sample_data <- intent.df[sample(nrow(intent.df), sample_size), ]
simple_sem_model <- "
  ProductRating =~ iBestValue + iEaseOfUse
  PurchaseIntent =~ ProductRating + iCost + iPurchaseIntent
"
sample_fit <- sem(simple_sem_model, data = sample_data)
summary(sample_fit, fit.measures=TRUE, standardized=TRUE)
full_fit <- sem(simple_sem_model, data = intent.df)
summary(full_fit, fit.measures=TRUE, standardized=TRUE)

#Q10
library(plspm)
path_matrix <- rbind(
  c(0, 1, 0), 
  c(0, 0, 1),  
  c(0, 0, 0)  
)
outer_model <- list(
  ProductRating = c("iBestValue", "iEaseOfUse"),
  PurchaseIntent = c("iCost", "iPurchaseIntent")
)
pls_fit <- plspm(intent.df, path_matrix, outer_model, modes = rep("A", length(outer_model)))
summary(pls_fit)
sem_fit <- sem(simple_sem_model, data = intent.df)
summary(sem_fit, fit.measures=TRUE, standardized=TRUE)


#Q11
set.seed(10011)
sample.obs2 <- sample(nrow(intent.df), 200)
pls.data2   <- intent.df[sample.obs2, ]
str(pls.data2)

#Q12
library(plspm)
intentPLS.mod2 <- plspm(data=pls.data2, path_matrix=intentPLSsm, blocks=intentPLSmm, modes="A")
intentPLS.fit2 <- intentPLS.mod2
library(boot)
boot_function <- function(data, indices) {
    d <- data[indices, ] 
    fit <- plspm(d, path_matrix=intentPLSsm, blocks=intentPLSmm, modes="A")
    return(fit$path_coefs) 
}
set.seed(10011)
results <- boot(data=pls.data2, statistic=boot_function, R=200)
summary(results)

