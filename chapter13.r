sportscar <- read.csv("https://goo.gl/8g7vtT")
sportscar$seat <- as.factor(sportscar$seat)

#Q1a
summary(sportscar)

#Q1b
tail(sportscar) 
#30

#Q1c
xtabs(~ trans + choice, data = sportscar)

#        choice
#trans       0    1
#  auto   1673 1328
#  manual 2327  672

#comsumers prefer auto car to manual car

#Q2a
install.packages("mlogit")
install.packages("dfidx")
library(mlogit)
sportscar_mlogit <- mlogit.data(sportscar, choice = "choice", shape = "long", 
                                alt.var = "alt", id.var = "resp_id")
model <- mlogit(choice ~ 0 + seat + trans + convert + price, data = sportscar_mlogit)
summary(model)

#Q2b
#Coefficients :
#              Estimate Std. Error  z-value  Pr(>|z|)    
#seat4       -0.0193861  0.0759029  -0.2554  0.798409    
#seat5        0.4245449  0.0752808   5.6395 1.706e-08 ***
#transmanual -1.2178833  0.0665276 -18.3064 < 2.2e-16 ***
#convertyes   0.2008115  0.0620854   3.2344  0.001219 ** 
#price       -0.1907023  0.0086739 -21.9859 < 2.2e-16 ***

#the ideal sportcar is 5-seat, auto, covertible top and cheap price

#Q2c
#price has the most pricesly estimated coefficients with se value=0.00867

#Q2d
WTP_convert <- coef(model)["convertyes"] / -coef(model)["price"]
WTP_convert
#convertyes 
#   1.05301 
# consumer willing to pay more 1050$ for a convertible top so the price 5000$ is too high and not resonable

#Q2e
newcars <- data.frame(seat=factor(c("2","4", "5")), 
                      trans=factor(c("manual", "auto", "auto")), 
                      convert=factor(c("no", "yes", "no")), 
                      price=c(40, 37, 35))
predict.mnl <- function(model, data) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}
predict.mnl(model, newcars)

#Q2f
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share) 
}
attrib <- list(seat = c("2", "4", "5"),
               trans = c("manual", "auto"),
               convert = c("no", "yes"), 
               price = c(30, 35, 40))
sens <- sensitivity.mnl(model, attrib=attrib, 
                        base.data=newcars[1,], competitor.data=newcars[2:3,])
barplot(sens$increase, names.arg=sens$level, ylab="Preference share", col="blue")

#Q3
racer_data <- subset(sportscar, segment == "racer")
library(mlogit)
racer_mlogit <- mlogit.data(racer_data, choice = "choice", shape = "long", alt.var = "alt")
racer_model <- mlogit(choice ~ 0 + seat + trans + convert + price, data = racer_mlogit, method = "nr")
summary(racer_model)
predicted_shares_racer <- predict.mnl(racer_model, newcars)
predicted_shares_racer
predicted_shares_full <- predict.mnl(model, newcars)
comparison <- data.frame(
  newcars = newcars$seat,
  shares_full = predicted_shares_full[, "share"],
  shares_racer = predicted_shares_racer[, "share"]
)
comparison

#Q4
my_rpar <- rep("n", length(model$coefficients)) 
names(my_rpar) <- names(model$coefficients)
m4 <- mlogit(choice ~ 0 + seat + trans + convert + price, 
              data = sportscar_mlogit, rpar = my_rpar, 
              correlation = TRUE)
summary(m4)
stdev(m4)
cov2cor(cov.mlogit(m4))
#stdev(m4)
#      seat4       seat5 transmanual  convertyes       price 
#  0.7213596   0.4724248   1.5932070   1.4550996   0.0808195 
#transmanual has the greatest variation

#Q5
install.packages("ChoiceModelR")
library(ChoiceModelR)
choice <- rep(0, nrow(sportscar))
choice[sportscar[,"alt"] == 1] <- sportscar[sportscar[,"choice"] == 1, "alt"] 
sportscar.coded <- model.matrix(~ seat + trans + convert + price, data = sportscar)
sportscar.coded <- sportscar.coded[,-1]
choicemodelr.data <- cbind(sportscar[,1:3], sportscar.coded, choice)
segment <- sportscar[sportscar$ques == 1 & sportscar$alt == 1, "segment"]
choicemodelr.demos <- 1 * cbind(segment == "basic", segment == "fun")
dir.create("choicemodelr_results", showWarnings = FALSE)
hb.post <- choicemodelr(
  data = choicemodelr.data, 
  xcoding = rep(1, 5), 
  demos = choicemodelr.demos,  
  mcmc = list(R = 20000, use = 10000), 
  options = list(save = TRUE),
  directory = "choicemodelr_results" 
)
summary(hb.post)

#the parameter estimates between the two models tend to be similar
#but there are differences in the standard deviations