#Final Paper- Rediet Tadele
#State Expenditure and Emergency Declarations 

install.packages("MASS")
install.stargazer("stargazer")
install.packages("ggplot2")
install.packages("DHARMa")

library(DHARMa)
library(ggplot2)
library(stargazer)
library(MASS)


setwd("C:/Users/ual-laptop/OneDrive/Documents/STATS FINAL")
df <- read.csv("Cleaned Data FP.csv")
df <- subset(df, State != "Dist. of Col.")

hist(df$Disasters, 
     main="Disaster Declarations", 
     xlab="Number of Disaster Declarations", 
     col="Pink", 
     border="black")

TotalDis <- aggregate(Disasters ~ Year, data = df, sum)

barplot(TotalDis$Disasters,
        names.arg = aggregated_data$Year,
        main = "Number of Disasters Declarations by Year",
        xlab = "Year",
        ylab = "Number of Disasters",
        col = "Orange", 
        border = "black")
#Are there patterns in gov exp and #declarations over the years 
TotalHwy<- aggregate(Hwy_exp ~ Year, data = df, sum)
combined_data <- merge(TotalHwy, TotalDis, by = "Year")
ggplot(combined_data, aes(x = Year)) +
  geom_line(aes(y = Hwy_exp, color = "Highway Expenditure"), size = 1) +
  geom_line(aes(y = Disasters, color = "Number of Disasters"), size = 1) +
  labs(title = "Highway Expenditure vs Number of Disasters by Year",
       x = "Year",
       y = "Value",
       color = "Legend") +
  theme_minimal()

Poisson_model <- glm( Disasters ~ Hwy_exp + Hcm_exp + Pw_exp+ Ed_exp, , family = "poisson", data = df)
summary(Poisson_model)

stargazer(Poisson_model, type = "latex", title = "Poisson Results", align = TRUE)

stargazer(Poisson_model, 
          type = "text",                 
          title = "Poisson Results", 
          dep.var.labels = "Number of Disaster Declarations ",  
          covariate.labels = c("Intercept", "Highway", "Housing and Community Development", "Public Welfare", "Higher Education"),
          star.cutoffs = c(0.1, 0.05, 0.01),  
          no.space = TRUE,            
          column.sep.width = "1pt",       
          style = "aer",              
          out = "C:/Users/ual-laptop/OneDrive/Documents/STATS FINAL/P.Model.text"  
)

#CHECK FOR DISPERSION 

dispersiontest(Poisson_model)
Dispersion<-dispersiontest(Poisson_model)
write.table(dispersion_results, 
            "C:/Users/ual-laptop/OneDrive/Documents/STATS FINAL/disperstest_results.txt", 
            quote = FALSE, row.names = FALSE, col.names = TRUE)

#yes, overdispersion-negative binomial

Nbin.model<- glm.nb(formula = Disasters ~ Hwy_exp + Hcm_exp + Pw_exp+ Ed_exp, data = df, init.theta = 1.1, 
                    link = log)
summary(Nbin.model)


stargazer(Nbin.model, 
          type = "text",                 
          title = "Negative Binomial  Results", 
          dep.var.labels = "Number of Disaster Declarations ",  
          covariate.labels = c("Intercept", "Highway", "Housing and Community Development", "Public Welfare", "Higher Education"),
          star.cutoffs = c(0.1, 0.05, 0.01),  
          no.space = TRUE,            
          column.sep.width = "1pt",       
          style = "aer",              
          out = "C:/Users/ual-laptop/OneDrive/Documents/STATS FINAL/Nb.Model.text"  
)

sim_fmnb <- simulateResiduals(Nbin.model, refit = TRUE, n = 99)
plotSimulatedResiduals(sim_fmnb)

plot(Nbin.model$residuals)
