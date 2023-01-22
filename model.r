rm(list=ls())

library(lmtest)
library(tseries)
library(car)
library(readxl)
library(dplyr)
library(stargazer) 
library(systemfit)
library(AER)
install.packages('prais')
install.packages('orcutt')
library(prais)
library(orcutt)

gen_plot <- function(data, x, name, type) {
    png(file=name,
    width=600, height=350)
    if(type == "hist")
        hist(data, breaks=50, col="green", xlab="reszty", ylab="czestotliwosc")
    else if(type == "scatter")
        if(x)
            plot(data, x, lwd=2, col='red')
        else
            plot(data, lwd=2, col='red')
    else if(type == "pacf")
        pacf(data)
    else
        plot(data, x, type)
    dev.off()
}

data <- read_excel("dane_vw.xlsx")
data <- data[2:130, ]

d_l_oil <- diff(log(data$oil), lag=12)

d_l_aluminium <- diff(log(data$aluminium), lag=12)

d_l_steel <- diff(log(data$steel), lag=12)

d_sales <- data$sales[13:129]

d_interest <- diff(log(data$interest), lag=12)

crisis <- data$crisis[14:130]

boom <- data$boom[14:130]

gen_plot(d_l_oil, NA, "./images/pacf_d_l_oil.png", "pacf")
gen_plot(d_l_aluminium, NA, "./images/pacf_d_l_aluminium.png", "pacf")
gen_plot(d_l_steel, NA, "./images/pacf_d_l_steel.png", "pacf")
gen_plot(d_sales, NA, "./images/pacf_d_sales.png", "pacf")
gen_plot(d_interest, NA, "./images/pacf_d_interest.png", "pacf")

print(adf.test(d_l_oil))
print(adf.test(d_l_aluminium))
print(adf.test(d_l_steel))
print(adf.test(d_sales))
print(adf.test(d_interest))

df <- data.frame(d_sales, d_l_steel,
                 d_l_aluminium, d_l_oil, crisis, 
                 boom, d_interest )

df <- df %>%
    dplyr::mutate(d_sales_12 = dplyr::lag(d_sales, n=12, default=NA)) %>%
    dplyr::mutate(d_l_oil_13 = dplyr::lag(d_l_oil, n=12, default=NA)) %>%
    dplyr::mutate(d_interest_1 = dplyr::lag(d_sales, n=1, default=NA)) %>%
    as.data.frame()

model <- lm(d_sales~d_l_oil_13 + d_l_steel + crisis + 
               d_sales_12 + d_interest_1, data=df)
print(summary(model))

# wykres sprzedazy
gen_plot(data$date, data$sales, "./images/plot_sales.png", "l")

## kryteria informacyjne
cat("AIC: ", AIC(model), "\n")
cat("BIC: ", BIC(model), "\n")
cat("VIF: ",vif(model), "\n")

## Normalność składnika losowego - test Jaque - Berry + histogram
ehat <- model$residuals
gen_plot(ehat, NA, "./images/hist.png", "hist")
print(jarque.bera.test(ehat))


### Niesferyczność - autokorelacja, heteroskedastyczność

## Autokorelacja - analiza wykresów reszt w czasie, scatterplotów i testów

# homoskedastyczność White test
print(bptest(model, data = df))

#Cochrane-Orcutt
CO <- cochrane.orcutt(model)
print(summary(CO))

ehat_CO <- CO$residuals
gen_plot(ehat_CO, FALSE, "./images/scatterplot_co.png", "scatter")
gen_plot(lag(ehat_CO,1), ehat_CO, "./images/scatterplot_co_delayed.png", "scatter")

print(Box.test(ehat_CO,lag=1,type="Box-Pierce"))
print(Box.test(ehat_CO,lag=1,type="Ljung-Box"))
print(Box.test(ehat_CO,lag=4,type="Box-Pierce"))
print(Box.test(ehat_CO,lag=4,type="Ljung-Box"))

print(bgtest(CO))
print(bgtest(CO, order=10))

#odporne błędy standardowe

print(HAC_NW <- NeweyWest(model, lag=4))

print(coeftest(model))
print(coeftest(model,vcov=HAC_NW))


# #Prais Winsten
# pw <- prais_winsten(d_sales~d_l_oil_13 + d_l_steel + crisis + 
#                d_sales_12 + d_interest_1, data=df, index="date")
# malfunction due to undefined columns selected (df)

### MZI
# pierwszy krok 
fst=lm(d_l_steel~ d_l_oil + crisis + d_l_oil_13 +
               d_sales_12 + d_interest_1, data=df)
print(summary(fst))
# Test na moc instrumentow
print(linearHypothesis(fst,"d_l_oil=0"))

# Zapisanie wartosci teoretycznych z pierwszego kroku 
fst_fitted <- rep(NA, times = 117)
fst_fitted[13:117] = fst$fitted
df2 <- df %>%
    dplyr::mutate(d_l_steel = fst_fitted)
## drugi krok 
tsls=lm(d_sales~d_l_oil_13 + d_l_steel + crisis + 
               d_sales_12 + d_interest_1, data=df2)

stargazer(model,fst,tsls,type="text")