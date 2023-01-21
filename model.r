rm(list=ls())

library(lmtest)
library(tseries)
library(car)
library(readxl)
library(dplyr)

gen_pacf <- function(data, name) {
    png(file=name,
    width=600, height=350)
    pacf(data)
    dev.off()
}

gen_plot <- function(x, name) {
    png(file=name,
    width=600, height=350)
    plot(data$date, x, type='l')
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

gen_pacf(d_l_oil, "./images/pacf_d_l_oil.png")
gen_pacf(d_l_aluminium, "./images/pacf_d_l_aluminium.png")
gen_pacf(d_l_steel, "./images/pacf_d_l_steel.png")
gen_pacf(d_sales, "./images/pacf_d_sales.png")
gen_pacf(d_interest, "./images/pacf_d_interest.png")

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

#wykres sprzedazy
gen_plot(data$sales, "./images/plot_sales.png")

#wykres reszt
ehat=model$residuals
png(file="./images/hist.png",
    width=600, height=350)
    hist(ehat,breaks=50,col="green",xlab="reszty",ylab="czestotliwosc")
    dev.off()
vif(model)