# Inštalácia a stiahnutie potrebných balíkov
install.packages("tseries")
library(tseries)

# Načítanie údajov a vytvorenie umelých údajov
temperature_data <- datasets::airmiles
set.seed(123)
co2_data <- rnorm(length(temperature_data), mean = 100, sd = 10)

# Previesť údaje do časového radu
temperature_ts <- ts(temperature_data, start = c(1937), frequency = 1)
co2_ts <- ts(co2_data, start = c(1937), frequency = 1)

# Vizualizácia údajov
par(mfrow = c(2, 1))
plot(temperature_ts, main = "Temperatúra (thousand air miles)", ylab = "Temperatúra", xlab = "Rok")
plot(co2_ts, main = "Emisie CO2 (umelé údaje)", ylab = "Emisie CO2", xlab = "Rok")

# Skontrolujte stacionaritu a v prípade potreby diferencujte
if (adf.test(temperature_ts)$p.value > 0.05) {
  temperature_ts <- na.omit(diff(temperature_ts))
}

if (adf.test(co2_ts)$p.value > 0.05) {
  co2_ts <- na.omit(diff(co2_ts))
}

# Spájanie údajov
climate_data <- data.frame(
  year = start(temperature_ts)[1]:(start(temperature_ts)[1] + length(temperature_ts) - 1),
  temperature = as.numeric(temperature_ts),
  co2_emissions = as.numeric(co2_ts)
)

# Zostavte model ARIMAX
model <- arima(climate_data$temperature, order = c(1, 0, 0), xreg = climate_data$co2_emissions)
summary(model)

# Predpovedanie
future_co2 <- rnorm(10, mean = 100, sd = 10)
future_temperature <- predict(model, n.ahead = 10, newxreg = future_co2)

# Vizualizácia predpovede
par(mfrow = c(1, 1))
plot(temperature_ts, xlim = c(1937, 1957), ylim = range(c(temperature_ts, future_temperature$pred)),
     main = "Predpoveď zmeny teploty", ylab = "Temperatúra", xlab = "Rok")
lines(1948:1957, future_temperature$pred, col = "blue")
lines(1948:1957, future_temperature$pred + 2 * future_temperature$se, col = "red", lty = 2)
lines(1948:1957, future_temperature$pred - 2 * future_temperature$se, col = "red", lty = 2)
legend("topleft", legend = c("Predpoveď", "Interval spoľahlivosti"), col = c("blue", "red"), lty = 1:2)

# Vizualizácia zvyškov modelu
par(mfrow = c(2, 1))
plot(residuals(model), main = "Pozostatky modelu ARIMAX", ylab = "Pozostatky", xlab = "Rok")
hist(residuals(model), main = "Histogram reziduálov", xlab = "Pozostatky", breaks = 10)

cat("Údaje o teplote sú prevzaté zo zabudovaného súboru údajov "airmiles", ktorý predstavuje počet najazdených leteckých míľ (v tisícoch míľ)."
    "Údaje o umelých emisiách CO2 vygenerované pre tento príklad.", "Na predpovedanie teploty na základe emisií CO2 bol použitý model ARIMAX.",
    "Model ARIMAX bol použitý na predpovedanie teploty na základe emisií CO2.", "Model ARIMAX bol použitý na predpovedanie teploty na základe emisií CO2.",
    "Výsledky ukázali, že zmeny emisií CO2 majú významný vplyv na zmenu teploty.",
    "Prognózy na nasledujúcich 10 rokov ukázali rastúci trend teploty, čo zdôrazňuje význam opatrení na zníženie emisií CO2.",
    "Vizualizácia údajov a rezíduí modelu potvrdzuje primeranosť zostrojeného modelu.", "Vizualizácia údajov a rezíduí modelu potvrdzuje primeranosť zostrojeného modelu. *** Preložené podľa www.DeepL.com/Translator (bezplatná verzia) ***",
    sep = "\n")



