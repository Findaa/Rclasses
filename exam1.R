---
title: "kolokwium1"
author: "Michał Cop"
date: "09/04/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

###Dla bezpieczenstwa dodaje wszystkie uzyteczne biblioteki.
```{r}
require(ggpubr) #wizualizacja
require(tidyverse)  
require(Hmisc)   #macierze korelacji
require(corrplot)  #korelogramy
require(dplyr)
```

### Dane zd2 posiadają 8 roznych kolumn. 7 z nich jest typu liczbowego, 6 całkowitych. Ostatni typ to zero jedynkowy typ dychotomiczny (w R po prostu Factor with 2 levels). Dwa najwazniejsze czynniki zostały przedstawione w dokladniejszy sposob - sprawdzamy ich odchylenie standardowe (z ktorego latwo uzyskac wariancje)
```{r}
dane<-read.csv("~/Downloads/zd2.csv", header=TRUE, sep=",",dec=".")
class(dane)
print("Informacje nt. zmiennych")
str(dane)
print("Summary: ")
summary(dane)
print('Odchylenie standardowe:  ')
cat("Ilosc dzieci: ", sd(dane$num.child))
print('')
cat("Ogolna ocena: ", sd(dane$overall))
```

### Czy dzieci są męczące? Średni wynik ocen dla poszczególnej dzietności rodzin. Najmniej zdecydowane były rodzinny z 2 dzieci. Juz w summary można zauważyć lekką zbierzność, gdyby nie rodziny 4dzietne. Sensownym wydaje sie zbadanie wspolczynnika korelacji. Uzyty zostanie wspolczynnik pearsona - nie ma potrzeby zachowania nieliniowej korelacji w takim przypadku.
### Wykres ewidentnie pokazuje tendencje wzrostową, ale interpretacja wyniku na podstawie takich danych jest błędem logicznym. Na wykresie wszystkich ocen taka korelacja jest juz bardziej widoczna.
```{r}
summarized <- group_by(dane, num.child) %>%
  summarise(
    count = n(),
    mean =  mean(overall, na.rm = TRUE),
    sd = sd(overall, na.rm = TRUE)
  )
summarized

res<-cor.test(dane$overall, dane$num.child, method = "pearson", na.rm = T)
res

ggscatter(summarized, x = "mean", y = "num.child", add = "reg.line", conf.int = FALSE,
          cor.coef = TRUE, cor.method = "kendall",
          ylab = "Ogólna Ocena", xlab = "Ilość dzieci")
```

```{r}
attach(dane)
qplot(overall, num.child, data = dane)
detach(dane)
```


### Wizualne przedstawienie gestosci rozkladu przebytego dystansu. Na 1 rzut oka widac, ze taki rozklad nie jest zblizony do normalnego
### Dodatkowo zostal przedstawiony wykres gestosci ogolnej oceny. Tutaj juz bardziej widac podobienstwo do rozkładu normalnego - warto skupić się na tej wartosci. 
```{r}
ggplot(dane, aes(x = dane$distance)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density()+
  stat_bin(bins = 150)

plot(density(dane$overall))
```

###Zad 3: Czy ocena ogolna ma zwiazek z ocena dotyczaca samych gier? Porownajmy srednie obu ocen. Wpierw sprawdzamy, czy obie zmienne sa dane rozkladem normalnym. p-value jest wieksze niz zalozony poziom ufnosci, tak wiec nie ma podsaw do odrzucenia H0, ktora w przypadku testu Shapiro-Wilka wskazuje na normalnosc rozkladow. 
### Przeprowadzony zostal rowniesz test t ktory wskazuje nam teraz w miare adekwatne srednie. Okazuje sie, ze srednia ocena za gry jest wieksza, niz srednia ocena ogolnego odczucia klienta.
```{r}
with(dane, shapiro.test(dane$overall))
with(dane, shapiro.test(dane$games))
```

```{r}
t.test(dane$games, dane$overall)

```





```{r}
dane$wskaznik[dane$overall < 33] <- "Niskie"
dane$wskaznik[dane$overall > 66] <- "Wysokie"
dane$wskaznik[dane$overall >= 33 & dane$overall <= 66] <- "Srednie"
fil <- dane[order(dane$overall),]
chisq.test(fil$weekend, fil$wskaznik)
```

###T


```{r}
sorted <- group_by(dane, distance)
ggscatter(sorted, x = "overall", y = "distance",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Odleglosc", ylab = "Ogolna ocena")
```


```{r} 
###Srednia ocena

tab<-table(dane$overall, dane$weekend)
chisq.test(tab)
```

```{r}
attach(dane)
boxplot(overall ~ weekend, dane = dane, xlab = "Czy to byl weekend?", ylab = "Srednia ocena", main = "X")
mean1 = with(dane, mean(overall[dane$weekend == "yes"]))
mean1
mean2 = with(dane, mean(overall[dane$weekend == "no"]))
mean2
detach(dane)
```
