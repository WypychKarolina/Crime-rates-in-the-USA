# Najniebezpieczniejszy stan
# Jaki wpływ mają AFAM na przestępczość (nienawiść może być skierowana w ich strone)
# Czy prawo do posiadania broni zmniejsza przestępczość


library(ggplot2)
library(dplyr)
library(tidyr)


guns <- read.table('Guns.csv', 
                   sep=",", 
                   header=TRUE, 
                   row.names = 1,
                   na.strings = c("","NA","NaN"))


# współczynnik morderstw - ilość morderstw na 100 000 mieszkańców
sum(is.na(guns))
sum(is.nan(as.matrix(guns)))  # nie ma brakujących wartości
mean(is.na(guns))

guns <- na.omit(guns)
#####################################################
# AFAM
#####################################################
guns_afam <- guns %>%
  select(state, murder, violent, afam)
guns_afam$Stan <- "pozostałe stany"
guns_afam$Stan[guns_afam$murder > 20] <- guns_afam$state[guns_afam$murder > 20]
guns_afam$Stan[guns_afam$murder < 1.5] <- guns_afam$state[guns_afam$murder < 1.5]
guns_afam$Stan[guns_afam$state == "Hawaii"] <- guns_afam$state[guns_afam$state == "Hawaii"]

scatter_afam_murder <- ggplot(data=guns_afam, 
                       mapping=aes(x=afam, y=murder, color=Stan)) + 
  geom_point() +
  scale_color_manual(values=c("red", "yellow", "blue", "purple", "green", "pink", "grey", "black", "orange")) +
  labs(x = "Populacja Afroamerykanów w stanie w latach 1977–1999 [%]",
       y = "Współczynnik morderstw") +
  ggeasy::easy_center_title()

scatter_afam_murder


##
guns_afam$Stan <- "pozostałe stany"
guns_afam$Stan[guns_afam$violent > 1400] <- guns_afam$state[guns_afam$violent > 1400]
guns_afam$Stan[guns_afam$violent < 100] <- guns_afam$state[guns_afam$violent < 100]
guns_afam$Stan[guns_afam$state == "Hawaii"] <- guns_afam$state[guns_afam$state == "Hawaii"]

scatter_afam_violent <- ggplot(data=guns_afam, 
                              mapping=aes(x=afam, y=violent, color=Stan)) + 
  geom_point() +
  scale_color_manual(values=c("red", "yellow", "green", "pink", "grey", "black",  "orange", "purple")) +
  labs(x = "Populacja Afroamerykanów w stanie w latach 1977–1999 [%]",
       y = "Współczynnik brutalnych przestępstw") +
  ggeasy::easy_center_title()

scatter_afam_violent


##
#lm_afam_violent = lm(violent ~ afam, data=guns_afam_DC_Hawaii)

guns_afam_DC_Hawaii <- filter(guns_afam, state!="District of Columbia" & state!="Hawaii")

scatter_afam_violent_DC_Hawaii <- ggplot(data=guns_afam_DC_Hawaii, 
                               mapping=aes(x=afam, y=violent, color=Stan)) + 
  geom_point() +
  scale_color_manual(values=c("green", "pink", "grey", "black", "orange")) +
  labs(x = "Populacja Afroamerykanów w stanie w latach 1977–1999 [%]",
       y = "Współczynnik brutalnych przestępstw") +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 6), se = FALSE, color="red", fill="red") +
  ggeasy::easy_center_title()

scatter_afam_violent_DC_Hawaii



##
lm_afam_murder = lm(murder ~ afam, data=guns_afam_DC_Hawaii)
summary(lm_afam_murder)

scatter_afam_murder_DC_Hawaii <- ggplot(data=guns_afam_DC_Hawaii, 
                                         mapping=aes(x=afam, y=murder, color=Stan)) + 
  geom_point() +
  scale_color_manual(values=c("green", "pink", "gray", "black", "orange")) +
  labs(x = "Populacja Afroamerykanów w stanie w latach 1977–1999 [%]",
       y = "Współczynnik morderstw") +
  geom_smooth(method=lm, color="red", fill="red") +
  ggeasy::easy_center_title()

scatter_afam_murder_DC_Hawaii


#####################################################
# prisoners
#####################################################
guns_prisoners <- guns %>%
  select(state, murder, violent, robbery, prisoners)
guns_prisoners$prisoners_rate <- "pozostałe stany"
guns_prisoners$prisoners_rate[guns_prisoners$violent > 1400] <- guns_prisoners$state[guns_prisoners$violent > 1400]
guns_prisoners$prisoners_rate[guns_prisoners$violent < 100] <- guns_prisoners$state[guns_prisoners$violent < 100]  

scatter_prisoners_violent <- ggplot(data=guns_prisoners, 
                              mapping=aes(x=violent, y=prisoners, color=prisoners_rate)) + 
  geom_point() +
  scale_color_manual(values=c("red", "blue", "black",  "grey", "green", "yellow", "pink", "navy")) +
  labs(x = "Współczynnik brutalnych przestępstw w stanie w latach 1977–1999 [%]",
       y = "Współczynnik więźniów") +
  ggeasy::easy_center_title()
# niska liczba skazanych w stosunku do wysokiego violent
# za taki wynik moga byc odpowiedzialni seryjni przestepcy
# aczkolwiek sytaucja powtarza sie na przestrzeni wielu lat wiec mozemy
# wyeliminowac to zalozenie i stwierdzic ze system sadownictwa nie działa wydajnie
scatter_prisoners_violent


##
guns_pr <- filter(guns, state %in% c("District of Columbia", "Hawaii", "New Hampshire", "North Dakota", "Vermont")) %>%
  select(state, murder, violent, robbery, prisoners) %>%
  group_by(state) %>%
  summarise('violent crime' = mean(violent), 
            murder = mean(murder), 
            robbery = mean(robbery),
            prisoners = mean(prisoners)) %>%
  gather(name, count, 'violent crime':robbery)

bar_pr2 <- ggplot(data = guns_pr) +
  geom_bar(aes(x=state, y = count, fill=name), stat="identity") +
  scale_fill_brewer(palette="Blues", direction=-1) +
  geom_bar(aes(x=state, y = prisoners), 
           stat="identity", 
           position="dodge",
           fill = "red",
           width = 0.5) +
  labs(x = "Stan",
       y = "Wysokość współczynnika") +
  labs(fill="Crimes") + # tytuł legendy
  ggeasy::easy_center_title()

bar_pr2

# to samo bez DC
bar_pr3 <- ggplot(data = filter(guns_pr, state!="District of Columbia")) +
  geom_bar(aes(x=state, y = count, fill=name), stat="identity") +
  scale_fill_brewer(palette="Blues", direction=-1) +
  geom_bar(aes(x=state, y = prisoners), 
           stat="identity", 
           position="dodge",
           fill = "red",
           width = 0.5) +
  labs(x = "Stan",
       y = "Wysokość współczynnika") +
  labs(fill="Crimes") + # tytuł legendy
  ggeasy::easy_center_title()

bar_pr3


#####################################################
# INCOME
#####################################################
guns_income <- guns %>%
  select(state, murder, violent, income)
guns_income$Stan <- "pozostałe stany"
guns_income$Stan[guns_income$murder > 20] <- guns_income$state[guns_income$murder > 20]
guns_income$Stan[guns_income$murder < 1.5] <- guns_income$state[guns_income$murder < 1.5]  

scatter_income_murder <- ggplot(data=guns_income, 
                              mapping=aes(x=income, y=murder, color=Stan)) + 
  geom_point() +
  scale_color_manual(values=c("red", "blue", "purple", "green", "pink", "grey", "black", "orange")) +
  labs(x = "Średne roczne zarobki w stanie [$]",
       y = "Współczynnik morderstw") +
  ggeasy::easy_center_title()

scatter_income_murder


##
guns_income$Stan <- "pozostałe stany"
guns_income$Stan[guns_income$violent > 1400] <- guns_income$state[guns_income$violent > 1400]
guns_income$Stan[guns_income$violent < 100] <- guns_income$state[guns_income$violent < 100]

scatter_income_violent <- ggplot(data=guns_income, 
                               mapping=aes(x=income, y=violent, color=Stan)) + 
  geom_point() +
  scale_color_manual(values=c("red", "green", "pink", "grey", "black", "orange")) +
  labs(x = "Średne roczne zarobki w stanie [$]",
       y = "Współczynnik brutalnych przestępstw") +
  ggeasy::easy_center_title()

scatter_income_violent


#####################################################
# GRUPOWANIE LATAMI
#####################################################
guns_year_DC <- filter(guns, state!="District of Columbia") %>%
  select(state, year, murder, violent) %>%
  group_by(year) %>%
  summarise(mean_violent = mean(violent), 
            mean_murder = mean(murder))

bar_year_DC_violent <- ggplot(guns_year_DC,
                   aes(year,mean_violent)) +
  geom_bar(stat="identity")+
  labs(x = "Rok",
       y = "Współczynnik brutalnych przestępstw") +
  ggeasy::easy_center_title()

bar_year_DC_violent

bar_year_DC_murder <- ggplot(guns_year_DC,
                              aes(year,mean_murder)) +
  geom_bar(stat="identity")+
  labs(x = "Rok",
       y = "Współczynnik morderstw") +
  ggeasy::easy_center_title()

bar_year_DC_murder


#####################################################
# GRUPOWANIE STANAMI
#####################################################
guns_state <- guns %>%
  select(state, murder, violent) %>%
  group_by(state) %>%
  summarise(mean_violent = mean(violent), mean_murder = mean(murder))

bar_state_murder <- ggplot(guns_state,
                   aes(state,mean_murder)) +
  geom_bar(stat="identity")+
  labs(x = "Stan",
       y = "Współczynnik morderstw") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggeasy::easy_center_title()

bar_state_murder


bar_state_violent <- ggplot(guns_state,
                           aes(state,mean_violent)) +
  geom_bar(stat="identity")+
  labs(x = "Stan",
       y = "Współczynnik brutalnych przestępstw") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggeasy::easy_center_title()

bar_state_violent

# wniosek ze wyrzucamy district of columbia bo zaburza statystyki, uznajemy go za brutalny
# zrobić na poczatku, najpierw sprawdzimy czy kotrys stan nie bedzie psul
# wyrzucic DC tylko przy grupowaniu latami itp


#####################################################
# SHALL CARRY LAW
#####################################################
guns_law <- guns %>%
  select(state, murder, violent, law) %>%
  group_by(law) %>%
  summarise(mean_violent = mean(violent), mean_murder = mean(murder))

bar_law <- ggplot(guns_law,
                    aes(law,mean_murder)) +
  geom_bar(stat="identity")+
  labs(x = "Stan",
       y = "Współczynnik morderstw") +
  ggeasy::easy_center_title()

bar_law



guns_law_DC <- filter(guns, state!="District of Columbia") %>%
  select(state, murder, violent, robbery, law) %>%
  group_by(law) %>%
  summarise('brutalne przestępstwa' = mean(violent), 'kradzieże'=mean(robbery), 'morderstwa' = mean(murder)) %>%
  gather(name, count, 'brutalne przestępstwa':'morderstwa')   ##########
###################### TO GATHER JEST BARDZO WAŻNE ############


bar_law_DC <- ggplot(guns_law_DC, aes(x=law)) +
  geom_bar(aes(y=count, fill=name), stat="identity", position="dodge") +
  scale_fill_brewer(palette="Spectral", direction=1) +
  labs(x = "Stan",
       y = "Wartość współczynnika") +
  labs(fill="Przestępstwa") +
  ggeasy::easy_center_title()

bar_law_DC


# niektóre stany jak Oklahoma wprowadziły prawo w połowie, 
#tym samym ograniczyli liczbę violent