

# Limpando Dados ----------------------------------------------------------

rm(list = ls())
graphics.off()

# Carregando Pacotes ------------------------------------------------------


packages <- c(
  "ipeadatar",
  "tidyverse",
  "data.table",
  "lubridate",
  "gridExtra"
)


# Instalando pacotes ainda não instalados

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == F)) {
  
  install.packages(packages[!installed_packages])
  
}

# Carregando Pacotes

invisible(lapply(packages, library, character.only = T))


# Carregando Dados --------------------------------------------------------

# IPCA

ipca <- ipeadata("PRECOS12_IPCA12", "br") %>%
  select(date, value) %>%
  filter(date %between% c("2000-01-01", "2020-01-01")) %>%
  rename("IPCA" = "value") %>%
  mutate(
    mes = month(date),
    ano = year(date),
    IPCA = IPCA / IPCA[length(IPCA)]
  )

# IPC - EUA

ipc <- ipeadata("BLS12_IPCEUAS12", "br") %>%
  select(date, value)

# inserindo dados manualmente do ICP pelo fato do IPEA não ter os dados atualizados
# para janeiro de 2020
# valor pelo no https://data.bls.gov/timeseries/CUSR0000SA0?output_view=pct_1mth


ipc[1285, 1:2] <- c("2020-01-01", 257.971)
ipc$value <- as.numeric(ipc$value)

ipc <- ipc %>%
  filter(date %between% c("2000-01-01", "2020-01-01")) %>%
  rename("IPC" = "value") %>%
  mutate(
    mes = month(date),
    ano = year(date),
    IPC = IPC / IPC[length(IPC)]
  )


# Calculando deflator


deflator <- ipca %>%
  left_join(ipc, by = "date") %>%
  mutate(deflator = IPC / IPCA) %>%
  select(date, deflator, mes.x, ano.x) %>%
  rename(
    "mes" = "mes.x",
    "ano" = "ano.x"
  )

# Câmbio

cambio <- ipeadata("GM366_ERC366", "br") %>%
  select(date, value) %>%
  filter(date %between% c("2000-01-01", "2020-01-31")) %>%
  rename("Cambio" = "value") %>%
  mutate(mes = month(date), ano = year(date))



# Contruindo Data Frame ---------------------------------------------------

df <- cambio %>%
  left_join(deflator, c("mes", "ano")) %>%
  mutate(cotacao = Cambio * deflator) %>%
  select(date.x, cotacao, Cambio) %>%
  rename("date" = "date.x")


# Gráfico 1 ---------------------------------------------------------------




# captamos os valores máximos e mínimos da série histórica e conforme o período os governos, além de criarmos uma classificação


max_serie1 <- round(max(df$cotacao) + 0.20, 1)
min_serie1 <- round(min(df$cotacao) - 0.20, 1)
breaks1 <- seq(min_serie1, max_serie1, .5)

# Estatística Descritiva - Governo Lula

max_lula <- round(as.numeric(df %>%
                               filter(
                                 date %between% c("2002-01-01", "2010-12-31")
                               ) %>%
                               top_n(1, cotacao) %>%
                               select(cotacao)), 3)

min_lula <- round(as.numeric(df %>%
                               filter(date %between% c("2002-01-01", "2010-12-30")) %>%
                               top_n(-1, cotacao) %>%
                               select(cotacao)), 3)

media_lula <- df %>%
  filter(date %between% c("2002-01-01", "2010-12-31")) %>%
  summarise(media = mean(cotacao, na.rm = T))

sd_lula <- df %>%
  filter(date %between% c("2002-01-01", "2010-12-31")) %>%
  summarise(sd = sd(cotacao, na.rm = T))

cv_lula <- (sd_lula / media_lula) * 100

max_lula <- format(as.numeric(max_lula), decimal.mark = ",", digits = 4)
min_lula <- format(as.numeric(min_lula), decimal.mark = ",", digits = 4)
media_lula <- format(as.numeric(media_lula), decimal.mark = ",", digits = 4)
sd_lula <- format(as.numeric(sd_lula), decimal.mark = ",", digits = 4)
cv_lula <- format(as.numeric(cv_lula), decimal.mark = ",", digits = 4)


# Estatística Descritiva - Governo Dilma

max_dilma <- round(as.numeric(df %>%
                                filter(date %between% c("2011-01-01", "2016-08-31")) %>%
                                top_n(1, cotacao) %>%
                                select(cotacao)), 3)

min_dilma <- round(as.numeric(df %>%
                                filter(date %between% c("2011-01-01", "2016-08-31")) %>%
                                top_n(-1, cotacao) %>%
                                select(cotacao)), 3)

media_dilma <- df %>%
  filter(date %between% c("2011-01-01", "2016-08-31")) %>%
  summarise(media = mean(cotacao, na.rm = T))

sd_dilma <- df %>%
  filter(date %between% c("2011-01-01", "2016-08-31")) %>%
  summarise(sd = sd(cotacao, na.rm = T))

cv_dilma <- (sd_dilma / media_dilma) * 100

max_dilma <- format(as.numeric(max_dilma), decimal.mark = ",", digits = 4)
min_dilma <- format(as.numeric(min_dilma), decimal.mark = ",", digits = 4)
media_dilma <- format(as.numeric(media_dilma), decimal.mark = ",", digits = 4)
sd_dilma <- format(as.numeric(sd_dilma), decimal.mark = ",", digits = 4)
cv_dilma <- format(as.numeric(cv_dilma), decimal.mark = ",", digits = 4)


# Estatística Descritiva - Governo Temer


max_temer <- round(as.numeric(df %>%
                                filter(date %between% c("2016-09-01", "2018-12-31")) %>%
                                top_n(1, cotacao) %>%
                                select(cotacao)), 3)

min_temer <- round(as.numeric(df %>%
                                filter(date %between% c("2016-09-01", "2018-12-31")) %>%
                                top_n(-1, cotacao) %>%
                                select(cotacao)), 3)

media_temer <- df %>%
  filter(date %between% c("2016-09-01", "2018-12-31")) %>%
  summarise(media = mean(cotacao, na.rm = T))

sd_temer <- df %>%
  filter(date %between% c("2016-09-01", "2018-12-31")) %>%
  summarise(sd = sd(cotacao, na.rm = T))

cv_temer <- (sd_temer / media_temer) * 100

max_temer <- format(as.numeric(max_temer), decimal.mark = ",", digits = 4)
min_temer <- format(as.numeric(min_temer), decimal.mark = ",", digits = 4)
media_temer <- format(as.numeric(media_temer), decimal.mark = ",", digits = 4)
sd_temer <- format(as.numeric(sd_temer), decimal.mark = ",", digits = 3)
cv_temer <- format(as.numeric(cv_temer), decimal.mark = ",", digits = 3)


# Estatística Descritiva - Governo Bolsonaro


max_bolsonaro <- round(as.numeric(df %>%
                                    filter(date %between% c("2019-01-01", "2020-01-31")) %>%
                                    top_n(1, cotacao) %>%
                                    select(cotacao)), 3)

min_bolsonaro <- round(as.numeric(df %>%
                                    filter(date %between% c("2019-01-01", "2020-01-31")) %>%
                                    top_n(-1, cotacao) %>%
                                    select(cotacao)), 3)

media_bolsonaro <- df %>%
  filter(date %between% c("2019-01-01", "2020-01-31")) %>%
  summarise(media = mean(cotacao, na.rm = T))

sd_bolsonaro <- df %>%
  filter(date %between% c("2019-01-01", "2020-01-31")) %>%
  summarise(sd = sd(cotacao, na.rm = T))

cv_bolsonaro <- (sd_bolsonaro / media_bolsonaro) * 100

max_bolsonaro <- format(as.numeric(max_bolsonaro), decimal.mark = ",", digits = 4)
min_bolsonaro <- format(as.numeric(min_bolsonaro), decimal.mark = ",", digits = 4)
media_bolsonaro <- format(as.numeric(media_bolsonaro), decimal.mark = ",", digits = 4)
sd_bolsonaro <- format(as.numeric(sd_bolsonaro), decimal.mark = ",", digits = 3)
cv_bolsonaro <- format(as.numeric(cv_bolsonaro), decimal.mark = ",", digits = 3)



g <- ggplot(df, aes(x = date, y = cotacao)) +
  annotate("rect",
           fill = "#1a9641", alpha = .1,
           xmin = date("2002-01-01"), xmax = date("2010-12-31"),
           ymin = min_serie1, ymax = max_serie1
  ) +
  annotate("rect",
           fill = "#FFEC1A", alpha = .1,
           xmin = date("2011-01-01"), xmax = date("2016-08-31"),
           ymin = min_serie1, ymax = max_serie1
  ) +
  annotate("rect",
           fill = "#fe9929", alpha = .1,
           xmin = date("2016-08-31"), xmax = date("2018-12-31"),
           ymin = min_serie1, ymax = max_serie1
  ) +
  annotate("rect",
           fill = "#AD2414", alpha = .2,
           xmin = date("2019-01-01"), xmax = date("2020-01-31"),
           ymin = min_serie1, ymax = max_serie1
  ) +
  geom_ribbon(aes(ymax = cotacao, ymin = min_serie1),
              fill = "#3182bd", alpha = 0.7
  ) +
  geom_line(color = "#08306b") +
  scale_y_continuous(limits = c(min_serie1, max_serie1), breaks = breaks1) +
  scale_x_date(
    breaks = seq(
      from = as.Date("2002-01-01"),
      to = as.Date("2020-02-24"), by = "year"
    ),
    date_labels = "%Y", limits = c(min(df$date), as.Date("2020-02-24"))
  ) +
  ggtitle(expression("Cotação diária do dólar (em reais), Brasil 2000 - 2020*")) +
  # ggtitle("Cotação diária do dólar (em reais), Brasil 2002 - 2020*")+
  labs(
    subtitle = paste0(
      "Série histórica da taxa de câmbio real**"
    ),
    y = "Cotação diária em reais",
    x = " ",
    caption = paste0(
      "Fonte: IPEADATA (https://www.ipeadata.gov.br)  \n",
      "Visualização: Alysson Oliveira   \n",
      "*Limite de datas: 03/01/2000 e 31/01/2020 \n",
      "** Valores atualizados para Janeiro de 2020 com base no IPCA e no IPC americano"
    )
  ) +
  annotate("segment",
           x = date("2002-01-01"), xend = date("2002-01-01"),
           y = min_serie1, yend = max_serie1, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("segment",
           x = date("2011-01-01"), xend = date("2011-01-01"),
           y = min_serie1, yend = max_serie1, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("segment",
           x = date("2016-08-31"), xend = date("2016-08-31"),
           y = min_serie1, yend = max_serie1, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("segment",
           x = date("2018-12-31"), xend = date("2018-12-31"),
           y = min_serie1, yend = max_serie1, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("text",
           x = date("2005-07-01"), y = 6,
           hjust = 0, color = "#451225",
           size = 3.7,
           label = paste(
             "Governo Lula \n",
             " Maior: R$", max_lula,
             "\n  Menor: R$", min_lula,
             "\n  Média: R$", media_lula,
             "\n  DP: R$", sd_lula,
             "\n  CV:", paste0(cv_lula, "%")
           )
  ) +
  annotate("text",
           x = date("2012-06-01"), y = 6,
           hjust = 0, color = "#451225",
           size = 3.7,
           label = paste(
             "Governo Dilma \n",
             " Maior: R$", max_dilma,
             "\n  Menor: R$", min_dilma,
             "\n  Média: R$", media_dilma,
             "\n  DP: R$", sd_dilma,
             "\n  CV:", paste0(cv_dilma, "%")
           )
  ) +
  annotate("text",
           x = date("2016-11-01"), y = 6.4,
           hjust = 0, color = "#451225",
           size = 3.5,
           label = paste(
             "Governo Temer \n",
             " Maior: R$", max_temer,
             "\n  Menor: R$", min_temer,
             "\n  Média: R$", media_temer,
             "\n  DP: R$", sd_temer,
             "\n  CV:", paste0(cv_temer, "%")
           )
  ) +
  annotate("text",
           x = date("2019-07-15"), y = 5.7,
           hjust = 0.2, color = "#451225",
           size = 3.1,
           label = paste(
             "Governo Bolsonaro \n",
             " Maior: R$ ", max_bolsonaro,
             "\n  Menor: R$", min_bolsonaro,
             "\n  Média: R$", media_bolsonaro,
             "\n  DP: R$", sd_bolsonaro,
             "\n CV:", paste0(cv_bolsonaro, "%")
           )
  ) +
  theme(
    plot.title = element_text(size = 14, colour = "black"),
    title = element_text(face = "italic", colour = "#636363"),
    plot.caption = element_text(hjust = 0, colour = "black")
  )



# Gráfico 2 ---------------------------------------------------------------



max_serie <- round(max(df$Cambio) + 0.20, 1)
min_serie <- round(min(df$Cambio) - 0.20, 1)
breaks <- seq(min_serie, max_serie, .5)

# Estatística Descritiva - Governo Lula

max_lula <- round(as.numeric(df %>%
                               filter(
                                 date %between% c("2002-01-01", "2010-12-31")
                               ) %>%
                               top_n(1, Cambio) %>%
                               select(Cambio)), 3)

min_lula <- round(as.numeric(df %>%
                               filter(date %between% c("2002-01-01", "2010-12-30")) %>%
                               top_n(-1, Cambio) %>%
                               select(Cambio)), 3)

media_lula <- df %>%
  filter(date %between% c("2002-01-01", "2010-12-31")) %>%
  summarise(media = mean(Cambio, na.rm = T))

sd_lula <- df %>%
  filter(date %between% c("2002-01-01", "2010-12-31")) %>%
  summarise(sd = sd(Cambio, na.rm = T))

cv_lula <- (sd_lula / media_lula) * 100

max_lula <- format(as.numeric(max_lula), decimal.mark = ",", digits = 4)
min_lula <- format(as.numeric(min_lula), decimal.mark = ",", digits = 4)
media_lula <- format(as.numeric(media_lula), decimal.mark = ",", digits = 4)
sd_lula <- format(as.numeric(sd_lula), decimal.mark = ",", digits = 4)
cv_lula <- format(as.numeric(cv_lula), decimal.mark = ",", digits = 4)


# Estatística Descritiva - Governo Dilma

max_dilma <- round(as.numeric(df %>%
                                filter(date %between% c("2011-01-01", "2016-08-31")) %>%
                                top_n(1, Cambio) %>%
                                select(Cambio)), 3)

min_dilma <- round(as.numeric(df %>%
                                filter(date %between% c("2011-01-01", "2016-08-31")) %>%
                                top_n(-1, Cambio) %>%
                                select(Cambio)), 3)

media_dilma <- df %>%
  filter(date %between% c("2011-01-01", "2016-08-31")) %>%
  summarise(media = mean(Cambio, na.rm = T))

sd_dilma <- df %>%
  filter(date %between% c("2011-01-01", "2016-08-31")) %>%
  summarise(sd = sd(Cambio, na.rm = T))

cv_dilma <- (sd_dilma / media_dilma) * 100

max_dilma <- format(as.numeric(max_dilma), decimal.mark = ",", digits = 4)
min_dilma <- format(as.numeric(min_dilma), decimal.mark = ",", digits = 4)
media_dilma <- format(as.numeric(media_dilma), decimal.mark = ",", digits = 4)
sd_dilma <- format(as.numeric(sd_dilma), decimal.mark = ",", digits = 4)
cv_dilma <- format(as.numeric(cv_dilma), decimal.mark = ",", digits = 4)


# Estatística Descritiva - Governo Temer


max_temer <- round(as.numeric(df %>%
                                filter(date %between% c("2016-09-01", "2018-12-31")) %>%
                                top_n(1, Cambio) %>%
                                select(Cambio)), 3)

min_temer <- round(as.numeric(df %>%
                                filter(date %between% c("2016-09-01", "2018-12-31")) %>%
                                top_n(-1, Cambio) %>%
                                select(Cambio)), 3)

media_temer <- df %>%
  filter(date %between% c("2016-09-01", "2018-12-31")) %>%
  summarise(media = mean(Cambio, na.rm = T))

sd_temer <- df %>%
  filter(date %between% c("2016-09-01", "2018-12-31")) %>%
  summarise(sd = sd(Cambio, na.rm = T))

cv_temer <- (sd_temer / media_temer) * 100

max_temer <- format(as.numeric(max_temer), decimal.mark = ",", digits = 4)
min_temer <- format(as.numeric(min_temer), decimal.mark = ",", digits = 4)
media_temer <- format(as.numeric(media_temer), decimal.mark = ",", digits = 4)
sd_temer <- format(as.numeric(sd_temer), decimal.mark = ",", digits = 3)
cv_temer <- format(as.numeric(cv_temer), decimal.mark = ",", digits = 3)


# Estatística Descritiva - Governo Bolsonaro


max_bolsonaro <- round(as.numeric(df %>%
                                    filter(date %between% c("2019-01-01", "2020-01-31")) %>%
                                    top_n(1, Cambio) %>%
                                    select(Cambio)), 3)

min_bolsonaro <- round(as.numeric(df %>%
                                    filter(date %between% c("2019-01-01", "2020-01-31")) %>%
                                    top_n(-1, Cambio) %>%
                                    select(Cambio)), 3)

media_bolsonaro <- df %>%
  filter(date %between% c("2019-01-01", "2020-01-31")) %>%
  summarise(media = mean(Cambio, na.rm = T))

sd_bolsonaro <- df %>%
  filter(date %between% c("2019-01-01", "2020-01-31")) %>%
  summarise(sd = sd(Cambio, na.rm = T))

cv_bolsonaro <- (sd_bolsonaro / media_bolsonaro) * 100

max_bolsonaro <- format(as.numeric(max_bolsonaro), decimal.mark = ",", digits = 4)
min_bolsonaro <- format(as.numeric(min_bolsonaro), decimal.mark = ",", digits = 4)
media_bolsonaro <- format(as.numeric(media_bolsonaro), decimal.mark = ",", digits = 4)
sd_bolsonaro <- format(as.numeric(sd_bolsonaro), decimal.mark = ",", digits = 3)
cv_bolsonaro <- format(as.numeric(cv_bolsonaro), decimal.mark = ",", digits = 3)



g2 <- ggplot(df, aes(x = date, y = Cambio)) +
  annotate("rect",
           fill = "#1a9641", alpha = .1,
           xmin = date("2002-01-01"), xmax = date("2010-12-31"),
           ymin = min_serie, ymax = max_serie
  ) +
  annotate("rect",
           fill = "#FFEC1A", alpha = .1,
           xmin = date("2011-01-01"), xmax = date("2016-08-31"),
           ymin = min_serie, ymax = max_serie
  ) +
  annotate("rect",
           fill = "#fe9929", alpha = .1,
           xmin = date("2016-08-31"), xmax = date("2018-12-31"),
           ymin = min_serie, ymax = max_serie
  ) +
  annotate("rect",
           fill = "#AD2414", alpha = .2,
           xmin = date("2019-01-01"), xmax = date("2020-01-31"),
           ymin = min_serie, ymax = max_serie
  ) +
  geom_ribbon(aes(ymax = Cambio, ymin = min_serie),
              fill = "#3182bd", alpha = 0.7
  ) +
  geom_line(color = "#08306b") +
  scale_y_continuous(limits = c(min_serie, max_serie), breaks = breaks) +
  scale_x_date(
    breaks = seq(
      from = as.Date("2002-01-01"),
      to = as.Date("2020-02-24"), by = "year"
    ),
    date_labels = "%Y", limits = c(min(df$date), as.Date("2020-02-24"))
  ) +
  ggtitle(expression("Cotação diária do dólar (em reais), Brasil 2000 - 2020*")) +
  # ggtitle("Cotação diária do dólar (em reais), Brasil 2002 - 2020*")+
  labs(
    subtitle = paste0(
      "Série histórica da taxa de câmbio nominal"
    ),
    y = "Cotação diária em reais",
    x = " ",
    caption = paste0(
      "Fonte: IPEADATA (https://www.ipeadata.gov.br)  \n",
      "Visualização: Alysson Oliveira   \n",
      "*Limite de datas: 03/01/2000 e 31/01/2020"
    )
  ) +
  annotate("segment",
           x = date("2002-01-01"), xend = date("2002-01-01"),
           y = min_serie, yend = max_serie, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("segment",
           x = date("2011-01-01"), xend = date("2011-01-01"),
           y = min_serie, yend = max_serie, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("segment",
           x = date("2016-08-31"), xend = date("2016-08-31"),
           y = min_serie, yend = max_serie, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("segment",
           x = date("2018-12-31"), xend = date("2018-12-31"),
           y = min_serie, yend = max_serie, linetype = "dashed", color = "#6F213F"
  ) +
  annotate("text",
           x = date("2005-07-01"), y = 3.5,
           hjust = 0, color = "#451225",
           size = 3.7,
           label = paste(
             "Governo Lula \n",
             " Maior: R$", max_lula,
             "\n  Menor: R$", min_lula,
             "\n  Média: R$", media_lula,
             "\n  DP: R$", sd_lula,
             "\n  CV:", paste0(cv_lula, "%")
           )
  ) +
  annotate("text",
           x = date("2012-06-01"), y = 3.8,
           hjust = 0, color = "#451225",
           size = 3.7,
           label = paste(
             "Governo Dilma \n",
             " Maior: R$", max_dilma,
             "\n  Menor: R$", min_dilma,
             "\n  Média: R$", media_dilma,
             "\n  DP: R$", sd_dilma,
             "\n  CV:", paste0(cv_dilma, "%")
           )
  ) +
  annotate("text",
           x = date("2016-11-01"), y = 4,
           hjust = 0, color = "#451225",
           size = 2.5,
           label = paste(
             "Governo Temer \n",
             " Maior: R$", max_temer,
             "\n  Menor: R$", min_temer,
             "\n  Média: R$", media_temer,
             "\n  DP: R$", sd_temer,
             "\n  CV:", paste0(cv_temer, "%")
           )
  ) +
  annotate("text",
           x = date("2019-07-15"), y = 3,
           hjust = 0.2, color = "#451225",
           size = 3.1,
           label = paste(
             "Governo Bolsonaro \n",
             " Maior: R$ ", max_bolsonaro,
             "\n  Menor: R$", min_bolsonaro,
             "\n  Média: R$", media_bolsonaro,
             "\n  DP: R$", sd_bolsonaro,
             "\n CV:", paste0(cv_bolsonaro, "%")
           )
  ) +
  theme(
    plot.title = element_text(size = 14, colour = "black"),
    title = element_text(face = "italic", colour = "#636363"),
    plot.caption = element_text(hjust = 0, colour = "black")
  )

x2 <- grid.arrange(g, g2)


# Salvando gráfico --------------------------------------------------------



ggsave(
  filename = "tx_cambio_real.png", 
  plot = g,
  path = "figs",
  width = 35,
  height = 15,
  units = 'cm'
)

ggsave(
  filename = "tx_cambio_real.png", 
  plot = g2,
  path = "figs",
  width = 35,
  height = 15,
  units = 'cm'
)

ggsave(
  filename = "grafico.png", 
  plot = x2,
  path = "figs",
  width = 35,
  height = 30,
  units = 'cm'
)


g
