
library(tidyverse)
library(pwr)
library(showtext)
## Loading Google fonts (https://fonts.google.com/)
font_add_google("EB Garamond")
showtext_auto()

Sys.setlocale("LC_ALL", "nb-NO.UTF-8")
theme_set(theme_minimal(base_size = 20) + theme(legend.position = "none",
                                                panel.grid = element_blank(),
                                                axis.text.x = element_text(angle = 45, hjust = 1)))

lavES <- 0.179
midES <- 0.358
hoyES <- 0.537

# Function to assess power
get_pwr <- function(n,es) {
  x <- pwr::pwr.t.test(n = n,d = es,sig.level = .05)
  
  return(x$power)
}


dat <- bind_rows(readxl::read_excel("studier/studier_JSNE.xlsx"),readxl::read_excel("studier/studier_metaanalyser.xlsx"))

dat <- dat |> 
  filter(N > 1) |> 
  mutate(lavES_power = map_dbl(N,~get_pwr(n = .x,es = lavES))) |> 
  mutate(midES_power = map_dbl(N,~get_pwr(n = .x,es = midES))) |> 
  mutate(hoyES_power = map_dbl(N,~get_pwr(n = .x,es = hoyES)))

dat <- dat |> 
  pivot_longer(cols = contains("_power"))

# Make groups
breaks <- c(-Inf,seq(0.1, 0.9, by = 0.1),Inf)

dat <- dat |> 
  mutate(power = cut(value, breaks = breaks, 
                     labels = c("<10%", "10–20%", "20–30%", "30–40%", 
                                "40–50%", "50–60%", "60–70%", 
                                "70–80%", "80–90%", ">90%"), 
                     right = FALSE))

# plot international studies
p1 <- dat |> 
  group_by(name) |> 
  count(power) |> 
  filter(name == "lavES_power") |> 
  mutate(over80 = power %in% c("80–90%",">90%")) |> 
  ggplot(aes(power,n,fill = over80)) + 
  geom_col(color = "black") + 
  scale_fill_manual(values = c("grey",viridis::viridis(n=1))) + 
  labs(x = "Statistisk kraft",y = "N studier",
       title = "Internasjonale studier")


dat |> 
  mutate(over80 = power %in% c("80–90%",">90%")) |> 
  group_by(name) |> 
  count(over80) |> 
  mutate(bigN = sum(n)) |> 
  ungroup() |> 
  mutate(prop = n/bigN)


dat2 <- readxl::read_excel("studier/studier_norske.xlsx")

dat2 <- dat2 |> 
  filter(N > 1) |> 
  mutate(lavES_power = map_dbl(N,~get_pwr(n = .x,es = lavES))) |> 
  mutate(midES_power = map_dbl(N,~get_pwr(n = .x,es = midES))) |> 
  mutate(hoyES_power = map_dbl(N,~get_pwr(n = .x,es = hoyES)))


dat2 <- dat2 |> 
  pivot_longer(cols = contains("_power"))


dat2 <- dat2 |> 
  mutate(power = cut(value, breaks = breaks, 
                     labels = c("<10%", "10–20%", "20–30%", "30–40%", 
                                "40–50%", "50–60%", "60–70%", 
                                "70–80%", "80–90%", ">90%"), 
                     right = FALSE))

# plot international studies
p2 <- dat2 |> 
  group_by(name) |> 
  count(power) |> 
  filter(name == "lavES_power") |> 
  mutate(over80 = power %in% c("80–90%",">90%")) |> 
  ggplot(aes(power,n,fill = over80)) + 
  geom_col(color = "black") + 
  scale_fill_manual(values = c("grey",viridis::viridis(n=1))) + 
  labs(x = "Statistisk kraft",y = "N studier",
       title = "Norske studier")


dat2 |> 
  mutate(over80 = power %in% c("80–90%",">90%")) |> 
  group_by(name) |> 
  count(over80) |> 
  mutate(bigN = sum(n)) |> 
  ungroup() |> 
  mutate(prop = n/bigN)


library(patchwork)

p1/p2


#ggsave("plots/kraft_hoved.pdf",width = 8,height = 11)
