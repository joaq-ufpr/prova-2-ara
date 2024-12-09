library(flexdashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggradar)

pokemon_data <- read_xlsx("./datasets/pokemon.xlsx")

head(df)
pokemon_data <- df %>%
  group_by(Name) %>%
  mutate(type_index = row_number()) %>%
  pivot_wider(names_from = type_index, values_from = Type, names_prefix = "type_") %>%
  rename(primary_type = type_1, secondary_type = type_2)

write.csv(pokemon_data, "./datasets/pokemon-cleaned.csv")

pokemon_data <- read.csv("./datasets/pokemon-cleaned.csv")

average_stats_by_type <- pokemon_data %>%
  group_by(primary_type) %>%
  summarise(
    # Avg_Total = mean(Total, na.rm = TRUE),
    Avg_HP = mean(HP, na.rm = TRUE),
    Avg_Attack = mean(Attack, na.rm = TRUE),
    Avg_Defense = mean(Defense, na.rm = TRUE),
    Avg_SpecialAttack = mean(Special.Attack, na.rm = TRUE),
    Avg_SpecialDefense = mean(Special.Defense, na.rm = TRUE),
    Avg_Speed = mean(Speed, na.rm = TRUE)) 
  # %>%
  # arrange(desc(Avg_Total))

ggradar(average_stats_by_type)

# Visualizar os resultados
print(average_stats_by_type)

radar_data <- average_stats_by_type %>%
  select(-Avg_Total) %>%
  column_to_rownames("primary_type")

radar_data <- rbind(
  rep(max(radar_data), ncol(radar_data)), # Limite superior
  rep(min(radar_data), ncol(radar_data)), # Limite inferior
  radar_data
)

# Criar o radar chart
radarchart(radar_data, axistype = 1, pcol = rainbow(nrow(radar_data) - 2), plwd = 2, cglty = 1,
           title = "Radar Chart: Médias por Tipo Principal")




strongest <- pokemon_data %>%
  group_by(primary_type) %>%
  summarise(media_total = mean(Total))

top_10_strongest <- pokemon_data %>%
  group_by(primary_type) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10) %>%
  ungroup()

ggplot(top_10_strongest, aes(x = reorder(Name, -Total), y = Total, fill = primary_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ primary_type, scales = "free_y") +
  labs(
    title = "Top 10 Pokémon Mais Fortes por Tipo",
    x = "Nome do Pokémon",
    y = "Total de Atributos",
    fill = "Tipo Pokemon"
  ) +
  theme_minimal()

ggplot(strongest, aes(x = primary_type, y = media_total)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Tipo Pokemon",
    y = "Total de Atributos"
  ) +
  theme_minimal()