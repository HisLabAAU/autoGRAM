library(tidyverse)
library(tidygraph)
library(ggraph)

df <- read.csv("disambiguated_entities_with_roles_location.csv") %>% 
  mutate(object_cluster_id = str_remove(object_cluster_id, "\\.\\d$"),
         Location = str_remove(Location, " \\#.+$"),
         Location = str_remove(Location, "\\*.+$"),
         Location = str_remove_all(Location, '"'),
         Location = trimws(Location)) %>% 
  rename(Action_id = 1)

groups <- unique(df$id)
sampled_groups <- sample(groups, 1)
df <- df %>% filter(id %in% sampled_groups)

instruments <- df %>% filter(str_count(Instrument) > 1) %>% 
  mutate(Instrument = str_remove(Instrument, " \\#.+$"),
         type = "object",
         Actor_id = paste(as.character(id), Instrument, sep = "_"),
         Label = Instrument) %>% 
  select(Actor_id, Label, type) %>% 
  group_by(Actor_id) %>% 
  slice(1) %>% 
  ungroup()

INSTRUMENT_RELATION <- df %>% filter(str_count(Instrument) > 1) %>% 
  mutate(relation_type = "HAS_INSTRUMENT",
         Actor_id = paste(as.character(id), Instrument, sep = "_"),
         Label = Instrument) %>% 
  select(Action_id, Actor_id, relation_type)

actors_1 <- df %>% 
  group_by(cluster_id) %>% 
  mutate(
    Title = case_when(Title == Subject ~ "", .default = Title),
    emic_labels = paste(Title, sep = "; ")) %>% 
  slice(1) %>% 
  select(cluster_id, canonical_subject, emic_labels) %>% 
  rename(Label = canonical_subject,
         Actor_id = cluster_id)

actors_2 <- df %>% 
  group_by(object_cluster_id) %>%
  slice(1) %>% 
  select(object_cluster_id, object_canonical) %>% 
  rename(Label = object_canonical)

actors_2 <- actors_2 %>% 
  anti_join(actors_1, join_by(object_cluster_id == Actor_id)) %>% 
  filter(str_count(object_cluster_id) > 1) %>% 
  rename(Actor_id = object_cluster_id)

actors <- actors_1 %>% 
  rbind(actors_2) %>%
  mutate(type = "actor")

places <- df %>% 
  select(Location, id) %>% 
  mutate(Actor_id = paste(id, "place", Location, sep = "_"),
         type = "place") %>% 
  rename(Label = Location) %>% select(-id) %>%
  group_by(Actor_id) %>%
  slice(1)

actions <- df %>% 
  select(Action_id, id, date, Infinitive, Snippet) %>% 
  mutate(type = "action", Action_id = as.character(Action_id)) %>% 
  rename(Label = Infinitive,
         Actor_id = Action_id,
         Text_id = id)

action_sequences <- df %>% 
  group_by(id) %>% 
  mutate(next_action_id = lead(Action_id),
         relation_type = "NEXT") %>% 
  ungroup() %>% 
  select(Action_id, next_action_id, relation_type) %>% 
  na.omit() %>% 
  rename(From = Action_id,
         To = next_action_id)

nodes <- actors %>% 
  rbind(instruments, places, actions) %>% 
  filter(str_count(Label) > 1)

TAKES_PLACE <- df %>% 
  filter(str_count(Location) > 1) %>%
  mutate(relation_type = location_gram_role,
         Actor_id = paste(id, "place", Location, sep = "_")) %>%
  select(Action_id, Actor_id, relation_type)

SUBJECT_RELATION <- df %>% 
  filter(str_count(cluster_id) > 1) %>%
  rename(relation_type = subject_gram_role) %>%
  select(Action_id, cluster_id, relation_type) %>% 
  rename(Actor_id = cluster_id)

OBJECT_RELATION <- df %>%
  filter(str_count(object_cluster_id) > 1) %>% 
  rename(relation_type = object_gram_role) %>% 
  select(Action_id, object_cluster_id, relation_type) %>% 
  rename(Actor_id = object_cluster_id)

TO_ACTION <- SUBJECT_RELATION %>% 
  rbind(INSTRUMENT_RELATION, OBJECT_RELATION, TAKES_PLACE) %>% 
  filter(str_count(relation_type) > 1) %>% 
  rename(From = Action_id, To = Actor_id) %>% 
  rbind(action_sequences) %>% 
  filter(!duplicated(paste(From, To, relation_type)))

Graph <- as_tbl_graph(TO_ACTION)  %>%
  activate(nodes) %>% 
  left_join(nodes, join_by(name == Actor_id))  %>% 
  mutate(Hybrid_label = case_when(str_count(emic_labels) > 1 ~ paste(Label, emic_labels, sep = "\n"), .default = Label),
         Hybrid_label = str_remove(Hybrid_label, "^to ")) %>% 
  activate(edges) %>% 
  mutate(weight = case_when(relation_type == "NEXT" ~ 2, .default = 0.2))

ggraph(Graph, layout = "fr") +
  geom_node_point(aes(colour = type), size = 3) +
  geom_node_text(aes(label = Hybrid_label),
                 size = 5, repel = TRUE) +
  geom_edge_link(aes(label = relation_type),
                 angle_calc = "along",
                 label_dodge = unit(1, "mm"),
                 alpha = 0.1, label_size = 4, label_alpha = 0.3) +
  theme_void() +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1))

df$text[1]

