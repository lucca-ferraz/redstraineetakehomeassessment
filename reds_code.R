setwd("~/Downloads/2025 Reds Analytics Trainee Take Home")
library(tidyverse)
data <- read_csv("data.csv")
sample_submission <- read_csv("sample_submission.csv")
predictions <- read_csv("sample_predictions.csv")
library(janitor)
data <- clean_names(data)

unique(data$pitch_type)
unique(data$pitch_name)
#take out unlabeled pitches
data |> 
  filter(is.na(pitch_name)) #493 out of 1286181 (~.04% of data is missing)

#map each pitch type to one of: fastball, breaking ball, off speed
data <- data |> 
  mutate(pitch_group = case_when(
    pitch_type %in% c("FF", "SI", "FC", "FA") ~ "Fastball",
    pitch_type %in% c("SL", "KC", "CU", "ST", "SV", "CS", "SC") ~ "Breaking Ball",
    pitch_type %in% c("CH", "FS", "EP", "KN", "FO") ~ "Off-Speed"
  ))

#group pitches by batter, calculate each batter's pitch distrubution and
#how each batter performed against each pitch
player_pitch_data <- data |> 
  filter(!is.na(pitch_group)) |> 
  group_by(player_name, game_year) |> 
  summarise(
    pitches = n(),
    at_bats = sum(!is.na(events)),
    fb_seen = sum(pitch_group == "Fastball"),
    bb_seen = sum(pitch_group == "Breaking Ball"),
    os_seen = sum(pitch_group == "Off-Speed"),
    pitch_type_fb = fb_seen / pitches, 
    pitch_type_bb = bb_seen / pitches,
    pitch_type_os = os_seen / pitches,
    strike_rate_fb = sum(type == "S" & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    strike_rate_bb = sum(type == "S" & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    strike_rate_os = sum(type == "S" & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    contact_rate_fb = sum(type == "X" & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    contact_rate_bb = sum(type == "X" & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    contact_rate_os = sum(type == "X" & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    avg_woba_fb = sum(woba_value[pitch_group == "Fastball"], na.rm = TRUE) / fb_seen,
    avg_woba_bb = sum(woba_value[pitch_group == "Breaking Ball"], na.rm = TRUE) / bb_seen,
    avg_woba_os = sum(woba_value[pitch_group == "Off-Speed"], na.rm = TRUE) / os_seen,
    avg_babip_fb = sum(babip_value[pitch_group == "Fastball"], na.rm = TRUE) / fb_seen,
    avg_babip_bb = sum(babip_value[pitch_group == "Breaking Ball"], na.rm = TRUE) / bb_seen,
    avg_babip_os = sum(babip_value[pitch_group == "Off-Speed"], na.rm = TRUE) / os_seen,
    weak_pct_fb = sum(launch_speed_angle == 1 & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    weak_pct_bb = sum(launch_speed_angle == 1 & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    weak_pct_os = sum(launch_speed_angle == 1 & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    topped_pct_fb = sum(launch_speed_angle == 2 & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    topped_pct_bb = sum(launch_speed_angle == 2 & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    topped_pct_os = sum(launch_speed_angle == 2 & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    under_pct_fb = sum(launch_speed_angle == 3 & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    under_pct_bb = sum(launch_speed_angle == 3 & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    under_pct_os = sum(launch_speed_angle == 3 & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    flare_burner_pct_fb = sum(launch_speed_angle == 4 & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    flare_burner_pct_bb = sum(launch_speed_angle == 4 & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    flare_burner_pct_os = sum(launch_speed_angle == 4 & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    solid_pct_fb = sum(launch_speed_angle == 5 & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    solid_pct_bb = sum(launch_speed_angle == 5 & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    solid_pct_os = sum(launch_speed_angle == 5 & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen,
    barrel_pct_fb = sum(launch_speed_angle == 6 & pitch_group == "Fastball", na.rm = TRUE) / fb_seen,
    barrel_pct_bb = sum(launch_speed_angle == 6 & pitch_group == "Breaking Ball", na.rm = TRUE) / bb_seen,
    barrel_pct_os = sum(launch_speed_angle == 5 & pitch_group == "Off-Speed", na.rm = TRUE) / os_seen
  ) |> 
  ungroup()

#plot pitch type distribution over time
#will training on one year and applying on another cause problems or are distrubitions consistent?
player_pitch_data |> 
  pivot_longer(cols = c(pitch_type_fb, pitch_type_bb, pitch_type_os), 
               names_to = "pitch_type", 
               values_to = "proportion") |> 
  select(game_year, proportion, pitch_type) |> 
  ggplot(aes(x = game_year, y = proportion, fill = pitch_type, group = pitch_type)) +
  geom_col(position = "fill") +
  labs(title = "Pitch Type Distribution Over Time",
       x = "Year",
       y = "Proportion of Pitches",
       fill = "Pitch Type") +
  ggthemes::theme_clean() +
  scale_fill_discrete(labels = c("Breaking Balls", "Fastballs", "Off-Speed Pitches"))

#filter data based on year
data_2021 <- player_pitch_data |> filter(game_year == 2021)
data_2022 <- player_pitch_data |> filter(game_year == 2022)
data_2023 <- player_pitch_data |> filter(game_year == 2023)

#train model: use 2021 data to predict 2022 pitch distributions
train <- data_2021 |> 
  inner_join(data_2022 |> select(player_name, response_fb = pitch_type_fb, 
                                 response_bb = pitch_type_bb,
                                 response_os = pitch_type_os)) |> 
  filter(os_seen > 0)

#test model on out-of-sample data: apply model 2022 data to predict 2023 pitch distributions
test <- data_2022 |> 
  inner_join(data_2023 |> select(player_name, response_fb = pitch_type_fb, 
                                 response_bb = pitch_type_bb,
                                 response_os = pitch_type_os)) |> 
  filter(os_seen > 0)

#baseline train RMSE: what does the accuracy look like just taking 2021 distributions to predict 2022?
baseline_train_rmse <- train |> 
  summarise(
  sqrt(mean(
    (response_fb - pitch_type_fb)^2 + (response_bb - pitch_type_bb)^2 +
      (response_os - pitch_type_os)^2
    ))) |> as.numeric() # 0.0659 train rmse using 2021 to predict 2022 without any modeling

#baseline test RMSE: what does the accuracy look like just taking 2022 distributions to predict 2023?
baseline_test_rmse <- test |> 
  summarise(
    sqrt(mean(
      (response_fb - pitch_type_fb)^2 + (response_bb - pitch_type_bb)^2 +
        (response_os - pitch_type_os)^2
    ))) |> as.numeric() # 0.0585 test rmse using 2022 to predict 2023 without any modeling

#build multinomial regression model
library(nnet)
multinom_model <- multinom(cbind(response_fb, response_bb, response_os) ~ ., 
                  data = train |> select(-player_name, -game_year, -fb_seen, -bb_seen, -os_seen),
                  maxit = 300)
summary(model)
predicted_probs_train_multinom <- predict(multinom_model, type = "probs")
logLik(model)
train_rmse_multinom <- sqrt(mean(
  (train$response_fb - predicted_probs_train_multinom[, 1])^2 +
    (train$response_bb - predicted_probs_train_multinom[, 2])^2 +
    (train$response_os - predicted_probs_train_multinom[, 3])^2))
train_rmse_multinom # 0.0477 train rmse using multinomial regression

predicted_probs_test_multinom <- predict(multinom_model, type = "probs", newdata = test)
test_rmse_multinom <- sqrt(mean(
  (test$response_fb - predicted_probs_test_multinom[, 1])^2 +
    (test$response_bb - predicted_probs_test_multinom[, 2])^2 +
    (test$response_os - predicted_probs_test_multinom[, 3])^2))
test_rmse_multinom # 0.0645 test rmse using multinomial regression

#build XGBoost model
library(xgboost)
x_train <- train |> 
  select(pitches, at_bats, pitch_type_fb:barrel_pct_os) |> 
  as.matrix()
x_test <- test |> 
  select(pitches, at_bats, pitch_type_fb:barrel_pct_os) |> 
  as.matrix()
train_response_fb <- train$response_fb
train_response_bb <- train$response_bb
train_response_os <- train$response_os

xgb_params <- list(
  objective = "reg:squarederror",  # regression task
  eval_metric = "rmse",            # error metric
  max_depth = 6,
  eta = 0.1,
  nthread = 4
)

#fit separate xgboost models for each pitch type
xg_fb <- xgboost(data = x_train, label = train_response_fb, params = xgb_params,
                 nrounds = 100, verbose = 0)
xg_bb <- xgboost(data = x_train, label = train_response_bb, params = xgb_params,
                 nrounds = 100, verbose = 0)
xg_os <- xgboost(data = x_train, label = train_response_os, params = xgb_params,
                 nrounds = 100, verbose = 0)

#adjust predictions so that the predicted percentages for each pitch type sum to 1
train_fb_xg <- predict(xg_fb, x_train)
train_bb_xg <- predict(xg_bb, x_train)
train_os_xg <- predict(xg_os, x_train)
xg_train_total <- train_fb_xg + train_bb_xg + train_os_xg
train_fb_normal <- train_fb_xg / xg_train_total
train_bb_normal <- train_bb_xg / xg_train_total
train_os_normal <- train_os_xg / xg_train_total
train_rmse_xgb <- sqrt(mean(
  (train$response_fb - train_fb_normal)^2 +
    (train$response_bb - train_bb_normal)^2 +
    (train$response_os - train_os_normal)^2))
train_rmse_xgb # 0.0012 train rmse using XGBoost

#adjust predictions so that the predicted percentages for each pitch type sum to 1
pred_fb_xg <- predict(xg_fb, x_test)
pred_bb_xg <- predict(xg_bb, x_test)
pred_os_xg <- predict(xg_os, x_test)
xg_pred_total <- pred_fb_xg + pred_bb_xg + pred_os_xg
pred_fb_normalized <- pred_fb_xg / xg_pred_total
pred_bb_normalized <- pred_bb_xg / xg_pred_total
pred_os_normalized <- pred_os_xg / xg_pred_total
test_rmse_xgb <- sqrt(mean(
  (test$response_fb - pred_fb_normalized)^2 +
    (test$response_bb - pred_bb_normalized)^2 +
    (test$response_os - pred_os_normalized)^2))
test_rmse_xgb # 0.0580 test rmse using XGBoost

data.frame(baseline_train_rmse, baseline_test_rmse, train_rmse_multinom, 
           test_rmse_multinom, train_rmse_xgb, test_rmse_xgb) |> 
  pivot_longer(cols = everything(), 
               names_to = c(".value", "model"), 
               names_pattern = "(train|test)_(.*)") |> 
  mutate(model = c("Baseline", "Multinomial", "XGBoost")) |> 
  rename(train_rmse = train, test_rmse = test) |> 
  knitr::kable()
  


# Build predictions for 2024
# find out which players in prediction set are not seen in 2023 data
predictions |> anti_join(data_2023, by = join_by(PLAYER_NAME == player_name))
# for players not seen in 2023 data, use 2022 as replacement
missing_preds <- data_2022 |> filter(player_name %in% c("Iglesias, Jose", "Hoskins, Rhys", 
                                       "Lux, Gavin", "Lopez, Otto"))
pred_data <- data_2023  |> bind_rows(missing_preds)

# apply xgboost model to make predictions
pred_matrix <- pred_data |> 
  select(pitches, at_bats, pitch_type_fb:barrel_pct_os) |> 
  as.matrix()
predicted_fb <- predict(xg_fb, pred_matrix)
predicted_bb <- predict(xg_bb, pred_matrix)
predicted_os <- predict(xg_os, pred_matrix)
predicted_total <- predicted_fb + predicted_bb + predicted_os
predicted_fb_normal <- predicted_fb / predicted_total
predicted_bb_normal <- predicted_bb / predicted_total
predicted_os_normal <- predicted_os / predicted_total

# format predictions csv as instructed in sample
pred_data$PITCH_TYPE_FB <- predicted_fb_normal
pred_data$PITCH_TYPE_BB <- predicted_bb_normal
pred_data$PITCH_TYPE_OS <- predicted_os_normal
official_predictions <- pred_data |> 
  select(PLAYER_NAME = player_name, PITCH_TYPE_FB, PITCH_TYPE_BB, PITCH_TYPE_OS) |> 
  mutate(GAME_YEAR = 2024) |> 
  inner_join(data |> select(PLAYER_NAME = player_name, BATTER_ID = batter_id)) |> 
  distinct() |> 
  select(BATTER_ID, PLAYER_NAME, GAME_YEAR, PITCH_TYPE_FB, PITCH_TYPE_BB, PITCH_TYPE_OS) |> 
  arrange(BATTER_ID)
write.csv(official_predictions, "predictions.csv", row.names = FALSE)

#predictions for all years (for use in shiny app)
all_years_data <- player_pitch_data |> 
  select(player_name:at_bats, pitch_type_fb:pitch_type_os, )

all_years_data <- data |> 
  filter(!is.na(pitch_group)) |> 
  group_by(player_name, game_year) |> 
  summarise(
    pitches = n(),
    at_bats = sum(!is.na(events)),
    fb_seen = sum(pitch_group == "Fastball"),
    bb_seen = sum(pitch_group == "Breaking Ball"),
    os_seen = sum(pitch_group == "Off-Speed"),
    pitch_type_fb = fb_seen / pitches, 
    pitch_type_bb = bb_seen / pitches,
    pitch_type_os = os_seen / pitches,
    strikeouts = sum(events == "strikeout", na.rm = TRUE),
    home_runs = sum(events == "home_run", na.rm = TRUE),
    walks = sum(events == "walk", na.rm = TRUE),
    wOBA = sum(woba_value, na.rm = TRUE) / at_bats,
    BABIP = sum(babip_value, na.rm = TRUE) / at_bats,
    ISO = sum(iso_value, na.rm = TRUE) / at_bats
    ) |> 
  ungroup() |> 
  inner_join(data |> select(player_name, batter_id)) |> 
  distinct()


# Create dataframe with all predictions for every year (for use in Shiny App)
all_matrix <- player_pitch_data |> 
  select(pitches, at_bats, pitch_type_fb:barrel_pct_os) |> 
  as.matrix()

all_fb <- predict(xg_fb, all_matrix)
all_bb <- predict(xg_bb, all_matrix)
all_os <- predict(xg_os, all_matrix)
all_total <- all_fb + all_bb + all_os
all_fb_normal <- all_fb / all_total
all_bb_normal <- all_bb / all_total
all_os_normal <- all_os / all_total

all_years_data$predicted_fb <- all_fb_normal
all_years_data$predicted_bb <- all_bb_normal
all_years_data$predicted_os <- all_os_normal

all_years_data <- all_years_data |> 
  inner_join(data |> select(player_name, batter_id)) |> 
  distinct()

write_csv(all_years_data, "all_years_stats.csv")
