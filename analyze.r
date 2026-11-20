suppressPackageStartupMessages({
  library(tidyverse)
})

expected_cols <- c(
  "Zone_ID", "SSID", "BSSID", "Channel", "Signal_Pct", "RSSI_dBm",
  "Rx_Rate_Mbps", "Tx_Rate_Mbps", "Ping_ms", "Packet_Dropped", "Download_Mbps"
)

df <- read_csv("data/wifi_raw.csv", show_col_types = FALSE)
if (!("Zone_ID" %in% names(df))) {
  df <- read_csv("data/wifi_raw.csv", col_names = FALSE, show_col_types = FALSE)
  n_cols <- min(ncol(df), length(expected_cols))
  names(df)[1:n_cols] <- expected_cols[1:n_cols]
}

df$Zone_ID        <- as.character(df$Zone_ID)
df$RSSI_dBm       <- as.numeric(df$RSSI_dBm)
df$Rx_Rate_Mbps   <- as.numeric(df$Rx_Rate_Mbps)
df$Ping_ms        <- ifelse(is.na(df$Ping_ms) | df$Ping_ms >= 9999, 500, as.numeric(df$Ping_ms))
df$Packet_Dropped <- as.integer(df$Packet_Dropped)
df$Download_Mbps  <- as.numeric(df$Download_Mbps)
df <- ungroup(fill(group_by(df, Zone_ID), Download_Mbps, .direction = "downup"))
df$Download_Mbps <- replace_na(df$Download_Mbps, 0)
df$Jitter_ms     <- abs(df$Ping_ms - lag(df$Ping_ms, default = first(df$Ping_ms)))
df$Sample_Sec    <- seq_len(nrow(df))


summary_tbl <- summarise(
  group_by(df, Zone_ID),
  Samples       = n(),
  Avg_RSSI_dBm  = round(mean(RSSI_dBm, na.rm = TRUE), 1),
  Avg_Rx_Mbps   = round(mean(Rx_Rate_Mbps, na.rm = TRUE), 1),
  Download_Mbps = round(mean(Download_Mbps, na.rm = TRUE), 2),
  Avg_Ping_ms   = round(mean(Ping_ms, na.rm = TRUE), 1),
  Avg_Jitter_ms = round(mean(Jitter_ms, na.rm = TRUE), 1),
  Loss_Pct      = round(mean(Packet_Dropped, na.rm = TRUE) * 100, 1),
  .groups = "drop"
)

print(as.data.frame(summary_tbl), row.names = FALSE)

cat("Strongest Wi-Fi Signal  : Zone", summary_tbl$Zone_ID[which.max(summary_tbl$Avg_RSSI_dBm)], "\n")
cat("Fastest Router Link     : Zone", summary_tbl$Zone_ID[which.max(summary_tbl$Avg_Rx_Mbps)], "\n")
cat("Fastest Download Speed  : Zone", summary_tbl$Zone_ID[which.max(summary_tbl$Download_Mbps)], "\n")
cat("Lowest Latency (Ping)   : Zone", summary_tbl$Zone_ID[which.min(summary_tbl$Avg_Ping_ms)], "\n")
cat("Most Stable (Low Jitter): Zone", summary_tbl$Zone_ID[which.min(summary_tbl$Avg_Jitter_ms)], "\n")


p1 <- ggplot(summary_tbl, aes(x = Zone_ID, y = Download_Mbps, fill = Zone_ID)) +
  geom_col(width = 0.5) +
  labs(title = "Download Speed by Zone", x = "Zone", y = "Download Speed (Mbps)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("plot_1_download_by_zone.png", p1, width = 7, height = 4, bg = "white")



p2 <- ggplot(df, aes(x = Zone_ID, y = Rx_Rate_Mbps, fill = Zone_ID)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Router Link Speed (Rx Rate) Distribution by Zone", x = "Zone", y = "Rx Rate (Mbps)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("plot_2_rx_rate_boxplot.png", p2, width = 7, height = 4, bg = "white")



p3 <- ggplot(df, aes(x = RSSI_dBm, y = Download_Mbps, color = Zone_ID)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Signal Strength vs. Download Speed", x = "RSSI (dBm)", y = "Download Speed (Mbps)") +
  theme_minimal()
ggsave("plot_3_rssi_vs_speed_scatter.png", p3, width = 7, height = 4, bg = "white")




df$Quality <- ifelse(df$Packet_Dropped == 1 | df$Ping_ms >= 200, "Poor",
              ifelse(df$Ping_ms >= 80 | df$Download_Mbps <= 2, "Moderate", "Good"))
df$Quality <- factor(df$Quality, levels = c("Good", "Moderate", "Poor"))

p4 <- ggplot(df, aes(x = Zone_ID, fill = Quality)) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Good" = "#2e8b57", "Moderate" = "#e69f00", "Poor" = "#d95f02")) +
  labs(title = "Connection Quality Breakdown by Zone", x = "Zone", y = "Percentage of Time", fill = "Quality") +
  theme_minimal()
ggsave("plot_4_quality_breakdown.png", p4, width = 7, height = 4, bg = "white")