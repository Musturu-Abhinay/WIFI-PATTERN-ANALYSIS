dir.create("data", showWarnings = FALSE)
output_file <- file.path("data", "wifi_raw.csv")
stdin_con <- if (!interactive()) file("stdin", open = "r") else NULL

ask_user <- function(prompt_text) {
  cat(prompt_text)
  if (interactive()) {
    return(trimws(readline()))
  }
  return(trimws(readLines(stdin_con,n = 1)))
}

enter_pressed <- function() {
  cmd <- "if ([Console]::KeyAvailable){
   $hit = $false; 
   while ([Console]::KeyAvailable) { 
      $k = [Console]::ReadKey($true); 
      if ($k.Key -eq 'Enter') { 
      $hit = $true 
    } 
   }; 
      if ($hit) { 
          'YES' 
      }
      else {
      'NO' 
      } 
    } 
      else { 
      'NO' 
      }"
  res <- system2("powershell", args = c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = FALSE)
  return(length(res) > 0 && trimws(res[1]) == "YES")
}

get_wifi_metrics <- function() {
  lines <- system2("netsh", args = c("wlan", "show", "interfaces"), stdout = TRUE, stderr = FALSE)
  ssid <- "Disconnected"; bssid <- "None"
  channel <- -1L; sig_pct <- 0L; rx_rate <- 0.0; tx_rate <- 0.0

  for (ln in lines) {
    parts <- strsplit(ln, " : ", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      k <- trimws(parts[1]); v <- trimws(parts[2])
      if (k == "SSID") ssid <- v
      if (k == "BSSID" || k == "AP BSSID") bssid <- v
      if (k == "Channel") channel <- as.integer(v)
      if (k == "Signal") sig_pct <- as.integer(sub("%", "", v, fixed = TRUE))
      if (k == "Receive rate (Mbps)") rx_rate <- as.numeric(v)
      if (k == "Transmit rate (Mbps)") tx_rate <- as.numeric(v)
    }
  }
  rssi_dbm <- ifelse(sig_pct > 0L, as.integer((sig_pct / 2) - 100), -100L)
  
  data.frame(
    SSID = ssid, BSSID = bssid, Channel = channel,
    Signal_Pct = sig_pct, RSSI_dBm = rssi_dbm,
    Rx_Rate_Mbps = rx_rate, Tx_Rate_Mbps = tx_rate
  )
}

get_ping_metrics <- function(host = "8.8.8.8") {
  lines <- system2("ping", args = c("-n", "1", "-w", "500", host), stdout = TRUE, stderr = FALSE)
  txt <- paste(lines, collapse = " ")
  m <- regexpr("time[=<][0-9]+", txt, ignore.case = TRUE)
  if (m[1] != -1) {
    ms <- as.numeric(sub("time[=<]", "", regmatches(txt, m), ignore.case = TRUE))
    return(data.frame(Ping_ms = ms, Packet_Dropped = 0L))
  }
  return(data.frame(Ping_ms = 9999, Packet_Dropped = 1L))
}

measure_speed <- function() {
  tmp <- tempfile()
  url <- "https://speed.cloudflare.com/__down?bytes=5000000"
  t0 <- proc.time()[["elapsed"]]
  status <- download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
  elapsed <- proc.time()[["elapsed"]] - t0
  fsize <- file.info(tmp)$size; unlink(tmp)
  if (status == 0 && elapsed > 0) return(round((fsize * 8) / (elapsed * 1e6), 2))
  return(0.0)
}

first_write <- !file.exists(output_file)
zone_id <- ask_user("Enter starting Zone_ID : ")
if (zone_id == "") zone_id <- "Study_Desk"

repeat {
  cat(sprintf("\nRecording [%s] (Press ENTER to pause)\n", zone_id))
  sec_count <- 0L
  
  repeat {
    t_loop <- Sys.time()
    sec_count <- sec_count + 1L
    wifi <- get_wifi_metrics()
    ping <- get_ping_metrics()
    
    row_df <- cbind(data.frame(Zone_ID = zone_id), wifi, ping, data.frame(Download_Mbps = NA_real_))
    write.table(row_df, file = output_file, sep = ",", row.names = FALSE,
                col.names = first_write, append = !first_write, qmethod = "double")
    first_write <- FALSE
    
    cat(sprintf("[%03ds] %-15s | RSSI: %4d dBm | Rx: %5.1f Mbps | Ping: %4.0f ms\n",
                sec_count, zone_id, wifi$RSSI_dBm, wifi$Rx_Rate_Mbps, ping$Ping_ms))
    
    if (enter_pressed()) break
    spent <- as.numeric(difftime(Sys.time(), t_loop, units = "secs"))
    if (spent < 1.0) Sys.sleep(1.0 - spent)
  }
  
  choice <- ask_user("\n[1] Next Zone  [2] Speed Test  [3] Resume Same Zone  [q] Quit: ")
  if (tolower(choice) == "q") break
  if (choice == "2") {
    cat("Running 5MB speed test...\n")
    spd <- measure_speed()
    cat(sprintf("Download Speed: %.2f Mbps\n", spd))
    spd_row <- cbind(data.frame(Zone_ID = zone_id), get_wifi_metrics(), get_ping_metrics(), data.frame(Download_Mbps = spd))
    write.table(spd_row, file = output_file, sep = ",", row.names = FALSE,
                col.names = FALSE, append = TRUE, qmethod = "double")
  }
  if (choice == "1") {
    next_z <- ask_user("Enter new Zone_ID name: ")
    if (next_z != "") zone_id <- next_z
  }
}

if (!is.null(stdin_con)) close(stdin_con)
