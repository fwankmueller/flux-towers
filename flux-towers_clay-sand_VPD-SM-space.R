# -----------------------------------------------------------------------------------------------------------------------
# library(lubridate)
# scale_colour_discrete <- function(...) scale_colour_brewer(..., palette="Dark2") # Dark2, Set3
# options(ggplot2.continuous.colour="viridis")

# -----------------------------------------------------------------------------------------------------------------------
# fpath <- "~/Sync/Work/R/Theta-crit/data-raw/Towers-in-Detail/FLX-Hourly-for-Martin/"
filesClay <- list.files(path = "Towers-in-Detail/FLX-Hourly-for-Martin/Clay/",
                        pattern = "csv", full.names = T)
namesClay <- substr(filesClay, 113, 118)
setattr(filesClay, 'names', namesClay)

filesSand <- list.files(path = "Towers-in-Detail/FLX-Hourly-for-Martin/Sand/",
                        pattern = "csv", full.names = T)
namesSand <- substr(filesSand, 113, 118)
setattr(filesSand, 'names', namesSand)

# -----------------------------------------------------------------------------------------------------------------------
# Load and bind all data sets
clay_raw <- rbindlist(lapply(filesClay, fread, na.strings = "-9999"), fill = T, idcol = "ID")
sand_raw <- rbindlist(lapply(filesSand, fread, na.strings = "-9999"), fill = T, idcol = "ID")

# Copy raw files for data manipulation
clay <- copy(clay_raw)
sand <- copy(sand_raw)

# -----------------------------------------------------------------------------------------------------------------------
# Correct selected SWC for Panama Towers: SWC_bCanv -> SWC_F_MDS_1
clay[ID %in% c("PA-SPn", "PA-SPs"), SWC_F_MDS_1 := SWC_bConv*100]

# Compute EF column
clay[LE_F_MDS >= 0 & H_F_MDS >= 0, EF := LE_F_MDS/(LE_F_MDS + H_F_MDS)]
sand[LE_F_MDS >= 0 & H_F_MDS >= 0, EF := LE_F_MDS/(LE_F_MDS + H_F_MDS)]

# -----------------------------------------------------------------------------------------------------------------------
# Get Day from Datetimestamp
clay[, day := as.POSIXct(substr(TIMESTAMP_START, 1, 8), format = "%Y%m%d")]; clay[, datetime := as.POSIXct(substr(TIMESTAMP_START, 1, 12), format = "%Y%m%d%H%M")]
setcolorder(clay, c("ID", "day", "datetime", "VPD_F", "SWC_F_MDS_1", "EF", "TA_F", "LE_F_MDS", "H_F_MDS", "NIGHT", "PPFD_IN", "WS_F"))

sand[, day := as.POSIXct(substr(TIMESTAMP_START, 1, 8), format = "%Y%m%d")]; sand[, datetime := as.POSIXct(substr(TIMESTAMP_START, 1, 12), format = "%Y%m%d%H%M")]
setcolorder(sand, c("ID", "day", "datetime", "VPD_F", "SWC_F_MDS_1", "EF", "TA_F", "LE_F_MDS", "H_F_MDS", "NIGHT", "PPFD_IN", "WS_F"))


# -----------------------------------------------------------------------------------------------------------------------
### DATA ISSUE 1 !!!!!!
# In clay, IT-Lsn has no PPFD_IN
# In sand, AU-TTE has no PPFD_IN

# Check correlation between PPFD_IN and SW_F
# t1 <- rbind(clay, sand, fill = T) %>%
#     ggplot(aes(SW_IN_F, PPFD_IN, col = ID), size = 0.5) +
#     theme_bw() +
#     facet_wrap(~ID) +
#     geom_point()
# t1
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY+SAND", "_Corr-PPFD~SW_Singles.pdf"), width = 18, height = 12)

### EASY Approach: PPFD_IN = 2.111 x SW_IN_F based on average(PA-SPn & PA-SPs)
m1c <- lm(PPFD_IN ~ SW_IN_F, data = clay[ID == "PA-SPn"])
summary(m1c)
m2c <- lm(PPFD_IN ~ SW_IN_F, data = clay[ID == "PA-SPs"])
summary(m2c)

avg.cf <- apply(rbind(coef(m1c), coef(m2c)), 2, mean)

clay[ID == "IT-Lsn", PPFD_IN := avg.cf[1] + avg.cf[2]*SW_IN_F]
sand[ID == "AU-TTE", PPFD_IN := avg.cf[1] + avg.cf[2]*SW_IN_F]



# -----------------------------------------------------------------------------------------------------------------------
# Apply DATA Filtering before plotting:

### Soil-2: Strictest criteria - All In
clay2 <- clay %>%                                                   # 683856 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[NIGHT == 0] %>%                                               # 360854. Daytime only
    .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%  # New: Daily Average Temp.
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 246765. Only positive surface energy fluxes (--> positve EF)
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 239221. No negative VPD, SM
    .[PPFD_IN > 500] %>%                                            # 149156. Enough sunlight
    .[WS_F > 1] %>%                                                 # 115996. u > 1 m/s: vegetation-atmosphere coupling
    .[VPD_F > 5] %>%                                                # 84651. VPD > 0.5 kPa
    .[meanDayTA > 15] %>%                                           # 71721. DATemp. > 15
    na.omit()                                                       # 71721. no NA's to remove

sand2 <- sand %>%                                                   # 824064 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[NIGHT == 0] %>%                                               # 430031. Daytime only
    .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%  # New: Daily Average Temp.
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 245276. Only positive surface energy fluxes (--> positve EF)
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 205196. No negative VPD, SM
    .[PPFD_IN > 500] %>%                                            # 110437. Enough sunlight
    .[WS_F > 1] %>%                                                 # 105261. u > 1 m/s: vegetation-atmosphere coupling
    .[VPD_F > 5] %>%                                                # 84108. VPD > 0.5 kPa
    .[meanDayTA > 15] %>%                                           # 75820. DATemp. > 15
    na.omit()                                                       # 75820. no NA's to remove



### Soil-21: Only TA criterion relaxed - otherwise soil-2
clay21 <- clay %>%                                                   # 683856 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[NIGHT == 0] %>%                                               # 360854. Daytime only
    .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%  # New: Daily Average Temp.
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 246765. Only positive surface energy fluxes (--> positve EF)
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 239221. No negative VPD, SM
    .[PPFD_IN > 500] %>%                                            # 149156. Enough sunlight
    .[WS_F > 1] %>%                                                 # 115996. u > 1 m/s: vegetation-atmosphere coupling
    .[VPD_F > 5] %>%                                                # 84651. VPD > 0.5 kPa
    # .[meanDayTA > 15] %>%                                           # 71721. DATemp. > 15
    na.omit()                                                       # 84651. no NA's to remove

sand21 <- sand %>%                                                   # 824064 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[NIGHT == 0] %>%                                               # 430031. Daytime only
    .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%  # New: Daily Average Temp.
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 245276. Only positive surface energy fluxes (--> positve EF)
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 205196. No negative VPD, SM
    .[PPFD_IN > 500] %>%                                            # 110437. Enough sunlight
    .[WS_F > 1] %>%                                                 # 105261. u > 1 m/s: vegetation-atmosphere coupling
    .[VPD_F > 5] %>%                                                # 84108. VPD > 0.5 kPa
    # .[meanDayTA > 15] %>%                                           # 75820. DATemp. > 15
    na.omit()                                                       # 84108. no NA's to remove



### Soil-1: Partly Relaxed criteria
clay1 <- clay %>%                                                   # 683856 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 276425. Only positive surface energy fluxes (--> positve EF)
    .[NIGHT == 0] %>%                                               # 246765. Daytime only
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 239221. No negative VPD, SM
    .[PPFD_IN > 500] %>%                                            # 149156. Enough sunlight
    .[WS_F > 1] %>%                                                 # 115996. u > 1 m/s: vegetation-atmosphere coupling
    # .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%  # New: Daily Average Temp.
    # .[meanDayTA > 15] %>%                                           # 85882. DATemp. > 15
    # .[VPD_F > 5] %>%                                                # 73514. VPD > 0.5 kPa
    na.omit()                                                       # 115996. no NA's to remove

sand1 <- sand %>%                                                   # 824064 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 260837. Only positive surface energy fluxes (--> positve EF)
    .[NIGHT == 0] %>%                                               # 245276. Daytime only
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 205196. No negative VPD, SM
    .[PPFD_IN > 500] %>%                                            # 110437. Enough sunlight
    .[WS_F > 1] %>%                                                 # 105261. u > 1 m/s: vegetation-atmosphere coupling
    # .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%  # New: Daily Average Temp.
    # .[meanDayTA > 15] %>%                                           # 87345. DATemp. > 15
    # .[VPD_F > 5] %>%                                                # 77670. VPD > 0.5 kPa
    na.omit()                                                       # 105255. 6 NA's to remove


clay0 <- clay %>%                                                   # 683856 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 276425. Only positive surface energy fluxes (--> positve EF)
    .[NIGHT == 0] %>%                                               # 246765. Daytime only
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%
    na.omit()                                                       # 224948. Na's removed

sand0 <- sand %>%                                                   # 824064 rows.
    .[, .(SWC_F_MDS_1, VPD_F, EF, LE_F_MDS, H_F_MDS, NIGHT, PPFD_IN, WS_F, TA_F, day, ID)] %>%
    # Select cols of interest
    .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # 260837. Only positive surface energy fluxes (--> positve EF)
    .[NIGHT == 0] %>%                                               # 245276. Daytime only
    .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # 205196. No negative VPD, SM
    na.omit()                                                       # 159186.



#### ------ FINAL Data Wrangling before PLOTTING -----------------------------------------------------------------

# SOL into plot!

sol_raw <- fread("data-raw/2023-05-24_sol.csv")
sol <- sol_raw[soilClass %in% c("Clay", "Sand"), .(soilClass, E, th)]
sol[, E_mm..d := E*86400*1e3*1e-6]

sol[, VPD_sand := E_mm..d / 4 * median(sand0$VPD_F)]
sol[, VPD_clay := E_mm..d / 4 * median(clay0$VPD_F)]
sol[, VPD_sand1 := E_mm..d / 4 * median(sand1$VPD_F)]
sol[, VPD_clay1 := E_mm..d / 4 * median(clay1$VPD_F)]
sol[, VPD_sand2 := E_mm..d / 4 * median(sand2$VPD_F)]
sol[, VPD_clay2 := E_mm..d / 4 * median(clay2$VPD_F)]
sol[, VPD_sand21 := E_mm..d / 4 * median(sand21$VPD_F)]
sol[, VPD_clay21 := E_mm..d / 4 * median(clay21$VPD_F)]


## modeled thetaCrits
crities_raw <- readRDS("data/2022-11-09_crities_varL3.rds")
dthc_raw <- readRDS("data/2023-01-26_Lopt_DT-hcrit.rds")

dthc_raw[, theta := -9999] # create column that dthc theta remains theta and crities_raw becomes i.theta
setnames(dthc_raw, "name", "soil")

crities <- dthc_raw[crities_raw, on = "soil", nomatch = 0]
crities[, theta := theta(hb, -hb_cm, thetaSat, thetaRes, lambda)]

ggplot(crities, aes(soilClass, theta*100 - i.theta*100, col = theta*100 - i.theta*100)) + geom_point() + scale_color_viridis_c()

# -----------------------------------------------------------------------------------------------------------------------
# Plot MEDIAN heatmaps soil-1, soil-2 and soil in comparison

# Axes based on both soil ranges
nBreaks <- 31                                                               # No. of bins + 1
clay[, lapply(.SD, range, na.rm = T), .SDcols = c("VPD_F", "SWC_F_MDS_1")]
sand[, lapply(.SD, range, na.rm = T), .SDcols = c("VPD_F", "SWC_F_MDS_1")]
xAx <- seq(0, 65, 2.5); xAx <- seq(0, 65, length.out = nBreaks)
yAx <- seq(0, 90, 3); yAx <- seq(0, 90, length.out = nBreaks)


### CLAY1 + SAND1 - relaxed criteria
g1_clay <-
    ggplot(clay1, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Clay"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Clay"], aes(th*100, VPD_clay1, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Clay"], aes(x = theta*100, y = clay1[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g1_clay # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


# Sand Sites
g1_sand <-
    ggplot(sand1, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Sand"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Sand"], aes(th*100, VPD_sand1, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Sand"], aes(x = theta*100, y = sand1[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g1_sand # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


{g1_clay} + {g1_sand}
ggsave(paste0("outputs/", Sys.Date(), "_CLAY1+SAND1", "_Relaxed-Criteria.pdf"),
       width = 18, height = 9, device = cairo_pdf)
ggsave(paste0("outputs/", Sys.Date(), "_CLAY1+SAND1", "_Relaxed-Criteria.svg"),
       width = 18, height = 9, device = svg)
ggsave(paste0("outputs/", Sys.Date(), "_CLAY1+SAND1", "_Relaxed-Criteria_lite.svg"),
       width = 18, height = 9, device = svglite)



### CLAY2 + SAND2 - strictest criteria
g2_clay <-
    ggplot(clay2, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Clay"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Clay"], aes(th*100, VPD_clay2, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Clay"], aes(x = theta*100, y = clay2[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g2_clay # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


# Sand Sites
g2_sand <-
    ggplot(sand2, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Sand"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Sand"], aes(th*100, VPD_sand2, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Sand"], aes(x = theta*100, y = sand2[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g2_sand # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


{g2_clay} + {g2_sand}
ggsave(paste0("outputs/", Sys.Date(), "_CLAY2+SAND2", "_Strictest-Criteria.pdf"),
       width = 18, height = 9, device = cairo_pdf)


### CLAY2 + SAND2 - strictest criteria
g21_clay <-
    ggplot(clay21, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Clay"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Clay"], aes(th*100, VPD_clay21, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Clay"], aes(x = theta*100, y = clay21[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g21_clay # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


# Sand Sites
g21_sand <-
    ggplot(sand21, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Sand"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Sand"], aes(th*100, VPD_sand21, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Sand"], aes(x = theta*100, y = sand21[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g21_sand # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


{g21_clay} + {g21_sand}
ggsave(paste0("outputs/", Sys.Date(), "_CLAY21+SAND21", "_TA-relaxed_quasi-Soil2.pdf"),
       width = 18, height = 9, device = cairo_pdf)




### CLAY + SAND - quasi raw (Night, negative LE+H, negative VPD|SM & NA's removed)
g_clay <-
    ggplot(clay0, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Clay"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Clay"], aes(th*100, VPD_clay, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Clay"], aes(x = theta*100, y = clay0[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g_clay # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


# Sand Sites
g_sand <-
    ggplot(sand0, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
    theme_bw() +
    scale_fill_viridis_c() +
    stat_summary_2d(fun = median, breaks = list(x = xAx, y = yAx)) + # default positioning center over data location
    geom_hline(aes(yintercept = median(VPD_F)), linetype = "dashed", col = "white") +
    geom_vline(data = crities[soil == "Sand"], aes(xintercept = theta*100), linetype = "dashed", col = "white") +
    geom_path(data = sol[soilClass == "Sand"], aes(th*100, VPD_sand, z = NULL), col = "red") +
    geom_point(data = crities[soil == "Sand"], aes(x = theta*100, y = sand0[, median(VPD_F)], z = NULL), col = "black") +
    scale_x_continuous(limits = range(xAx), expand = c(0,0)) +
    scale_y_continuous(limits = range(yAx), expand = c(0,0)) +
    labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
         fill = "EF [-]",
         x = bquote(italic(theta)~"[-]"),
         y = bquote(VPD~"[hPa]"))
g_sand # + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)


{g_clay} + {g_sand}
ggsave(paste0("outputs/", Sys.Date(), "_CLAY0+SAND0", "_Quasi-raw.pdf"),
       width = 18, height = 9, device = cairo_pdf)


# -----------------------------------------------------------------------------------------------------------------------
### FINISH
# -----------------------------------------------------------------------------------------------------------------------






#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# g_clay <-
#     ggplot(clay, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
#     theme_bw() +
#     scale_fill_viridis_c() +
#     # stat_summary_2d() +
#     stat_summary_2d(fun = median, breaks = list(x = pretty(clay$SWC_F_MDS_1, n = 30), y = seq(0, 50, 5/3))) +
#     scale_x_continuous(limits = c(0, 58)) +
#     labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
#          fill = "EF [-]")
# g_clay
#
# g1_clay <-
#     ggplot(clay1, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
#     theme_bw() +
#     scale_fill_viridis_c() +
#     # stat_summary_2d() +
#     stat_summary_2d(fun = median, breaks = list(x = pretty(clay$SWC_F_MDS_1, n = 30), y = seq(0, 50, 5/3))) +
#     scale_x_continuous() +
#     labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
#          fill = "EF [-]")
# g1_clay
#
# g0_clay <-
#     ggplot(clay1, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
#     theme_bw() +
#     scale_fill_gradient2() +
#     # stat_summary_2d() +
#     stat_summary_2d(fun = function(x) median(x) - mean(x), breaks = list(x = pretty(clay$SWC_F_MDS_1, n = 30), y = seq(0, 50, 5/3))) +
#     scale_x_continuous() +
#     labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
#          fill = "EF [-]")
# g0_clay
# # ggsave(paste0("outputs/", Sys.Date(), "_CLAY", "_HeatmapNicer.pdf"), width = wdh, height = wdh, units = "mm")
#
#
# # Sand Sites
# sand1 <- sand %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[PPFD_IN > 500] %>%
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[VPD_F > 5] %>%
#     .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     .[meanDayTA > 15]
#
#
# g1_sand <-                                          # meanDayVPD > 0.5
#     ggplot(sand1, aes(x = SWC_F_MDS_1, y = VPD_F, z = EF)) +
#     theme_bw() +
#     stat_summary_2d(fun = median, bins = 50) +
#     scale_fill_viridis_c() +
#     labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
#          fill = "EF [-]")
# g1_sand
#
# {g1_clay + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)} + {g1_sand + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)}
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY+SAND", "_EF_NIGHT_PPFD_WS_VPD_TA_+AU-TTE+IT-Lsn_NA-MEDIAN.pdf"),
#        width = 18, height = 9)
#
#
# # -----------------------------------------------------------------------------------------------------------------------
#
#
# # -----------------------------------------------------------------------------------------------------------------------
# ############################# NEW TESTS
#
# ### MANUAL Binning data
#
# set.seed(12345)
# l <- 1e4
# demo <- data.table(x = runif(l, 0, 60), y = runif(l, 0, 90), data = runif(l, min = 0, max = 1))
# demo[, xint := findInterval(x, seq(0, 60, length.out = 31))]
# demo[, yint := findInterval(y, seq(0, 90, length.out = 31))]
#
# demo[, xint2 := xint * 2 -1]
# demo[, yint2 := yint * 3 -1.5]
#
# ggplot(demo, aes(xint2, yint2, fill = data)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# demo[, medX := median(data), .(xint2, yint2)]
# demo[, meanX := mean(data), .(xint2, yint2)]
#
# ggplot(demo, aes(xint2, yint2, fill = medX)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ggplot(demo, aes(xint2, yint2, fill = meanX)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ggplot(demo, aes(xint2, yint2, fill = medX-meanX)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ggplot(demo, aes(xint2, yint2, fill = data-medX)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ### REASON found why geom_tile(stat = "identity") (default) is different from mean/median per tile: It uses last observation per tile!
# # https://stackoverflow.com/questions/66958570/summarising-data-before-plotting-with-geom-tile-renders-different-results
#
# # Summarize either manually or correct on the fly in ggplot using stat_summary(geom = "tile")
#
# ggplot(demo, aes(x = x, y = y, z = data)) +
#     theme_bw() +
#     stat_summary_2d(geom = "tile", breaks = list(x = pretty(demo$x, n = 30), y = seq(0, 90, 3))) +
#     scale_x_continuous(limits = c(0, 65)) +
#     # scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ggplot(demo, aes(x = round(x, 2), y = round(y, 1), z = data)) +
#     theme_bw() +
#     stat_summary_2d(geom = "tile", bins = 30) +
#     # scale_x_continuous(limits = c(0, 65)) +
#     # scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ggplot(demo, aes(x = xint2, y = yint2, z = data)) +
#     theme_bw() +
#     stat_summary_2d(bins = 29) +
#     # scale_x_continuous(limits = c(0, 65)) +
#     # scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# ggplot(demo, aes(x = xint2, y = yint2, fill = meanX)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     # scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
#
# ggplot(diamonds, aes(carat, depth)) +
#     stat_summary_2d(aes(z = price))
#
# ggplot(diamonds, aes(carat, depth, z = price)) +
#     geom_tile(aes(fill = price))
#
# # ----------------------------------------------------------------------------------------
#
# ggplot(unique(demo, by = c("xint2", "yint2")), aes(xint2, yint2, fill = data-medX)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
# # demo[, xint3 := xint * 2]
# # demo[, yint3 := yint * 3]
#
# ggplot(demo, aes(xint, yint, fill = data)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 65)) +
#     scale_y_continuous(limits = c(0, 95)) +
#     scale_fill_viridis_c()
#
#
# # ggplot(demo, aes(xint3, yint3, fill = data)) +
# #     theme_bw() +
# #     geom_tile() +
# #     scale_x_continuous(limits = c(0, 65)) +
# #     scale_y_continuous(limits = c(0, 95)) +
# #     scale_fill_viridis_c()
#
#
# demo[, binx := cut(x, breaks = seq(0, 60, length.out = 31), dig.lab = 1)]
#
# ggplot(demo, aes(binx, yint, fill = data)) +
#     theme_bw() +
#     geom_tile() +
#     # scale_x_continuous(limits = c(0, 20)) +
#     scale_fill_viridis_c()
#
# xmin <- 0; xmax <- 60
# ymin <- 0; ymax <- 90
# nbin <- 30
#
# demo[, xbin := as.numeric(cut(x, breaks = seq(xmin, xmax, length.out = nbin+1),
#                               labels = (-1 + seq(xmin, xmax, length.out = nbin+1)[-1])))]
# demo[, ybin := as.numeric(cut(y, breaks = seq(ymin, ymax, length.out = nbin+1),
#                               labels = (-1.5+seq(ymin, ymax, length.out = nbin+1)[-1])))]
#
# demo[, xbin2 := cut(x, breaks = seq(xmin, xmax, length.out = nbin+1), labels = (-1 + seq(xmin, xmax, length.out = nbin+1)[-1]))]
# demo[, xbin2 := as.numeric(levels(xbin2))]
# demo[, ybin := as.numeric(cut(y, breaks = seq(ymin, ymax, length.out = nbin+1),
#                               labels = (-1.5+seq(ymin, ymax, length.out = nbin+1)[-1])))]
#
# summary(demo)
#
# ggplot(demo, aes(xbin, ybin, fill = data)) +
#     theme_bw() +
#     geom_tile() +
#     scale_x_continuous(limits = c(0, 60)) +
#     scale_fill_viridis_c()
#
# ### Unbinned DATA - raw
# ggplot(demo, aes(x,y, col = data, fill = data)) +
#     theme_bw() +
#     geom_tile() +
#     scale_fill_viridis_c() +
#     scale_color_viridis_c()
#
# # as.factor()
# ggplot(demo[1:100], aes(factor(x), factor(y), col = data, fill = data)) +
#     theme_bw() +
#     geom_tile() +
#     scale_fill_viridis_c() +
#     scale_color_viridis_c()
#
# ggplot(demo, aes(x,y, col = data, fill = data)) +
#     geom_tile() +
#     geom_point() +
#     scale_fill_viridis_c() +
#     scale_color_viridis_c()
#
# ggplot(demo, aes(x,y, fill = data)) +
#     geom_tile() +
#     scale_x_binned() +
#     scale_y_binned() +
#     scale_fill_viridis_c()
#
#
#
# image(z = t(as.matrix(clay[, .(VPD_F, SWC_F_MDS_1)])), legend.lab = "Temperature (°C)",
#            ylab = "Depth (m)", xlab = "Time")
#
#
# # create tidy version of volcano data
# nx = 87
# ny = 61
# volcano_data <- data.frame(height = c(volcano), x = rep(1:nx, ny), y = rep(1:ny, each = nx))
#
# # take a look at the dataset. it's indeed tidy.
# head(volcano_data)
# #   height x y
# # 1    100 1 1
# # 2    101 2 1
# # 3    102 3 1
# # 4    103 4 1
# # 5    104 5 1
# # 6    105 6 1
#
# # plot
# ggplot(volcano_data, aes(x, y, fill=height)) +
#   geom_raster() +
#     # scale_x_binned(n.breaks = 87) +
#     # scale_y_binned(n.breaks = 61) +
#   coord_fixed(expand = FALSE) +
#   scale_fill_viridis_c()
#
# str(iris)
# ggplot(iris, aes(Sepal.Width, Sepal.Length, fill=Petal.Length)) +
#   geom_raster() +
#     # scale_x_binned(breaks = ) +
#     # scale_y_binned(n.breaks = 10) +
#   coord_fixed(expand = FALSE) +
#   scale_fill_viridis_c()
#
#
# # -----------------------------------------------------------------------------------------------------------------------
# ### Try pipe and ggplot for convenient on/off of multiple criteria to remove nightime data etc.
#
# ### Previous attempts
# # .[VPD_F >= 0 & SWC_F_MDS_1 >= 0] %>%          # Outliers: VPD & SWC --> NO effect
# # .[NETRAD > 50] %>%                              # Daytime only: Novick et al. 2016
#
#
# ######## --------- UPDATE: All plots before 15:39 on 31.05. were wrongly VPD-filtered (>0.5 hPa instead of >0.5 kPa) !!!!
#
# # Clay Sites
# g1_clay <- clay %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := mean(PPFD_IN), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[, .(SWC_F_MDS_1, VPD_F, EF, ID)] %>%
#     na.omit() %>%
#     .[] %>%
#     # .[, meanDayVPD := mean(VPD_F), by = .(ID, day)] %>%             # New: Daily Average VPD
#     # .[meanDayVPD > 5] %>%                                         # meanDayVPD > 0.5
#     # .[, meanDayTA := mean(TA_F), by = .(ID, day)] %>%               # New: Daily Average VPD
#     # .[meanDayTA > 15] %>%                                           # meanDayVPD > 0.5
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
#     theme_bw() +
#     geom_raster() +
#     # geom_raster(hjust = 0.5, vjust = 0.5) + # default positioning center over data location
#     # geom_raster(hjust = 0, vjust = 0) + # default positioning center over data location
#     # geom_raster(hjust = 1, vjust = 1) + # default positioning center over data location
#     # geom_rect(aes(xmin = 0, xmax = 60, ymin = 5, ymax = 50)) + # default positioning center over data location
#     scale_fill_viridis_c() +
#     scale_x_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 65), name = "SM [%]") +
#     scale_y_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 90), name = "VPD [hPa]") +
#     # geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
#          fill = "EF [-]")
# g1_clay
#
# # Sand Sites
# g1_sand <- sand %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := mean(PPFD_IN), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     # .[, meanDayVPD := mean(VPD_F), by = .(ID, day)] %>%             # New: Daily Average VPD
#     # .[meanDayVPD > 5] %>%                                         # meanDayVPD > 0.5
#     # .[, meanDayTA := mean(TA_F), by = .(ID, day)] %>%               # New: Daily Average VPD
#     # .[meanDayTA > 15] %>%                                           # meanDayVPD > 0.5
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
#     theme_bw() +
#     geom_raster(hjust = 0.5, vjust = 0.5) + # default positioning center over data location
#     scale_fill_viridis_c() +
#     scale_x_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 65), name = "SM [%]") +
#     scale_y_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 90), name = "VPD [hPa]") +
#     geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
#          fill = "EF [-]")
# g1_sand
#
# g1_clay / g1_sand
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY+SAND", "_EF_NIGHT_PPFD_WS_VPD_TA.pdf"), width = 9, height = 18)
# dev.off()
#
# # -----------------------------------------------------------------------------------------------------------------------
# # Plot heatmaps with corrected data (PPFD_IN) & remove NA doing mean()
#
# ### ISSUE 1 !!!!!!
# # In clay, IT-Lsn has no PPFD_IN
# # In sand, AU-TTE has no PPFD_IN
# # --> all plots from morning of 31.05.23 are without the two sites!
#
# # Check correlation between PPFD_IN and SW_F
# t1 <- rbind(clay, sand, fill = T) %>%
#     ggplot(aes(SW_IN_F, PPFD_IN, col = ID), size = 0.5) +
#     theme_bw() +
#     facet_wrap(~ID) +
#     geom_point()
# t1
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY+SAND", "_Corr-PPFD~SW_Singles.pdf"), width = 18, height = 12)
#
# ### EASY Approach: PPFD_IN = 2.111 x SW_IN_F based on average(PA-SPn & PA-SPs)
# m1c <- lm(PPFD_IN ~ SW_IN_F, data = clay[ID == "PA-SPn"])
# summary(m1c)
# m2c <- lm(PPFD_IN ~ SW_IN_F, data = clay[ID == "PA-SPs"])
# summary(m2c)
#
# avg.cf <- apply(rbind(coef(m1c), coef(m2c)), 2, mean)
#
# clay[ID == "IT-Lsn", PPFD_IN := avg.cf[1] + avg.cf[2]*SW_IN_F]
# sand[ID == "AU-TTE", PPFD_IN := avg.cf[1] + avg.cf[2]*SW_IN_F]
#
#
# ### Plot heatmaps now
#
# # Clay Sites
# g1_clay <- clay %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := mean(PPFD_IN, na.rm = T), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[, meanDayVPD := mean(VPD_F, na.rm = T), by = .(ID, day)] %>%             # New: Daily Average VPD
#     .[meanDayVPD > 5] %>%                                           # meanDayVPD > 0.5 kPa
#     .[, meanDayTA := mean(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     .[meanDayTA > 15] %>%                                           # meanDayTA > 15 °C
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
#     theme_bw() +
#     geom_raster(hjust = 0.5, vjust = 0.5) + # default positioning center over data location
#     scale_fill_viridis_c() +
#     scale_x_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 65), name = "SM [%]") +
#     scale_y_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 90), name = "VPD [hPa]") +
#     geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
#          fill = "EF [-]")
# g1_clay
#
# # Sand Sites
# g1_sand <- sand %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := mean(PPFD_IN, na.rm = T), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[, meanDayVPD := mean(VPD_F, na.rm = T), by = .(ID, day)] %>%             # New: Daily Average VPD
#     .[meanDayVPD > 5] %>%                                           # meanDayVPD > 0.5 kPa
#     .[, meanDayTA := mean(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     .[meanDayTA > 15] %>%                                           # meanDayVPD > 0.5
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
#     theme_bw() +
#     geom_raster(hjust = 0.5, vjust = 0.5) + # default positioning center over data location
#     scale_fill_viridis_c() +
#     scale_x_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 65), name = "SM [%]") +
#     scale_y_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 90), name = "VPD [hPa]") +
#     geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
#          fill = "EF [-]")
# g1_sand
#
# g1_clay / g1_sand
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY+SAND", "_EF_NIGHT_PPFD_WS_VPD_TA_+AU-TTE+IT-Lsn_NA-mean.pdf"), width = 9, height = 18)
# dev.off()
#
#
# # -----------------------------------------------------------------------------------------------------------------------
# #### ------ FINAL PLOTS -----------------------------------------------------------------
# # -----------------------------------------------------------------------------------------------------------------------
# # Plot MEDIAN heatmaps with corrected data (PPFD_IN) & remove NA doing MEDIAN()
#
# # Clay Sites
# nBreaks <- 30
# xClayLim <- c(0, 64)
# yClayLim <- c(0, 90)
#
# xClay_nice <- seq(xClayLim[1], xClayLim[2], abs(xClayLim[1] - xClayLim[2]) / nBreaks); xClay_nice[seq(2, length(xClay_nice), 2)] <- ""            # Nicer x breaks
# yClay_nice <- seq(yClayLim[1], yClayLim[2], abs(yClayLim[1] - yClayLim[2]) / (nBreaks)); yClay_nice[seq(2, length(yClay_nice), 2)] <- ""            # Nicer y breaks
#
# g1_clay <- clay %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := median(PPFD_IN, na.rm = T), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     # .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[PPFD_IN > 500] %>%
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[, meanDayVPD := median(VPD_F, na.rm = T), by = .(ID, day)] %>%             # New: Daily Average VPD
#     # .[meanDayVPD > 5] %>%                                           # meanDayVPD > 0.5 kPa
#     .[VPD_F > 5] %>%
#     .[, .(SWC_F_MDS_1, VPD_F, EF, ID)] %>%
#     na.omit() %>%
#     # .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     # .[meanDayTA > 15] %>%                                           # meanDayVPD > 0.5
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
#     theme_bw() +
#     scale_fill_viridis_c() +
#     scale_x_binned(n.breaks = nBreaks, nice.breaks = T, limits = xClayLim, name = "SM [%]", expand = c(0, 0)) +
#     scale_y_binned(n.breaks = nBreaks, nice.breaks = T, limits = yClayLim, name = "VPD [hPa]", expand = c(0, 0)) +
#     geom_tile() + # default positioning center over data location
#     geom_rug(aes(colour = EF)) +
#     stat_contour(aes(z = EF)) +
#     geom_hex() +
#     # geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     labs(title = "Clay Sites: US-MMS, CH-Oe2, IT-Lsn, PA-SPn, PA-SPs",
#          fill = "EF [-]")
# g1_clay + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)
# # ggsave(paste0("outputs/", Sys.Date(), "_CLAY", "_HeatmapNicer.pdf"), width = wdh, height = wdh, units = "mm")
#
#
# # Sand Sites
#
# xSand_nice <- seq(0, 64, 2); xSand_nice[seq(2, length(xSand_nice), 2)] <- ""            # Nicer x breaks
# ySand_nice <- seq(0, 90, 3); ySand_nice[seq(2, length(ySand_nice), 2)] <- ""            # Nicer y breaks
#
# g1_sand <- sand %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[PPFD_IN > 500] %>%
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[VPD_F > 5] %>%
#     .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     .[meanDayTA > 15] %>%                                           # meanDayVPD > 0.5
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
#     theme_bw() +
#     geom_raster(hjust = 0.5, vjust = 0.5) + # default positioning center over data location
#     scale_fill_viridis_c() +
#     scale_x_binned(n.breaks = 30, nice.breaks = T, limits = c(0, 64), name = "SM [%]", expand = c(0, 0)) +
#     scale_y_binned(n.breaks = 30, nice.breaks = F, limits = c(5, 90), name = "VPD [hPa]", expand = c(0, 0)) +
#     # geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     labs(title = "Sand Sites: AU-TTE, FR-Bil, NL-Loo, SN-Dhr, ZM-Mon",
#          fill = "EF [-]")
# g1_sand + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)
#
# {g1_clay + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)} + {g1_sand + geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)}
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY+SAND", "_EF_NIGHT_PPFD_WS_VPD_TA_+AU-TTE+IT-Lsn_NA-MEDIAN.pdf"),
#        width = 18, height = 9)
# # dev.off()
#
# # -----------------------------------------------------------------------------------------------------------------------
# ### Calculate average VPD of site plateaus & average VPD for all!
#
# thMeas_raw <- readRDS("data/2022-11-09_theta-all.rds")
# thMeas <- copy(thMeas_raw)
#
#
# # Correct PA-nama thetaCrit (from Louis' mail Mon 15/05/2023 18:49): PA-Spn 0.31; PA-Sps 0.345
# thMeas[Fluxnet_site == "PA-SPn", thetaCrit := 0.31]
# thMeas[Fluxnet_site == "PA-SPs", thetaCrit := 0.345]
#
# # join observed thetaCrit [thMeas] + sand
# sand2 <- sand[thMeas, on = c(ID = "Fluxnet_site"), nomatch = 0] %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := median(PPFD_IN, na.rm = T), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     # .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[PPFD_IN > 500] %>%
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[, meanDayVPD := median(VPD_F, na.rm = T), by = .(ID, day)] %>%             # New: Daily Average VPD
#     # .[meanDayVPD > 5] %>%                                           # meanDayVPD > 0.5 kPa
#     .[VPD_F > 5] %>%
#     .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     .[meanDayTA > 15] %>%
#     .[SWC_F_MDS_1 > thetaCrit*100, meanPlatVPD := median(VPD_F, na.rm = T), by = .(ID)]
#
# sand2[, summary(meanPlatVPD)]
# sand2[, summary(VPD_F)]
#
#
# # sand2[SWC_F_MDS_1 <= thetaCrit, c("ID", "day", "datetime", "VPD_F", "SWC_F_MDS_1", "EF", "TA_F", "LE_F_MDS", "H_F_MDS", "NIGHT", "PPFD_IN", "WS_F"), by = ID, with = F]
# # sand2[SWC_F_MDS_1 <= thetaCrit, as.list(summary(VPD_F)), by = ID]
#
# # join observed thetaCrit [thMeas] + clay
# clay2 <- clay[thMeas, on = c(ID = "Fluxnet_site"), nomatch = 0] %>%
#     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
#     .[NIGHT == 0] %>%                                               # Daytime only:
#     .[, meanDayPPFD := median(PPFD_IN, na.rm = T), by = .(ID, day)] %>%          # New: Daily Average PPFD
#     # .[meanDayPPFD > 500] %>%                                        # meanDayPPFD > 500 µmol..
#     .[PPFD_IN > 500] %>%
#     .[WS_F > 1] %>%                                                 # u > 1 m/s:
#     .[, meanDayVPD := median(VPD_F, na.rm = T), by = .(ID, day)] %>%             # New: Daily Average VPD
#     # .[meanDayVPD > 5] %>%                                           # meanDayVPD > 0.5 kPa
#     .[VPD_F > 5] %>%
#     .[, meanDayTA := median(TA_F, na.rm = T), by = .(ID, day)] %>%               # New: Daily Average VPD
#     .[meanDayTA > 15] %>%
#     .[SWC_F_MDS_1 > thetaCrit*100, meanPlatVPD := median(VPD_F, na.rm = T), by = .(ID)]
#
# clay2[, summary(meanPlatVPD)]
# clay2[, summary(VPD_F)]
#
#
# # -----------------------------------------------------------------------------------------------------------------------
# ### Density-Plots
# # Sand
# g2_sand <-
#     sand2 %>%
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F)) +
#     geom_bin_2d(bins = 30) +
#     geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5) +
#     scale_color_gradient()
# g2_sand
#
# # Clay
# g2_clay <-
#     clay2 %>%
#     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F)) +
#     geom_bin_2d(bins = 30) +
#     geom_density2d(contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.5)
# g2_clay
#
# # Save together
# g2_clay + g2_sand
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY1+SAND1", "_Density-Plots.pdf"),
#        width = 18, height = 9)
# # dev.off()
#
#
# newdat_sand <- setDT(ggplot_build(g2_sand)$data[[1]])
#
# g1_sand + geom_text(data = newdat_sand[ncount > 0.05, .SD, by = .(xbin, ybin)],
#                     aes(fill = NULL, (xmin + xmax)/2, (ymin + ymax)/2, label=count), col="white", size = 2)
#
#
# newdat_clay <- setDT(ggplot_build(g2_clay)$data[[1]])
#
# g1_clay + geom_text(data = newdat_clay[ncount > 0.05, .SD, by = .(xbin, ybin)],
#                     aes(fill = NULL, (xmin + xmax)/2, (ymin + ymax)/2, label=count), col="white", size = 2)
#
# # -----------------------------------------------------------------------------------------------------------------------
# ### SOL into plot!
#
# sol_raw <- fread("data-raw/2023-05-24_sol.csv")
# sol <- sol_raw[soilClass %in% c("Clay", "Sand"), .(soilClass, E, th)]
# sol[, E_mm..d := E*86400*1e3*1e-6]
# sol[, VPD_sand := E_mm..d / 4 * median(sand2$VPD_F)]
# sol[, VPD_clay := E_mm..d / 4 * median(clay2$VPD_F)]
#
# ggplot(sol[soilClass == "Clay"], aes(th*100, E_mm..d, col = soilClass)) +
#     theme_bw() +
#     geom_path() +
#     geom_point()
#
# gsol_clay <-
# ggplot(sol[soilClass == "Clay"], aes(th*100, E_mm..d)) +
#     theme_void() +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     geom_path(show.legend = F, col = "red", linewidth = 2)
# gsol_clay
#
#
# g3_clay <-
#     g1_clay +
#     # geom_text(data = newdat_clay[ncount > 0.05, .SD, by = .(xbin, ybin)],
#     #                 aes(fill = NULL, (xmin + xmax)/2, (ymin + ymax)/2, label=count), col="white", size = 2) +
#     scale_x_continuous(sec.axis = )
#     geom_path(data = sol[soilClass == "Clay"], aes(th*100, VPD_clay, fill = NULL, col = "median")) +
#     geom_path(data = sol[soilClass == "Clay"], aes(th*100, E_mm..d/4*mean(clay2$VPD_F), fill = NULL, col = "mean")) +
#     scale_color_manual(values = c("red", "black"))
# g3_clay
#
# g3_sand <-
#     g1_sand +
#     geom_text(data = newdat_sand[ncount > 0.05, .SD, by = .(xbin, ybin)],
#                     aes(fill = NULL, (xmin + xmax)/2, (ymin + ymax)/2, label=count), col="white", size = 2) +
#     geom_path(data = sol[soilClass == "Sand"], aes(th*100, VPD_sand, fill = NULL, col = "median")) +
#     geom_path(data = sol[soilClass == "Sand"], aes(th*100, E_mm..d/4*mean(sand2$VPD_F), fill = NULL, col = "mean")) +
#     scale_color_manual(values = c("red", "black"))
# g3_sand
#
# # Save together
# g3_clay + g3_sand
# ggsave(paste0("outputs/", Sys.Date(), "_CLAY2+SAND2", "_Heatmaps+ncounts-5pc_SOLs-mean-median-allVPD.pdf"),
#        width = 18, height = 9)
# # dev.off()
#
#
# sol[, summary(th), soilClass]
# sol[, summary(E_mm..d), soilClass]
#
# cl_max <- sol[soilClass == "Clay", max(E_mm..d, na.rm = T)] / 4 * clay2[, median(VPD_F, na.rm = T)]
# sa_max <- sol[soilClass == "Sand", max(E_mm..d, na.rm = T)] / 4 * sand2[, median(VPD_F, na.rm = T)]
#
# ### Limits: x 0-64; y 0-90
# # Clay: x: 24 -> 0, 47.5 -> 64; y: 0.017 -> 0, 8.16/4*med(VPD) -> 90
# gFinal_clay <-
#     g1_clay +
#     geom_density2d(bins = 5, contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.05) +
#     labs(title = "Clay") +
#     theme(plot.title = element_text(face = "bold", hjust = 0.5),
#           legend.position = c(0.4, 0.8),
#           legend.key.size = unit(3, "mm"),
#           legend.text = element_text(size = lg.tSize)) +
#     inset_element(
#     ggplot(sol[soilClass == "Clay"], aes(th*100, E_mm..d)) +
#         theme_void() +
#         coord_cartesian(clip =  "off") +
#         scale_x_continuous(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         geom_path(show.legend = F, col = "red", linewidth = 0.5),
#     left = 24/64, bottom = 0.017/90, right = 47.5/64, top = cl_max/90, align_to = "panel", clip = F)
# gFinal_clay
# # ggsave(paste0("outputs/", Sys.Date(), "_ClayHeatMap+SOL.pdf"),
# #        width = wdh*2, height = wdh*2, units = "mm")
#
# # Sand: x: 3 -> 0, 43.7 -> 64; y: 0.001 -> 0, 8.18/4*med(VPD) -> 90
# gFinal_sand <-
#     g1_sand +
#     geom_density2d(bins = 5, contour_var = "count", col = "white", linetype = "dotted", linewidth = 0.05) +
#     labs(title = "Sand") +
#     theme(plot.title = element_text(face = "bold", hjust = 0.5),
#           legend.position = c(0.4, 0.8),
#           legend.key.size = unit(3, "mm"),
#           legend.text = element_text(size = lg.tSize)) +
#     inset_element(
#     ggplot(sol[soilClass == "Sand"], aes(th*100, E_mm..d)) +
#         theme_void() +
#         coord_cartesian(clip =  "off") +
#         scale_x_continuous(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         geom_path(show.legend = F, col = "red", linewidth = 0.5),
#     left = 3/64, bottom = 0.001/90, right = 43.7/64, top = sa_max/90, align_to = "panel", clip = F)
# gFinal_sand
#
#
# ### Save together
# gFinal_clay + gFinal_sand
# ggsave(paste0("outputs/", Sys.Date(), "_finalCLAY+finalSAND_MEDIAN-HeatMaps+SOL.pdf"), width = wdh*2, height = wdh, units = "mm", device = cairo_pdf)
# ggsave(paste0("outputs/", Sys.Date(), "_finalCLAY+finalSAND_MEDIAN-HeatMaps+SOL_svglite.svg"), width = wdh*2, height = wdh, units = "mm", device = svglite)
# ggsave(paste0("outputs/", Sys.Date(), "_finalCLAY+finalSAND_MEDIAN-HeatMaps+SOL.svg"), width = wdh*2, height = wdh, units = "mm", device = svg)
# ggsave(paste0("outputs/", Sys.Date(), "_finalCLAY+finalSAND_MEDIAN-HeatMaps+SOL.emf"), width = wdh*2, height = wdh, units = "mm")
#
# #
# # ######
# # # -----------------------------------------------------------------------------------------------------------------------
# # ### EF ~ SM to check
# #
# # ### PLOTS
# # # Sand with criteria
# # ggplot(sand2, aes(SWC_F_MDS_1, EF, col = VPD_F), size = 0.1) +
# #     theme_bw() +
# #     geom_point() +
# #     scale_color_viridis_c(option = "turbo") +
# #     facet_wrap(~ID) +
# #     geom_vline(aes(xintercept = thetaCrit*100), linetype = "dashed")
# # ggsave(paste0("outputs/", Sys.Date(), "_SAND2", "_EF~SM_.pdf"), width = 12, height = 9)
# #
# # # Sand raw
# # ggplot(sand[thMeas, on = c(ID = "Fluxnet_site"), nomatch = 0], aes(SWC_F_MDS_1, EF, col = VPD_F), size = 0.1) +
# #     theme_bw() +
# #     geom_point() +
# #     scale_color_viridis_c(option = "turbo") +
# #     facet_wrap(~ID) +
# #     geom_vline(aes(xintercept = thetaCrit*100), linetype = "dashed")
# # ggsave(paste0("outputs/", Sys.Date(), "_SAND", "_EF~SM_.pdf"), width = 12, height = 9)
# #
# #
# # # Clay with criteria
# # ggplot(clay2, aes(SWC_F_MDS_1, EF, col = VPD_F), size = 0.1) +
# #     theme_bw() +
# #     geom_point() +
# #     scale_color_viridis_c(option = "turbo") +
# #     facet_wrap(~ID) +
# #     geom_vline(aes(xintercept = thetaCrit*100), linetype = "dashed")
# # ggsave(paste0("outputs/", Sys.Date(), "_CLAY2", "_EF~SM_.pdf"), width = 12, height = 9)
# #
# # # Clay raw
# # ggplot(clay[thMeas, on = c(ID = "Fluxnet_site"), nomatch = 0], aes(SWC_F_MDS_1, EF, col = VPD_F), size = 0.1) +
# #     theme_bw() +
# #     geom_point() +
# #     scale_color_viridis_c(option = "turbo") +
# #     facet_wrap(~ID) +
# #     geom_vline(aes(xintercept = thetaCrit*100), linetype = "dashed")
# # ggsave(paste0("outputs/", Sys.Date(), "_CLAY", "_EF~SM_.pdf"), width = 12, height = 9)
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # ###
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# #
# #
# # sand[, unique(ID)]
# # sand1[, unique(ID)]
# # sand2[, unique(ID)]
# # sand1[, summary(meanPlatVPD), ID]
# #
# # sand1[SWC_F_MDS_1 < thetaCrit, .(datetime, VPD_F), by = .(ID)]
# # # sand %>%
# # #     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%
# # #     melt(id.vars = c("SWC_F_MDS_1", "VPD_F"), measure.vars = "EF") %>% # we can use . as placeholder for previous line output!
# # #     .[, -"variable"] %>%
# # #     .[, lapply(.SD, round, -0.5), .SDcols = SWC_F_MDS_1:VPD_F, by = value] %>%
# # #     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = value)) +
# # #     theme_bw() +
# # #     geom_bin_2d()
# #
# #
# #
# #
# # ggplot(setDT(copy(faithful))[3, eruptions := NA], aes(eruptions, waiting)) +
# #     geom_bin2d() +
# #     geom_density2d()
# #
# # ggplot(faithfuld, aes(eruptions, waiting, fill = density)) +
# #     geom_raster() +
# #     geom_density2d()
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # g1_clay <- clay %>%
# #     # dplyr::filter(LE_F_MDS >= 0 & H_F_MDS >= 0) %>%
# #     .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%
# #     melt(id.vars = c("SWC_F_MDS_1", "VPD_F"), measure.vars = "EF") %>% # we can use . as placeholder for previous line output!
# #     .[, -"variable"] %>%
# #     .[, lapply(.SD, round, -0.5), .SDcols = SWC_F_MDS_1:VPD_F, by = value] %>%
# #     ggplot(aes(x = SWC_F_MDS_1, y = VPD_F, fill = value)) +
# #     theme_bw() +
# #     geom_raster(hjust = 0, vjust = 1) +
# #     # geom_tile(aes(width = 1, height = 1, hjust = 0, vjust = 0)) +
# #     scale_fill_viridis_c()
# # g1_clay
# #
# # p1_piped <- mtcars %>%
# #   rename(transmission = am, weight = wt) %>%
# #   mutate(lp100km = (100 * 3.785411784) / (1.609344 * mpg)) %>%
# #   select(weight, lp100km) %>%
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # mround <- function(x,base){
# # 	base*round(x/base)
# # }
# # clay[, VPD5_3 := mround(VPD_F, 5/3)]
# # clay[, binVPD := cut(VPD_F, breaks = 29)]
# # clay[, binVPD := cut(VPD_F, breaks = seq(0, 50, length.out = 31), dig.lab = 1)]
# #
# # clay[!is.na(VPD_F), binVPD := cut(VPD_F, breaks = 29)]; nrow(clay[!is.na(binVPD)])
# # clay[!is.na(SWC_F_MDS_1), binSWC := cut(SWC_F_MDS_1, breaks = 24)]; nrow(clay[!is.na(binSWC)])
# #
# # clay[, SWC2.5 := mround(SWC_F_MDS_1, 2.5)]
# # clay[, binSWC := cut(SWC_F_MDS_1, breaks = 24)]
# # clay[, binSWC := cut(SWC_F_MDS_1, breaks = seq(0, 80, length.out = 31), dig.lab = 1)]
# #
# # ggplot(na.omit(clay[, .(SWC2.5, VPD5_3, EF)]), aes(x = SWC2.5, y = VPD5_3, fill = EF)) +
# #     theme_bw() +
# #     geom_raster(hjust = 1, vjust = 1) +
# #     # geom_tile(aes(width = 1, height = 1, hjust = 0, vjust = 0)) +
# #     scale_fill_viridis_c()
# #
# # ggplot(clay, aes(x = binSWC, y = binVPD, fill = EF)) +
# #     theme_bw() +
# #     geom_raster(hjust = 1, vjust = 1) +
# #     # geom_tile(aes(width = 1, height = 1, hjust = 0, vjust = 0)) +
# #     scale_fill_viridis_c(na.value = "white")
# #
# # ggplot(na.omit(clay[, .(binSWC, binVPD, EF)]), aes(x = binSWC, y = binVPD, fill = EF)) +
# #     theme_bw() +
# #     geom_raster(hjust = 0, vjust = 1) +
# #     # geom_tile(aes(width = 1, height = 1, hjust = 0, vjust = 0)) +
# #     scale_fill_viridis_c() +
# #     scale_x_discrete(labels = c(5, 10, 50))
# #
# # lbl <- round(seq(0, 80, length.out = 31), 0)
# # lbl[seq(1, length(lbl), 2)] <- ""
# #
# # ggplot(clay, aes(x = SWC_F_MDS_1, y = VPD_F, fill = EF)) +
# #     theme_bw() +
# #     geom_raster(hjust = 0, vjust = 1) +
# #     # geom_tile(aes(width = 1, height = 1, hjust = 0, vjust = 0)) +
# #     scale_fill_viridis_c() +
# #     scale_x_binned(n.breaks = 30, labels = lbl) +
# #     scale_y_binned(n.breaks = 28)
# #
# # # cut(dt2$L, breaks = 20)
# # # dt2[, bin3 := cut(dt2$L, breaks = 10)]
# # # clay[, binVPD := cut(VPD_F, breaks = 20)]
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# #
# # # clay[, EF := LE_F_MDS/(LE_F_MDS + H_F_MDS)]
# # # clay[LE_F_MDS < 0 | H_F_MDS < 0, EF := NA]
# # #
# # # sand[, EF := LE_F_MDS/(LE_F_MDS + H_F_MDS)]
# # # sand[LE_F_MDS < 0 | H_F_MDS < 0, EF := NA]
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # # select variables, calculate EF and plot heatmap
# #
# # # clay[LE_F_MDS >= 0 & H_F_MDS >= 0, plot(SWC_F_MDS_1, VPD_F, col = LE_F_MDS/(LE_F_MDS + H_F_MDS))]
# #
# # # ggplot(data =  clay[LE_F_MDS >= 0 & H_F_MDS >= 0], aes(x = SWC_F_MDS_1, y = LE_F_MDS/(LE_F_MDS + H_F_MDS))) +
# # #     theme_bw() +
# # #     geom_point(aes(col = ID), alpha = 1, size = 0.1)
# # # ggsave(paste0("outputs/", Sys.Date(), "_EF~SWC_ClayTowers.pdf"))
# #
# # mClay <- melt(clay[LE_F_MDS >= 0 & H_F_MDS >= 0], id.vars = c("SWC_F_MDS_1", "VPD_F"), measure.vars = "EF")
# # ggplot(data =  mClay[, -"variable"][, lapply(.SD, round, 0), .SDcols = SWC_F_MDS_1:VPD_F, by = value], # We need integers to display the image!!!
# #        aes(x = SWC_F_MDS_1, y = VPD_F, fill = value)) +
# #     theme_bw() +
# #     geom_raster() +
# #     scale_fill_viridis_c(option = "magma")
# # ggsave(paste0("outputs/", Sys.Date(), "_EF~(VPD+SWC)_ClayTowers.pdf"))
# #
# # mSand <- melt(sand[LE_F_MDS >= 0 & H_F_MDS >= 0], id.vars = c("SWC_F_MDS_1", "VPD_F"), measure.vars = "EF")
# # ggplot(data =  mSand[, -"variable"][, lapply(.SD, round, 0), .SDcols = SWC_F_MDS_1:VPD_F, by = value], # We need integers to display the image!!!
# #        aes(x = SWC_F_MDS_1, y = VPD_F, fill = value)) +
# #     theme_bw() +
# #     geom_tile() +
# #     scale_fill_viridis_c(option = "magma")
# # ggsave(paste0("outputs/", Sys.Date(), "_EF~(VPD+SWC)_SandTowers.pdf"))
# #
# # ggplot(data =  mSand[, -"variable"][, lapply(.SD, round, 0), .SDcols = SWC_F_MDS_1:VPD_F, by = value], # We need integers to display the image!!!
# #        aes(x = SWC_F_MDS_1, y = VPD_F, fill = value)) +
# #     theme_bw() +
# #     stat_bin_2d(aes(fill = factor(value)), bins = 20) +
# #     scale_fill_viridis_c(option = "magma")
# #
# # ggplot(data =  mSand[, -"variable"][, lapply(.SD, round, 0), .SDcols = SWC_F_MDS_1:VPD_F, by = value], # We need integers to display the image!!!
# #        aes(x = SWC_F_MDS_1, y = VPD_F, fill = as.factor(value))) +
# #     theme_bw() +
# #     geom_tile() +
# #     scale_fill_viridis_c(option = "magma")
# #
# # fld <- interp(na.omit(sand$SWC_F_MDS_1), na.omit(sand$VPD_F), na.omit(sand$EF), nx = 50)
# # # prepare data in long format
# # df <- melt(fld$z, na.rm = TRUE)
# # names(df) <- c("x", "y", "Rain")
# # df$Lon <- fld$x[df$x]
# # df$Lat <- fld$y[df$y]
# # ggplot(df, aes(x = Lon, y = Lat, fill = Rain)) +
# #     geom_tile()
# #
# # fld <- interp(copdat$Lon, copdat$Lat, copdat$Rain, nx = 20)
# # # prepare data in long format
# # df <- melt(fld$z, na.rm = TRUE)
# # names(df) <- c("x", "y", "Rain")
# # df$Lon <- fld$x[df$x]
# # df$Lat <- fld$y[df$y]
# # ggplot(df, aes(x = Lon, y = Lat, fill = Rain)) +
# #     geom_tile()
# #
# #
# # # # Dummy data
# # # x <- runif(20, 1, 60)
# # # y <- runif(20, 0, 45)
# # # data <- expand.grid(X=x, Y=y)
# # # data$Z <- runif(400, 0, 1)
# # #
# # # # Heatmap
# # # ggplot(data, aes(X, Y, fill= Z)) +
# # #   geom_raster()
# #
# # # df <- data.table(X = rep(nrow(volcano), Y = 1:ncol(volcano), Z = c(volcano)) # c() = as.vector()
# # # df <- setDT(reshape2::melt(volcano))
# # # ggplot(df, aes(Var1, Var2, fill= value)) +
# # #   geom_tile()
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # flx_spn_dd <- fread(fileList[[1]], na.strings = "-9999")
# # flx_spn_dd[, 1] <- lapply(flx_spn_dd[, 1], function(x) as.POSIXct(strptime(x, format = "%Y%m%d", tz = "UTC")))
# #
# # flx_spn_hh <- fread(fileList[[2]], na.strings = "-9999")
# # flx_spn_hh[, 1:2] <- lapply(flx_spn_hh[, 1:2], function(x) as.POSIXct(strptime(x, format = "%Y%m%d%H%M", tz = "UTC")))
# #
# # flx_sps_dd <- fread(fileList[[3]], na.strings = "-9999")
# # flx_sps_dd[, 1] <- lapply(flx_sps_dd[, 1], function(x) as.POSIXct(strptime(x, format = "%Y%m%d", tz = "UTC")))
# #
# # flx_sps_hh <- fread(fileList[[4]], na.strings = "-9999")
# # flx_sps_hh[, 1:2] <- lapply(flx_sps_hh[, 1:2], function(x) as.POSIXct(strptime(x, format = "%Y%m%d%H%M", tz = "UTC")))
# #
# # wolf_spn_hh <- fread(fileList[[5]])
# # wolf_spn_hh[, Time := ifelse(nchar(Time) == 1, paste0("000", Time), ifelse(nchar(Time) == 2, paste0("00", Time), ifelse(nchar(Time) == 3, paste0(0, Time), Time)))]
# # wolf_spn_hh[, TS := paste(Year, Date, Time, sep = "")]
# # wolf_spn_hh[, TS := as.POSIXct(TS, format = "%Y%d.%m.%H%M", tz = "UTC")]
# #
# # wolf_sps_hh <- fread(fileList[[6]], select = 1:7)
# # wolf_sps_hh[, Time := ifelse(nchar(Time) == 1, paste0("000", Time), ifelse(nchar(Time) == 2, paste0("00", Time), ifelse(nchar(Time) == 3, paste0(0, Time), Time)))]
# # wolf_sps_hh[, TS := paste(Year, Date, Time, sep = "")]
# # wolf_sps_hh[, TS := as.POSIXct(TS, format = "%Y%d.%m.%H%M", tz = "UTC")]
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # ggplot() +
# #     geom_point(data = flx_spn_dd, aes(TIMESTAMP, SWC_F_MDS_1)) +
# #     geom_line(data = flx_spn_dd, aes(TIMESTAMP, SWC_F_MDS_1, col = "flx_spn_dd")) +
# #     geom_point(data = flx_spn_hh, aes(TIMESTAMP_START, SWC_F_MDS_1)) +
# #     geom_line(data = flx_spn_hh, aes(TIMESTAMP_START, SWC_F_MDS_1, col = "flx_spn_hh"))
# #
# # # Comparison SPn
# # ggplot(flx_spn_hh) +
# #     geom_point(aes(TIMESTAMP_END, SWC_F_MDS_1)) +
# #     geom_line(aes(TIMESTAMP_END, SWC_F_MDS_1, col = "flx_spn_hh")) +
# #     geom_point(data = flx_spn_hh[which.max(SWC_F_MDS_1)], aes(TIMESTAMP_END, SWC_F_MDS_1), col = "red", size = 2) +
# #     geom_point(data = wolf_spn_hh, aes(TS, SWC_5cm*100)) +
# #     geom_line(data = wolf_spn_hh, aes(TS, SWC_5cm*100, col = "wolf_spn_hh")) +
# #     geom_point(data = wolf_spn_hh[which.max(SWC_5cm)], aes(TS, SWC_5cm*100), col = "red", size = 2)
# #
# # # Comparison SPs
# # ggplot(flx_sps_hh) +
# #     geom_point(aes(TIMESTAMP_END, SWC_F_MDS_1)) +
# #     geom_line(aes(TIMESTAMP_END, SWC_F_MDS_1, col = "flx_sps_hh")) +
# #     geom_point(data = flx_sps_hh[which.max(SWC_F_MDS_1)], aes(TIMESTAMP_END, SWC_F_MDS_1), col = "red", size = 2) +
# #     geom_point(data = wolf_sps_hh, aes(TS, SWC_5cm*100)) +
# #     geom_line(data = wolf_sps_hh, aes(TS, SWC_5cm*100, col = "wolf_sps_hh")) +
# #     geom_point(data = wolf_sps_hh[which.max(SWC_5cm)], aes(TS, SWC_5cm*100), col = "red", size = 2)
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # id_wolf_sps <- which.max(wolf_sps_hh$SWC_5cm)
# # wolf_sps_hh[(id_wolf_sps-3):(id_wolf_sps+3), .(TS, SWC_5cm)]
# #
# # id_wolf_sps <- wolf_sps_hh[which.max(SWC_5cm), TS]
# # wolf_sps_hh[TS %in% (id_wolf_sps - 90*60):(id_wolf_sps + 90*60), .(TS, SWC_5cm)]
# # flx_sps_hh[TIMESTAMP_END %in% (id_wolf_sps - 90*60):(id_wolf_sps + 90*60), .(TIMESTAMP_START, TIMESTAMP_END, SWC_F_MDS_1)]
# #
# #
# # # Back Conversion FLUXNET SWC
# # # -----------------------------------------------------------------------------------------------------------------------
# # flx_sps_hh[, SWC_bConv := SWC_F_MDS_1/100/0.9*max(wolf_sps_hh$SWC_5cm, na.rm = T)]
# #
# # flx_spn_hh[, SWC_bConv := SWC_F_MDS_1/100/0.9*max(wolf_spn_hh$SWC_5cm, na.rm = T)]
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # # Comparison SPn
# # ggplot(flx_spn_hh) +
# #     theme_bw() +
# #     geom_point(aes(TIMESTAMP_END, SWC_bConv*100), size = 0.5) +
# #     geom_line(aes(TIMESTAMP_END, SWC_bConv*100, col = "flx_spn_hh"), alpha = 0.5) +
# #     geom_point(data = flx_spn_hh[which.max(SWC_bConv*100)], aes(TIMESTAMP_END, SWC_bConv*100), col = "red", size = 2) +
# #     geom_point(data = wolf_spn_hh, aes(TS, SWC_5cm*100), size = 0.5) +
# #     geom_line(data = wolf_spn_hh, aes(TS, SWC_5cm*100, col = "wolf_spn_hh"), alpha = 0.5) +
# #     geom_point(data = wolf_spn_hh[which.max(SWC_5cm)], aes(TS, SWC_5cm*100), col = "red", size = 2)
# # ggsave("outputs/PanamaTowers_SPn_hh.pdf", width = 25, height = 5)
# #
# # # Comparison SPs
# # ggplot(flx_sps_hh) +
# #     theme_bw() +
# #     geom_point(aes(TIMESTAMP_END, SWC_bConv*100), size = 0.5) +
# #     geom_line(aes(TIMESTAMP_END, SWC_bConv*100, col = "flx_sps_hh"), alpha = 0.5) +
# #     geom_point(data = flx_sps_hh[which.max(SWC_bConv*100)], aes(TIMESTAMP_END, SWC_bConv*100), col = "red", size = 2) +
# #     geom_point(data = wolf_sps_hh, aes(TS, SWC_5cm*100), size = 0.5) +
# #     geom_line(data = wolf_sps_hh, aes(TS, SWC_5cm*100, col = "wolf_sps_hh"), alpha = 0.5) +
# #     geom_point(data = wolf_sps_hh[which.max(SWC_5cm)], aes(TS, SWC_5cm*100), col = "red", size = 2)
# # ggsave("outputs/PanamaTowers_SPs_hh.pdf", width = 25, height = 5)
# #
# #
# # # EF ~ SM
# # # -----------------------------------------------------------------------------------------------------------------------
# # flx_spn_hh[, EF := LE_F_MDS/(LE_F_MDS + H_F_MDS)]
# # flx_spn_hh[LE_F_MDS < 0 | H_F_MDS < 0, EF := NA]
# # flx_sps_hh[, EF := LE_F_MDS/(LE_F_MDS + H_F_MDS)]
# # flx_sps_hh[LE_F_MDS < 0 | H_F_MDS < 0, EF := NA]
# #
# # # library('minpack.lm')# to demonstrate a backup method for nls
# # # ########## source code from GitHub for lin_plateau() function
# # # devtools::source_url("https://raw.githubusercontent.com/austinwpearce/SoilTestCocaCola/main/lin_plateau.R")
# #
# # g1 <-
# # ggplot(flx_spn_hh, aes(SWC_bConv, EF)) +
# #     labs(title = "flx_spn_hh") +
# #     theme_bw() +
# #     geom_point()
# #     # geom_point() +
# #     # geom_smooth(method = "nlsLM",
# #     #             formula = y ~ SSlinp(x, a, b, jp),
# #     #             se = F,
# #     #             color = "#CC0000",
# #     #             size=2)
# #
# # g2 <-
# # ggplot(flx_sps_hh) +
# #     labs(title = "flx_sps_hh") +
# #     theme_bw() +
# #     geom_point(aes(SWC_bConv, EF))
# #
# # g1 + g2
# # ggsave("outputs/PanamaTowers_EF~SM.pdf", height = 5, width = 10)
# #
# # # -----------------------------------------------------------------------------------------------------------------------
# # # SPn
# # new_flx_spn_hh <- copy(flx_spn_hh)
# # new_flx_spn_hh[, 1:2] <- new_flx_spn_hh[, lapply(.SD, function(x) as.POSIXct(format(x, format = "%Y-%m-%d"))), .SDcols = c("TIMESTAMP_START", "TIMESTAMP_END")]
# # setnames(new_flx_spn_hh, old = "TIMESTAMP_START", new = "TIMESTAMP")
# # new_flx_spn_hh$TIMESTAMP_END <- NULL
# #
# # summary(new_flx_spn_hh$SWC_bConv)
# #
# # new_flx_spn_dd <- new_flx_spn_hh[, lapply(.SD, function(x) mean(x, na.rm = T)), by = TIMESTAMP, .SDcols = c("SWC_bConv", "EF")]
# # summary(new_flx_spn_dd$SWC_bConv)
# # str(new_flx_spn_dd)
# #
# # ggplot(new_flx_spn_dd) +
# #     labs(title = "new_flx_spn_dd") +
# #     theme_bw() +
# #     geom_point(aes(SWC_bConv, EF))
# #
# # ggplot(new_flx_spn_dd) +
# #     theme_bw() +
# #     labs(title = "PA-SPn") +
# #     geom_point(aes(TIMESTAMP, SWC_bConv*100), size = 0.5) +
# #     geom_line(aes(TIMESTAMP, SWC_bConv*100, col = "new_flx_spn_dd"), alpha = 0.5) +
# #     geom_point(data = flx_spn_dd, aes(TIMESTAMP, SWC_F_MDS_1), size = 0.5) +
# #     geom_line(data = flx_spn_dd, aes(TIMESTAMP, SWC_F_MDS_1, col = "flx_spn_dd"), alpha = 0.5)
# # ggsave("outputs/PanamaTowers_Comparison-Self-Aggregation-To-Daily_PA-SPn.pdf", height = 5, width = 25)
# #
# # # SPs
# # new_flx_sps_hh <- copy(flx_sps_hh)
# # new_flx_sps_hh[, 1:2] <- new_flx_sps_hh[, lapply(.SD, function(x) as.POSIXct(format(x, format = "%Y-%m-%d"))), .SDcols = c("TIMESTAMP_START", "TIMESTAMP_END")]
# # setnames(new_flx_sps_hh, old = "TIMESTAMP_START", new = "TIMESTAMP")
# # new_flx_sps_hh$TIMESTAMP_END <- NULL
# #
# # summary(new_flx_sps_hh$SWC_bConv)
# #
# # new_flx_sps_dd <- new_flx_sps_hh[, lapply(.SD, function(x) mean(x, na.rm = T)), by = TIMESTAMP, .SDcols = c("SWC_bConv", "EF")]
# # summary(new_flx_sps_dd$SWC_bConv)
# # str(new_flx_sps_dd)
# #
# # ggplot(new_flx_sps_dd) +
# #     labs(title = "new_flx_sps_dd") +
# #     theme_bw() +
# #     geom_point(aes(SWC_bConv, EF))
# #
# # ggplot(new_flx_sps_dd) +
# #     theme_bw() +
# #     labs(title = "PA-sps") +
# #     geom_point(aes(TIMESTAMP, SWC_bConv*100), size = 0.5) +
# #     geom_line(aes(TIMESTAMP, SWC_bConv*100, col = "new_flx_sps_dd"), alpha = 0.5) +
# #     geom_point(data = flx_sps_dd, aes(TIMESTAMP, SWC_F_MDS_1), size = 0.5) +
# #     geom_line(data = flx_sps_dd, aes(TIMESTAMP, SWC_F_MDS_1, col = "flx_sps_dd"), alpha = 0.5)
# # ggsave("outputs/PanamaTowers_Comparison-Self-Aggregation-To-Daily_PA-sps.pdf", height = 5, width = 25)
# #
# #
# # # Back Conversion DD
# # # -----------------------------------------------------------------------------------------------------------------------
# #
# # flx_spn_dd[, SWC_bConv := SWC_F_MDS_1/100/0.9*max(wolf_spn_hh$SWC_5cm, na.rm = T)]
# # flx_sps_dd[, SWC_bConv := SWC_F_MDS_1/100/0.9*max(wolf_sps_hh$SWC_5cm, na.rm = T)]
# #
# # g1 <-
# # ggplot(new_flx_spn_dd) +
# #     theme_bw() +
# #     labs(title = "PA-SPn") +
# #     geom_point(aes(TIMESTAMP, SWC_bConv*100), size = 0.5) +
# #     geom_line(aes(TIMESTAMP, SWC_bConv*100, col = "new_flx_spn_dd"), alpha = 0.5) +
# #     geom_point(data = flx_spn_dd, aes(TIMESTAMP, SWC_bConv*100), size = 0.5) +
# #     geom_line(data = flx_spn_dd, aes(TIMESTAMP, SWC_bConv*100, col = "flx_spn_dd"), alpha = 0.5)
# # g1
# # ggsave("outputs/PanamaTowers_Comparison-Self-Aggregation-To-Daily_PA-SPn_AbsoluteSWC.pdf", height = 5, width = 25)
# #
# # g2 <-
# # ggplot(new_flx_sps_dd) +
# #     theme_bw() +
# #     labs(title = "PA-SPs") +
# #     geom_point(aes(TIMESTAMP, SWC_bConv*100), size = 0.5) +
# #     geom_line(aes(TIMESTAMP, SWC_bConv*100, col = "new_flx_sps_dd"), alpha = 0.5) +
# #     geom_point(data = flx_sps_dd, aes(TIMESTAMP, SWC_bConv*100), size = 0.5) +
# #     geom_line(data = flx_sps_dd, aes(TIMESTAMP, SWC_bConv*100, col = "flx_sps_dd"), alpha = 0.5)
# # g2
# # ggsave("outputs/PanamaTowers_Comparison-Self-Aggregation-To-Daily_PA-SPs_AbsoluteSWC.pdf", height = 5, width = 25)
# #
# # g1 / g2
# # ggsave("outputs/BOTH_PanamaTowers_Comparison-Self-Aggregation-To-Daily_AbsoluteSWC.pdf", height = 5, width = 25)
# #
# #
# # # SAVE Improved FLUXNET files
# # # -----------------------------------------------------------------------------------------------------------------------
# # # Daily
# # fwrite(x = flx_spn_dd, file = "data-raw/FLX_PA-SPn_FLUXNET2015_SUBSET_DD_2007-2009_1-4_IMPROVED.csv", dateTimeAs = "squash")
# # fwrite(x = flx_sps_dd, file = "data-raw/FLX_PA-SPs_FLUXNET2015_SUBSET_DD_2007-2009_1-4_IMPROVED.csv", dateTimeAs = "squash")
# #
# # # Hourly
# # fwrite(x = flx_spn_hh, file = "data-raw/FLX_PA-SPn_FLUXNET2015_SUBSET_HH_2007-2009_1-4_IMPROVED.csv", dateTimeAs = "squash")
# # fwrite(x = flx_sps_hh, file = "data-raw/FLX_PA-SPs_FLUXNET2015_SUBSET_HH_2007-2009_1-4_IMPROVED.csv", dateTimeAs = "squash")
