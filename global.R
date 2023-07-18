domainvisualization <- function(DateTime,
                                BiddingZone1,
                                BiddingZone2,
                                Reference,
                                Category) {
  
  # overview Core bidding zones
  CoreBZ <- data.frame(BiddingZoneAbb = c("AT", "BE", "HR", "CZ", "DE", "FR", "HU", "NL", "PL", "RO", "SK", "SI", "ALBE", "ALDE"),
                       BiddingZone = c("Austria", "Belgium", "Croatia", "Czech Republic", "Germany/Luxembourg", "France", "Hungary",
                                       "Netherlands", "Poland", "Romania", "Slovakia", "Slovenia", "ALEGrO Belgium", "ALEGrO Germany"))
  
  # bidding zone abbreviations
  BiddingZoneAbb1 = CoreBZ$BiddingZoneAbb[CoreBZ$BiddingZone == BiddingZone1]
  BiddingZoneAbb2 = CoreBZ$BiddingZoneAbb[CoreBZ$BiddingZone == BiddingZone2]
  
  # define reference positions for zones (where to slice)
  if (Reference == "MCP") {
    
    df_slice <- JAOPuTo::JAOPuTo_netpositions(DateTime, DateTime + lubridate::hours(1)) %>%
      dplyr::mutate(NetPosition = dplyr::case_when(BiddingZone %in% c(BiddingZone1, BiddingZone2) ~ 0,
                                                   TRUE ~ NetPosition))
    
  } else if (Reference == "Zero-balanced") {
    
    df_slice <- JAOPuTo::JAOPuTo_netpositions(DateTime, DateTime + lubridate::hours(1)) %>%
      dplyr::mutate(NetPosition = 0)
  }
  
  # download presolved final domains
  df_finaldomain <- JAOPuTo::JAOPuTo_finaldomain(DateTime, DateTime + lubridate::hours(1)) %>%
    dplyr::filter(cneEic != "NA")
  
  df_finaldomain <- df_finaldomain %>%
    mutate(Intercept = (ram - ptdf_ALBE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "ALBE"]
                        - ptdf_ALDE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "ALDE"]
                        - ptdf_AT * df_slice$NetPosition[df_slice$BiddingZoneAbb == "AT"]
                        - ptdf_BE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "BE"]
                        - ptdf_HR * df_slice$NetPosition[df_slice$BiddingZoneAbb == "HR"]
                        - ptdf_CZ * df_slice$NetPosition[df_slice$BiddingZoneAbb == "CZ"]
                        - ptdf_DE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "DE"]
                        - ptdf_FR * df_slice$NetPosition[df_slice$BiddingZoneAbb == "FR"]
                        - ptdf_HU * df_slice$NetPosition[df_slice$BiddingZoneAbb == "HU"]
                        - ptdf_NL * df_slice$NetPosition[df_slice$BiddingZoneAbb == "NL"]
                        - ptdf_PL * df_slice$NetPosition[df_slice$BiddingZoneAbb == "PL"]
                        - ptdf_RO * df_slice$NetPosition[df_slice$BiddingZoneAbb == "RO"]
                        - ptdf_SK * df_slice$NetPosition[df_slice$BiddingZoneAbb == "SK"]
                        - ptdf_SI * df_slice$NetPosition[df_slice$BiddingZoneAbb == "SI"])
           / df_finaldomain[[paste0("ptdf_", BiddingZoneAbb2)]],
           Slope = - df_finaldomain[[paste0("ptdf_", BiddingZoneAbb1)]] /
             df_finaldomain[[paste0("ptdf_", BiddingZoneAbb2)]]) %>%
    mutate(CNEC = paste0(cneName, " | ", contName, " - ", direction),
           Type = "Presolved",
           Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
           RAM = ram / fmax) %>%
    select(CNEC, TSO, Location, Type, Intercept, Slope, RAM) %>%
    filter(!Intercept %in% c("Inf", "NaN", NA))
  
  # download active constraints
  df_activeconstraints <- tibble(CNEC = character(),
                                 TSO = character(),
                                 Location = character(),
                                 Intercept = double(),
                                 Slope = double(),
                                 RAM = double())
  
  tryCatch({
    df_activeconstraints <- JAOPuTo::JAOPuTo_shadowprices(DateTime, DateTime + lubridate::hours(1)) %>%
      dplyr::filter(cnecEic != "NA")
    
    df_activeconstraints <- df_activeconstraints %>%
      mutate(Intercept = (ram - hub_ALBE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "ALBE"]
                          - hub_ALDE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "ALDE"]
                          - hub_AT * df_slice$NetPosition[df_slice$BiddingZoneAbb == "AT"]
                          - hub_BE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "BE"]
                          - hub_HR * df_slice$NetPosition[df_slice$BiddingZoneAbb == "HR"]
                          - hub_CZ * df_slice$NetPosition[df_slice$BiddingZoneAbb == "CZ"]
                          - hub_DE * df_slice$NetPosition[df_slice$BiddingZoneAbb == "DE"]
                          - hub_FR * df_slice$NetPosition[df_slice$BiddingZoneAbb == "FR"]
                          - hub_HU * df_slice$NetPosition[df_slice$BiddingZoneAbb == "HU"]
                          - hub_NL * df_slice$NetPosition[df_slice$BiddingZoneAbb == "NL"]
                          - hub_PL * df_slice$NetPosition[df_slice$BiddingZoneAbb == "PL"]
                          - hub_RO * df_slice$NetPosition[df_slice$BiddingZoneAbb == "RO"]
                          - hub_SK * df_slice$NetPosition[df_slice$BiddingZoneAbb == "SK"]
                          - hub_SI * df_slice$NetPosition[df_slice$BiddingZoneAbb == "SI"])
             / df_activeconstraints[[paste0("hub_", BiddingZoneAbb2)]],
             Slope = - df_activeconstraints[[paste0("hub_", BiddingZoneAbb1)]] /
               df_activeconstraints[[paste0("hub_", BiddingZoneAbb2)]]) %>%
      mutate(CNEC = paste0(cnecName, " | ", contName, " - ", direction),
             Type = "Active",
             Location = ifelse(hubFrom == hubTo, "Internal", "Cross-border"),
             RAM = ram / fmax) %>%
      select(CNEC, TSO, Location, Type, Intercept, Slope, RAM) %>%
      filter(!Intercept %in% c("Inf", "NaN", NA))
  }, error=function(e){})
  
  # merge presolved and active constraints
  df_domain <- rbind(df_finaldomain, df_activeconstraints)
  
  # download Core net positions
  df_netpositions <- JAOPuTo::JAOPuTo_netpositions(DateTime, DateTime + lubridate::hours(1))
  
  # download LTA values and define LTA domain
  df_LTA <- JAOPuTo::JAOPuTo_LTA(DateTime, DateTime + lubridate::hours(1)) %>%
    dplyr::filter(substr(Border, 1, 2) %in% c(BiddingZoneAbb1,
                                              BiddingZoneAbb2) |
                    substr(Border, 6, 7) %in% c(BiddingZoneAbb1,
                                                BiddingZoneAbb2))
  
  df_LTAdomain <- tibble(Type = rep("LTA", 5),
                         x = c(sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAbb1], na.rm = TRUE),
                               sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAbb1], na.rm = TRUE),
                               -sum(df_LTA$LTA[substr(df_LTA$Border, 4, 5) == BiddingZoneAbb1], na.rm = TRUE),
                               -sum(df_LTA$LTA[substr(df_LTA$Border, 4, 5) == BiddingZoneAbb1], na.rm = TRUE),
                               sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAbb1], na.rm = TRUE)),
                         y = c(-sum(df_LTA$LTA[substr(df_LTA$Border, 4, 5) == BiddingZoneAbb2], na.rm = TRUE),
                               sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAbb2], na.rm = TRUE),
                               sum(df_LTA$LTA[substr(df_LTA$Border, 1, 2) == BiddingZoneAbb2], na.rm = TRUE),
                               -sum(df_LTA$LTA[substr(df_LTA$Border, 4, 5) == BiddingZoneAbb2], na.rm = TRUE),
                               -sum(df_LTA$LTA[substr(df_LTA$Border, 4, 5) == BiddingZoneAbb2], na.rm = TRUE))) %>%
    sf::st_as_sf(coords = c("x", "y")) %>%
    dplyr::summarize(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(Type = "LTA")
  
  # calculate flow-based domain
  innermost <- function(slopes, intercepts, a, b) {
    
    meetings <- function(slopes, intercepts) {
      meets_at <- function(i1, s1, i2, s2) {
        ifelse(s1 - s2 == 0, NA, (i2 - i1)/(s1 - s2))
      }
      xvals <- outer(seq_along(slopes), seq_along(slopes), function(i, j) {
        meets_at(intercepts[i], slopes[i], intercepts[j], slopes[j])
      })
      
      yvals <- outer(seq_along(slopes), seq_along(slopes), function(i, j) {
        intercepts + slopes *
          meets_at(intercepts[i], slopes[i], intercepts[j], slopes[j])
      })
      
      cbind(x = xvals[lower.tri(xvals)], y = yvals[lower.tri(yvals)])
      
    }
    
    xy <- meetings(slopes, intercepts)
    xy[,1] <- xy[,1] - a
    xy[,2] <- xy[,2] - b
    
    is_cut <- function(x, y, slopes, intercepts, a, b) {
      d <- sqrt(x^2 + y^2)
      slope <- y / x
      xvals <- (intercepts + slopes*a - b) / (slope - slopes)
      yvals <- xvals * slopes + (intercepts + slopes*a - b)
      ds <- sqrt(xvals^2 + yvals^2)
      any(d - ds > 1e-6 & sign(xvals) == sign(x) & sign(yvals) == sign(y))
    }
    
    xy <- xy[sapply(seq(nrow(xy)), function(i) {
      !is_cut(xy[i, 1], xy[i, 2], slopes, intercepts, a, b)
    }),]
    
    xy <- xy[order(atan2(xy[,2], xy[,1])),]
    
    xy[,1] <- xy[,1] + a
    xy[,2] <- xy[,2] + b
    
    as.data.frame(rbind(xy, xy[1,]))
  }
  
  # define MCPx and MCPy (i.e. point to enclose by domain)
  if (Reference == "MCP") {
    
    MCPx <- df_netpositions$NetPosition[df_netpositions$BiddingZone == BiddingZone1] - sign(df_netpositions$NetPosition[df_netpositions$BiddingZone == BiddingZone1]) * 10
    MCPy <- df_netpositions$NetPosition[df_netpositions$BiddingZone == BiddingZone2] - sign(df_netpositions$NetPosition[df_netpositions$BiddingZone == BiddingZone2]) * 10
    
  } else if (Reference == "Zero-balanced") {
    
    MCPx <- 0
    MCPy <- 0
    
  }
  
  # calculate flow-based domain
  df_FBdomain <- innermost(df_finaldomain$Slope, df_finaldomain$Intercept, MCPx, MCPy) %>%
    na.omit() %>%
    sf::st_as_sf(coords = c(x = "x", y = "y")) %>%
    dplyr::summarize(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON") %>%
    dplyr::mutate(Type = "FB")
  
  # merge FB and LTA domain
  df_domainpolygons <- rbind(df_LTAdomain, df_FBdomain) %>%
    select(Type, geometry)
  
  # CNEC category
  if (Category == "Presolved / Active") {
    
    # ggiraph
    gg = ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linewidth = .25) +
      ggplot2::geom_vline(xintercept = 0, linewidth = .25) +
      ggiraph::geom_sf_interactive(data = df_domainpolygons,
                                   mapping = aes(fill = Type, tooltip = paste0(Type, " domain")),
                                   alpha = .2, colour = NA) +
      ggiraph::geom_abline_interactive(data = df_domain,
                                       mapping = ggplot2::aes(intercept = Intercept, slope = Slope, group = CNEC, colour = Type, linewidth = Type,
                                                              tooltip = paste0("CNEC: ", CNEC,
                                                                               "\nTSO: ", TSO,
                                                                               "\nType: ", Type,
                                                                               "\nRAM (% of Fmax): ", scales::percent(RAM, accuracy = .1, decimal.mark = ",")))) +
      ggiraph::geom_point_interactive(data = df_netpositions,
                                      mapping = ggplot2::aes(x = NetPosition[BiddingZone == BiddingZone1],
                                                             y = NetPosition[BiddingZone == BiddingZone2],
                                                             tooltip = paste0("Net positions for:\n",
                                                                              BiddingZone1, ": ", format(round(NetPosition[BiddingZone == BiddingZone1], 0), big.mark = ".", decimal.mark = ","), " MW\n",
                                                                              BiddingZone2, ": ", format(round(NetPosition[BiddingZone == BiddingZone2], 0), big.mark = ".", decimal.mark = ","), " MW")),
                                      shape = 21, size = 2, fill = "#005F83", colour = "white") +
      ggplot2::scale_x_continuous(name = paste0("Net position\n", BiddingZone1),
                                  labels = scales::number_format(big.mark = ".",
                                                                 decimal.mark = ",",
                                                                 suffix = " MW"),
                                  breaks = seq(-10000, 10000, 2500)) +
      ggplot2::scale_y_continuous(name = paste0("Net position\n", BiddingZone2),
                                  labels = scales::number_format(big.mark = ".",
                                                                 decimal.mark = ",",
                                                                 suffix = " MW"),
                                  breaks = seq(-10000, 10000, 2500)) +
      ggplot2::scale_colour_manual(name = "Type of network element:",
                                   values = c("Active" = "#C5003E",
                                              "Presolved" = "#C8C9C7")) +
      ggplot2::scale_linewidth_manual(name = "Type of network element:",
                                      values = c("Active" = .5,
                                                 "Presolved" = .25)) +
      ggplot2::scale_fill_manual(name = "Domain:",
                                 values = c("LTA" = "#FF8200",
                                            "FB" = "#005F83")) +
      ggplot2::coord_sf(xlim = c(-10000, 10000),
                        ylim = c(-10000, 10000),
                        expand = FALSE) +
      ggplot2::labs(caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
      ggplot2::theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = rel(.4)),
            legend.title = element_text(size = rel(.4),
                                        face = "bold"),
            legend.key.size = unit(.25, "cm"),
            axis.title.y = element_text(size = rel(.4),
                                        angle = 0,
                                        vjust = .5),
            axis.title.x = element_text(size = rel(.4)),
            axis.text = element_text(size = rel(.4)),
            axis.line = element_blank(),
            axis.ticks = element_line(linewidth = .25),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0,
                                        size = rel(.4)),
            plot.background = element_rect(colour = NA,
                                           fill = "#f5f2e9"))
    
    ggiraph::girafe(ggobj = gg,
                    options = list(opts_sizing(rescale = TRUE)),
                    bg = "#f5f2e9")
    
  } else if (Category == "Cross-border / Internal") {
    
    # ggiraph
    gg = ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linewidth = .25) +
      ggplot2::geom_vline(xintercept = 0, linewidth = .25) +
      ggiraph::geom_sf_interactive(data = df_domainpolygons,
                                   mapping = aes(fill = Type, tooltip = paste0(Type, " domain")),
                                   alpha = .2, colour = NA) +
      ggiraph::geom_abline_interactive(data = df_domain,
                                       mapping = ggplot2::aes(intercept = Intercept, slope = Slope, group = CNEC, colour = Location, linewidth = Location,
                                                              tooltip = paste0("CNEC: ", CNEC,
                                                                               "\nTSO: ", TSO,
                                                                               "\nType: ", Type,
                                                                               "\nRAM (% of Fmax): ", scales::percent(RAM, accuracy = .1, decimal.mark = ",")))) +
      ggiraph::geom_point_interactive(data = df_netpositions,
                                      mapping = ggplot2::aes(x = NetPosition[BiddingZone == BiddingZone1],
                                                             y = NetPosition[BiddingZone == BiddingZone2],
                                                             tooltip = paste0("Net positions for:\n",
                                                                              BiddingZone1, ": ", format(round(NetPosition[BiddingZone == BiddingZone1], 0), big.mark = ".", decimal.mark = ","), " MW\n",
                                                                              BiddingZone2, ": ", format(round(NetPosition[BiddingZone == BiddingZone2], 0), big.mark = ".", decimal.mark = ","), " MW")),
                                      shape = 21, size = 2, fill = "#005F83", colour = "white") +
      ggplot2::scale_x_continuous(name = paste0("Net position\n", BiddingZone1),
                                  labels = scales::number_format(big.mark = ".",
                                                                 decimal.mark = ",",
                                                                 suffix = " MW"),
                                  breaks = seq(-10000, 10000, 2500)) +
      ggplot2::scale_y_continuous(name = paste0("Net position\n", BiddingZone2),
                                  labels = scales::number_format(big.mark = ".",
                                                                 decimal.mark = ",",
                                                                 suffix = " MW"),
                                  breaks = seq(-10000, 10000, 2500)) +
      ggplot2::scale_colour_manual(name = "Type of network element:",
                                   values = c("Internal" = "#C5003E",
                                              "Cross-border" = "#C8C9C7")) +
      ggplot2::scale_linewidth_manual(name = "Type of network element:",
                                      values = c("Internal" = .5,
                                                 "Cross-border" = .25)) +
      ggplot2::scale_fill_manual(name = "Domain:",
                                 values = c("LTA" = "#FF8200",
                                            "FB" = "#005F83")) +
      ggplot2::coord_sf(xlim = c(-10000, 10000),
                        ylim = c(-10000, 10000),
                        expand = FALSE) +
      ggplot2::labs(caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
      ggplot2::theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = rel(.6)),
            legend.title = element_text(size = rel(.6),
                                        face = "bold"),
            legend.key.size = unit(.25, "cm"),
            axis.text = element_text(size = rel(.6)),
            axis.title = element_text(size = rel(.6)),
            axis.line = element_blank(),
            axis.ticks = element_line(linewidth = .25),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0,
                                        size = rel(.5)),
            plot.background = element_rect(colour = NA,
                                           fill = "#f5f2e9"),
            plot.margin = margin(0, 0, 0, 0, "cm"))
    
    ggiraph::girafe(ggobj = gg,
                    options = list(opts_sizing(rescale = TRUE)),
                    bg = "#f5f2e9")
    
  } else if (Category == "RAM  above / below 20% minRAM") {
    
    # ggiraph
    gg = ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linewidth = .25) +
      ggplot2::geom_vline(xintercept = 0, linewidth = .25) +
      ggiraph::geom_sf_interactive(data = df_domainpolygons,
                                   mapping = aes(fill = Type, tooltip = paste0(Type, " domain")),
                                   alpha = .2, colour = NA) +
      ggiraph::geom_abline_interactive(data = df_domain,
                                       mapping = ggplot2::aes(intercept = Intercept, slope = Slope, group = CNEC, colour = RAM < .2, linewidth = RAM < .2,
                                                              tooltip = paste0("CNEC: ", CNEC,
                                                                               "\nTSO: ", TSO,
                                                                               "\nType: ", Type,
                                                                               "\nRAM (% of Fmax): ", scales::percent(RAM, accuracy = .1, decimal.mark = ",")))) +
      ggiraph::geom_point_interactive(data = df_netpositions,
                                      mapping = ggplot2::aes(x = NetPosition[BiddingZone == BiddingZone1],
                                                             y = NetPosition[BiddingZone == BiddingZone2],
                                                             tooltip = paste0("Net positions for:\n",
                                                                              BiddingZone1, ": ", format(round(NetPosition[BiddingZone == BiddingZone1], 0), big.mark = ".", decimal.mark = ","), " MW\n",
                                                                              BiddingZone2, ": ", format(round(NetPosition[BiddingZone == BiddingZone2], 0), big.mark = ".", decimal.mark = ","), " MW")),
                                      shape = 21, size = 2, fill = "#005F83", colour = "white") +
      ggplot2::scale_x_continuous(name = paste0("Net position\n", BiddingZone1),
                                  labels = scales::number_format(big.mark = ".",
                                                                 decimal.mark = ",",
                                                                 suffix = " MW"),
                                  breaks = seq(-10000, 10000, 2500)) +
      ggplot2::scale_y_continuous(name = paste0("Net position\n", BiddingZone2),
                                  labels = scales::number_format(big.mark = ".",
                                                                 decimal.mark = ",",
                                                                 suffix = " MW"),
                                  breaks = seq(-10000, 10000, 2500)) +
      ggplot2::scale_colour_manual(name = "Type of network element:",
                                   values = c("TRUE" = "#C5003E",
                                              "FALSE" = "#C8C9C7"),
                                   labels = c("TRUE" = "RAM below 20%",
                                              "FALSE" = "RAM above 20%")) +
      ggplot2::scale_linewidth_manual(name = "Type of network element:",
                                      values = c("TRUE" = .5,
                                                 "FALSE" = .25),
                                      labels = c("TRUE" = "RAM below 20%",
                                                 "FALSE" = "RAM above 20%")) +
      ggplot2::scale_fill_manual(name = "Domain:",
                                 values = c("LTA" = "#FF8200",
                                            "FB" = "#005F83")) +
      ggplot2::coord_sf(xlim = c(-10000, 10000),
                        ylim = c(-10000, 10000),
                        expand = FALSE) +
      ggplot2::labs(caption = "Figure by Nico Schoutteet | Data by Core TSOs via JAO Publication Tool") +
      ggplot2::theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = rel(.6)),
            legend.title = element_text(size = rel(.6),
                                        face = "bold"),
            legend.key.size = unit(.25, "cm"),
            axis.text = element_text(size = rel(.6)),
            axis.title = element_text(size = rel(.6)),
            axis.line = element_blank(),
            axis.ticks = element_line(linewidth = .25),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0,
                                        size = rel(.5)),
            plot.background = element_rect(colour = NA,
                                           fill = "#f5f2e9"))
    
    ggiraph::girafe(ggobj = gg,
                    options = list(opts_sizing(rescale = TRUE)),
                    bg = "#f5f2e9")
    
  }
  
  
  
}
