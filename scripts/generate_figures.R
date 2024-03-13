#' generate_figures
#'
#' Generate package figures
#' @param data Default = NULL. rds file of preprocessed data, overrides folder
#' @param folder Default = NULL. Path to folder with tethys outputs
#' @param temporal_scale Default = "annual". Option between "annual", "monthly" or "all"
#' @keywords figures
#' @export
#' @importFrom magrittr %>%
#' @examples
#'\dontrun{
#' library(khanetal2022tethysSSPRCP)
#' khanetal2022tethysSSPRCP::generate_figures()
#'}

generate_figures <- function(folder=NULL,
                             annual_rds=NULL,
                             monthly_rds=NULL,
                             temporal_scale="annual") {
  NULL -> Year -> Value -> GCM -> Sector -> SSP -> RCP

  secabbs <- c("Domestic" = "dom", "Electricity" = "elec", "Manufacturing" = "mfg", 
               "Mining" = "min", "Irrigation" = "irr", "Livestock" = "liv")

  water_pal = c("Domestic"="dodgerblue",
                "Electricity"="darkslateblue",
                "Manufacturing"="#cef4d1",
                "Mining"="grey75",
                "Irrigation"="forestgreen",
                "Livestock"="goldenrod2")
  
  sec_pal = c("dom"="dodgerblue",
                "elec"="darkslateblue",
                "mfg"="#cef4d1",
                "min"="grey75",
                "irr"="forestgreen",
                "liv"="goldenrod2")

  gcm_pal = c("gfdl" = "gold",
              "hadgem" = "darkorange",
              "ipsl" = "red3",
              "miroc" = "deeppink",
              "noresm" = "darkviolet")

  crop_pal = c("Corn" = "gold3",
               "FiberCrop" = "gold4",
               "MiscCrop" = "darkorange4",
               "OilCrop" = "gray20",
               "OtherGrain" = "indianred2",
               "PalmFruit" = "firebrick3",
               "Rice" = "steelblue2",
               "Root_Tuber" = "mediumpurple",
               "SugarCrop" = "yellow2",
               "Wheat" = "burlywood",
               "FodderHerb" = "darkseagreen4",
               "FodderGrass" = "mediumseagreen",
               "biomass" = "#00931d")
  
  all_pal = c("dom"="dodgerblue",
              "elec"="darkslateblue",
              "mfg"="#cef4d1",
              "min"="grey75",
              #"irr"="forestgreen",
              "liv"="goldenrod2",
              "Corn" = "gold3",
              "FiberCrop" = "gold4",
              "MiscCrop" = "darkorange4",
              "OilCrop" = "gray20",
              "OtherGrain" = "indianred2",
              "PalmFruit" = "firebrick3",
              "Rice" = "steelblue2",
              "Root_Tuber" = "mediumpurple",
              "SugarCrop" = "yellow2",
              "Wheat" = "burlywood",
              "FodderHerb" = "darkseagreen4",
              "FodderGrass" = "mediumseagreen",
              "biomass" = "#00931d")

  # Initialize
  print("Starting generate_figures ...")


  # Check if folder exists
  # Check if required files exist (sectors total, irr etc.)


  if(grepl("annual|all",temporal_scale,ignore.case=T)){

    # get all annual data

    if (is.null(annual_rds)) {
      if (is.null(folder)) {
        print("Provide prepared .rds file or tethys outputs folder")
      } else {
        print(paste("Preparing raw tethys data from", folder))
        annual_data <- prepare_annual(folder)
        print("To speed up subsequent runs, use annual_rds='annual_data.rds'")
      }
    } else {
      print(paste("Reading data from", annual_rds))
      annual_data <- readRDS(annual_rds)
    }

    # Annual plots

    # Figure 1: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Lines: GCMs are colored lines
    # X axis: Years
    # Aggregated to all grid cells (sector = total)

    print("Building Figure 1")
    data1 <- dplyr::filter(annual_data, names(Sector) == "total")
    p1 <- ggplot2::ggplot(data = data1,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       group = GCM)) +
      ggplot2::geom_line(ggplot2::aes(color=GCM)) +
      ggplot2::scale_color_manual(values=gcm_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Total Global Annual Water Withdrawal by SSP-RCP-GCM") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p1
    ggplot2::ggsave(filename =  "figure1.png",
                    plot = p1,
                    width = 13,
                    height = 10) # save plot

    # Figure 2: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Lines: Sectors are colored lines (multiple GCMS same color)
    # X axis: Years
    # Aggregated to all grid cells (sectors = dom,elec,mfg,min,irr,liv)
    print("Building Figure 2")
    data2 <-  dplyr::filter(annual_data, (names(Sector)!="total") &
                              (names(Sector)!="nonag"))
    p2 <- ggplot2::ggplot(data = data2,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       group = interaction(GCM, Sector))) +
      ggplot2::geom_line(ggplot2::aes(color=Sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-GCM and Sector") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p2
    ggplot2::ggsave(filename =  "figure2.png",
                    plot = p2,
                    width = 13,
                    height = 10) # save plot

    # Figures 3a-e: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Bars: Sectors are stacked colored bars (GCMs are figures 3a-e)
    # X axis: Years
    # Aggregated to all grid cells (sectors = dom,elec,mfg,min,irr,liv)

    # ggprotos common to all figures 3
    base_fig3 <- list(ggplot2::aes(x = Year, y = Value, fill = Sector),
      ggplot2::geom_bar(position="stack", stat="identity"),
      ggplot2::facet_grid(RCP~SSP,scales="fixed"),
      ggplot2::scale_fill_manual(values=water_pal),
      ggplot2::xlab("Year"),
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))),
      ggplot2::theme_bw(),
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))
      )

    print("Building Figure 3a")
    data3a <-  dplyr::filter(annual_data, (GCM=="gfdl") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3a <- ggplot2::ggplot(data = data3a) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: gfdl") +
      base_fig3; p3a
    ggplot2::ggsave(filename = "figure3a.png",
                    plot = p3a,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3b")
    data3b <-  dplyr::filter(annual_data, (GCM=="hadgem") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3b <- ggplot2::ggplot(data = data3b) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: hadgem") +
      base_fig3; p3b
    ggplot2::ggsave(filename = "figure3b.png",
                    plot = p3b,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3c")
    data3c <-  dplyr::filter(annual_data, (GCM=="ipsl") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3c <- ggplot2::ggplot(data = data3c) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: ipsl") +
      base_fig3; p3c
    ggplot2::ggsave(filename = "figure3c.png",
                    plot = p3c,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3d")
    data3d <-  dplyr::filter(annual_data, (GCM=="miroc") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3d <- ggplot2::ggplot(data = data3d) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: miroc") +
      base_fig3; p3d
    ggplot2::ggsave(filename = "figure3d.png",
                    plot = p3d,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3e")
    data3e <-  dplyr::filter(annual_data, (GCM=="noresm") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3e <- ggplot2::ggplot(data = data3e) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: noresm") +
      base_fig3; p3e
    ggplot2::ggsave(filename = "figure3e.png",
                    plot = p3e,
                    width = 13,
                    height = 10) # save plot

    data6 <- readRDS("../data/annual_crops.rds")
    p6 <- ggplot2::ggplot(data = data6,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       group = interaction(GCM, Crop))) +
      ggplot2::geom_line(ggplot2::aes(color=Crop)) + #linetype=GCM
      ggplot2::scale_color_manual(values=crop_pal) +
      #ggplot2::scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Annual Crop Water Withdrawal by SSP-RCP-GCM") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p6
    ggplot2::ggsave(filename =  paste0(images, "figure6.png"),
                    plot = p6,
                    width = 13,
                    height = 10) # save plot
    
    
    ### consumption figures
    annual_consumption <- readRDS("../data/consumption_sums.rds")
    annual_consumption <- dplyr::group_by(annual_consumption, scenario, sector, year)
    annual_consumption <- dplyr::summarise(annual_consumption, value = sum(value))
    annual_consumption <- dplyr::ungroup(annual_consumption)
    annual_consumption <- dplyr::mutate(annual_consumption, SSP = paste0("SSP ", substr(scenario, 4, 4)),
                                        RCP = paste0("RCP ", substr(scenario, 9, 9), ".", substr(scenario, 10, 10)),
                                        GCM = substr(scenario, 12, 17))
    
    total_consumption <- dplyr::filter(annual_consumption, sector=="total")
    total_plot <- ggplot2::ggplot(data = total_consumption,
                                  ggplot2::aes(x = year,
                                               y = value,
                                               group = GCM)) +
      ggplot2::geom_line(ggplot2::aes(color=GCM)) +
      ggplot2::scale_color_manual(values=gcm_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Total Global Annual Water Consumption by SSP-RCP-GCM") +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Water Consumption (km3/year)") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))
    ggplot2::ggsave(filename = paste0(images, "figure1_c.png"),
                    plot = total_plot,
                    width = 13,
                    height = 10)
    
    sectoral_consumption <- dplyr::filter(annual_consumption, sector %in% c("dom", "elec", "irr", "liv", "mfg", "min"))
    sectoral_consumption$sector <- c("dom" = "Domestic",
                                     "elec" = "Electricity",
                                     "mfg" = "Manufacturing",
                                     "min" = "Mining",
                                     "irr" = "Irrigation",
                                     "liv" = "Livestock")[sectoral_consumption$sector]
    
    sectoral_plot <- ggplot2::ggplot(data = sectoral_consumption,
                                     ggplot2::aes(x = year,
                                                  y = value,
                                                  group = interaction(GCM, sector))) +
      ggplot2::geom_line(ggplot2::aes(color=sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Consumption by SSP-RCP-GCM and Sector") +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Water Consumption (km3/year)") + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))
    ggplot2::ggsave(filename = paste0(images, "figure2_c.png"),
                    plot = sectoral_plot,
                    width = 13,
                    height = 10)
    
    crop_consumption <- dplyr::filter(annual_consumption, !sector %in% c("dom", "elec", "irr", "liv", "mfg", "min", "nonag", "total"))
    
    crop_plot <- ggplot2::ggplot(data = crop_consumption,
                                 ggplot2::aes(x = year,
                                              y = value,
                                              group = interaction(GCM, sector))) +
      ggplot2::geom_line(ggplot2::aes(color=sector)) +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Annual Crop Water Consumption by SSP-RCP-GCM and Sector") +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Water Consumption (km3/year)") + 
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))
    ggplot2::ggsave(filename = paste0(images, "figure6_c.png"),
                    plot = crop_plot,
                    width = 13,
                    height = 10)
    
    
  }

  # Monthly plots
  if(grepl("monthly|all",temporal_scale,ignore.case=T)){
    # get all monthly data

    if (is.null(monthly_rds)) {
      if (is.null(folder)) {
        print("Provide prepared .rds file or tethys outputs folder")
      } else {
        print(paste("Preparing raw tethys data from", folder))
        monthly_data <- prepare_monthly(folder)
        print("To speed up subsequent runs, use monthly_rds='monthly_data.rds'")
      }
    } else {
      print(paste("Reading data from", monthly_rds))
      monthly_data <- readRDS(monthly_rds)
    }

    # all sectors
    print("Building Figure 4a")
    data4a <- dplyr::filter(monthly_data, (GCM=="gfdl")&(Year %in% c(2010, 2100)))
    data4a <- dplyr::mutate(data4a, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p4a <- ggplot2::ggplot(data = data4a,
                          ggplot2::aes(x = Month,
                                       y = Value,
                                       group = interaction(GCM, Sector, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by Year") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p4a
    ggplot2::ggsave(filename =  "figure4a.png",
                    plot = p4a,
                    width = 13,
                    height = 10) # save plot

    # non-irrigation
    print("Building Figure 4b")
    data4b <- dplyr::filter(monthly_data, (Sector!= "Irrigation")&(GCM=="gfdl")&(Year %in% c(2010, 2100)))
    data4b <- dplyr::mutate(data4b, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p4b <- ggplot2::ggplot(data = data4b,
                           ggplot2::aes(x = Month,
                                        y = Value,
                                        group = interaction(GCM, Sector, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by Year") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p4b
    ggplot2::ggsave(filename =  "figure4b.png",
                    plot = p4b,
                    width = 13,
                    height = 10) # save plot

    # Crops
    monthly_crops <- readRDS("../data/monthly_crops.rds")

    filter_out_crops <- c("PalmFruit", "FodderGrass", "Root_Tuber", "FodderHerb", "OtherGrain")
    filter_crops <- c("Rice", "Wheat", "FiberCrop", "SugarCrop", "biomass")
    data5 <- dplyr::filter(monthly_crops, GCM == "gfdl" & Year %in% c(2010, 2100) &
                             Crop %in% filter_crops)
    data5 <- dplyr::mutate(data5, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p5 <- ggplot2::ggplot(data = data5,
                           ggplot2::aes(x = Month,
                                        y = Value,
                                        group = interaction(GCM, Crop, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Crop)) +
      ggplot2::scale_color_manual(values=crop_pal[names(crop_pal) %in% filter_crops]) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by Year, Selected Crops") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p5
    ggplot2::ggsave(filename = paste0(images, "figure5.png"),
                    plot = p5,
                    width = 13,
                    height = 10)
  }

  # Workflow illustrations
  if(grepl("workflow|all",temporal_scale,ignore.case=T)){
    
    # pretty overview total picture
    mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                       sectors = c("total"),
                       years=2010, months=0)
    
    rm <- rmap::map(mydata, save=F, show=F, background = T, legendType="continuous",
                    crop_to_underLayer = T,
                    underLayer = rmap::mapCountries,
                    overLayer =  rmap::mapCountries)
    
    a <- rm[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      rescaler = ~(.x/max(.x))^0.25,
      guide="none")
    
    grDevices::png(paste0(images,"total.png"),width=13,height=7,units="in",res=288); print(a); grDevices::dev.off()
    
    
    ### Alternate spatial downscaling fig
    
    sector_names = c("dom"         = "Domestic",
                     "elec"        = "Electricity",
                     "liv"         = "Livestock",
                     "irr"         = "Irrigation",
                     "mfg"         = "Manufacturing",
                     "min"         = "Mining")
    
    region_data <- readRDS("../data/region_data.rds")
    region_data <- dplyr::ungroup(region_data)
    region_data <- dplyr::filter(region_data, year==2010, scenario=="ssp1_rcp26_gfdl")
    region_data <- dplyr::select(region_data, region=subRegion_GCAMReg32, basin=subRegion_GCAMBasin, sector=class, value)
    region_data_irr <- dplyr::filter(region_data, sector=="Irrigation")
    region_data_nonirr <- dplyr::filter(region_data, sector!="Irrigation")
    
    converter_nonirr <- dplyr::select(rmap::mapping_tethys_grid_basin_region_country, lon, lat, region=regionName)
    converter_irr <- dplyr::select(rmap::mapping_tethys_grid_basin_region_country, lon, lat, region=regionName, basin=basinName)
    
    region_data_nonirr <- dplyr::full_join(region_data_nonirr, converter_nonirr)
    region_data_irr <- dplyr::full_join(region_data_irr, converter_irr)
    region_data_irr$sector <- "Irrigation"
    region_data_irr$value <- tidyr::replace_na(region_data_irr$value, 0)
    
    region_data <- dplyr::bind_rows(region_data_irr, region_data_nonirr)
    region_data$sector <- factor(region_data$sector, levels=sector_names)
    region_data$name <- "Region/Basin Scale"
    region_data <- dplyr::select(region_data, lon, lat, sector, name, value)
    region_data <- tidyr::drop_na(region_data)
    
    region_map <- rmap::map(region_data, save=F, show=F, background=T,
                            row="sector", col="name",
                            legendType = "continuous")
    region_map <- region_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      limits = c(0, max(region_data$value)),
      #guide="none",
      name = "Water\nWithdrawals\n(km3)") + 
      ggplot2::theme(legend.position = "bottom",
                     legend.text = ggplot2::element_text(angle=90))
    
    grid_data <- get_data(folder=folder, scenarios="ssp1_rcp26_gfdl", sectors=c("dom", "elec", "irr", "liv", "mfg", "min"),
                          years=2010, months=0)
    grid_data$sector <- factor(sector_names[grid_data$sector], levels=sector_names)
    grid_data$name <- "Gridded Scale"
    grid_data <- dplyr::select(grid_data, lon, lat, sector, name, value)
    
    grid_map <- rmap::map(grid_data, save=F, show=F, background=T,
                          row="sector", col="name",
                          legendType = "continuous")
    grid_map <- grid_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      #guide="none",
      name = "Water\nWithdrawals\n(km3)") + 
      ggplot2::theme(legend.position = "bottom",
                     legend.text = ggplot2::element_text(angle=90))
    
    
    together <- region_map + grid_map
    grDevices::png(paste0(images,"spatialworkflow.png"),width=9,height=11,units="in",res=300); print(together); grDevices::dev.off()
    
    #######################
    
    
    # using ssp1_rcp26_gfdl, 2010 to show spatial downscaling
    library(rmap)

    ssp_rcp_gcm <- "ssp1_rcp26_gfdl"
    data_year <- 2010
    secNames <- c("Domestic", "Electricity", "Irrigation", "Livestock", "Manufacturing", "Mining")

    print("Starting spatial downscaling example map ...")

    basinRegion <- dplyr::distinct(rmap::mapIntersectGCAMBasin32Reg@data[c("subRegion_GCAMBasin", "subRegion_GCAMReg32")])

    # In order to plot with rmap facets, need to use the same map for all sectors,
    # and mapIntersectGCAMBasin32Reg is the "common denominator"
    # Irrigation data is actually region intersect basin scale, all others are region scale
    # For non-irrigation, each region_X_basin value is SET to the region's total value,
    # so the value displayed does not represent the value of the basin
    region_data <- readRDS("../data/region_data.rds")
    region_data <- dplyr::filter(region_data, scenario==ssp_rcp_gcm, year==data_year)

    region_data <- dplyr::full_join(tidyr::crossing(basinRegion, class=secNames), region_data)
    region_data <- dplyr::arrange(region_data, subRegion_GCAMReg32, class) # sort so that below works
    region_data <- tidyr::fill(region_data, scenario, year, value, .direction = "up") # sets all basin intersections to region's total withdrawal
    region_data <- tidyr::drop_na(region_data) # get rid of region only rows

    region_data <- dplyr::mutate(region_data, .keep="unused", subRegion=paste0(
      subRegion_GCAMBasin, "_X_", subRegion_GCAMReg32))


    # read grid data

    print("Loading grid data")
    files <- paste0(folder, "/", ssp_rcp_gcm, "/wd",
                    c("dom", "elec", "irr", "liv", "mfg", "min"), "_km3peryr.csv")
    grid_data <- data.table::rbindlist(lapply(files, function(x){ # stacks long
      return (data.table::fread(x, select=c(2, 3, 6+(data_year-2010)/5),
                                col.names=c("lon", "lat", "value"))) }))
    grid_data <- dplyr::bind_cols(class=rep(secNames, each=67420), grid_data)

    # make the figures
    print("Building spatial workflow figure")
    region_map <- rmap::map(data=region_data, title="Region/Basin Scale Values",
                            ncol=3, legendType = "continuous",
                            underLayer = rmap::mapIntersectGCAMBasin32Reg,
                            overLayer = rmap::mapIntersectGCAMBasin32Reg,
                            crop_to_underLayer = T,
                            background = T, save=F, show=F)
    region_map2 <- region_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      limits=c(0, max(region_data$value)),
      breaks=c(0, 1, 10, 100),
      name = "Water\nWithdrawals\n(km3)"
      )
    
    grid_map <- rmap::map(data=grid_data, title="Spatially Downscaled Gridded Values",
                          ncol=3, legendType = "continuous",
                          underLayer = rmap::mapIntersectGCAMBasin32Reg,
                          overLayer = rmap::mapIntersectGCAMBasin32Reg,
                          crop_to_underLayer = T,
                          background = T, save=F, show=F)
    grid_map2 <- grid_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      limits=c(0, max(grid_data$value)),
      breaks=c(0, 0.1, 1, 5),
      name = "Water\nWithdrawals\n(km3)"
    )
    
    spatial_wf <- region_map2/ grid_map2 # library(patchwork)
    grDevices::png(paste0(images,"spatialworkflow.png"),width=13,height=10,units="in",res=300); print(spatial_wf); grDevices::dev.off()
    
    print("Spatial workflow figure complete")


    # Temporal Workflow

    # Annual and monthly maps, using ssp1_rcp26_gfdl, 2010, irrigation
    annual_grid_data <- data.table::fread(paste0(folder,"/ssp1_rcp26_gfdl/wdirr_km3peryr.csv"),
                                          select=c(2, 3, 6), col.names=c("lon", "lat", "value"))
    monthly_grid_data <- data.table::fread(paste0(folder,"/ssp1_rcp26_gfdl/twdirr_km3permonth.csv"),
                                          select=c(2, 3, 6:17), col.names=c("lon", "lat", month.abb))
    monthly_grid_data <- data.table::melt(monthly_grid_data, measure.vars=month.abb, variable.name="scenario")

    annual_map <- rmap::map(annual_grid_data, legendType = "continuous",
                            title = "Annual Gridded Values",
                            overLayer=rmap::mapCountries,
                            background=T, save=F, show=F)
    annual_map1 <- annual_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      limits=c(0, max(annual_grid_data$value)),
      breaks=c(0, 0.1, 1, 5),
      name = "Water\nWithdrawals\n(km3)")
    
    monthly_map <- rmap::map(monthly_grid_data, legendType = "continuous",
                             title = "Temporally Downscaled Monthly Gridded Values",
                             overLayer=rmap::mapCountries,
                             background=T, save=F, show=F)
    monthly_map1 <- monthly_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      limits=c(0, max(monthly_grid_data$value)),
      breaks=c(0, 0.01, 0.1, 0.5),
      name = "Water\nWithdrawals\n(km3)")
    
    temporal_wf1 <- annual_map1 / monthly_map1 + 
      patchwork::plot_layout(nrow=2, heights=c(1,1), guides="collect")
    grDevices::png(paste0(images,"temporalworkflow.png"),width=8,height=10,units="in",res=300); print(temporal_wf1); grDevices::dev.off()
    
    # annual graph, monthly graph
    annual_data <- readRDS("annual_data.rds")
    dataA <- dplyr::filter(annual_data, (SSP=="SSP 1") & (RCP=="RCP 2.6") &
                             (GCM=="gfdl") &
                             (Year %in% c(2010)) &
                           (names(Sector)!="total") &
                             (names(Sector)!="nonag"))
    dataA <- dplyr::mutate(dataA, Year=factor(Year))
    pA <- ggplot2::ggplot(data=dataA,
                          ggplot2::aes(x = Year, y = Value, fill = Sector))+
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_manual(values=water_pal)+
      ggplot2::ggtitle("Annual Water Withdrawal by Sector (2010)")+
      ggplot2::xlab("Year")+
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year)))+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5));pA

    monthly_data <- readRDS("monthly_data.rds")
    dataB <- dplyr::filter(monthly_data, (SSP=="SSP 1") & (RCP=="RCP 2.6") &
                             (GCM=="gfdl") &
                             (Year %in% c(2010)))
    dataB <- dplyr::mutate(dataB, Year=factor(Year), Month=factor(Month, labels=month.abb))
    pB <- ggplot2::ggplot(data=dataB,
                          ggplot2::aes(x = Month, y = Value, fill = Sector))+
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_manual(values=water_pal)+
      ggplot2::ggtitle("Monthly Water Withdrawal by Sector (2010)")+
      ggplot2::xlab("Month")+
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ month)))+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5));pB
    temporal_wf <- cowplot::plot_grid(pA, pB)
    ggplot2::ggsave(filename = "temporalworkflow.png",
                    plot = temporal_wf,
                    width = 13,
                    height = 10) # save plot
  }

  # validation plots
  if(grepl("validation|all",temporal_scale,ignore.case=T)){
    
    # Validation  1: Spatial Downscaling
    # 1 point for each ssp-rcp-gcm (75) x GCAM Year (19) x [Sector (6) or Crop (13)]
    # x-axis: total water withdrawal for that point (sum over GCAM regions/basins)
    # y-axis: total water withdrawal for that point (sum over 67420 Tethys grids)
    
    GCAM_data <- readRDS("../data/gcam_data.rds")
    Tethys_data <- readRDS("../data/region_sums_wd.rds")
    Tethys_data <- dplyr::select(Tethys_data,-month)
    Tethys_data <- dplyr::rename(Tethys_data, tethys_value = value)
    comparison <- dplyr::full_join(GCAM_data, Tethys_data)
    comparison <- dplyr::mutate(comparison, tethys_value=tidyr::replace_na(tethys_value, 0))
 
    # 6 sectors
    comparison_sectors <- dplyr::filter(comparison, sector %in% c("dom","elec","irr","liv","mfg","min"))
    v1 <- ggplot2::ggplot(comparison_sectors,
                          ggplot2::aes(x=value, y=tethys_value, color=sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=sec_pal) +
      ggplot2::ggtitle("GCAM vs Spatially Downscaled") +
      ggplot2::xlab(bquote("GCAM Water Withdrawal " ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Spatially Downscaled, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation1.png"),
                    plot = v1,
                    width = 13,
                    height = 10)
    
    # 13 crops
    comparison_crops <- dplyr::filter(comparison, !sector %in% c("dom","elec","irr","liv","mfg","min"))
    v2 <- ggplot2::ggplot(comparison_crops,
                          ggplot2::aes(x=value, y=tethys_value, color=sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::ggtitle("GCAM vs Spatially Downscaled") +
      ggplot2::xlab(bquote("GCAM Water Withdrawal " ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Spatially Downscaled, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation2.png"),
                    plot = v2,
                    width = 13,
                    height = 10)
    
    # CONSUMPTION
    GCAM_data <- readRDS("../data/gcam_consumption.rds")
    Tethys_data <- readRDS("../data/consumption_sums.rds")
    Tethys_data <- dplyr::filter(Tethys_data, sector != "total")
    Tethys_data <- dplyr::mutate(Tethys_data, basin = dplyr::case_when(
      sector %in% c("dom", "elec","liv", "mfg", "min") ~ NA_character_,
      TRUE ~ basin))
    Tethys_data <- dplyr::group_by(Tethys_data, scenario, sector, year, region, basin)
    Tethys_data <- dplyr::summarise(Tethys_data, value = sum(value))
    Tethys_data <- dplyr::rename(Tethys_data, tethys_value = value)
    comparison <- dplyr::full_join(GCAM_data, Tethys_data)
    comparison <- dplyr::mutate(comparison, tethys_value=tidyr::replace_na(tethys_value, 0))
    
    # 6 sectors
    
    comparison_sectors <- dplyr::filter(comparison, sector %in% c("dom","elec", "irr", "liv", "mfg","min"))
    # irrigation dominates, reorders so all sectors are shown
    comparison_sectors$sector <- factor(comparison_sectors$sector, levels=c("irr", "liv", "elec", "mfg", "dom", "min"))
    comparison_sectors <- dplyr::arrange(comparison_sectors, sector)

    v5 <- ggplot2::ggplot(comparison_sectors,
                          ggplot2::aes(x=value, y=tethys_value, color=sector)) +
      ggplot2::geom_point(shape=3, alpha=1) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=sec_pal) +
      ggplot2::ggtitle("GCAM vs Spatially Downscaled") +
      ggplot2::xlab(bquote("GCAM Water Consumption " ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Spatially Downscaled, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation5.png"),
                    plot = v5,
                    width = 13,
                    height = 10)
    
    # 13 crops
    comparison_crops <- dplyr::filter(comparison, !sector %in% c("dom","elec","irr","liv","mfg","min"))
    v6 <- ggplot2::ggplot(comparison_crops,
                          ggplot2::aes(x=value, y=tethys_value, color=sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::ggtitle("GCAM vs Spatially Downscaled") +
      ggplot2::xlab(bquote("GCAM Water Consumption " ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Spatially Downscaled, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation6.png"),
                    plot = v6,
                    width = 13,
                    height = 10)
    
    # temporal
    
    Tethys_spatial <- readRDS("../data/region_sums_wd.rds")
    Tethys_spatial <- dplyr::select(Tethys_spatial,-month)
    Tethys_temporal <- readRDS("../data/month_sums.rds")
    Tethys_temporal <- dplyr::rename(Tethys_temporal, temporal_value=value)
    comparison <- dplyr::full_join(Tethys_spatial, Tethys_temporal)
    
    comparison_sectors <- dplyr::filter(comparison, sector %in% c("dom","elec","irr","liv","mfg","min"))
    v3 <- ggplot2::ggplot(comparison_sectors,
                          ggplot2::aes(x=value, y=temporal_value, color = sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=sec_pal) +
      ggplot2::ggtitle("Tethys Annual vs Tethys Monthly") +
      ggplot2::xlab(bquote("Tethys Annual Withdrawal" ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Tethys Monthly Withdrawal, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation3.png"),
                    plot = v3,
                    width = 13,
                    height = 10) # save plot
    
    comparison_crops <- dplyr::filter(comparison, !sector %in% c("dom","elec","irr","liv","mfg","min"))
    v4 <- ggplot2::ggplot(comparison_crops,
                          ggplot2::aes(x=value, y=temporal_value, color = sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::ggtitle("Tethys Annual vs Tethys Monthly") +
      ggplot2::xlab(bquote("Tethys Annual Withdrawal" ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Tethys Monthly Withdrawal, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation4.png"),
                    plot = v4,
                    width = 13,
                    height = 10) # save plot
    
    ### CONSUMPTION
    Tethys_spatial <- readRDS("../data/consumption_sums.rds")
    Tethys_spatial <- dplyr::filter(Tethys_spatial, sector != "total")
    Tethys_spatial <- dplyr::mutate(Tethys_spatial, basin = dplyr::case_when(
      sector %in% c("dom", "elec","liv", "mfg", "min") ~ NA_character_,
      TRUE ~ basin))
    Tethys_spatial <- dplyr::group_by(Tethys_spatial, scenario, sector, year, region, basin)
    Tethys_spatial <- dplyr::summarise(Tethys_spatial, value = sum(value))

    Tethys_temporal <- readRDS("../data/consumption_sums_monthly.rds")
    Tethys_temporal <- dplyr::mutate(Tethys_temporal, basin = dplyr::case_when(
      sector %in% c("dom", "elec","liv", "mfg", "min") ~ NA_character_,
      TRUE ~ basin))
    Tethys_temporal <- dplyr::group_by(Tethys_temporal, scenario, sector, year, region, basin)
    Tethys_temporal <- dplyr::summarise(Tethys_temporal, value = sum(value))
    Tethys_temporal <- dplyr::rename(Tethys_temporal, temporal_value=value)
    
    comparison <- dplyr::full_join(Tethys_spatial, Tethys_temporal)
    
    comparison_sectors <- dplyr::filter(comparison, sector %in% c("dom","elec","irr","liv","mfg","min"))
    v7 <- ggplot2::ggplot(comparison_sectors,
                          ggplot2::aes(x=value, y=temporal_value, color = sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=sec_pal) +
      ggplot2::ggtitle("Tethys Annual vs Tethys Monthly") +
      ggplot2::xlab(bquote("Tethys Annual Consumption" ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Tethys Monthly Consumption, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation7.png"),
                    plot = v7,
                    width = 13,
                    height = 10) # save plot
    
    comparison_crops <- dplyr::filter(comparison, !sector %in% c("dom","elec","irr","liv","mfg","min"))
    v8 <- ggplot2::ggplot(comparison_crops,
                          ggplot2::aes(x=value, y=temporal_value, color = sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::ggtitle("Tethys Annual vs Tethys Monthly") +
      ggplot2::xlab(bquote("Tethys Annual Consumption" ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Tethys Monthly Consumption, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation8.png"),
                    plot = v8,
                    width = 13,
                    height = 10) # save plot
    
    }
  
  if(grepl("comparisons|all",temporal_scale,ignore.case=T)) {
    
    #monthly plots
    wf_data <- tibble::tibble()
    for (i in 1:12){
      filename <- paste0(r"{C:\Users\thom927\Documents\Data\WFbl_m3mo_30m\wfbl_m}",
                         gsub(" ", "0", format(i, width=2)), r"{_30m\w001001.adf}")
      r <- raster::raster(filename)
      vals <- raster::values(r) * 10^-9
      coords <- round(raster::coordinates(r), 2)
      wf_data <- dplyr::bind_rows(wf_data, tibble::tibble(lon=coords[,"x"],
                                                          lat=coords[,"y"],
                                                          month=i,
                                                          wf_value=vals))
    }
    
    
    all_data <- get_data(folder=folder, scenarios="ssp1_rcp26_gfdl", sectors=c("dom", "elec", "irr", "liv", "mfg", "min"),
                         consumption=T,
                         years=2010, months=1:12)
    all_data <- dplyr::group_by(all_data, lon, lat, region, basin, month)
    all_data <- dplyr::summarise(all_data, value=sum(value))
    all_data <- dplyr::ungroup(all_data)
    all_data <- dplyr::full_join(all_data, wf_data)
    all_data <- dplyr::filter(all_data, !is.na(value))
    all_data <- dplyr::filter(all_data, !is.na(region))
    all_data$wf_value <- tidyr::replace_na(all_data$wf_value, 0)
    all_data$month <- month.name[all_data$month]
    all_data$month <- factor(all_data$month, levels=month.name)
    
    comaprison_plot2 = ggplot2::ggplot(all_data, ggplot2::aes(x=value, y=wf_value, color=region)) +
      ggplot2::geom_point() +
      ggplot2::geom_blank(ggplot2::aes(y=value, x=wf_value)) +
      ggplot2::theme(aspect.ratio = 1, legend.position = "bottom") +
      ggplot2::facet_wrap(vars(month), scales="free") + 
      #ggplot2::ggtitle("Comparing Total Gridded Water Demands by Month") +
      ggplot2::xlab("Khan et al. (2022) value for 2010 (km3)") +
      ggplot2::ylab("Mekonnen and Hoekstra (2011) value for 1996-2005 (km3)")
    ggplot2::ggsave(filename = paste0(images, "comparison2.png"),
                    plot = comaprison_plot2, width = 13, height = 10)
    
    
    ### sectoral plots
    huangetal2018 = r"{C:\Users\thom927\Documents\Data\Huangetal\}"
    nonag = c("domestic", "electricity", "manufacturing", "mining", "livestock")
    nonag_abb = c("dom", "elec", "mfg", "min", "liv")
    
    sector_names = c("dom" = "Domestic",
                     "elec" = "Electricity",
                     "liv" = "Livestock",
                     "irr" = "Irrigation",
                     "mfg" = "Manufacturing",
                     "min" = "Mining")
    
    areas <- data.table::fread(r"{C:\Users\thom927\Documents\Data\example_v1_3_0\Input\Grid_Areas_ID.csv}")
    
    all_data <- tibble::tibble()
    for (i in seq(1,5)) {
      # Withdrawal
      nc = ncdf4::nc_open(paste0(huangetal2018, nonag[i], r"{ water use v2\withd_}", nonag_abb[i], ".nc"))
      withd = ncdf4::ncvar_get(nc, paste0("withd_", nonag_abb[i]), start=c(1, 469))
      sector_data <- get_data(folder=folder, consumption=F, scenarios="ssp1_rcp26_gfdl", sectors=nonag_abb[i], years=2010, months=0)
      sector_data$demand <- "Withdrawal"
      sector_data <- dplyr::mutate(sector_data, huang_value=rowSums(withd) * areas$V1 * 10^-8)
      sector_data <- dplyr::select(sector_data, lon, lat, region, basin, sector, demand, value, huang_value)
      all_data <- dplyr::bind_rows(all_data, sector_data)
      
      # Consumption
      nc = ncdf4::nc_open(paste0(huangetal2018, nonag[i], r"{ water use v2\cons_}", nonag_abb[i], ".nc"))
      withd = ncdf4::ncvar_get(nc, paste0("cons_", nonag_abb[i]), start=c(1, 469))
      sector_data <- get_data(folder=folder, consumption=T, scenarios="ssp1_rcp26_gfdl", sectors=nonag_abb[i], years=2010, months=0)
      sector_data$demand <- "Consumption"
      sector_data <- dplyr::mutate(sector_data, huang_value=rowSums(withd) * areas$V1 * 10^-8)
      sector_data <- dplyr::select(sector_data, lon, lat, region, basin, sector, demand, value, huang_value)
      all_data <- dplyr::bind_rows(all_data, sector_data)
    }
    # irrigation different
    {
      nc = ncdf4::nc_open(r"{C:\Users\thom927\Documents\Data\Huangetal\irrigation water use v2\withd_irr_pcrglobwb.nc}")
      withd = tibble::tibble(raw = rowSums(ncdf4::ncvar_get(nc, "withd_irr", start=c(1, 469))))
      withd$lat = ncdf4::ncvar_get(nc, "lat")
      withd$lon = ncdf4::ncvar_get(nc, "lon")
      sector_data <- get_data(folder=folder, scenarios="ssp1_rcp26_gfdl", sectors="irr", consumption=F, years=2010, months=0)
      sector_data$demand <- "Withdrawal"
      sector_data$area <- areas
      sector_data <- dplyr::full_join(sector_data, withd)
      sector_data <- dplyr::mutate(sector_data, huang_value = raw * area * 10^-8)
      sector_data <- dplyr::mutate(sector_data, huang_value = dplyr::case_when(is.nan(huang_value) ~ 0,
                                                                               is.na(huang_value) ~ 0,
                                                                               TRUE ~ huang_value))
      sector_data <- dplyr::select(sector_data, lon, lat, region, basin, sector, demand, value, huang_value)
      all_data <- dplyr::bind_rows(all_data, sector_data)
      
      nc = ncdf4::nc_open(r"{C:\Users\thom927\Documents\Data\Huangetal\irrigation water use v2\cons_irr_pcr.nc}")
      withd = tibble::tibble(raw = rowSums(ncdf4::ncvar_get(nc, "cons_irr", start=c(1, 469))))
      withd$lat = ncdf4::ncvar_get(nc, "lat")
      withd$lon = ncdf4::ncvar_get(nc, "lon")
      sector_data <- get_data(folder=folder, scenarios="ssp1_rcp26_gfdl", sectors="irr", consumption=T, years=2010, months=0)
      sector_data$demand <- "Consumption"
      sector_data$area <- areas
      sector_data <- dplyr::full_join(sector_data, withd)
      sector_data <- dplyr::mutate(sector_data, huang_value = raw * area * 10^-8)
      sector_data <- dplyr::mutate(sector_data, huang_value = dplyr::case_when(is.nan(huang_value) ~ 0,
                                                                               is.na(huang_value) ~ 0,
                                                                               TRUE ~ huang_value))
      sector_data <- dplyr::select(sector_data, lon, lat, region, basin, sector, demand, value, huang_value)
      all_data <- dplyr::bind_rows(all_data, sector_data)
    }
    
    all_data <- dplyr::filter(all_data, !is.na(region))
    #all_data <- dplyr::group_by(all_data, sector, demand, region)
    #all_data <- dplyr::mutate(all_data, value=value/sum(value), huang_value=huang_value/sum(huang_value))
    #all_data <- dplyr::ungroup(all_data)
    all_data$value[is.nan(all_data$value)] <- 0
    all_data$huang_value[is.nan(all_data$huang_value)] <- 0
    
    all_data$sector <- sector_names[all_data$sector]
    
    sector_plot = ggplot2::ggplot(all_data, ggplot2::aes(x=value, y=huang_value, color=region)) +
      ggplot2::geom_point() +
      ggplot2::geom_blank(ggplot2::aes(y=value, x=huang_value)) +
      ggplot2::theme(aspect.ratio = 1, legend.position = "bottom") +
      ggplot2::facet_wrap(demand~sector,scales="free", ncol=6) + 
      #ggplot2::ggtitle("Comparing 2010 Gridded Water Demands by Sector") +
      ggplot2::xlab("Khan et al. (2022) value for 2010 (km3)") +
      ggplot2::ylab("Huang et al. (2018) value for 2010 (km3)")
    ggplot2::ggsave(filename = paste0(images, "comparison.png"),
                    plot = sector_plot, width = 15, height = 10)
    
    
    
    ### comparing all 3
    huangetal2018 = r"{C:\Users\thom927\Documents\Data\Huangetal\}"
    files_w = c(r"{domestic water use v2\withd_dom.nc}",
                r"{electricity water use v2\withd_elec.nc}",
                r"{irrigation water use v2\withd_irr_pcrglobwb.nc}",
                r"{livestock water use v2\withd_liv.nc}",
                r"{manufacturing water use v2\withd_mfg.nc}",
                r"{mining water use v2\withd_min.nc}")
    files_c = c(r"{domestic water use v2\cons_dom.nc}",
                r"{electricity water use v2\cons_elec.nc}",
                r"{irrigation water use v2\cons_irr_pcr.nc}",
                r"{livestock water use v2\cons_liv.nc}",
                r"{manufacturing water use v2\cons_mfg.nc}",
                r"{mining water use v2\cons_min.nc}")
    vars_w = paste0("withd_", c("dom", "elec", "irr", "liv", "mfg", "min"))
    vars_c = paste0("cons_",  c("dom", "elec", "irr", "liv", "mfg", "min"))
    
    areas <- data.table::fread(r"{C:\Users\thom927\Documents\Data\example_v1_3_0\Input\Grid_Areas_ID.csv}")
    areas$lon <- rmap::mapping_tethys_grid_basin_region_country$lon
    areas$lat <- rmap::mapping_tethys_grid_basin_region_country$lat
    
    huang_data <- tibble::tibble()
    for (i in seq(1,6)) {
      # Withdrawal
      nc <- ncdf4::nc_open(paste0(huangetal2018, files_w[i]))
      for (m in 1:12) { # could probably do all at once then melt, oh well
        partial <- tibble::tibble(raw = ncdf4::ncvar_get(nc, vars_w[i], start=c(1, 468+m), count=c(-1,1)))
        partial$lon <- ncdf4::ncvar_get(nc, "lon")
        partial$lat <- ncdf4::ncvar_get(nc, "lat")
        partial <- dplyr::left_join(partial, areas)
        partial <- dplyr::mutate(partial, value = raw * V1 * 10^-8)
        partial$month <- m
        partial$demand <- "Withdrawal"
        huang_data <- dplyr::bind_rows(huang_data, partial)
      }
      # Consumption
      nc <- ncdf4::nc_open(paste0(huangetal2018, files_c[i]))
      for (m in 1:12) {
        partial <- tibble::tibble(raw = ncdf4::ncvar_get(nc, vars_c[i], start=c(1, 468+m), count=c(-1,1)))
        partial$lon <- ncdf4::ncvar_get(nc, "lon")
        partial$lat <- ncdf4::ncvar_get(nc, "lat")
        partial <- dplyr::left_join(partial, areas)
        partial <- dplyr::mutate(partial, value = raw * V1 * 10^-8)
        partial$month <- m
        partial$demand <- "Consumption"
        huang_data <- dplyr::bind_rows(huang_data, partial)
      }
    }
    huang_data$value[is.nan(huang_data$value)] <- 0
    huang_data <- dplyr::group_by(huang_data, demand, lon, lat, month)
    huang_data <- dplyr::summarise(huang_data, value=sum(value))
    huang_data <- dplyr::ungroup(huang_data)
    huang_data$set <- "Huang et al. (2018)"
    
    
    wf_data <- tibble::tibble()
    for (m in 1:12){
      filename <- paste0(r"{C:\Users\thom927\Documents\Data\WFbl_m3mo_30m\wfbl_m}",
                         gsub(" ", "0", format(m, width=2)), r"{_30m\w001001.adf}")
      r <- raster::raster(filename)
      coords <- round(raster::coordinates(r), 2)
      wf_data <- dplyr::bind_rows(wf_data, tibble::tibble(lon=coords[,"x"],
                                                          lat=coords[,"y"],
                                                          month=m,
                                                          value=raster::values(r) *10^-9))
    }
    wf_data$set <- "Mekonnen and Hoekstra (2011)"
    wf_data$demand <- "Consumption"
    
    
    tethys_data_w <- get_data(folder=folder, scenarios="ssp1_rcp26_gfdl", sectors=c("dom", "elec", "irr", "liv", "mfg", "min"),
                              consumption=F,
                              years=2010, months=1:12)
    tethys_data_w <- dplyr::group_by(tethys_data_w, lon, lat, month)
    tethys_data_w <- dplyr::summarise(tethys_data_w, value=sum(value))
    tethys_data_w <- dplyr::ungroup(tethys_data_w)
    tethys_data_w$set <- "Khan et al. (2022)"
    tethys_data_w$demand <- "Withdrawal"
    
    tethys_data_c <- get_data(folder=folder, scenarios="ssp1_rcp26_gfdl", sectors=c("dom", "elec", "irr", "liv", "mfg", "min"),
                              consumption=T,
                              years=2010, months=1:12)
    tethys_data_c <- dplyr::group_by(tethys_data_c, lon, lat, month)
    tethys_data_c <- dplyr::summarise(tethys_data_c, value=sum(value))
    tethys_data_c <- dplyr::ungroup(tethys_data_c)
    tethys_data_c$set <- "Khan et al. (2022)"
    tethys_data_c$demand <- "Consumption"
    
    all_data <- dplyr::bind_rows(tethys_data_w, tethys_data_c, huang_data, wf_data)
    
    all_data_spatial <- dplyr::group_by(all_data, set, demand, lon, lat)
    all_data_spatial <- dplyr::summarise(all_data_spatial, value=sum(value))
    all_data_spatial <- dplyr::ungroup(all_data_spatial)
    all_data_spatial <- dplyr::filter(all_data_spatial, paste0(lon, ",", lat) %in% paste0(
      rmap::mapping_tethys_grid_basin_region_country$lon, ",",
      rmap::mapping_tethys_grid_basin_region_country$lat)
    )
    
    all_data_temporal <- dplyr::group_by(all_data, set, demand, month)
    all_data_temporal <- dplyr::summarise(all_data_temporal, value=sum(value))
    all_data_temporal <- dplyr::ungroup(all_data_temporal)
    all_data_temporal$month <- month.abb[all_data_temporal$month]
    all_data_temporal$month <- factor(all_data_temporal$month, levels = month.abb)
    
    myplot <- ggplot2::ggplot(all_data_temporal, ggplot2::aes(x=month, y=value,
                                                              color=set, linetype = demand,
                                                              group=interaction(set, demand))) +
      ggplot2::geom_line() +
      ggplot2::labs(color="Data Set", linetype="Demand Type") +
      ggplot2::xlab("Month") +
      ggplot2::ylab("Water Demand (km3/month)")
    ggplot2::ggsave(filename = paste0(images, "comparison_temporal.png"),
                    plot = myplot, width = 13, height = 6)
    
    mymap <- rmap::map(all_data_spatial, save=F, show=F, background=T,
                       legendType = "continuous",
                       overLayer = rmap::mapCountries,
                       row="set", col="demand")
    
    mymap2 <- mymap[[1]] + 
      ggplot2::scale_fill_gradientn(
        colors = jgcricol()$pal_hot,
        trans = scales::trans_new(name = '4th root',
                                  transform = function(x){x^0.25},
                                  inverse = function(x){x^4}),
        name = "Water\nDemand\n(km3)")
    
    grDevices::png(paste0(images,"comparison_spatial.png"),width=13,height=10,units="in",res=300); print(mymap2); grDevices::dev.off()
    
      
    
  }
  
  
  if(grepl("animations|all",temporal_scale,ignore.case=T)) {
    gridLookup <- readRDS("rmap_tethys_grid_basin_region_country.rds")
    files <- paste0(folder, "/", "ssp5_rcp85_gfdl", "/crops_wdirr_",
                    names(crop_pal), "_km3peryr.csv")

    all_years_raw <- data.table::rbindlist(lapply(files, function(x){
      return (data.table::fread(x, select=c(1, 6:24))) }))
    all_years_raw <- dplyr::bind_cols(Crop=rep(names(crop_pal), each=67420), all_years_raw)


    all_years <- data.table::melt(all_years_raw, id.vars=c(1,2), variable.name="year")
    all_years <- tibble::as_tibble(all_years)
    all_years <- dplyr::filter(all_years, Crop != "biomass")
    all_years <- dplyr::group_by(all_years, Grid_ID, year)
    all_years <- dplyr::summarise(all_years, Crop = Crop[which.max(value)],
                                  max_value=max(value))
    all_years <- dplyr::ungroup(all_years)
    all_years <- dplyr::filter(all_years, max_value > 0)

    ay <- dplyr::transmute(all_years,
                           lon = gridLookup$lon[Grid_ID],
                           lat = gridLookup$lat[Grid_ID],
                           value = match(Crop, names(crop_pal)),
                           year = year,
                           param="SSP 5, RCP 8.5, gfdl,")

    numeric2Cat_list <-list(numeric2Cat_param = list(c("SSP 5, RCP 8.5, gfdl,")),
                            numeric2Cat_breaks = list(c(-Inf,1:12+0.5,Inf)),
                            numeric2Cat_labels = list(names(crop_pal)),
                            numeric2Cat_palette = list(crop_pal),
                            numeric2Cat_legendTextSize = list(c(0.7)))

    rm <- rmap::map(ay, background=T, save=T, show =F, animate=T,
                    numeric2Cat_list = numeric2Cat_list,
                    underLayer= rmap::mapCountries,
                    overLayer = rmap::mapCountries,
                    crop_to_underLayer = T)
  }

  if(grepl("regions|all",temporal_scale,ignore.case=T)){

    # overview of locations (map)
    data <- data.frame(subRegion = c("Indus", "Nile", "Upper_Colorado_River"),
                       value = c(0,0,0))
    overview_map <- rmap::map(data, background=T, crop = F, save=F, legendShow = F,
                              labels = T, labelSize = 5, labelColor = "black",
                              labelRepel = 1,
                              underLayer = rmap::mapCountries,
                              overLayer = rmap::mapCountries)
    ggplot2::ggsave(filename =  paste0(images, "overview_map.png"),
                    plot = overview_map$map_param_KMEANS,
                    width = 13,
                    height = 7)


    # Maps for Indus basin
    
    # Total withdrawals (annual), selected years
    
    # all_data <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
    #                      sectors = "total",
    #                      basins = "Indus",
    #                      years=c(2025,2050,2075,2100), months=0)
    # 
    my_basins <- c("Indus", "Nile", "Upper_Colorado_River")

    my_sectors <- c("total", "dom", "elec", "mfg", "min", "liv", "irr", "Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit", "Rice", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb", "FodderGrass", "biomass")
    my_sectors <- c("total", "dom", "elec", "mfg", "min", "liv", "irr", "Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "Rice", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb", "biomass")
    my_sectors <- c("total", "dom", "elec", "mfg", "min", "liv", "irr", "Corn", "MiscCrop", "OilCrop", "OtherGrain", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb", "biomass")
    my_scenarios <- c("ssp1_rcp26_gfdl", "ssp2_rcp45_hadgem", "ssp3_rcp60_ipsl", "ssp4_rcp45_miroc", "ssp5_rcp85_noresm")
    
    all_data <- readRDS("../data/metarepo_regional_spatial.rds")
    
    target_dim <- NULL
    for (my_basin in my_basins) {
      for (my_sector in my_sectors) {
        max_value <- max(dplyr::filter(all_data, basin == my_basin & sector == my_sector)$value)
        for (my_scenario in my_scenarios) {
          sector_data <- dplyr::filter(all_data, basin == my_basin & sector == my_sector & scenario == my_scenario)
          # Having a column named "year" makes rmap make a bunch of other maps
          sector_data <- dplyr::select(sector_data, lon, lat, category=year, value) 
          sector_map <- rmap::map(sector_data, save=F, show=F,
                                  background=T, zoom=-0.5, legendType="continuous",
                                  title = paste0(my_basin, " basin: ", my_sector, ", ", my_scenario),
                                  legendTitle = "Water Withdrawals (km3)",
                                  underLayer = rmap::mapGCAMReg32,
                                  overLayer = rmap::mapGCAMReg32,
                                  col="category", ncol=4)
          sector_map[[1]] <- sector_map[[1]] + ggplot2::scale_fill_gradientn(
            colors = jgcricol()$pal_hot,
            limits = c(0, max_value*1.01),
            name = "Water\nWithdrawals\n(km3)")
          if (is.null(target_dim)){
            target_dim <- patchwork::get_dim(sector_map[[1]])
          } else {
            sector_map[[1]] <- patchwork::set_dim(sector_map[[1]], target_dim)
          }
          filename <- paste0(images, "regional/spatial_", my_basin, "_w", my_sector, "_", my_scenario, ".png")
          grDevices::png(filename,width=13,height=4,units="in",res=144); print(sector_map[[1]]); grDevices::dev.off()
        }
      }
    }
    
    
    all_data <- readRDS("../data/metarepo_regional_spatial_c.rds")
    for (my_basin in my_basins) {
      for (my_sector in my_sectors) {
        max_value <- max(dplyr::filter(all_data, basin == my_basin & sector == my_sector)$value)
        for (my_scenario in my_scenarios) {
          sector_data <- dplyr::filter(all_data, basin == my_basin & sector == my_sector & scenario == my_scenario)
          # Having a column named "year" makes rmap make a bunch of other maps
          sector_data <- dplyr::select(sector_data, lon, lat, category=year, value) 
          sector_map <- rmap::map(sector_data, save=F, show=F,
                                  background=T, zoom=-0.5, legendType="continuous",
                                  title = paste0(my_basin, " basin: ", my_sector, ", ", my_scenario),
                                  legendTitle = "Water Consumption (km3)",
                                  underLayer = rmap::mapGCAMReg32,
                                  overLayer = rmap::mapGCAMReg32,
                                  col="category", ncol=4)
          sector_map[[1]] <- sector_map[[1]] + ggplot2::scale_fill_gradientn(
            colors = jgcricol()$pal_hot,
            limits = c(0, max_value*1.01),
            name = "Water\nConsumption\n(km3)")
          if (is.null(target_dim)){
            target_dim <- patchwork::get_dim(sector_map[[1]])
          } else {
            sector_map[[1]] <- patchwork::set_dim(sector_map[[1]], target_dim)
          }
          filename <- paste0(images, "regional/spatial_", my_basin, "_c", my_sector, "_", my_scenario, ".png")
          grDevices::png(filename,width=13,height=4,units="in",res=144); print(sector_map[[1]]); grDevices::dev.off()
        }
      }
    }
    
    ### heatmap by sector
    
    # indus_data <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
    #                        sectors = my_sectors,
    #                        basins = "Indus")
    
    my_basins <- c("Indus", "Nile", "Upper_Colorado_River")
    my_sectors <- c("total", "dom", "elec", "mfg", "min", "liv", "irr", "Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "Rice", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb", "biomass")
    my_scenarios <- c("ssp1_rcp26_gfdl", "ssp2_rcp45_hadgem", "ssp3_rcp60_ipsl", "ssp4_rcp45_miroc", "ssp5_rcp85_noresm")
    
    all_data2 <- readRDS("../data/metarepo_regional_temporal.rds")
    all_data2 <- dplyr::ungroup(all_data2)
    all_data2 <- tidyr::complete(all_data2, tidyr::nesting(region, basin), sector, scenario, year, month, fill=list(value=0))
    all_data2 <- dplyr::arrange(all_data2, region, basin)
    all_data2 <- tidyr::fill(all_data2, basin)
    all_data2 <- dplyr::group_by(all_data2, basin, sector, scenario, year, month)
    all_data2 <- dplyr::summarise(all_data2, value=sum(value))
    
    totals <- dplyr::filter(all_data2, sector %in% c("dom", "elec", "mfg", "min", "liv", "irr"))
    totals <- dplyr::group_by(totals, dplyr::across(c(-sector, -value)))
    totals <- dplyr::summarise(totals, value=sum(value))
    totals$sector <- "total"
    
    all_data2 <- dplyr::bind_rows(all_data2, totals)

    
    target_dim <- NULL
    for (my_basin in my_basins) {
      for (my_sector in my_sectors) {
        max_value <- max(dplyr::filter(all_data2, basin == my_basin & sector == my_sector)$value)
        for (my_scenario in my_scenarios) {
          sector_data <- dplyr::filter(all_data2, basin == my_basin & sector == my_sector & scenario == my_scenario)
          sector_data$month <- factor(sector_data$month, labels = month.abb)
          
          heatmap <- ggplot2::ggplot(sector_data, ggplot2::aes(x=year,
                                                               y=month,
                                                               fill= value)) +
            ggplot2::geom_tile() +
            ggplot2::scale_x_continuous(expand=c(0,0)) +
            ggplot2::scale_y_discrete(limits=rev(levels(sector_data$month)), expand=c(0,0)) +
            ggplot2::scale_fill_gradientn(
              colors = jgcricol()$pal_hot,
              limits = c(0, max_value*1.01),
              name = "Water\nWithdrawals\n(km3)") +
            ggplot2::theme(panel.background = ggplot2::element_blank(),
                           plot.background = ggplot2::element_blank())
          if (is.null(target_dim)){
            target_dim <- patchwork::get_dim(heatmap)
          } else {
            heatmap <- patchwork::set_dim(heatmap, target_dim)
          }
          ggplot2::ggsave(filename = paste0(images, "regional/temporal_", my_basin, "_w", my_sector, "_", my_scenario, ".png"),
                          plot = heatmap,
                          width = 13,
                          height = 3)
        }
      }
    }
    all_data3 <- readRDS("../data/metarepo_regional_temporal_c.rds")
    all_data3 <- dplyr::ungroup(all_data3)
    all_data3 <- tidyr::complete(all_data3, tidyr::nesting(region, basin), sector, scenario, year, month, fill=list(value=0))
    all_data3 <- dplyr::arrange(all_data3, region, basin)
    #all_data3 <- tidyr::fill(all_data3, basin)
    all_data3 <- dplyr::group_by(all_data3, basin, sector, scenario, year, month)
    all_data3 <- dplyr::summarise(all_data3, value=sum(value))
    
    totals <- dplyr::filter(all_data3, sector %in% c("dom", "elec", "mfg", "min", "liv", "irr"))
    totals <- dplyr::group_by(totals, dplyr::across(c(-sector, -value)))
    totals <- dplyr::summarise(totals, value=sum(value))
    totals$sector <- "total"
    
    all_data3 <- dplyr::bind_rows(all_data3, totals)
    
    for (my_basin in my_basins) {
      for (my_sector in my_sectors) {
        max_value <- max(dplyr::filter(all_data3, basin == my_basin & sector == my_sector)$value)
        for (my_scenario in my_scenarios) {
          sector_data <- dplyr::filter(all_data3, basin == my_basin & sector == my_sector & scenario == my_scenario)
          sector_data$month <- factor(sector_data$month, labels = month.abb)
          
          heatmap <- ggplot2::ggplot(sector_data, ggplot2::aes(x=year,
                                                               y=month,
                                                               fill= value)) +
            ggplot2::geom_tile() +
            ggplot2::scale_x_continuous(expand=c(0,0)) +
            ggplot2::scale_y_discrete(limits=rev(levels(sector_data$month)), expand=c(0,0)) +
            ggplot2::scale_fill_gradientn(
              colors = jgcricol()$pal_hot,
              limits = c(0, max_value*1.01),
              name = "Water\nConsumption\n(km3)") +
            ggplot2::theme(panel.background = ggplot2::element_blank(),
                           plot.background = ggplot2::element_blank())
          if (is.null(target_dim)){
            target_dim <- patchwork::get_dim(heatmap)
          } else {
            heatmap <- patchwork::set_dim(heatmap, target_dim)
          }
          ggplot2::ggsave(filename = paste0(images, "regional/temporal_", my_basin, "_c", my_sector, "_", my_scenario, ".png"),
                          plot = heatmap,
                          width = 13,
                          height = 3)
        }
      }
    }
    
    #####
    # crop types
    
    crop_pal2 <- c(crop_pal[names(crop_pal) != "biomass"], "none" = "gray80")
    
    crop_data <- readRDS("../data/metarepo_regional_spatial.rds")
    crop_data <- dplyr::filter(crop_data, sector %in% c("Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "Rice", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb")) #, "biomass"))

    crop_data <- dplyr::group_by(crop_data, dplyr::across(c(-sector, -value)))
    crop_data <- dplyr::summarise(crop_data, sector = sector[which.max(value)],
                                  max_value=max(value))
    crop_data <- dplyr::ungroup(crop_data)
    crop_data <- dplyr::mutate(crop_data, sector = dplyr::case_when(max_value <= 0 ~ "none",
                                                                    TRUE ~ sector))
    crop_data <- dplyr::mutate(crop_data, value=match(sector, names(crop_pal2)), param="param")
    crop_data <- dplyr::select(crop_data, lon, lat, basin, scenario, year, value, param)

    numeric2Cat_list <-list(numeric2Cat_param = list(c("param")),
                            numeric2Cat_breaks = list(c(-Inf,1:12+0.5,Inf)),
                            numeric2Cat_labels = list(names(crop_pal2)),
                            numeric2Cat_palette = list(crop_pal2),
                            numeric2Cat_legendTextSize = list(c(0.7)))
    
    target_dim <- NULL
    for (my_basin in my_basins) {
      for (my_scenario in my_scenarios) {
        sector_data <- dplyr::filter(crop_data, basin == my_basin & scenario == my_scenario)
        # Having a column named "year" makes rmap make a bunch of other maps
        sector_data <- dplyr::select(sector_data, lon, lat, category=year, value, param) 
        crop_map <- rmap::map(sector_data, background=T, zoom=-0.5, save=F, show=F,
                              numeric2Cat_list = numeric2Cat_list,
                              legendTitle = "Crop",
                              title = paste0("Maximum Crop by Withdrawals per Grid Cell, ", my_basin, " basin: ", my_scenario),
                              underLayer= rmap::mapGCAMReg32,
                              overLayer = rmap::mapGCAMReg32,
                              col="category", ncol=4)
        if (is.null(target_dim)){
          target_dim <- patchwork::get_dim(crop_map[[1]])
        } else {
          crop_map[[1]] <- patchwork::set_dim(crop_map[[1]], target_dim)
        }
        filename <- paste0(images, "regional/most_crop_", my_basin, "_w_", my_scenario, ".png")
        grDevices::png(filename,width=13,height=4,units="in",res=144); print(crop_map[[1]]); grDevices::dev.off()
      }
    }
    
    crop_data <- readRDS("../data/metarepo_regional_spatial_c.rds")
    crop_data <- dplyr::filter(crop_data, sector %in% c("Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "Rice", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb", "biomass"))
    
    crop_data <- dplyr::group_by(crop_data, dplyr::across(c(-sector, -value)))
    crop_data <- dplyr::summarise(crop_data, sector = sector[which.max(value)],
                                  max_value=max(value))
    crop_data <- dplyr::ungroup(crop_data)
    crop_data <- dplyr::filter(crop_data, max_value > 0)
    crop_data <- dplyr::mutate(crop_data, value=match(sector, names(crop_pal)), param="param")
    crop_data <- dplyr::select(crop_data, lon, lat, basin, scenario, year, value, param)
    
    for (my_basin in my_basins) {
      for (my_scenario in my_scenarios) {
        sector_data <- dplyr::filter(crop_data, basin == my_basin & scenario == my_scenario)
        # Having a column named "year" makes rmap make a bunch of other maps
        sector_data <- dplyr::select(sector_data, lon, lat, category=year, value, param) 
        crop_map <- rmap::map(sector_data, background=T, zoom=-0.5, save=F, show=F,
                              numeric2Cat_list = numeric2Cat_list,
                              legendTitle = "Crop",
                              title = paste0(my_basin, " basin: ", my_scenario),
                              underLayer= rmap::mapGCAMReg32,
                              overLayer = rmap::mapGCAMReg32,
                              col="category", ncol=4)
        if (is.null(target_dim)){
          target_dim <- patchwork::get_dim(crop_map[[1]])
        } else {
          crop_map[[1]] <- patchwork::set_dim(crop_map[[1]], target_dim)
        }
        filename <- paste0(images, "regional/most_crop_", my_basin, "_c_", my_scenario, ".png")
        grDevices::png(filename,width=13,height=4,units="in",res=300); print(crop_map[[1]]); grDevices::dev.off()
      }
    }
    
    # Crop line plots
    crop_data <- readRDS("../data/metarepo_regional_temporal.rds")
    crop_data <- dplyr::filter(crop_data, sector %in% c("Corn", "FiberCrop", "MiscCrop", "OilCrop", "OtherGrain", "Rice", "Root_Tuber", "SugarCrop", "Wheat", "FodderHerb", "biomass"))
    crop_data <- dplyr::filter(crop_data, year %% 5 == 0)
    crop_data <- dplyr::group_by(crop_data, basin, sector, scenario, year)
    crop_data <- dplyr::summarise(crop_data, value = sum(value))
    
    my_basins <- c("Indus", "Nile", "Upper_Colorado_River")
    my_scenarios <- c("ssp1_rcp26_gfdl", "ssp2_rcp45_hadgem", "ssp3_rcp60_ipsl", "ssp4_rcp45_miroc", "ssp5_rcp85_noresm")
    
    for (my_basin in my_basins) {
      max_value <- max(dplyr::filter(crop_data, basin == my_basin)$value)
      for (my_scenario in my_scenarios) {
        sector_data <- dplyr::filter(crop_data, basin == my_basin & scenario == my_scenario)
        crop_plot <- ggplot2::ggplot(data = sector_data,
                                     ggplot2::aes(x = year,
                                                  y = value)) +
          #ggplot2::geom_bar(position="stack", stat="identity") +
          #ggplot2::scale_fill_manual(values=crop_pal) +
          ggplot2::geom_line(size = 2, ggplot2::aes(color=sector)) +
          ggplot2::scale_color_manual(values=crop_pal) +
          ggplot2::ylim(0, max_value) +
          ggplot2::ggtitle(paste0("Annual Crop Water Withdrawals: ", my_basin, ", ", my_scenario)) +
          ggplot2::xlab("Year") +
          ggplot2::ylab(bquote(Water ~ Withdrawals ~ (km^3 ~ per ~ year))) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))
        ggplot2::ggsave(filename = paste0(images, "regional/crops_annual_", my_basin, "_w_", my_scenario, ".png"),
                        plot = crop_plot,
                        width = 13,
                        height = 4)
      }
    }
    
   
  }

  # Initialize
  print("generate_figures completed succesfuly!")
} # Close generate_figures function


## helpers and other functions below

prepare_annual <- function(folder=NULL, outfile="annual_data.rds") {
  annual_data <- all_combos()
  files <- dplyr::transmute(annual_data, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/wd", names(Sector), "_km3peryr.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 50 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                  "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
    }))
  print("Data loaded")

  annual_data <- tidyr::crossing(annual_data, Year=seq(2010,2100,by=5)) # each year for each combo
  annual_data <- dplyr::bind_cols(annual_data, values) # SSP, RCP, GCM, Sector, Year; Value
  saveRDS(annual_data, file = outfile)
  print(paste("Annual totals saved to", outfile))
  return (annual_data)
}

prepare_monthly <- function(folder=NULL, outfile="monthly_data.rds") {
  monthly_data <- all_combos()
  monthly_data <- dplyr::filter(monthly_data, (names(Sector) != "total") & (names(Sector) != "nonag"))
  files <- dplyr::transmute(monthly_data, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/twd", names(Sector), "_km3permonth.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 50 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                   "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of month columns
  }))
  print("Data loaded")

  monthly_data <- tidyr::crossing(monthly_data, Year=2010:2100, Month=1:12) # each year and month for each combo
  monthly_data <- dplyr::bind_cols(monthly_data, values) # SSP, RCP, GCM, Sector, Year; Value
  saveRDS(monthly_data, file = outfile)
  print(paste("Monthly totals saved to", outfile))
  return (monthly_data)
}

prepare_annual_crops <- function(folder=NULL, outfile="annual_crops.rds") {
  crop_names = c("biomass",
                 "Corn",
                 "FiberCrop",
                 "FodderGrass",
                 "FodderHerb",
                 "MiscCrop",
                 "OilCrop",
                 "OtherGrain",
                 "PalmFruit",
                 "Rice",
                 "Root_Tuber",
                 "SugarCrop",
                 "Wheat")
  annual_crops <- dplyr::distinct(dplyr::select(all_combos(), -Sector)) # rewrite all_combos to handle this
  annual_crops <- tidyr::crossing(annual_crops, Crop = crop_names)

  # uncomment for local testing
  annual_crops <- dplyr::filter(annual_crops, SSP == "SSP 1" & RCP == "RCP 2.6" & GCM == "gfdl")

  files <- dplyr::transmute(annual_crops, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/crops_wdirr_", Crop, "_km3peryr.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 13 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                   "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
  }))
  print("Data loaded")

  annual_crops <- tidyr::crossing(annual_crops, Year=seq(2010,2100,by=5)) # each year for each combo
  annual_crops <- dplyr::bind_cols(annual_crops, values) # SSP, RCP, GCM, Crop, Year; Value
  saveRDS(annual_crops, file = outfile)
  print(paste("Annual crop totals saved to", outfile))
  return (annual_crops)
}

prepare_monthly_crops <- function(folder=NULL, outfile="monthly_crops.rds") {
  crop_names = c("biomass",
                 "Corn",
                 "FiberCrop",
                 "FodderGrass",
                 "FodderHerb",
                 "MiscCrop",
                 "OilCrop",
                 "OtherGrain",
                 "PalmFruit",
                 "Rice",
                 "Root_Tuber",
                 "SugarCrop",
                 "Wheat")
  monthly_crops <- dplyr::distinct(dplyr::select(all_combos(), -Sector)) # rewrite all_combos to handle this
  monthly_crops <- tidyr::crossing(monthly_crops, Crop = crop_names)

  # uncomment for local testing
  monthly_crops <- dplyr::filter(monthly_crops, SSP == "SSP 1" & RCP == "RCP 2.6" & GCM == "gfdl")

  files <- dplyr::transmute(monthly_crops, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/crops_twdirr_", Crop, "_km3permonth.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 13 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                   "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
  }))
  print("Data loaded")

  monthly_crops <- tidyr::crossing(monthly_crops, Year=2010:2100, Month=1:12) # each year and month for each combo
  monthly_crops <- dplyr::bind_cols(monthly_crops, values) # SSP, RCP, GCM, Crop, Year: Value
  saveRDS(monthly_crops, file = outfile)
  print(paste("Monthly crop totals saved to", outfile))
  return (monthly_crops)
}

prepare_GCAM <- function(GCAM_withdrawals_csv=NULL, outfile="region_data.rds") {
  subReg_to_map <- tibble::deframe(rmap::mappings("mappingGCAMBasins"))
  subReg_to_map <- setNames(names(subReg_to_map), subReg_to_map)

  scenarios <- dplyr::transmute(all_combos(), paste0(
    names(SSP), "_",names(RCP), "_", GCM))
  scenarios <- dplyr:::distinct(scenarios)[[1]]

  print("Loading GCAM data")
  region_data <- tibble::as_tibble(data.table::fread(GCAM_withdrawals_csv, select=c(2,3,5,6,7)))

  print("Filtering to dataset scenarios and years")
  region_data <- dplyr::filter(region_data, 2010<=year & year<=2100 & is.element(scenario, scenarios))

  # further filter (only need this scenario-year right now)
  #region_data <- dplyr::filter(region_data, scenario=="ssp1_rcp26_gfdl" & year == 2010)

  region_data <- dplyr::mutate(region_data, .keep="unused", .after="scenario",
                               subRegion_GCAMBasin = dplyr::case_when(
                                 grepl("water_td_irr_", input) ~ subReg_to_map[gsub("water_td_irr_|_W", "", input)],
                                 TRUE ~ NA_character_),
                               subRegion_GCAMReg32 = sub("-", "_", region), # need "EU_12", not "EU-12"
                               class = dplyr::case_when(
                                 grepl("water_td_dom_", input) ~ "Domestic",
                                 grepl("water_td_elec_", input) ~ "Electricity",
                                 grepl("water_td_irr_", input) ~ "Irrigation",
                                 grepl("water_td_an_", input) ~ "Livestock",
                                 grepl("water_td_ind_", input) ~ "Manufacturing",
                                 grepl("water_td_pri_", input) ~ "Mining"))
  region_data <- dplyr::group_by(region_data, scenario, year, subRegion_GCAMBasin, subRegion_GCAMReg32, class)
  region_data <- dplyr::summarise(region_data, value=sum(value))

  saveRDS(region_data, file = outfile)
  print(paste("GCAM data saved to", outfile))
  return (region_data)
}

prepare_GCAM_crops <- function(GCAM_withdrawals_csv=NULL, outfile="region_crops.rds") {
  subReg_to_map <- tibble::deframe(rmap::mappings("mappingGCAMBasins"))
  subReg_to_map <- setNames(names(subReg_to_map), subReg_to_map)

  scenarios <- dplyr::transmute(all_combos(), paste0(
    names(SSP), "_",names(RCP), "_", GCM))
  scenarios <- dplyr:::distinct(scenarios)[[1]]

  print("Loading GCAM data")
  region_crops <- tibble::as_tibble(data.table::fread(GCAM_withdrawals_csv, select=2:7))

  print("Filtering to dataset scenarios and years")
  region_crops <- dplyr::filter(region_crops, 2010<=year & year<=2100 & is.element(scenario, scenarios))

  # filter to irrigation only
  region_crops <- dplyr::filter(region_crops, grepl("water_td_irr_", input))

  region_crops <- dplyr::mutate(region_crops, .keep="unused", .after="scenario",
                               subRegion_GCAMBasin = subReg_to_map[gsub("water_td_irr_|_W", "", input)],
                               subRegion_GCAMReg32 = sub("-", "_", region), # need "EU_12", not "EU-12"
                               Crop = dplyr::case_when(
                                 grepl("biomass", subsector) ~ "biomass",
                                 TRUE ~ sub("_[^_]+$", "", subsector)) # only part before the final underscore
  )

  saveRDS(region_crops, file = outfile)
  print(paste("GCAM data saved to", outfile))
  return (region_crops)
}

process_GCAM_csv <- function(GCAM_csv=NULL, outfile=NULL) {
  subReg_to_map <- tibble::deframe(rmap::mapping_gcambasins)
  subReg_to_map <- setNames(names(subReg_to_map), subReg_to_map)
  
  scenarios <- dplyr::transmute(all_combos(), paste0(
    names(SSP), "_",names(RCP), "_", GCM))
  scenarios <- dplyr:::distinct(scenarios)[[1]]
  
  print("Loading GCAM data")
  GCAM_data <- data.table::fread(GCAM_csv, select=2:7)
  
  print("Filtering to dataset scenarios and years")
  GCAM_data <- dplyr::filter(GCAM_data, 2010<=year & year<=2100 & scenario %in% scenarios)
  
  print("Converting sector names")
  GCAM_data <- dplyr::mutate(GCAM_data, .keep="unused", .after="scenario",
                             region = sub("-", "_", region), # need "EU_12", not "EU-12"
                             basin = dplyr::case_when(
                               grepl("water_td_irr_", input) ~ subReg_to_map[gsub("water_td_irr_|_[WC]", "", input)],
                               TRUE ~ NA_character_),
                             sector = dplyr::case_when(
                               grepl("water_td_dom_", input) ~ "dom",
                               grepl("water_td_elec_", input) ~ "elec",
                               grepl("water_td_an_", input) ~ "liv",
                               grepl("water_td_ind_", input) ~ "mfg",
                               grepl("water_td_pri_", input) ~ "min",
                               grepl("water_td_irr_", input) & grepl("^biomass", subsector) ~ "biomass",
                               TRUE ~ sub("_[^_]+$", "", subsector))) # irr but not biomass, extract crop name
  print("Aggregating subsectors")
  GCAM_data <- dplyr::group_by(GCAM_data, scenario, year, region, basin, sector)
  GCAM_data <- dplyr::summarise(GCAM_data, value=sum(value))
  GCAM_data <- dplyr::ungroup(GCAM_data)
  
  print("Aggrregating irrigation")
  irr_data <- dplyr::filter(GCAM_data, !sector %in% c("dom", "elec", "liv", "mfg", "min")) # is a crop
  irr_data$sector <- "irr"
  irr_data <- dplyr::group_by(irr_data, scenario, year, region, basin, sector)
  irr_data <- dplyr::summarise(irr_data, value=sum(value))
  irr_data <- dplyr::ungroup(irr_data)
  GCAM_data <- dplyr::bind_rows(GCAM_data, irr_data)
  
  saveRDS(GCAM_data, file = outfile)
  print(paste("GCAM data saved to", outfile))
  #return (GCAM_data)
}

build_gridLookup <- function() {
  basinIDs <- data.table::fread("data/basin.csv", col.names="basinID")
  regionIDs <- data.table::fread("data/region32_grids.csv", col.names="regionID")

  # some kind of off by 1 error?
  countryIDs <- data.table::fread("data/country.csv", col.names="countryID") -1

  coordinates <- data.table::fread("data/coordinates.csv",
                                   col.names=c("gridID",
                                               "lon",
                                               "lat",
                                               "ilon",
                                               "ilat"))


  # correct Arkansas_White_Red_Basin to Arkansas_White_Red, etc,
  basinID_to_basinName <- gsub("_Basin$", "", tibble::deframe(data.table::fread(
    "data/gcam_basin_lookup.csv", select=c(1,2))[order(basin_id)]))
  basinID_to_basinName["102"] <- "Hamun_i_Mashkel" # from "HamuniMashkel"
  basinID_to_basinName["109"] <- "Hong_Red_River" # from "Hong_(Red_River)"

  # correct EU-12 to EU_12,
  regionID_to_regionName <- gsub("-", "_", tibble::deframe(data.table::fread(
    "data/RgnNames.csv", select=c(2,1))[order(region_id)]))

  countryID_to_countryName <- tibble::deframe(data.table::fread(
    "data/country-names.csv"))


  gridLookup <- dplyr::bind_cols(coordinates, basinIDs, regionIDs, countryIDs)
  gridLookup <- dplyr::mutate(gridLookup,
                              basinName = basinID_to_basinName[as.character(basinID)],
                              regionName = regionID_to_regionName[as.character(regionID)],
                              countryName = countryID_to_countryName[as.character(countryID)])
  gridLookup <- dplyr::relocate(gridLookup,  basinName, .after=basinID)
  gridLookup <- dplyr::relocate(gridLookup,  regionName, .after=regionID)
  gridLookup <- dplyr::relocate(gridLookup,  countryName, .after=countryID)

  saveRDS(gridLookup, file = "rmap_tethys_grid_basin_region_country.rds")
  return(gridLookup)
}

get_data <- function(folder=NULL, scenarios=NULL, sectors=NULL,
                     years=NULL, months=NULL, consumption = F,
                     regions=NULL, basins=NULL, countries=NULL,
                     sum_spatial = F, sum_temporal = F, filter_zero = F) {
  lookup <- rmap::mapping_tethys_grid_basin_region_country
  crop_names = c("biomass", "Corn", "FiberCrop", "FodderGrass", "FodderHerb",
                 "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit", "Rice",
                 "Root_Tuber", "SugarCrop", "Wheat")
  scenario_list <- rep(scenarios, each=length(sectors))
  sector_list <- rep(sectors, length(scenarios))
  if (is.null(years)) {years = 2010:2100}
  if (is.null(months)) {months = 1:12}
  if (months[1] == 0) {
    years <- years[years %% 5 == 0]
    cols <- 6 + (years - 2010)/5
  } else {
    cols <- 5+(rep(years, each=length(months)) - 2010)*12 + rep(months, length(years))
  }
  filename_after = c("_km3permonth.csv", "_km3peryr.csv")[1 + (months[1] == 0)]
  scenario_after = c("_withdrawals", "_consumption")[1+consumption]
  #scenario_after = "" # my local directories are named differently

  all_data <- data.table::rbindlist(lapply(1:length(scenario_list), function(i){
    case <- 1 + consumption*4 + (months[1] == 0)*2 + sector_list[i] %in% crop_names # abusing T=1, F=0
    filename_before = c("/twd", "/crops_twdirr_", "/wd", "/crops_wdirr_",
                        "/tcd", "/crops_tcdirr_", "/cd", "/crops_cdirr_")[case]
    filename <- paste0(folder, "/", scenario_list[i], scenario_after, filename_before, sector_list[i], filename_after)
    print(filename)
    file_data <- data.table::fread(filename, select=c(1, cols))

    if (!is.null(regions)) {
      file_data <- dplyr::filter(file_data, lookup$regionName[Grid_ID] %in% regions)
    }
    if (!is.null(basins)) {
      file_data <- dplyr::filter(file_data, lookup$basinName[Grid_ID] %in% basins)
    }
    if (!is.null(countries)) {
      file_data <- dplyr::filter(file_data, lookup$countryName[Grid_ID] %in% countries)
    }

    file_data <- dplyr::mutate(file_data, .after="Grid_ID",
                               region = lookup$regionName[Grid_ID],
                               basin = lookup$basinName[Grid_ID])
    if (sum_spatial) {
      file_data[,Grid_ID:=NULL]
      file_data <- file_data[, lapply(.SD, sum), by=list(region, basin)]
      file_data <- data.table::melt(file_data, id.vars=1:2)
    } else {
      file_data <- data.table::melt(file_data, id.vars=1:3)
      file_data <- dplyr::mutate(file_data, .before="region",
                                 lon = lookup$lon[Grid_ID],
                                 lat = lookup$lat[Grid_ID])
    }
    if (filter_zero) {
      file_data <- dplyr::filter(file_data, value != 0)
    }
    file_data <- dplyr::mutate(file_data, .before="region",
                               scenario = scenario_list[i],
                               sector = sector_list[i],
                               year  = as.integer(substr(variable, 1, 4)),
                               month = as.integer(substr(variable, 5, 6)))
    file_data <- dplyr::select(file_data, -variable)
    if (sum_temporal) {
      file_data[,month:=NULL]
      file_data <- file_data[, lapply(.SD, sum), by=setdiff(names(file_data), "value")]
    }
    return (file_data)}))
  return (all_data)
}


# all valid ssp-rcp-gcm-sector (including total, nonag sectors)
all_combos <- function() {
  NULL -> SSP -> RCP -> GCM -> Sector
  combos <- tidyr::crossing(SSP = c(ssp1 = "SSP 1",
                                    ssp2 = "SSP 2",
                                    ssp3 = "SSP 3",
                                    ssp4 = "SSP 4",
                                    ssp5 = "SSP 5"),
                            RCP = c(rcp26 = "RCP 2.6",
                                    rcp45 = "RCP 4.5",
                                    rcp60 = "RCP 6.0",
                                    rcp85 = "RCP 8.5"),
                            GCM = c("gfdl",
                                    "hadgem",
                                    "ipsl",
                                    "miroc",
                                    "noresm"),
                            Sector = c(dom = "Domestic",
                                       elec= "Electricity",
                                       irr = "Irrigation",
                                       liv = "Livestock",
                                       mfg = "Manufacturing",
                                       min = "Mining",
                                       nonag = "Non-Agriculture",
                                       total = "Total")
  )
  combos <- dplyr::filter(combos, !(SSP=="SSP 3" & RCP=="RCP 2.6") & # if rcp26, exclude ssp3
                            !(SSP!="SSP 5" & RCP=="RCP 8.5") # if rcp85, must use ssp5
  )
  return(combos)
}
