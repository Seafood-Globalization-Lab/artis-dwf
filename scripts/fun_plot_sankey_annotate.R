# custom function to annotate looping through sankey plots
library(exploreARTIS)
library(knitr)


plot_sankey_annote <- function(df, common_name_value, figure_i, country_iso3c) {
  # Read the annotation script
  annotation_chunk_label <- paste0("fig-sankey-consump-", figure_i) 
  knitr::read_chunk(file.path(anndir, glue::glue("{country_iso3c}.R")))
  
  plot <- plot_sankey(df %>% 
                        filter(common_name_value == common_name),
                      cols = c("eez_name", "producer_iso3c_name", "consumer_iso3c_name"),
                      plot.title = common_name_value) + 
    theme(plot.title = element_text(size=14)) +
    scale_x_discrete(labels = c("EEZ", "Producer", "Consumer")) +
    theme(axis.text.x = element_text(size = 8))
  
  list(plot = plot, annotation_chunk_label = annotation_chunk_label)
}
