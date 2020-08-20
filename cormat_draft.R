library(ggplot2)
library(patchwork)
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

behavioral_df_all <- data.table::data.table(data.frame(readr::read_csv('data/behavior_data.csv')))

acormat <- cor(behavioral_df_all[,-1], use = 'pairwise.complete.obs', method = 'spearman')

acormat_reo <- reorder_cormat(acormat)
# acormat_reo[lower.tri(acormat_reo)] <- NA
diag(acormat_reo) <- NA
acormat_reo_dt <- data.table::melt(data.table::data.table(acormat_reo)[, row := 1:.N], 
                                   id.vars = 'row', 
                                   measure.vars = 1:dim(acormat_reo)[1],
                                   variable.name = 'col', 
                                   na.rm = T)

ordered_p <- ggplot(data = acormat_reo_dt, aes(row, col, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, 
                       limit =  c(round(min(acormat_reo_dt$value),2), round(max(acormat_reo_dt$value),2)), 
                       breaks = c(round(min(acormat_reo_dt$value),2), 0, round(max(acormat_reo_dt$value),2)), 
                       space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  coord_fixed() + 
  labs(title = 'Ordered via hclust')

print(ordered_p)
