# Models used for Red List assessments underestimate climate-related extinction risk of range-shifting species


#-------------------------------------------------------------------------------

# -------------------------------------------------------------------- #
#                         00. Text labels for plots                    #
# -------------------------------------------------------------------- #


#-------------------------------------------------------------------------------
# all text labels required for the figures in the main text and appendix

# Load packages
library(ggplot2)

# empty plot for the upper left corner of the plots
t0 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "") +
  theme_void()

# text plots for niche widths
t_nn <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Narrow \nniche") +
  theme_void()

t_wn <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Wide \nniche") +
  theme_void()

#text plots for niche optima
t_c_small <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6.5,
           label = "Marginal niche position") +
  theme_void()

t_w_small <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6.5,
           label = "Central niche position") +
  theme_void()

t_c_medium <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Marginal niche position") +
  theme_void()

t_w_medium <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Central niche position") +
  theme_void()

t_c_large <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 10,
           label = "Marginal niche position") +
  theme_void()

t_w_large <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 10,
           label = "Central niche position") +
  theme_void()

# text plots for years
t_year0 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Year 0") +
  theme_void()

t_year10 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Year 10") +
  theme_void()

t_year20 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Year 20") +
  theme_void()

t_year30 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Year 30") +
  theme_void()

#label for scenarios
l1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nNarrow niche\nSlow gr. rate \nShort disp.") +
  theme_void()

l2 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nNarrow niche\nSlow gr. rate \nLong disp.") +
  theme_void()

l3 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nNarrow niche\nFast gr. rate \nShort disp.") +
  theme_void()

l4 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nNarrow niche\nFast gr. rate \nLong disp.") +
  theme_void()

l5 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nWide niche\nSlow gr. rate \nShort disp.") +
  theme_void()

l6 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nWide niche\nSlow gr. rate \nLong disp.") +
  theme_void()

l7 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nWide niche\nFast gr. rate \nShort disp.") +
  theme_void()

l8 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Marginal niche position\nWide niche\nFast gr. rate \nLong disp.") +
  theme_void()

l9 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nNarrow niche\nSlow gr. rate \nShort disp.") +
  theme_void()

l10 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nNarrow niche\nSlow gr. rate \nLong disp.") +
  theme_void()

l11 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nNarrow niche\nFast gr. rate \nShort disp.") +
  theme_void()

l12 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nNarrow niche\nFast gr. rate \nLong disp.") +
  theme_void()

l13 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nWide niche\nSlow gr. rate \nShort disp.") +
  theme_void()

l14 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nWide niche\nSlow gr. rate \nLong disp.") +
  theme_void()

l15 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nWide niche\nFast gr. rate \nShort disp.") +
  theme_void()

l16 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 4,
           label = "Central niche position\nWide niche\nFast gr. rate \nLong disp.") +
  theme_void()

# dispersal growth rate combinations

t_ss <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Slow growth \n rate \n Short dispersal") +
  theme_void()

t_sl <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Slow growth \n rate \n Long dispersal") +
  theme_void()

t_fs <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Fast growth \n rate \n Short dispersal") +
  theme_void()

t_fl <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Fast growth \n rate \n Long dispersal") +
  theme_void()

# niche combinations

t_cn <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Marginal niche position & Narrow niche") +
  theme_void()

t_cw <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Marginal niche position & Wide niche") +
  theme_void()

t_wna <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Central niche position & Narrow niche") +
  theme_void()

t_ww <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 6,
           label = "Central niche position & Wide niche") +
  theme_void()

# threatened values
t_VU <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Vulnerable") +
  theme_void()

t_EN <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Endangered") +
  theme_void()

t_CR <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "Critically endangered") +
  theme_void()

# text plots for landscapes
t_l1 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Landscape 1") +
  theme_void()

t_l2 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Landscape 2") +
  theme_void()

t_l3 <- ggplot() +
  annotate("text",
           x = 1,
           y = 1,
           size = 7,
           label = "Landscape 3") +
  theme_void()
