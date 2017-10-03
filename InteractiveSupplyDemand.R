
#### Loads Function 
library(manipulate)
supplyDemand <- function(sInter, dInter){

  demand <- function(q) (q - dInter)^2 
  supply <- function(q) q^2 + 2*q + sInter

  x <- 1:10 
  q <- uniroot(function(x) demand(x) - supply(x), range(x))$root
  # Equilibrium price
  p <- supply(q)
  # We can now annotate the chart with the equilibrium point.
  # print(p)
  
  z <- seq(0, q, 0.01) # For Surplus zones
  domain <-  c(0, q)
  
  # print(supply)
  # print(demand)
  print(domain)
  
  # ### Surplus Values
  # # Note: Getting error like below with certain values; uncomment to see print.noquote(
  # #   Error in uniroot(function(x) demand(x) - supply(x), domain) : 
  # #     f() values at end points not of opposite sign
  # surpluses <- function(demand, supply, domain) {
  #   q <- uniroot(function(x) demand(x) - supply(x), domain)$root
  #   print(q)
  #   p <- supply(q)
  #   consumer_surplus <- integrate(demand, 0, q)$value - p*q
  #   producer_surplus <- p*q - integrate(supply, 0, q)$value
  #   return(list("consumer" = consumer_surplus,
  #        "producer" = producer_surplus))
  # }
  # surplusVals <- surpluses(demand, supply, domain)
  # print(paste0("Consumer Surplus: ", round(surplusVals$consumer, 2)))
  # print(paste0("Producer Surplus: ", round(surplusVals$producer, 2)))
  
  ### Chart Supply & Demand 
  chart <- ggplot() +
    stat_function(aes(x, color = "Demand"), fun = demand) +
    stat_function(aes(x, color = "Supply"), fun = supply) +
    annotate("point", x = q, y = p, color = "grey30") +
    annotate("segment", x = q, xend = q, y = 0, yend = p,
             linetype = "dashed", color = "grey30") +
    # Note: I addded the below lineshow (x, y) intercept on plot
    annotate("text", x = q + .5, y = p, 
             label = paste0("( ", signif(q, 2), ", ", signif(p, 2),")")) + 
    annotate("segment", x = 0, xend = q, y = p, yend = p,
             linetype = "dashed", color = "grey30") +
    geom_ribbon(aes(x = z, ymin = supply(z), ymax = p,
                    fill = "Producer surplus"), alpha = 0.25) +
    geom_ribbon(aes(x = z, ymin = p, ymax = demand(z),
                    fill = "Consumer surplus"), alpha = 0.25) +
    # Drop junk
    scale_x_continuous(expand = c(0, 0), 
                       breaks = q, labels = "q*") +
    scale_y_continuous(expand = c(0, 0), 
                       breaks = p, labels = "p*") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          legend.position = c(1, 1), 
          legend.justification = c(1, 1),
          legend.spacing = unit(0, "cm"), 
          legend.margin = margin(0, 0, 0, 0, "cm")) +
    labs(x = "Quantity", y = "Price", 
         color = NULL, fill = NULL) +
    coord_cartesian(ylim = c(0, 100), xlim = c(0, 10))

  print(chart)
  
}

## Dynamic Ggplot Output:
manipulate(
  
  supplyDemand(sInter, dInter),
  # Note: "=" Is needed below
  sInter = slider(min = 8, max = 20, step = 1, initial = 8),
  dInter = slider(8, 20, step = 1, initial = 10)
  
)
  
