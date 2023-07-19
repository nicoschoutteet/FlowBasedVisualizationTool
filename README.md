# Flow-Based Visualization Tool
This repository contains the code for a Shiny app, which can be used to visualize the Core DA FBMC (presolved) capacity domain, active constraints, market clearing points and long-term allocated domains.
More information can be found via LinkedIn.

The Shiny application is published and can be freely accessed here: https://nicoschoutteet.shinyapps.io/FlowBasedVisualizationTool/

The tool uses a number of functions from the `JAOPuTo` package, which is available through the following repository:
https://github.com/nicoschoutteet/JAOPuTo, and can be downloaded and installed:

```{r}
devtools::install_github("nicoschoutteet/JAOPuTo")
libraries(JAOPuTo)
```
