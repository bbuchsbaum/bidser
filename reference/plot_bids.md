# Plot a comprehensive visual overview of a BIDS project

This function creates a multi-panel visualization of a BIDS project
structure, showing file distributions, completeness, and data
characteristics.

## Usage

``` r
plot_bids(
  x,
  interactive = TRUE,
  color_scheme = "viridis",
  include_derivatives = TRUE,
  file_size_scale = "log",
  highlight_missing = TRUE,
  visualization_mode = "standard",
  debug = FALSE
)
```

## Arguments

- x:

  A `bids_project` object

- interactive:

  Logical. Whether to create an interactive plot (default TRUE)

- color_scheme:

  Character. Name of the color palette to use (default "viridis")

- include_derivatives:

  Logical. Whether to include derivatives data in the visualization
  (default TRUE)

- file_size_scale:

  Character. Whether to scale file sizes ("log", "sqrt", or "linear",
  default "log")

- highlight_missing:

  Logical. Whether to highlight missing data points (default TRUE)

- visualization_mode:

  Character. The mode of visualization to use ("standard", "heatmap", or
  "complete")

- debug:

  Logical. Whether to print debugging information (default FALSE)

## Value

A plot object (ggplot2, plotly, or other depending on settings)

## Examples

``` r
# \donttest{
# Create a basic BIDS project and plot it
tryCatch({
  ds001_path <- get_example_bids_dataset("ds001")
  proj <- bids_project(ds001_path)
  plot_bids(proj)
  
  # Create an interactive plot
  plot_bids(proj, interactive=TRUE)
  
  # Clean up
  # Example datasets are cached; leave the cache in place.
}, error = function(e) {
  message("Example requires internet connection: ", e$message)
})

{"x":{"data":[{"visible":false,"showlegend":false,"xaxis":null,"yaxis":null,"hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.305936073059364,"r":7.3059360730593621,"b":10.958904109589042,"l":10.958904109589042},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.050000000000000003,1.05],"tickmode":"array","ticktext":[],"tickvals":[],"categoryorder":"array","categoryarray":[],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":9.2984640929846396},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":10.62681610626816}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.050000000000000003,1.05],"tickmode":"array","ticktext":[],"tickvals":[],"categoryorder":"array","categoryarray":[],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.6529680365296811,"tickwidth":0.66417600664176002,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":9.2984640929846396},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":10.62681610626816}},"hoverformat":".2f"},"shapes":[],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"rgba(0,0,0,1)","family":"","size":9.2984640929846396}},"hovermode":"closest","barmode":"relative","updatemenus":[{"buttons":[{"method":"relayout","args":["showlegend",true],"label":"Show Legend"},{"method":"relayout","args":["showlegend",false],"label":"Hide Legend"}],"type":"buttons","direction":"right","xanchor":"center","yanchor":"top","x":0.5,"y":1.2}]},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"21181451b910":{"type":"scatter"}},"cur_data":"21181451b910","visdat":{"21181451b910":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# }
```
