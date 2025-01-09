# Load packages
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Extract coefficients from the appropriate models
a <- coef(H3_mediate_results$model.m)["TDDS_p_z"]
b <- coef(H3_mediate_results$model.y)["SOI_a_r_z"]
c_prime <- coef(H3_mediate_results$model.y)["TDDS_p_z"]
c_total <- summary(H3_mediate_results$tau.coef)

# Create the diagram and save it as an SVG object
diagram <- grViz(paste0("
  digraph mediation {
    graph [
      layout = dot,
      rankdir = LR,
      labelloc = b,
      labeljust = c,
      fontname = 'Arial',
      fontsize = 12,
      margin = 0.5,
      nodesep = 0.4,
      ranksep = 0,
      bgcolor = 'transparent'  // Set background to transparent
    ]

    label = <
      <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
        <TR>
          <TD>
            <FONT POINT-SIZE='18'><I>Sexual Strategies Mediation Model</I></FONT>
          </TD>
        </TR>
      </TABLE>
    >;

    node [
      shape = ellipse,
      fixedsize = false,
      width = 2,
      height = .8,
      fontsize = 12,
      fontname = 'Arial',
      style = filled,
      fillcolor = '#0055A4:#CAFOF8',
      fontcolor = white
    ]

    A [label = 'Pathogen Disgust Sensitivity']
    B [label = 'Restricted Sociosexual Attitudes']
    C [label = 'Religiosity']

    edge [
      color = black,
      fontcolor = black,
      penwidth = 1.5,
      fontsize = 14,
      fontname = 'Arial',
      arrowsize = 1.2
    ]

    A -> B [label = 'a = ", round(a, 2), "*']
    B -> C [label = 'b = ", round(b, 2), "*']
    A -> C [label = 'c` = ", round(c_prime, 2), "*']

    edge [
      style = dashed
    ]
    A -> C [label = 'c = ", round(c_total, 2), "*']
  }
"))

# Export the diagram as an SVG
diagram_svg <- export_svg(diagram)

# Convert the SVG to PNG (transparent background)
rsvg_png(charToRaw(diagram_svg), file = "images/sexual_strategies_path_model.png",
         height = 1000, width = 2000)



