This folder contains R scripts to generate figures for
Visscher & Yengo (2023), "The effect of the scale of grant scoring on ranking accuracy"

-- CorrEquiSpaced.R: an R script defining the function "CorrEquiSpaced(K)", which input is the number of score categories and output is a list containing (1) the expected correlation between the continuous and discretised scoring scales, (2) the expected proprtions of scores within each category, (3) the thresholds used to discretize the continuous scale, and (4) the spacing between thresholds.

-- Figure1.R : Uses CorrEquiSpaced.R to reproduce Figure 1.
Figure 1 legend: Representation of the multiple threshold model for k = 5 categories.
The x-axis shows the unobserved continuous scale in standard deviation units and the y-axis the density. The position of each of the 4 thresholds is shown as a vertical red line.

-- Figure2.R : Uses CorrEquiSpaced.R to reproduce Figure 2.
Figure 2 legend:. Correlation between the observed categorical score and the underlying continuous score. The x-axis is the number of discrete categorical scores (k) and the y-axis shows the correlation between the observed categorical score (Y) and the underlying continuous score (u). The red horizontal line denotes a correlation of 0.95.

-- Figure3.R : Uses CorrEquiSpaced.R to reproduce Figure 3.
Figure 3 legend: Loss of information relative to scoring on a continuous scale. Each panel shows the loss of information (Equation [3]) when scoring a finite number of categories relative to the continuous score, as a function of the number of assessors (panels a to d) and the proportion of variation in scores due to the quality of the grant (x-axis).

-- Figure4.R : Uses CorrEquiSpaced.R to reproduce Figure 4.
Figure 4 legend: Loss of information induced by scoring too few grants in extreme categories. The x-axis is the number of discrete categorical scores (k) and the y-axis shows the correlation (Rk) between the observed categorical score (Y) and the underlying continuous score (u). The correlation Rk is calculated under three scenarios defined by the variance (s2s) of the distribution of underlying scores. The grey horizontal line denotes a correlation of 0.95 or 0.99.


