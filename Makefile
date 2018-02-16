# run plots in R:
TrumpVote.pdf x2.pdf: 
	Rscript plot.R

# run R scripts:
id1.rda rc.rda: read.R
	Rscript read.R

xbar.rda:
	Rscript -e 'load("id1.rda")'
	Rscript -e 's <- summary("id1")'
	Rscript -e 'xbar <- cbind(s$xm,s$xHDR[,,1])'
	Rscript -e 'save("xbar", file="xbar.rda")'

# run R scripts for D3:
timestamp.csv nvotes.csv repLoess.csv demLoess.csv plotData.csv: id1.rda xbar.rda rc.rda dat16.rda 
	Rscript d3Write.R
	
# run D3 graphs:
#scatter.html long.html: style.css timeStamp.js

# Push Git
git-%: 
	git commit -m "$(@:git-%=%)"
	git push origin master