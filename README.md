### SIGHTS R-package 
SIGHTS is a suite of normalization methods, statistical tests, and diagnostic graphical tools for high throughput screening (HTS) assays.
HTS assays use microtitre plates to screen large libraries of compounds for their biological, chemical, or biochemical activity.

* [Abstract](#abstract)
* [Links](#links)
* [Installation](#install)
* [Citation](#cite)
* [License](#license)

#### <a name="abstract"></a>Abstract

Identifying rare biological events in high-throughput screens requires using the best available normalization and statistical inference procedures. It is not always clear, however, which algorithms are best suited for a particular screen. The Statistics and dIagnostics Graphs for High Throughput Screening (**SIGHTS**) *R* package is designed for statistical analysis and visualization of HTS assays. It provides graphical diagnostic tools to guide researchers in choosing the most appropriate normalization algorithm and statistical test for identifying active constructs.

#### <a name="links"></a>Links

- [Bioconductor website](http://bioconductor.org/packages/sights/)  
- [GitHub page](https://eg-r.github.io/privsig/)  
- [HTML vignette](http://bioconductor.org/packages/devel/bioc/vignettes/sights/inst/doc/sights.html)  
- [PDF manual](http://bioconductor.org/packages/devel/bioc/manuals/sights/man/sights.pdf)  

#### <a name="install"></a>Installation

Run in R :

- Bioconductor (try http:// if https:// URLs are not supported)
```
source("https://bioconductor.org/biocLite.R")  
biocLite("sights")  
library(sights)
```

- GitHub
```
install.packages("devtools")
library(devtools)
install_github("eg-r/sights")
library(sights)
```

#### <a name="cite"></a>Citation

Run in R: `citation("sights")`

#### <a name="license"></a>License

[GPL-3](https://raw.githubusercontent.com/eg-r/sights/master/LICENSE)

