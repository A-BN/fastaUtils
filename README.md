# fastaUtils
Simple fasta exploration and manipulation

## Installation
To install it, simply enter the following command in R:
``` r
devtools::install_github('A-BN/fastaUtils')
```
or in your terminal:

``` bash
R --slave -e "devtools::install_github('A-BN/fastaUtils')"
```

## Usage

You can use it in R or directly in your terminal like this:
``` bash
R --slave -e "fastaUtils::fastanalyze(fasta = 'path/to/your/file.fasta', 
                                      metrics = TRUE, 
                                      plot = TRUE,
                                      verbose = TRUE)"
```
