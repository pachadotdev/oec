[![CRAN-version](https://img.shields.io/badge/CRAN version-2.2-blue.svg)](https://cran.r-project.org/web/packages/oec/)
[![Github-version](https://img.shields.io/badge/Github version-3.0-blue.svg)](https://github.com/pachamaltese/oec) [![d3plus](https://img.shields.io/badge/D3plus-1.9.8-green.svg)](https://github.com/alexandersimoes/d3plus) [![mitlicense](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/pachamaltese/oec.svg?branch=master)](https://travis-ci.org/pachamaltese/oec)
[![Build status](https://ci.appveyor.com/api/projects/status/5xvlffxy8ro4wc34?svg=true)](https://ci.appveyor.com/project/pachamaltese/oec)
[![CRAN downloads](http://cranlogs.r-pkg.org/badges/oec)](http://cran.rstudio.com/web/packages/oec/index.html)

# The Observatory of Economic Complexity (R Package)

Use [The Observatory of Economic Complexity](http://atlas.media.mit.edu/en/)'s API from R console to obtain international trade data to create spreadsheets (csv format) and [D3Plus](http://d3plus.org/) visualizations. This package is released under the MIT license (see below).

## Examples

### Treemap

  * [What does Chile export to China? (2014)](http://pacha.hk/oec/chl_chn_2014_6char_treemap_exports.html)
  * [What does Chile export to China? (2011-2014)](http://pacha.hk/oec/chl_chn_2011_2014_1_6char_treemap_exports.html)

### Network

  * [What does Chile export to China? (2014)](http://pacha.hk/oec/chl_chn_2014_6char_network_exports.html)
  * [What does Chile export to China? (2011-2014)](http://pacha.hk/oec/chl_chn_2011_2014_1_6char_network_exports.html)

Read the documentation for more interesting uses and examples.

## How to install

### Using CRAN (version 2.2)

```r
install.packages("oec")
```

### Using Github (version 2.3)

This a version subject to changes and improvements. The commit we upload here are working and stable, but please notice that the CRAN version is tested under different platforms.

```r
install.packages("devtools") #if needed
library(devtools)
install_github("pachamaltese/oec/r_package")
```

## How to use

The documentation is available [here](http://pacha.hk/oec/oec.pdf). If you run `demos()` after `library(oec)` a demo script (`demo_examples.R`) will be copied to your working directory. Please check the demo script to see how `oec` works.

## Notice

[The Observatory of Economic Complexity](http://atlas.media.mit.edu/en/) original work is an idea of Mr. [Alexander Simoes](https://github.com/alexandersimoes/oec). This R package is just an extension to his work.

## Authors

* Cesar A. Hidalgo `<hidalgo@media.mit.edu>` [aut],
* Alexander Simoes `<alex@datawheel.us>` [aut, cph],
* Mauricio Vargas S. `<mvargas@dcc.uchile.cl>` [aut, cre]

## The MIT License

Copyright (c) 2016, Mauricio Vargas S.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
