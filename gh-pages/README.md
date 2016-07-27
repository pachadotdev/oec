[![version](https://img.shields.io/badge/version-1.0.2-blue.svg)](https://github.com/pachamaltese/oec) [![d3plus](https://img.shields.io/badge/d3plus-1.9.7-green.svg)](https://github.com/alexandersimoes/d3plus) [![mitlicense](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)

# The Observatory of Economic Complexity - R Package

Use The Observatory of Economic Complexity's API from R console to obtain international trade data to create spreadsheets (csv format) and D3Plus visualizations. This package is released under the MIT license (see below).

## Examples

  * [Treemap of chilean exports to the world in the year 2014](http://pacha.hk/oec/chl_all_2014_6char_treemap_exports.html)
  * [Network of chilean exports to the world in the year 2014](http://pacha.hk/oec/chl_all_2014_6char_network_exports.html)
  * [Compared network of chilean exports to the world between the years 1995 and 2014](http://pacha.hk/oec/chl_all_1995_2014_6char_network_exports.html)

## How to install

### Using CRAN
```r
install.packages("oec")
```

### Using Github
```r
install.packages("devtools") #if needed
library(devtools)
install_github("pachamaltese/oec")
```

## How to use 

The documentation is available [here](http://pacha.hk/oec/oec.pdf). Read the documentation and then run `demos()` after the library is installed.

### The MIT License

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
