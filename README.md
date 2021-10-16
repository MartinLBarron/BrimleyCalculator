# BrimleyCalculator
An R package to calculate statistics related to the Brimley/Cocoon line.

This package was inspired by the great twitter account @BrimleyLine.

## Installation

Use devtools to install.

```
devtools::install_github("https://github.com/MartinLBarron/BrimleyCalculator.git")
```

## Usage
Simply feed a birthday to the calculator to learn when someone will cross the 
Brimley/Cocoon line.

```
library(BrimleyCalculator)
BrimleyCalculator(as.Date("1973-12-18"))

