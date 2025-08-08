[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12698471.svg)](https://doi.org/10.5281/zenodo.12698471)

<img src="https://github.com/blenback/LULCC-CH/blob/main/LULCC-CH-guide/img/LULCC-CH-logo.svg" alt="LULCC-CH logo" align="center" width="50%"/>

A Land Use and Land Cover Change model developed for simulating
alternative future scenarios with policy interventions in Switzerland.
LULCC-CH combines
![](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
and [Dinamica EGO](https://csr.ufmg.br/dinamica/).

This repository contains the following:

1.  Guide for the preparation and use of LULCC-CH in the
    [`LULCC-CH-guide`](LULCC-CH-guide) folder.
2.  The model and some of the input files needed to run the model.

It specifically does not include all of the required input data (e.g.
spatial predictors). Some of this data is generated programmatically by
the [Preparation](Scripts/Preparation) scripts and other data can be
found in the accompanying [Zenodo
repository](https://doi.org/10.5281/zenodo.8263509)

## Expansions of LULCC-CH:

-   **High performance computing version**: LULCC-CH has a branch
    wherein the model has been adapted for usage in a high-performance
    computing environment: [HPC
    branch](https://github.com/blenback/LULCC-CH/tree/hpc) with an
    accompanying containerized version of the Dinamica model:
    [`dinamica-ego-docker`
    container](https://github.com/cbueth/dinamica-ego-docker/)

-   **Integration with Ecosystem service and biodiversity models**:
    LULCC-CH has been integrated with 10 models of Ecosystem Services
    and indicators of biodiversity with a computational pipeline:
    [evoland-plus-hpc](https://github.com/ethzplus/evoland-plus-HPC)

-   **Evolution in to evoland-plus**: LULCC-CH is currently in the
    process of being re-factored as an R package and will be re-named
    [evoland-plus](https://github.com/ethzplus/evoland-plus)

## Repo Structure

-   [Data](Data): Input spatial data
-   [Model](Model): Dinamica EGO model and required sub-models
-   [Scripts](Scripts): R scripts
-   [Tools](Tools): Specifications and inputs
-   [renv](renv): R environment management

## Quick Start

``` r
renv::restore()
source("Scripts/LULCC_CH_master.R")
```

## How to cite

When using LULCC-CH, please cite the following paper:

> BibTex
>
> ``` bibtex
> @article{black2024,
> title = {Broadening the horizon in land use change modelling: Normative scenarios for nature positive futures in Switzerland},
> volume = {24},
> ISSN = {1436-378X},
> url = {http://dx.doi.org/10.1007/s10113-024-02261-0},
> DOI = {10.1007/s10113-024-02261-0},
> number = {3},
> journal = {Regional Environmental Change},
> publisher = {Springer Science and Business Media LLC},
> author = {Black,  Benjamin and Adde,  Antoine and Farinotti,  Daniel and Guisan,  Antoine and K\"{u}lling,  Nathan and Kurmann,  Manuel and Martin,  Caroline and Mayer,  Paula and Rabe,  Sven-Erik and Streit,  Jan and Zekollari,  Harry and Gr\^et-Regamey,  Adrienne},
> year = {2024},
> month = jul
> }
> ```

> Atribution
>
> **Black, B., Adde, A., Farinotti, D., Guisan, A., Külling, N., Kurmann, M., Martin, C., Mayer, P., Rabe, S.E., Streit, J., Zekollari, H., & Gret-Regamey, A.** (2024). Broadening the horizon in land use change modelling: Normative scenarios for nature positive futures in Switzerland. Regional Environmental Change. <http://dx.doi.org/10.1007/s10113-024-02261-0>

## Contact

[blenback\@ethz.ch](mailto:blenback@ethz.ch) or
[\@blenback](https://github.com/blenback)

## Acknowledgements

This project was funded under the
[ValPar.CH](https://valpar.ch/index_en.php?page=home_en) project, funded
as part of a pilot project of the "Action Plan for the Swiss
Biodiversity Strategy (AP SBS)" by the Federal Office for the
Environment (FOEN).

LULCC-CH was developed within the chair of [Planning of Landscape and
Urban Systems (PLUS)](https://plus.ethz.ch/) at [ETH
Zurich](https://www.ethz.ch/en.html)
