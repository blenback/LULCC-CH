[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12698471.svg)](https://doi.org/10.5281/zenodo.12698471)

This branch of the repository represents a limited version of the original LULCC-CH code base specifically intended for reproducing the results of future Land Use and Land Cover change between 2020 and 2100 under the five scenarios of Shared Socioeconomic Pathways (SSPs) for Switzerland developed by the [Swiss National Centre for Climate Services (NCCS)'s](https://www.nccs.admin.ch/nccs/en/home/climate-change-and-impacts/nccs-impacts/socioeconomic-pathways.html) [SSP-CH project](https://www.wsl.ch/en/projects/ssp-ch/) led by the [Swiss Federal Institute for Forest, Snow and Landscape Research (WSL)](https://www.wsl.ch/en/).\
\
For a comprehensive introduction to the model, users should refer to the main branch of the repository: [LULCC-CH main branch](https://github.com/blenback/LULCC-CH/tree/main) and the accompanying guide: [LULCC-CH guide](https://blenback.github.io/LULCC-CH/).

This repository contains the following:

1.  The model and input files needed to run the model.
2.  Scripts to reproduce some of the latter stage inputs (i.e. future transition rates, allocation parameters, and spatial intervention masks)

It specifically does not include all of the required input data (e.g. spatial predictors). This data can be found in the accompanying [Zenodo repository](https://doi.org/10.5281/zenodo.8263509)

## Repo Structure

-   [Model](Model): Dinamica EGO model and required sub-models
-   [Scripts](Scripts): R scripts
-   [Tools](Tools): Specifications and inputs
-   [renv](renv): R environment management

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

[blenback\@ethz.ch](mailto:blenback@ethz.ch) or [\@blenback](https://github.com/blenback)

## Acknowledgements

LULCC-CH was developed within the chair of [Planning of Landscape and Urban Systems (PLUS)](https://plus.ethz.ch/) at [ETH Zurich](https://www.ethz.ch/en.html)
