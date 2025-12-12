[![DOI](https://zenodo.org/badge/1006411199.svg)](https://doi.org/10.5281/zenodo.17592982)

# Do Errors Impair the Performance of Chess Players? Evidence from one Million Games of Online-Chess
This repository contains the code needed to replicate all analysis in the paper. The raw game data is too large to share on GitHub. Data can be extracted from Lichess [here](https://database.lichess.org/db-univ/). The paper used data from June, 2021.

Following download of the raw data, they can be filtered to smaller .pgn files using an adapted version of the file `filter` depending on the time format. Then we obtained smaller (albeit still too large to share) data using `pgn-extract`. 

These data were then prepared in `lichess_data_prep.R` and finally analyzed in `pes_paper_prep.R` and `pes_paper_results.R`.
