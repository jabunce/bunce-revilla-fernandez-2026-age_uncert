# bunce-revilla-fernandez-2026-age_uncert
Analysis code for the publication:

**Bunce, JA, C Revilla-Minaya, and CI Fernández (2026) Estimating mean growth trajectories when measurements are sparse and age is uncertain. American Journal of Biological Anthropology 191:e70358**, available open-access [here](https://onlinelibrary.wiley.com/doi/10.1002/ajpa.70358)

The original preprint is on bioRXiv [here](https://www.biorxiv.org/content/10.64898/2026.02.24.707738v1)

<br/>
Steps to reproduce the analysis:

1) Create a project folder on your machine. Name it whatever you want. 

2) Inside this project folder, put the file ``RunAll.R`` and the file ``I_cov_mat.RDS``. 

3) Also inside the project folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``.

4) Inside the ``Data`` folder, put the file ``Berkeley.csv``.

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your project folder. Then run its parts in order in R.

Fitting the models in Stan with the numbers of chains and samples used in the manuscript can take several days to run. However, you can usually get fairly reasonable quick estimates with only two chains of 1000 samples each. Within ``RunAll.R`` you can change the numbers of chains and samples.

Figures will appear in the ``Plots`` folder.
