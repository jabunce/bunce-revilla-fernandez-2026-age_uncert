# bunce-revilla-fernandez-2026-age_uncert
Analysis code for the manuscript:

**Bunce, JA, C Revilla-Minaya, and CI Fernández (2026) Estimating mean growth trajectories when measurements are sparse and age is uncertain**.

<br/>
Steps to reproduce the analysis:

1) Create a project folder on your machine. Name it whatever you want. 

2) Inside this project folder, put the file ``RunAll.R`` and the file ``post_USMatsi.RDS``. 

3) Also inside the project folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``.

4) Inside the ``Data`` folder, put the file ``Berkeley.csv``.

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your project folder. Then run its parts in order in R.

Fitting the models in Stan with the numbers of chains and samples used in the manuscript can take several days to run. However, you can usually get fairly reasonable quick estimates with only two chains of 1000 samples each. Within ``RunAll.R`` you can change the numbers of chains and samples.

Figures will appear in the ``Plots`` folder.
