# Nowcasting Endpoint

Lots of moving parts to this.
- Many of the methods are developed in R. So we should do a bit of testing to make sure that running R models in airflow is viable; otherwise we'll need to setup something like the postgres operator to communicate w/another machine
- We need to figure out how to evaluate models, and what to publish about those evaluations
- We need to figure out output format; this is likely to just be [hubverse](https://hubverse.io/quickstart/submit.html) format.

- [NoBS](https://cran.r-project.org/web//packages/NobBS/NobBS.pdf) and/or 
- [epi-nowcast](https://www.epinowcast.org/)
- [epinow2](https://epiforecasts.io/EpiNow2/)
- [delphiRF](https://github.com/cmu-delphi/DelphiRF)
