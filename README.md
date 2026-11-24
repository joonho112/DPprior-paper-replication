# Design-Conditional Prior Elicitation for Dirichlet Process Mixtures

JoonHo Lee (jlee296@ua.edu) · The University of Alabama

The [paper](https://arxiv.org/abs/2602.06301v3) shows how to choose a concentration prior from an expected cluster count and check the implied population weights. The [table and figure index](https://joonho112.github.io/DPprior-paper-replication/08-artifact-index.html) links each result in the paper and supplement to its data and code.

To reproduce the tables and figures, follow the [setup instructions](https://joonho112.github.io/DPprior-paper-replication/01-start.html), then run this command from the repository root.

```sh
Rscript exhibits/00_build_all.R
```

The build uses saved model summaries. To fit the models yourself, see [data access](DATA_ACCESS.md) and [refitting instructions](refit/README.md).
