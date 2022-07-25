# GSON Functions

- enhance `gson` constructors for novel species.
  - build `gson` with KEGG mapper
  - build `gson` with uniprot annotation
  - build `gson` with interproscan results
- add multiple ids of genes.
- set primary id by an additional slot `primary_id`. This setting will trigger the update of `gson` object and thus enable the update of the mapping of `gsid2gene`.
