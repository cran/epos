[![Build Status](https://travis-ci.org/ZBMEDLABS/epilepsyontologysimilarities.svg?branch=master)](https://travis-ci.org/ZBMEDLABS/epilepsyontologysimilarities)

[![](https://cranlogs.r-pkg.org/badges/epos)](https://cran.r-project.org/package=epos)

# epos
This is an R package that is developed for analyzing and visualizing statistical information of biomedical named entities that were automatically identified with a UIMA-based text mining workflow on the corpus of LIVIVO. The major scope of this R package is the comparison of drug names that co-occur with entities of epilepsy ontologies in documents of the LIVIVO corpus.

Basically, the UIMA-based workflow takes as input a dictionary containing biomedical entities with synonyms for identifying them in documents of the LIVIVO corpus. The epilepsy ontologies EpSO, ESSO, and EPILONT are used for creating three different dictionaries for epilepsy. The current version of DrugBank is taken for creating a dictionary for drug names. The results of the text mining workflow is written into a MongoDB databases. Finally, the information in the MongoDB is aggregated for creating lists of drug names that are sorted by their frequency of documents in which they co-occur with entities from each of the epilepsy dictionaries. These three frequency sorted lists are saved as R-objects in inst/resources/[tepso.rda,tesso.rda,tepi.rda].

The major entry point of this R package is the execution of the function main () that calls all relevant steps for creating a final results table written into inst/results/neurotable.tex

The UIMA-based text mining workflow is described in the following three publications:

Müller B, Hagelstein A (2016) Beyond Metadata – Enriching Life Science Publications in LIVIVO with Semantic Entities from the Linked Data Cloud. In: Joint Proceedings of the Posters and Demos Track of the 12th International Conference on Semantic Systems – SEMANTiCS2016 and the 1st International Workshop on Semantic Change & Evolving Semantics SuCCESS’16, Leipzig, Germany

Müller B, Hagelstein A, Gübitz T (2016) Life Science Ontologies in Literature Retrieval: A Comparison of Linked Data Sets for Use on Semantic Search on a Heterogeneous Corpus. In: Proceedings of the 20th International Conference on Knowledge Engineering and Knowledge Management. Bologna, Italy

Müller B, Rebholz-Schuhmann D (2020) Selected Approaches Ranking Contextual Term for the BioASQ Multi-label Classification (Task6a and 7a). In: Proceedings of the Joint European Conference on Machine Learning and Knowledge Discovery in Databases ECML PKDD 2019, Würzburg, Germany