# SporeSizeFungalKingdom

You only need one file to reproduce the figures 1, 2, S1, S2 and S3; the apriori contrasts of spore size and sma for spore shape reported in the google docs (as it is by the end of April 2020):

1. SummarySporeTraitAnalysis.RMD 


That RMD file already loads the three datasets used for those figures: 

a) output/Spore_Database_Fungi.csv: Spore size and shape data

b) output/FungalTaxanomy_col.csv: Fungal taxonomy as in the Catalogue of Life

c) output/GuildData.csv: Fungal functional groups.

The three datasets are already uploaded to this repo. Further details on those datasets and the r packages used can be found in "MatchingSpore_FunctionData.R"
7

# Information about the algorithm for spore extraction

The algorithm for spore extraction was developed by [@FranzKrah](https://github.com/FranzKrah) (the original code can be found [here](https://github.com/aguilart/SporeSizeFungalKingdom/blob/master/AlgorithmSporeExtraction/extract_spore_info_FromFranz_original.R)). The algorithm takes text from downloaded Mycobank descriptions that roughly follows the format "... *Spore ... d x d um ... ". The algorithm then extracts the spore name and quantities from the text and gathers the results in a table with rows representing each species name. This original code allows extractions for all spore types at once. Carlos Aguilar and Jeff Powell later seperated and modified the code to enhance extraction accuracy for different spore types, which follow slightly differnt notation conventions(these modified codes can be found in the folder [AlgorithmSporeExtraction](https://github.com/aguilart/SporeSizeFungalKingdom/tree/master/AlgorithmSporeExtraction).

