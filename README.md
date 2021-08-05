# BodyLanguage
Replication repository for the paper 'Body Language and Gender Stereotypes in Campaign Video'.
Authors: Markus Neumann, Erika Franklin Fowler, Travis Ridout (all Wesleyan Media Project)

We don't have permission to share the videos used in the paper. This replication repository shows how our pipeline works for one example video from our dataset which can also be found on YouTube. This is done in three Colab notebooks: One for the face detection and face recognition, one for the pose detection, and one to combine them. Note that even if we were able to share the videos, our pose detection pipeline wouldn't be 100% replicable because there is some amount of randomness [randomness in PyTorch](https://pytorch.org/docs/stable/notes/randomness.htmlhttps://pytorch.org/docs/stable/notes/randomness.html) that depends on the machine the code is run on.

The repository also contains an R script (and data) for the final analysis presented in the paper. The analysis was done using R version 4.0.1.
