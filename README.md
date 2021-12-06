# BodyLanguage
Replication repository for the paper 'Body Language and Gender Stereotypes in Campaign Video' in Computational Communication Research.

Authors: Markus Neumann, Erika Franklin Fowler, Travis Ridout (all Wesleyan Media Project)

We don't have permission to share the videos used in the paper. The code folder contains all the scripts we used to create the results in the paper. The first three scripts `01_face_detection_recognition.ipynb`, `02_pose_detection.ipynb` and `03_combine.ipynb` require the videos we can't share. We can however share the results they create, which can be downloaded through this GDrive [link](https://drive.google.com/file/d/10iEYrEz1A12mmvXL02wn2Si-PTRP8h5-/view?usp=sharing). In order to demonstrate how the three scripts relying on our videos work, we added another folder, called `demo`, which contains a simplified version of these scripts that works on a single video (the Tom Steyer video shown in the paper) which can be downloaded from YouTube. This is intended to be run through Google Colab, which allows anyone an easy way to run our code. The Colab notebook can be directly accessed [here](https://colab.research.google.com/drive/1913b6EzehlMuGpznfkB1P2qvHPMg4I2a?usp=sharing). Note that even if we were able to share the videos, our pose detection pipeline wouldn't be 100% replicable because there is some amount of randomness [randomness in PyTorch](https://pytorch.org/docs/stable/notes/randomness.htmlhttps://pytorch.org/docs/stable/notes/randomness.html) that depends on the machine the code is run on. `code/04_measure_assertiveness.R` can be used to parse the face recognition/pose detection results (its results are also in `results/dominance/convhull_on_all_data.rdata`), and finally, `code/05_analysis.ipynb` can be used to replicate the results from the paper. Scripts to replicate the validation are also provided. The analysis was done using Python version 3.7.12 and R version 4.0.1.
