import lang2vec.lang2vec as l2v
import pandas as pd
import numpy as np

languages = ["dut", "eng", "fin", "ger", "gre", "heb", "ita", "kor", "nor", "rus", "spa", "tur"]
features =  ["syntax_wals", "syntax_sswl", "syntax_ethnologue"]

df = pd.DataFrame.from_dict(l2v.get_features(languages, features))

# TODO: KNN for missing values instead of dropping
df = df.replace("--", np.NaN).dropna()

nunique = df.nunique(axis=1)
df = df.drop(nunique[nunique == 1]).reset_index()

display(df)