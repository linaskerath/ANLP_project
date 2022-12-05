import lang2vec.lang2vec as l2v
import pandas as pd
import numpy as np
from sklearn.impute import KNNImputer
from pyglottolog import Glottolog
from scipy import spatial

languages = ["nld", "eng", "ekk", "fin", "deu", "ell", "heb", "ita", "nor", "rus", "spa", "tur"]
features =  ["syntax_wals", "syntax_sswl", "syntax_ethnologue"]


#URIEL data
df_uriel = pd.DataFrame.from_dict(l2v.get_features(languages, features))
df_uriel = df_uriel.replace("--", np.NaN).dropna(how='all')

imputer = KNNImputer(n_neighbors=5).fit(df_uriel).transform(df_uriel)
imputer[imputer >= 0.5] = 1
imputer[imputer < 0.5] = 0

df_uriel = pd.DataFrame(imputer, columns=df_uriel.columns)

nunique_uriel = df_uriel.nunique(axis=1)
df_uriel = df_uriel.drop(nunique_uriel[nunique_uriel == 1])

#Glottolog data
glottolog = Glottolog('../data/glottolog-4.6', cache=True)

glottolog_dict = {}
for l in languages:
    languoid = glottolog.languoid(l)
    families = {family:1 for family in [str(f.id) for f in languoid.ancestors]}
    glottolog_dict[l] = families
    
df_glottolog = pd.DataFrame.from_dict(glottolog_dict).fillna(0)

nunique = df_glottolog.nunique(axis=1)
df_glottolog = df_glottolog.drop(nunique[nunique == 1])

#concat & compute cosine similarity
df = pd.concat([df_uriel, df_glottolog])

ls_dict = {}
for l1 in languages:
    ls_dict[l1] = {}
    for l2 in languages:
        ls = spatial.distance.cosine(df[l1], df[l2])
        ls_dict[l1][l2] = ls

df_ls = pd.DataFrame.from_dict(ls_dict)
df_ls.to_csv("../data/outputs/linguistic_similarities.csv")