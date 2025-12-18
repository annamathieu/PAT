# -*- coding: utf-8 -*-
"""
Created on Thu Dec 11 09:31:59 2025

@author: erwan
"""
import os
from sentence_transformers import SentenceTransformer
from bertopic import BERTopic
from sklearn.datasets import fetch_20newsgroups
from sentence_transformers import SentenceTransformer
from bertopic.vectorizers import ClassTfidfTransformer
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP
import numpy as np
from sklearn.datasets import fetch_20newsgroups
import openpyxl

from sklearn.datasets._twenty_newsgroups import (
    strip_newsgroup_header,
    strip_newsgroup_footer,
    strip_newsgroup_quoting
)

PAT = pd.read_excel("data/PATtrad.xlsx")
PAT = PAT.iloc[:, 1].tolist()

def clean_newsgroup_text(text):
    text = strip_newsgroup_header(text)
    text = strip_newsgroup_footer(text)
    text = strip_newsgroup_quoting(text)
    return text

PAT_clean = [clean_newsgroup_text(t) for t in PAT if isinstance(t, str)]


docs = fetch_20newsgroups(subset='all',  remove=('headers', 'footers', 'quotes'))['data']

PAT_clean = PAT_clean + docs

##################
#JEU PRELEMMATISE#
##################
#Definition de la racine du projet
os.chdir("C:/Users/erwan/Documents/M2+/PAT")
os.getcwd()

#Importation des textes lemmatisés reconstruits 
import pandas as pd

df = pd.read_csv("python/texte lemma.csv", sep=";",encoding='utf-8')

#On extrait les descriptions
liste_texte = df.iloc[:, 1].astype(str).tolist()

#On précise les paramètres d'UMAP pour pouvoir fixer une seed
umap_model = UMAP(n_neighbors=15, n_components=5,min_dist=0.0, metric='cosine', random_state=42)
vectorizer_model = CountVectorizer(max_df=0.7,analyzer="word")

seed_topic_list = [["projet", "action", "plan", "acteur"], #gouvernance
                   ["culture", "tourisme", "terroir", "touriste"], #culture tourisme
                   ["éducation", "alimentation", "jeune", "école"], #éduc alimentaire
                   ["nutrition", "santé", "sain"], #nutri santé
                   ["filière", "agriculture", "circuit", "agricole"], #économie alimentaire
                   ["accessibilité", "lutte ", "précarité", "solidaire"],#justice pro
                   ["environnement", "eau", "pollution", "agroécologie","vertueu"], #environnement
                   ["restaurant", "collectif", "formation", "cuisine","cantine"], #restauration collective
                   ["création", "foncier", "urbain", "zone"]]

#On import BERTopic
topic_model = BERTopic(language="multilingual",umap_model=umap_model,
                       vectorizer_model = vectorizer_model,
                       seed_topic_list=seed_topic_list,
                       nr_topics=9)

#On extrait les topics et les probabilités 
topics, probs = topic_model.fit_transform(liste_texte)

nb_topics = topic_model.get_topic_info()

for topic_id, words in topic_model.get_topics().items():
    if topic_id == -1: #ça représente l'ensembles des tokens donc aucune information
        continue
    print(f"\n=== Topic {topic_id} ===")
    for word, score in words:
        print(f"{word}: {score:.4f}")

fig = topic_model.visualize_topics()
#Une représentation graphique est disponible
fig.write_html("topics.html")

#########################
#DONNEES BRUTES EN COURS#
#########################

df1 = pd.read_csv("data/pats-20250710-win1252.csv", sep=";",encoding='windows-1252')

liste_texte = df1.iloc[:, 77].astype(str).tolist()
liste_texte = [x for x in liste_texte if x not in ("nan",None)]