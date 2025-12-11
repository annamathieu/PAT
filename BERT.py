# -*- coding: utf-8 -*-
"""
Created on Thu Dec 11 09:31:59 2025

@author: erwan
"""

from sentence_transformers import SentenceTransformer
from bertopic import BERTopic
from sklearn.datasets import fetch_20newsgroups


model = SentenceTransformer("all-MiniLM-L6-v2", device="cpu")

docs = fetch_20newsgroups(subset='all',  remove=('headers', 'footers', 'quotes'))['data']

topic_model = BERTopic(
    embedding_model=model,
    umap_model=None,
    hdbscan_model=None
)

topics, probs = topic_model.fit_transform(docs)

topic_model.get_topic_info()
topic_model.get_topic(0)



#####
# MISE EN PRATIQUE
#####▼

import os
#Definition de la racine du projet
os.chdir("C:/Users/erwan/Documents/M2+/PAT")
os.getcwd()

#Importation des textes lemmatisés reconstruits 
import pandas as pd

df = pd.read_csv("texte lemma.csv", sep=";",encoding='utf-8')

liste_texte = df.iloc[:, 1].astype(str).tolist()

topic_model = BERTopic(
    embedding_model=model,
    umap_model=None,
    hdbscan_model=None
)

topics, probs = topic_model.fit_transform(liste_texte)

topic_model.get_topic_info()
topic_model.get_topic(0)

fig = topic_model.visualize_topics()
fig.write_html("topics.html")
