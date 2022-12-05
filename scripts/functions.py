import pandas as pd
from sklearn.model_selection import GroupShuffleSplit
from tqdm import tqdm
import numpy as np


def map_language(df):
    """
    Function to map languages to integer.
    Arguments:
        df: dataframe containing all samples.
    Returns: lookup dictionary for all languages.
    """
    languages = df['lang'].unique()
    languages.sort()
    len_languages = len(languages)
    language_lookup = dict(zip(languages, range(len_languages)))
    return language_lookup


def custom_train_test_split(data, columns):
    """
    Function for train test split.
    Arguments:
        data: pandas dataset (with train columns, labels ('lang_code') and groups ('subid'))
        columns: columns to train on.
    Returns: train and test sets with labels.
    """
    train_subset = data[columns]
    train_label_subset = data['lang_code']
    gss = GroupShuffleSplit(n_splits=1, test_size = 0.4, random_state=42)
    split_indexes = list(gss.split(train_subset, train_label_subset, data['subid']))[0]
    train_idx = list(split_indexes[0])
    test_idx = list(split_indexes[1])

    X_train = train_subset.iloc[train_idx]
    y_train = train_label_subset.iloc[train_idx]
    X_test = train_subset.iloc[test_idx]
    y_test = train_label_subset.iloc[test_idx]

    test_lang = data['lang'].iloc[test_idx]
    return X_train, X_test, y_train, y_test, test_lang

def get_cv_score(data, columns, model_type, model, cv = 5):
    """
    Function to get cv score. 
    Arguments:
        data: pandas dataset (with train columns, labels ('lang_code') and groups ('subid'))
        columns: columns to train on.
        model_type: LogisticRegression or LSTM.
        model: defined model.
        cv: how many cv splits, default = 5.
    Returns: train and test sets with labels.
    """
    train_subset = data[columns]
    train_label_subset = data['lang_code']

    gss = GroupShuffleSplit(n_splits = cv, test_size = 0.4, random_state=42)
    cv_scores = []

    if model_type == 'LogisticRegression':
        for train_idx, test_idx in tqdm(gss.split(train_subset, train_label_subset, groups = data['subid'])): # wtf what are labels
            model.fit(train_subset.iloc[train_idx], train_label_subset.iloc[train_idx])
            score = model.score(train_subset.iloc[test_idx], train_label_subset.iloc[test_idx])
            cv_scores.append(score)
        print(np.mean(cv_scores))
        return cv_scores

    elif model_type == 'LSTM':
        print('TBD. Must be implemented if logreg framwork does not work.')
        return 

    else:
        print("Model type must be 'LogisticRegression' or 'LSTM'.")