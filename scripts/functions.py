import pandas as pd


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