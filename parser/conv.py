import pandas
import pickle


data = pickle.load(open('df_annotation.pickle', 'rb'))

words = data['interest_area']
sentences = data['sentence_num']

data = []
prevSentence = 0
for word, sentence in zip(words, sentences):
    if sentence != prevSentence:
        prevSentence = sentence
        data.append([])
    data[-1].append(word)

for sent in data:
    for wordIdx, word  in enumerate(sent):
        conll = ['_'] * 10
        conll[0] = str(wordIdx + 1)
        conll[9] = word
        conll[1] = word.replace(',', '').replace('.', '')
        print('\t'.join(conll))
    print()

