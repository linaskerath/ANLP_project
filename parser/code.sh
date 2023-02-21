
# Convert data to conll format
python3 conv.py df_annotation.pickle > gaze.input.conllu

# Install MaChAmp TODO make this version public
git clone https://robvanderg@bitbucket.org/robvanderg/machamp-noallennlp.git
cd machamp-noallennlp
git reset --hard 95305da3de09c4428e2aeb4a81bc680a8b36040e
pip3 install -r requirements
./scripts/0.prep.ud.sh

# Train models
python3 train.py --dataset_configs ../UD_English-EWT.json --name ewt.mluke --parameters_config params-mluke.json
python3 train.py --dataset_configs ../UD_English-EWT.json --name ewt.xlmr --parameters_config params-xlmr.json
python3 train.py --dataset_configs ../UD_English-EWT.json --name ewt.mbert --parameters_config params-mbert.json

# predict on the input data
python3 predict.py logs/ewt.mluke/*/model.pt ../gaze.input.conllu ../gaze.out.mluke.conllu
python3 predict.py logs/ewt.xlmr/*/model.pt ../gaze.input.conllu ../gaze.out.xlmr.conllu
python3 predict.py logs/ewt.mbert/*/model.pt ../gaze.input.conllu ../gaze.out.mbert.conllu

