import sys
sys.path.append("/mnt/code/machine_learning/")

import log_error_classifier

model_path = '/mnt/artifacts/models/log_classification_20231019_205618'
classifier = log_error_classifier.HuggingFaceClassifier(path_or_pretrained=model_path)


#my_model = 

def predict(text):
    predictions, probabilities = classifier.predict(text)
    prediction_strings = [classifier.label_to_error[pred] for pred in predictions]
    print(prediction_strings)

    return dict(error_class=prediction_strings)

if __name__ == "__main__":
    tst_string = "This is a test"
    text = [tst_string for i in range(0, 2)]
    #text = ["This is a sentence. Does it have an error?", "There are many more to go"]
    predict(text)
    print("Did I make it here?")