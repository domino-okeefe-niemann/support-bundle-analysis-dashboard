import log_error_classifier

model_path = '/mnt/artifacts/models/log_classification_20231019_205618'
classifier = HuggingFaceClassifier(pretrained_or_path=model_path)
# print("Did I make it here?")
#my_model = 


#def predict(text):

# This is a sample Python model
# You can publish a model API by clicking on "Publish" and selecting
# "Model APIs" in your quick-start project.
 
# Import dependencies
import random
 
 
# Define a helper function to generate a random number:
def random_number(start, stop):
    return random.uniform(start, stop)
 
 
# Define a function to create an API
# To call, use {"data": {"start": 1, "stop": 100}}
def my_model(start, stop):
    return dict(a_random_number=random_number(start, stop))