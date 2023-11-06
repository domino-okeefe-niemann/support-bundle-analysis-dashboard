import os
import random
import pandas as pd
import numpy as np
from typing import Optional, List, Tuple
from datetime import datetime

from sklearn.model_selection import train_test_split
from datasets import Dataset, DatasetDict

from tqdm.auto import tqdm

import torch
from torch.utils.data import DataLoader, TensorDataset
from transformers import AdamW, AutoTokenizer, AutoModelForSequenceClassification, get_linear_schedule_with_warmup




class HuggingFaceClassifier:
    def __init__(self, path_or_pretrained: str, num_labels: Optional[int] = None):
        """
        The classifier allows models to either be freshly created or uploaded from a datapath
            - If num_labels is None, path_or_pretrained should be the path to saved model
            - Else, path should instead be the desired pretrained model from HF
            
        path_or_pretrained: path to precreated model OR name of pretrained model from HF
        num_labels: number of labels. This is not needed if loading a model that's already been fine-tuned 
        """
        
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.tokenizer = AutoTokenizer.from_pretrained(path_or_pretrained)

        if num_labels is not None:
            self.model = AutoModelForSequenceClassification.from_pretrained(path_or_pretrained, num_labels = num_labels)
        else:
            self.model = AutoModelForSequenceClassification.from_pretrained(path_or_pretrained)
        
        self.model.to(self.device)
        self.model.eval()

    def train(self, df, epochs=3, batch_size=8, learning_rate=5e-5, warmup_steps=0):
        
        # Create a DataLoader
        dataset_logs = Dataset.from_pandas(df)
        tokenized_datasets_logs = dataset_logs.map(self._tokenize_function)
        tokenized_datasets_logs = tokenized_datasets_logs.remove_columns(["text"])
        tokenized_datasets_logs = tokenized_datasets_logs.rename_column("label", "labels")
        tokenized_datasets_logs.set_format("torch")
        train_dataloader_logs = DataLoader(tokenized_datasets_logs, shuffle=True, batch_size=batch_size)
        
        
        # Define optimizer and scheduler
        optimizer = AdamW(self.model.parameters(), lr=learning_rate)
        num_training_steps = len(train_dataloader_logs) * epochs
        
        scheduler = get_linear_schedule_with_warmup(optimizer, num_warmup_steps=warmup_steps, num_training_steps=num_training_steps)
        
        # progress_bar = tqdm(range(num_training_steps))
        
        # Training loop
        self.model.train()
        for epoch in range(epochs):
            for batch in train_dataloader_logs:
                batch = {k: v.to(self.device) for k, v in batch.items()}
                outputs = self.model(**batch)
                loss = outputs.loss
                loss.backward()

                optimizer.step()
                scheduler.step()
                optimizer.zero_grad()
                
                # progress_bar.update(1)

        # Set model back to evaluation mode
        self.model.eval()

    def save_model(self, path: str):
        self.model.save_pretrained(model_directory)
        self.tokenizer.save_pretrained(model_directory)
        
    def predict(self, texts: List[str], batch_size: int = 32) -> Tuple[List[int], np.ndarray]:
        """
        Transform a batch of texts to tensors and get predictions
        """

        # Ensure texts is a list. If a single string is passed, convert it into a list.
        if isinstance(texts, str):
            texts = [texts]

        all_predicted_classes = []
        all_probs = []

        # Split texts into smaller batches and predict
        for i in range(0, len(texts), batch_size):
            batch_texts = texts[i: i + batch_size]
            
            # Tokenize the batch of texts
            inputs = self.tokenizer(batch_texts, return_tensors="pt", truncation=True, max_length=512, padding="max_length")

            for key, value in inputs.items():
                inputs[key] = value.to(self.device)

            with torch.no_grad():
                outputs = self.model(**inputs)
                logits = outputs.logits
                probs = torch.nn.functional.softmax(logits, dim=-1)
                predicted_classes = torch.argmax(probs, dim=-1).tolist()

            all_predicted_classes.extend(predicted_classes)
            all_probs.append(probs.cpu().numpy())

        return all_predicted_classes, np.concatenate(all_probs, axis=0)

    def _tokenize_function(self, df):
        return self.tokenizer(df['text'], padding="max_length", truncation=True)

# Usage example:
if __name__ == "__main__":
    #path_or_pretrained = "bert-base-cased"
    base_dir = '/mnt/artifacts/models'

    path_or_pretrained = os.path.join(base_dir, os.listdir(base_dir)[1])
    classifier = HuggingFaceClassifier(path_or_pretrained)
    
    project_name = 'allstate_log_github'
    data_directory = '/mnt/data/' + project_name + '/'

    dir_name = os.path.join(data_directory, 'classification_data')
    df_train = pd.read_csv(os.path.join(dir_name, "train_small.csv"))
    df_train = df_train[1:100]
    df_test = pd.read_csv(os.path.join(dir_name, "test_small.csv"))
    
    unique_labels = df_train['label'].unique()
    text_to_label = {'none':0, 'cluster':1, 'domino':2, 'user':3}
    label_to_text = {str(label):text for text, label in text_to_label.items()}
    
    df_train['label'] = df_train['label'].apply(lambda x: text_to_label[x])
    df_test['label'] = df_test['label'].apply(lambda x: text_to_label[x])
    

    text = df_test['text'].to_list()

    predictions = classifier.predict(text)

    print("The predictions have been done!")

