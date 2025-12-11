from datasets import load_dataset
from sentence_transformers import SentenceTransformer

models = [
    'TurkuNLP/sbert-cased-finnish-paraphrase',
    'TurkuNLP/sbert-uncased-finnish-paraphrase',
    'lightonai/modernbert-embed-large',
    'sentence-transformers/LaBSE',
    'intfloat/multilingual-e5-small',
    'intfloat/multilingual-e5-base',
    'intfloat/multilingual-e5-large',
    'Snowflake/snowflake-arctic-embed-l-v2.0',
]

data = load_dataset(
    'sentence-transformers/mr-tydi',
    data_files='fi-triplet-100/train-00000-of-00001.parquet'
)

num_rows = 1000

for model in models:
    emb = SentenceTransformer(model)
    correct, total, crr = 0, 0, 0
    for i in range(num_rows):
        row = data['train'][i]
        docs = [row['positive']] + [row[f'negative_{j}'] for j in range(1, 101)]
        x = emb.encode_query(row['anchor'])
        y = emb.encode_document(docs)
        sim = emb.similarity(x, y).flatten()
        correct += int(sim.argmax() == 0)
        rank = int((sim.argsort(descending=True) == 0).nonzero().flatten()) + 1
        crr += 1 / rank
        total += 1 
        mrr = crr/total * 100
        if total % 100 == 0:
            print(f'\r{model} {correct}/{total} mrr={mrr:.1f}%', flush=True)
    print(f'\r{model} {correct}/{total} mrr={mrr:.1f}%', flush=True)

