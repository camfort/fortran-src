import sqlite3
from openai.embeddings_utils import cosine_similarity
from openai.embeddings_utils import get_embedding
import pandas as pd

dbfile = 'embeddings.db'
tabname = 'embeddings'

con = sqlite3.connect(dbfile)

query = "SELECT path, name AS fname, firstLine, lastLine, vectorid, elem AS emb FROM embeddings JOIN vectors ON embeddings.vectorid = vectors.id ORDER BY vectorid, ord"
df = pd.read_sql_query(query, con).groupby('vectorid').agg({'emb': list, 'path': 'first', 'fname': 'first', 'firstLine': 'first', 'lastLine': 'first'}).reset_index()

def search_functions(df, code_query, n=3, pprint=True, n_lines=7):
    embedding = get_embedding(code_query, engine='code-search-babbage-text-001')
    df['similarities'] = df.emb.apply(lambda x: cosine_similarity(x, embedding))

    res = df.sort_values('similarities', ascending=False).head(n)
    if pprint:
        for r in res.iterrows():
            print(f'{r[1].path}:{r[1].firstLine}: function or subroutine {r[1].fname}: score={str(round(r[1].similarities, 3))}')
            #import pdb;pdb.set_trace()
            # print("\n".join(r[1].code.split("\n")[:n_lines]))
            print('-'*70)
    return res

res = search_functions(df, 'generate daily weather data', n=3)
