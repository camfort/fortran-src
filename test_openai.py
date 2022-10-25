from openai.embeddings_utils import get_embedding
from openai.error import InvalidRequestError
#from transformers import GPT2Tokenizer
import json
import sys
import sqlite3

dbfile = 'embeddings.db'
tabname = 'embeddings'
create_table_sql = f'CREATE TABLE {tabname} ( path TEXT, name TEXT, firstLine INTEGER, lastLine INTEGER, vectorid INTEGER PRIMARY KEY ); CREATE TABLE vectors ( elem DOUBLE, ord INT, id INTEGER, FOREIGN KEY(id) REFERENCES {tabname}(vectorid) );'

con = sqlite3.connect(dbfile)

if con.execute("SELECT count(name) FROM sqlite_master WHERE type='table' AND name=?", (tabname,)).fetchone()[0]==0:
    con.executescript(create_table_sql)

# with open('nav.dump') as fp:
#     extracts = json.load(fp)
extracts = json.load(sys.stdin)
#tokenizer = GPT2Tokenizer.from_pretrained("gpt2")
for ex in extracts:
    print(f"file {ex['path']} pu {ex['name']} lines {ex['firstLine']}-{ex['lastLine']}")
    if con.execute("SELECT count(*) FROM embeddings WHERE path=? AND firstLine=?", (ex['path'], ex['firstLine'])).fetchone()[0] == 0:
        ex['src'] = []
        with open(ex['path']) as f:
            for line_num, line in enumerate(f):
                if line_num >= ex['firstLine']-1 and line_num <= ex['lastLine']-1:
                    ex['src'].append(line)
        ex['src']=('').join(ex['src'])
        #print(len(tokenizer(ex['src'])['input_ids']))
        txt=ex['src'][:2048]
        emb=[]
        while len(emb) == 0 and len(txt) > 2:
            try:
                emb=get_embedding(txt, engine='code-search-babbage-code-001')
            except Exception as err:
                print(err)
                txt=txt[:int(len(txt)/2)]
                print(f'trying with len={len(txt)}')
        ex['embedding']=emb
        cur = con.execute("INSERT INTO embeddings (path, name, firstLine, lastLine) VALUES (:path, :name, :firstLine, :lastLine)", ex)
        vid = cur.lastrowid
        for i,x in enumerate(ex['embedding']):
            con.execute("INSERT INTO vectors (id, ord, elem) VALUES (?, ?, ?)", (vid, i, x))
        con.commit()




# f = """
# integer function f(x)
#     integer :: x
#     f = 2 * x
#   end function f
# """


#print(get_embedding(f, engine='code-search-babbage-code-001'))
