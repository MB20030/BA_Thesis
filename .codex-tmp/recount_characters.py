import re, unicodedata, json
from pathlib import Path
pages=json.loads(Path('.codex-tmp/thesis_pages.json').read_text(encoding='utf-8'))
start=next(i for i,t in enumerate(pages) if re.search(r'(?m)^\d+ Introduction\s*$',t))
end=next(i for i,t in enumerate(pages[start:],start) if re.search(r'(?m)^\d+ Bibliography\s*$',t))
parts=[]
for i in range(start,end):
    parts.append(re.sub(r'\n\s*'+str(i-start+1)+r'\s*$', '', pages[i]))
text=unicodedata.normalize('NFC','\n'.join(parts))
text=re.sub(r'(?<=\w)-[ \t]*\n[ \t]*(?=\w)', '', text)
text=''.join(c for c in text if unicodedata.category(c)!='Co')
text=re.sub(r'\s+', ' ', text).strip()
Path('.codex-tmp/thesis_body_extracted.txt').write_text(text,encoding='utf-8')
a=len(text); b=len(re.sub(r'\s','',text))
print(json.dumps({'with_spaces':a,'without_spaces':b,'decrease_with_spaces':134848-a,'decrease_without_spaces':114782-b,'printed_pages':f'1-{end-start}','start':text[:60],'end':text[-180:]},ensure_ascii=False,indent=2))
