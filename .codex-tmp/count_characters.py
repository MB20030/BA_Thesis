from pypdf import PdfReader
import re, unicodedata, json
from pathlib import Path
reader=PdfReader('Thesis/Thesis.pdf')
parts=[]
for i in range(7,49):
    text=reader.pages[i].extract_text()
    text=re.sub(r'\n\s*'+str(i-6)+r'\s*$', '', text)
    parts.append(text)
text='\n'.join(parts)
text=unicodedata.normalize('NFC',text)
text=re.sub(r'(?<=\w)-[ \t]*\n[ \t]*(?=\w)', '', text)
text=''.join(c for c in text if unicodedata.category(c)!='Co')
text=re.sub(r'\s+', ' ', text).strip()
Path('.codex-tmp/thesis_body_extracted.txt').write_text(text,encoding='utf-8')
print(json.dumps({'with_spaces':len(text),'without_spaces':len(re.sub(r'\s','',text)),'start':text[:160],'end':text[-240:],'printed_pages':'1-42','pdf_pages':'8-49'},ensure_ascii=False,indent=2))
