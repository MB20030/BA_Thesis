from pypdf import PdfReader
import re, unicodedata, json
pages=[p.extract_text() for p in PdfReader('Thesis/Thesis.pdf').pages]
start=next(i for i,t in enumerate(pages) if re.search(r'(?m)^\d+ Introduction\s*$',t))
end=next(i for i,t in enumerate(pages[start:],start) if re.search(r'(?m)^\d+ Bibliography\s*$',t))
parts=[]
for i in range(start,end):
    parts.append(re.sub(r'\n\s*'+str(i-start+1)+r'\s*$', '', pages[i]))
text=unicodedata.normalize('NFC','\n'.join(parts))
text=re.sub(r'(?<=\w)-[ \t]*\n[ \t]*(?=\w)', '', text)
text=''.join(c for c in text if unicodedata.category(c)!='Co')
text=re.sub(r'\s+', ' ', text).strip()
a=len(text); b=len(re.sub(r'\s','',text))
print(json.dumps({'with_spaces':a,'without_spaces':b,'decrease_with_spaces':114245-a,'decrease_without_spaces':97370-b,'printed_pages':f'1-{end-start}','start':text[:100],'end':text[-260:],'bibliography_start':pages[end][:100]},ensure_ascii=False,indent=2))
