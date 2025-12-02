import requests
import time
import re
import os  


DIR_ATUAL = os.path.dirname(os.path.abspath(__file__))


DIR_BASE = os.path.join(DIR_ATUAL, "..", "base")


os.makedirs(DIR_BASE, exist_ok=True)

LIMIT = 500
OUTPUT_FILE = os.path.join(DIR_BASE, "jogos_populares.pl")
OUTPUT_CATS = os.path.join(DIR_BASE, "todas_categorias.txt")




IGNORED_TAGS = {
    'acesso_antecipado', 'gratuitos_para_jogar', 'indie', 
    'um_jogador', 'multijogador', 'co_op', 'pvp', 
    'analise_muito_positiva'
}


MAX_TAGS_PER_GAME = 8 

HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
}
COOKIES = {
    'birthtime': '631180801', 
    'lastagecheckage': '1-January-1990'
}

def clean_string(s):
    if not s: return "''"
    cleaned = s.replace("'", "\\'").replace('"', '')
    return f"'{cleaned}'"

def clean_atom(s):
    if not s: return None
    s = s.lower().strip()
    s = s.replace(' ', '_').replace('-', '_').replace('é', 'e').replace('ó', 'o').replace('ç', 'c')
    s = re.sub(r'[.,:;&\'"()\[\]/!]', '', s)
    return s

def get_top_ids(limit):
    target = limit * 2
    ids = []
    url = "https://store.steampowered.com/search/results/?query&start={}&count=50&dynamic_data=&sort_by=_ASC&snr=1_7_7_7000_7&filter=topsellers&infinite=1"
    start = 0
    while len(ids) < target:
        try:
            res = requests.get(url.format(start)).json()
            found = re.findall(r'data-ds-appid="(\d+)"', res.get('results_html', ''))
            for app_id in found:
                if app_id not in ids: ids.append(app_id)
            start += 50
            time.sleep(1)
        except: break
    return ids

def get_api_details(app_id):
    """Pega Preço, Nome e Tipo via API (mais confiável para números)"""
    try:
        url = f"https://store.steampowered.com/api/appdetails?appids={app_id}&l=brazilian"
        data = requests.get(url).json()
        if data and data[str(app_id)]['success']:
            return data[str(app_id)]['data']
    except: return None

def get_store_tags(app_id):
    url = f"https://store.steampowered.com/app/{app_id}/?l=brazilian"
    try:
        res = requests.get(url, headers=HEADERS, cookies=COOKIES)
        
        if res.status_code != 200: return []

        html = res.text
    
        tags_found = re.findall(r'class="app_tag"[^>]*>\s*([^<]+)\s*</a>', html)
        
        clean_tags = []
        for t in tags_found:
            atom = clean_atom(t)
            if atom and atom not in IGNORED_TAGS:
                clean_tags.append(atom)
                
        return clean_tags
    except Exception as e:
        print(f"Erro ao pegar tags HTML: {e}")
        return []

def main():
    potential_ids = get_top_ids(LIMIT)
    facts = []
    all_cats = set()
    count_validos = 0
    
    print(f"Coletando {LIMIT} jogos com Tags Precisas (HTML Parsing)...")

    for i, app_id in enumerate(potential_ids):
        if count_validos >= LIMIT: break

        api_data = get_api_details(app_id)
        
        if not api_data or api_data.get('type') != 'game':
            continue 


        tags_list = get_store_tags(app_id)
        
        if not tags_list:
            print(f"   (Aviso: Usando tags genéricas para ID {app_id})")
            for g in api_data.get('genres', []):
                tags_list.append(clean_atom(g['description']))

        tags_list = tags_list[:MAX_TAGS_PER_GAME]
        
        name = clean_string(api_data.get('name', 'Unknown'))
        
        price = 0.0
        if not api_data.get('is_free') and api_data.get('price_overview'):
            price = api_data.get('price_overview')['final'] / 100.0
            
        cat_ids = [c['id'] for c in api_data.get('categories', [])]
        mode = 'multiplayer' if 1 in cat_ids else ('singleplayer' if 2 in cat_ids else 'desconhecido')

        for t in tags_list: all_cats.add(t)
            
        genres_str = "[" + ", ".join(tags_list) + "]"
        
        fact = f"jogo({app_id}, {name}, {price}, {mode}, {genres_str})."
        facts.append(fact)
        count_validos += 1
        
        print(f"[{count_validos}/{LIMIT}] {api_data.get('name')} -> {tags_list}")
        
        time.sleep(1.6)

    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        f.write("% ID, Nome, Preco, Modo, [Tags_Da_Comunidade]\n")
        f.write("\n".join(facts))
        
    with open(OUTPUT_CATS, 'w', encoding='utf-8') as f:
        f.write("\n".join(sorted(all_cats)))
    
    print("\nProcesso finalizado!")

if __name__ == "__main__":
    main()