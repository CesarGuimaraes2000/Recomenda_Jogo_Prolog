import requests
import json
import os
import sys


DIR_ATUAL = os.path.dirname(os.path.abspath(__file__))
DIR_RAIZ = os.path.join(DIR_ATUAL, "..") 
DIR_BASE = os.path.join(DIR_ATUAL, "..", "base")
CONFIG_FILE = os.path.join(DIR_RAIZ, "key.json") 
OUTPUT_FILE = os.path.join(DIR_BASE, "biblioteca.pl")

os.makedirs(DIR_BASE, exist_ok=True)

def load_api_key():
    if not os.path.exists(CONFIG_FILE):
        print(f"ERRO: Arquivo '{CONFIG_FILE}' não encontrado na raiz.")
        return None
        
    try:
        with open(CONFIG_FILE, 'r') as f:
            config = json.load(f)
        return config.get("steam_api_key")
    except Exception as e:
        print(f"Erro ao ler key.json: {e}")
        return None

def clean_string(s):
    if not s: return "''"
    cleaned = s.replace("'", "\\'").replace('"', '')
    return f"'{cleaned}'"

def get_library_data_fast(api_key, steam_id):
    url = f"http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key={api_key}&steamid={steam_id}&format=json&include_appinfo=1"
    try:
        res = requests.get(url, timeout=10)
 
        if res.status_code in [401, 403]:
            return None
            
        data = res.json()
        if 'response' in data and 'games' in data['response']:
            return data['response']['games']
        
        if 'response' in data and not data['response']:
            return []
            
    except Exception as e:
        print(f"Erro de conexão: {e}")
        return None
    return None

def testar_conexao(steam_id):
    api_key = load_api_key()
    if not api_key:
        return False, "Erro: key.json não configurado."
        
    games = get_library_data_fast(api_key, steam_id)
    
    if games is None:
        return False, "Falha: Perfil Privado, ID incorreto ou Key inválida."
    
    count = len(games)
    return True, f"Sucesso! {count} itens encontrados."

def gerar_arquivo_biblioteca(steam_id, progress_callback=None):
    api_key = load_api_key()
    games = get_library_data_fast(api_key, steam_id)
    
    if games is None:
        return False

    facts = []
    total = len(games)
    
    if progress_callback:
        progress_callback(total // 2, total, "Baixado! Formatando dados...")

    for game in games:
        app_id = game.get('appid')
        name = game.get('name', 'Unknown')
        
        if app_id:
            clean_name = clean_string(name)
            fact = f"possui({app_id}, {clean_name})."
            facts.append(fact)

    try:
        with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
            f.write(f"% Biblioteca rapida do usuario {steam_id}\n")
            f.write(f"% Total de itens: {total}\n")
            f.write("% Estrutura: possui(ID, 'Nome').\n\n")
            for fact in facts:
                f.write(fact + "\n")
        
        if progress_callback:
            progress_callback(total, total, "Concluído!")
            
        return True
    except Exception as e:
        print(f"Erro ao salvar arquivo: {e}")
        return False
