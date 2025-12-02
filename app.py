import streamlit as st
import os
import sys
import re
import time
import json
from pyswip import Prolog
import google.generativeai as genai

# Adiciona a pasta atual ao path para importar módulos
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from coleta import Lista_Usuario

# ================= CONFIGURAÇÕES =================
st.set_page_config(page_title="Recomendador de Jogos", page_icon="🎮", layout="wide")

# Caminhos
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.join(ROOT_DIR, "base")
KEY_FILE = os.path.join(ROOT_DIR, "key.json")

def carregar_categorias():
    arquivo = os.path.join(BASE_DIR, "todas_categorias.txt")
    if os.path.exists(arquivo):
        with open(arquivo, "r", encoding="utf-8") as f:
            return sorted([line.strip() for line in f.readlines()])
    return []

def get_gemini_key():
    if os.path.exists(KEY_FILE):
        try:
            with open(KEY_FILE, 'r') as f:
                data = json.load(f)
                # Tenta pegar gemini_api_key
                key = data.get("gemini_api_key")
                if not key:
                    # Fallback de aviso
                    st.warning("Aviso: 'gemini_api_key' não encontrada no JSON.")
                return key
        except json.JSONDecodeError:
            st.error("Erro: O arquivo key.json está mal formatado.")
        except Exception as e:
            st.error(f"Erro ao ler key.json: {e}")
    else:
        st.error(f"Arquivo não encontrado: {KEY_FILE}")
    return None

# ================= LÓGICA PROLOG =================
def buscar_prolog(cats_selecionadas, preco_min, preco_max):
    prolog = Prolog()
    arq_temp = os.path.join(ROOT_DIR, "temp_kb.pl")
    
    try:
        conteudo_total = [":- encoding(utf8).", ":- dynamic possui/2.", ":- dynamic jogo/5."]
        
        # Leitura dos arquivos
        path_jogos = os.path.join(BASE_DIR, "jogos_populares.pl")
        path_lib = os.path.join(BASE_DIR, "biblioteca.pl")
        path_regras = os.path.join(BASE_DIR, "regras.pl")

        # Função de correção de sintaxe (Tags numéricas)
        def corrigir_sintaxe_tags(linha):
            match = re.search(r'\[(.*?)\]\)\.\s*$', linha)
            if match:
                tags = [t.strip() for t in match.group(1).split(',')]
                tags_fixed = [f"'{t}'" if t and t[0].isdigit() and not t.startswith("'") else t for t in tags]
                return linha.replace(match.group(1), ", ".join(tags_fixed))
            return linha

        if os.path.exists(path_jogos):
            with open(path_jogos, "r", encoding="utf-8") as f:
                conteudo_total.extend([corrigir_sintaxe_tags(line) for line in f])
        
        if os.path.exists(path_lib):
            with open(path_lib, "r", encoding="utf-8") as f:
                conteudo_total.append(f.read())

        if os.path.exists(path_regras):
            with open(path_regras, "r", encoding="utf-8") as f:
                conteudo_total.extend([l for l in f if ":- ensure_loaded" not in l and ":- consult" not in l])

        # Grava e Consulta
        arq_temp_fixed = arq_temp.replace("\\", "/")
        with open(arq_temp, "w", encoding="utf-8") as f:
            f.write("\n".join(conteudo_total))
        
        prolog.consult(arq_temp_fixed)
        
        # Query
        cats_str = "[" + ", ".join([f"'{c}'" for c in cats_selecionadas]) + "]"
        query = f"recomendar_top_5({cats_str}, {preco_min}, {preco_max}, Resultado)"
        result = list(prolog.query(query))
        
        try: os.remove(arq_temp)
        except: pass

        if result:
            return result[0]['Resultado']
        return []

    except Exception as e:
        try: os.remove(arq_temp)
        except: pass
        return []

# ================= LÓGICA IA (GEMINI) =================
def buscar_ia(cats_selecionadas, preco_min, preco_max, ignorar_lib):
    api_key = get_gemini_key()
    if not api_key:
        return "Erro: Chave Gemini não encontrada no key.json (adicione 'gemini_api_key')"

    # Configuração do Gemini
    try:
        genai.configure(api_key=api_key)
    except Exception as e:
        return f"Erro ao configurar Gemini: {e}"

    # 1. Lê a base de dados como texto para dar contexto
    contexto_jogos = ""
    path_jogos = os.path.join(BASE_DIR, "jogos_populares.pl")
    if os.path.exists(path_jogos):
        with open(path_jogos, "r", encoding="utf-8") as f:
            contexto_jogos = f.read()
    
    contexto_lib = ""
    if ignorar_lib:
        path_lib = os.path.join(BASE_DIR, "biblioteca.pl")
        if os.path.exists(path_lib):
            with open(path_lib, "r", encoding="utf-8") as f:
                contexto_lib = f.read()

    # 2. Monta o Prompt Completo
    prompt_completo = f"""
    Atue como um assistente especialista em recomendar jogos.
    
    INSTRUÇÕES:
    1. Use ESTRITAMENTE os dados fornecidos em formato Prolog abaixo na seção 'Contexto de Jogos Populares'.
    2. Não invente jogos que não estejam na lista.
    3. Filtre os jogos populares que se encaixam no preço (entre {preco_min} e {preco_max}) e nas categorias ({', '.join(cats_selecionadas)}).
    4. Remova os jogos que estão na lista 'Contexto de Jogos que o usuário JÁ POSSUI'.
    5. Escolha os 5 melhores dessa lista filtrada.
    6. Responda APENAS com uma lista formatada assim para cada jogo, separados por linha:
       Nome do Jogo | Preço | Tags Principais

    --- DADOS ---
    Contexto de Jogos Populares:
    {contexto_jogos}

    Contexto de Jogos que o usuário JÁ POSSUI (Não recomendar estes):
    {contexto_lib}
    """

    # 3. Descoberta Automática de Modelo (Resolve erro 404)
    try:
        # Lista todos os modelos disponíveis para sua chave
        modelos_disponiveis = []
        for m in genai.list_models():
            if 'generateContent' in m.supported_generation_methods:
                modelos_disponiveis.append(m.name)
        
        if not modelos_disponiveis:
            return "Erro: Sua chave de API não tem acesso a nenhum modelo de geração de texto."

        # Prioriza modelos 'flash' (mais rápidos) ou 'pro' (mais estáveis)
        modelo_escolhido = None
        
        # Tenta achar 'gemini-1.5-flash' ou similar
        for m in modelos_disponiveis:
            if 'flash' in m:
                modelo_escolhido = m
                break
        
        # Se não achar flash, tenta qualquer 'gemini'
        if not modelo_escolhido:
            for m in modelos_disponiveis:
                if 'gemini' in m:
                    modelo_escolhido = m
                    break
        
        # Fallback: pega o primeiro da lista
        if not modelo_escolhido:
            modelo_escolhido = modelos_disponiveis[0]

        # Executa com o modelo descoberto
        model = genai.GenerativeModel(modelo_escolhido)
        response = model.generate_content(prompt_completo)
        return response.text

    except Exception as e:
        return f"Erro Crítico na IA: {e}"

# ================= INTERFACE (UI) =================
st.title("Recomendador de Jogos: Prolog vs Gemini")
st.markdown("---")

# Filtros

st.header("Sua Biblioteca Steam")
usar_filtro = st.checkbox("Ignorar Jogos que já possuo")
steam_id = st.text_input("Steam ID:") 
if usar_filtro and st.button("Atualizar Biblioteca Local"):
    status = st.empty()
    # Container para capturar o total de jogos
    info_biblioteca = {"total": 0}

    def atualizar_progresso(atual, total, texto):
        info_biblioteca["total"] = total
        status.text(texto)

    Lista_Usuario.gerar_arquivo_biblioteca(steam_id, atualizar_progresso)
    status.success(f"Atualizado! {info_biblioteca['total']} jogos encontrados na biblioteca.")

#with col_pref:
st.header("O que você quer jogar?")
lista_cats = carregar_categorias()
if lista_cats:
    cats = st.multiselect("Categorias (Selecione no máximo 5):", options=lista_cats, max_selections=5)
else:
    st.warning("Nenhuma categoria carregada.")
    cats = []
st.header("Faixa de Preço")
c1, c2 = st.columns(2)
min_p = c1.number_input("Mínimo:", min_value=0.0, value=0.0, step = 10.0)
max_p = c2.number_input("Máximo:", min_value=0.0, value=200.0, step = 10.0 )

st.markdown("---")

if st.button("Me recomende jogos", type="primary", use_container_width=True):
    if not cats:
        st.error("Escolha pelo menos uma categoria.")
        st.stop()
    
    # Colunas de Resultado
    col_prolog, col_ia = st.columns(2)

    # --- EXECUÇÃO PROLOG ---
    with col_prolog:
        st.header("Prolog (Lógica Pura)")
        start_time = time.time()
        
        res_prolog = buscar_prolog(cats, min_p, max_p)
        
        end_time = time.time()
        tempo_prolog = end_time - start_time
        
        st.caption(f"⏱️ Tempo de Resposta: {tempo_prolog:.4f} segundos")
        
        if res_prolog:
            for item in res_prolog:
                # Parsing manual seguro
                nome, preco, tags = "Desconhecido", 0, ""
                if isinstance(item, str): # Fallback regex
                    m = re.search(r"item\((.+?),\s*([0-9\.]+),\s*.*,\s*\[(.*?)\]\)", item)
                    if m: 
                        nome = m.group(1).strip("'")
                        preco = float(m.group(2))
                        tags = m.group(3).replace("'", "")
                else: # Objeto PySwip
                    try:
                        nome = item.args[0]
                        if isinstance(nome, bytes): nome = nome.decode('utf-8')
                        preco = item.args[1]
                        tags = ", ".join([str(t.decode('utf-8') if isinstance(t, bytes) else t) for t in item.args[3]])
                    except: pass
                
                st.success(f"**{nome}**\nR$ {preco} | {tags}")
        else:
            st.warning("Nenhum resultado lógico encontrado.")

    # --- EXECUÇÃO IA ---
    with col_ia:
        st.header("Gemini (IA Generativa)")
        start_time = time.time()
        
        res_ai = buscar_ia(cats, min_p, max_p, usar_filtro)
        
        end_time = time.time()
        tempo_ia = end_time - start_time
        
        st.caption(f"⏱️ Tempo de Resposta: {tempo_ia:.4f} segundos")
        
        st.info(res_ai)