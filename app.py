import streamlit as st
import os
import sys
import re 
from pyswip import Prolog


sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from coleta import Lista_Usuario

st.set_page_config(page_title="Recomendador Steam Prolog", page_icon="🎮", layout="centered")

def carregar_categorias():
    arquivo = os.path.join("base", "todas_categorias.txt")
    if os.path.exists(arquivo):
        with open(arquivo, "r", encoding="utf-8") as f:
            return sorted([line.strip() for line in f.readlines()])
    return []

def buscar_recomendacoes_prolog(cats_selecionadas, preco_min, preco_max):
    prolog = Prolog()
    
    root_dir = os.path.dirname(os.path.abspath(__file__))
    base_dir = os.path.join(root_dir, "base")
    
    arq_jogos = os.path.join(base_dir, "jogos_populares.pl")
    arq_lib = os.path.join(base_dir, "biblioteca.pl")
    arq_regras = os.path.join(base_dir, "regras.pl")
    
    arq_temp = os.path.join(root_dir, "temp_kb.pl")
    
    try:
        conteudo_total = []
        
        conteudo_total.append(":- encoding(utf8).")
        conteudo_total.append(":- dynamic possui/2.")
        conteudo_total.append(":- dynamic jogo/5.")
        
        def corrigir_sintaxe_tags(linha):
            match = re.search(r'\[(.*?)\]\)\.\s*$', linha)
            if match:
                conteudo_tags = match.group(1)
                tags = conteudo_tags.split(',')
                tags_corrigidas = []
                for t in tags:
                    t = t.strip()
                    if t and t[0].isdigit() and not t.startswith("'"):
                        tags_corrigidas.append(f"'{t}'")
                    else:
                        tags_corrigidas.append(t)
                nova_lista = ", ".join(tags_corrigidas)
                return linha.replace(f"[{conteudo_tags}]", f"[{nova_lista}]")
            return linha

        if os.path.exists(arq_jogos):
            with open(arq_jogos, "r", encoding="utf-8") as f:
                for line in f:
                    line_fixed = corrigir_sintaxe_tags(line)
                    conteudo_total.append(line_fixed)
        else:
            st.error("Arquivo 'jogos_populares.pl' não encontrado. Rode o script de coleta.")
            return []
            
        if os.path.exists(arq_lib):
            with open(arq_lib, "r", encoding="utf-8") as f:
                conteudo_total.append(f.read())
                
        if os.path.exists(arq_regras):
            with open(arq_regras, "r", encoding="utf-8") as f:
                for line in f:
                    if ":- ensure_loaded" not in line and ":- consult" not in line:
                        conteudo_total.append(line)
        else:
            st.error("Arquivo 'regras.pl' não encontrado.")
            return []

        arq_temp_prolog = arq_temp.replace("\\", "/")
        
        with open(arq_temp, "w", encoding="utf-8") as f:
            f.write("\n\n".join(conteudo_total))
            
        prolog.consult(arq_temp_prolog)
        
        cats_formatadas = [f"'{c}'" for c in cats_selecionadas]
        cats_str = "[" + ", ".join(cats_formatadas) + "]"
        
        query = f"recomendar_top_5({cats_str}, {preco_min}, {preco_max}, Resultado)"
        
        # DEPURAÇÃO (Visível apenas se houver erro ou dúvida)
        # with st.expander("Debug (Ver dados enviados ao Prolog)"):
        #    st.code(f"Arquivo Temp: {arq_temp}\nQuery: {query}")
            
        result = list(prolog.query(query))
        
        try: os.remove(arq_temp)
        except: pass
            
        if result:
            return result[0]['Resultado']
        return []
        
    except Exception as e:
        st.error(f"Erro ao consultar Prolog: {e}")
        try: os.remove(arq_temp)
        except: pass
        return []

st.title("🎮 Recomendação de jogo por Prolog")
st.markdown("---")

st.header("1. Sua Biblioteca")
usar_filtro = st.checkbox("Ignorar jogos que eu já tenho")

steam_id = ""
if usar_filtro:
    col_input, col_btn = st.columns([3, 1])
    
    with col_input:
        steam_id = st.text_input("Steam ID:", placeholder="76561198...")
        
    with col_btn:
        st.write("") 
        st.write("")
        btn_testar = st.button("Testar")

    if btn_testar:
        if steam_id:
            ok, msg = Lista_Usuario.testar_conexao(steam_id)
            with col_input:
                if ok: st.success(msg)
                else: st.error(msg)
        else:
            with col_input:
                st.warning("Por favor, digite um ID para testar.")

st.markdown("---")

st.header("2. O que você quer jogar?")
lista_cats = carregar_categorias()

if not lista_cats:
    st.warning("Arquivo de categorias não encontrado. Rode o 'Gera_Lista.py' primeiro.")

categorias = st.multiselect(
    "Gêneros (Selecione no máximo 5):", 
    options=lista_cats,
    max_selections=5
)

st.markdown("---")

st.header("3. Orçamento")
col1, col2 = st.columns(2)
with col1: min_p = st.number_input("Mínimo (R$):", min_value=0.0, value=0.0 ,step=10.0)
with col2: max_p = st.number_input("Máximo (R$):", min_value=0.0, value=200.0, step=10.0)

st.markdown("---")

if st.button("Buscar Recomendações", type="primary", use_container_width=True):
    
    if not categorias:
        st.error("Selecione pelo menos uma categoria.")
        st.stop()

    if usar_filtro:
        if not steam_id:
            st.error("Insira seu Steam ID.")
            st.stop()
            
        status = st.empty()
        bar = st.progress(0)
        
        def update_bar(atual, total, txt):
            pct = int((atual/total)*100)
            bar.progress(pct)
            status.text(f"{txt} ({pct}%)")
            
        sucesso = Lista_Usuario.gerar_arquivo_biblioteca(steam_id, progress_callback=update_bar)
        
        if not sucesso:
            st.error("Falha ao baixar biblioteca. Verifique ID/Privacidade.")
            st.stop()
            
        status.empty()
        bar.empty()

    with st.spinner("Consultando o Oráculo Lógico..."):
        sugestoes = buscar_recomendacoes_prolog(categorias, min_p, max_p)
    
    if sugestoes:
        st.subheader("Jogos Recomendados:")
        
        for item in sugestoes:
            nome = ""
            preco = 0.0
            modo = ""
            tags = []

            if isinstance(item, str):
                match = re.search(r"item\((.+?),\s*([0-9\.]+),\s*([a-zA-Z0-9_]+),\s*\[(.*?)\]\)", item)
                if match:
                    raw_name = match.group(1)
                    if raw_name.startswith("'") and raw_name.endswith("'"):
                        nome = raw_name[1:-1]
                    else:
                        nome = raw_name
                    
                    preco = float(match.group(2))
                    modo = match.group(3).capitalize()
                    tags_str = match.group(4)
                    tags = [t.strip().strip("'") for t in tags_str.split(",") if t.strip()]
            else:
                try:
                    nome = item.args[0]
                    if isinstance(nome, bytes): nome = nome.decode('utf-8')
                    preco = item.args[1]
                    modo = str(item.args[2]).capitalize()
                    raw_tags = item.args[3]
                    for t in raw_tags:
                        if isinstance(t, bytes): tags.append(t.decode('utf-8'))
                        else: tags.append(str(t))
                except Exception as e:
                    st.error(f"Erro ao processar item: {e}")
                    continue
            
            if nome:
                with st.container():
                    st.info(f"**{nome}**")
                    c1, c2, c3 = st.columns(3)
                    c1.metric("Preço", f"R$ {preco:.2f}")
                    c2.metric("Modo", modo)
                    c3.write(f"Tags: {', '.join(tags)}")
    else:
        st.warning("Nenhum jogo encontrado com esses critérios.")
        st.markdown("""
        **Dicas:**
        - Tente selecionar menos categorias. O filtro exige **todas** elas simultaneamente.
        - Verifique se a faixa de preço é compatível.
        """)