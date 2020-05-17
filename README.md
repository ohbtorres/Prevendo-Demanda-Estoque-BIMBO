# Prevendo Demanda de Estoque do grupo BIMBO
Projeto com o objetivo de prever com precisão a demanda de estoque com base nos dados históricos de vendas do grupo BIMBO, baseado na competição do Kaggle: https://www.kaggle.com/c/grupo-bimbo-inventory-demand

# Notas

Há vários produtos na base de teste que não existem na base de treino. Este comportamento é esperado, já que sempre existem novos produtos sendo vendidos todos os dias. O modelo deve ser sensível a essa peculiariedade.

Existem  valores duplicados de Cliente_ID na tabela cliente_tabla, o que significa que um Cliente_ID deve possuir vários NombreCliente que são similares. Isto ocorre, pois NombreCliente é um dado não padronizado, então cabe a quem estiver manipulando os dados, decidir como limpar e usar a informação.

O ajuste de demanda (Demanda_uni_equil) será sempre maior que zero, já que a demanda sempre deve ser um valor positivo. A razão pela qual Venta_uni_hoy - Dev_uni_proxima pode resultar em valores negativos, é que os registros de retorno às vezes duram algumas semanas.

# Descrição dos arquivos
train.csv — dados de treino

test.csv — dados de teste

cliente_tabla.csv — nomes dos clientes (pode ser unido com os arquivos train/test a partir de Cliente_ID)

producto_tabla.csv — nomes dos produtos (pode ser unido com os arquivos train/test a partir de Producto_ID)

town_state.csv — cidade e estado (pode ser unido com os arquivos train/test a partir de Agencia_ID)

PopMexico2015 - informações sobre a população do México de 2015 (pode ser unido com os arquivos train/test a partir de State)

# Campos

Semana (de terça a quarta, em número)

Agencia_ID — Identificador da agência

Canal_ID — Identificador do canal de vendas

Ruta_SAK — ID da rota (Várias rotas = vários depósitos)

Cliente_ID — Cliente ID

NombreCliente — Nome do Cliente

Producto_ID — Produto ID

NombreProducto — Nome do Produto

Venta_uni_hoy — Unidades do produto vendidas na semana (inteiro)

Venta_hoy — Vendas da semana, em pesos

Dev_uni_proxima — Retorno da próxima semana (Inteiro)

Dev_proxima — Valor do retorno da próxima semana, em pesos

Demanda_uni_equil — Demanda (inteiro) (Variável Alvo)
