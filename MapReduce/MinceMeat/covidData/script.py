import mincemeat
import csv


def load_file():
    f = open('dataset\\covidData.txt','r')
    try:
        return f.read().splitlines()
    finally:
        f.close()

        
lines = load_file()

source = dict(enumerate(lines))


def mapfn(k, v):
    print('map ', k)

    dados_Covid = v.split(',')
    data_Evento = dados_Covid[0]
    pais_Evento = dados_Covid[2]

    novos_Casos = int(dados_Covid[4])
    novos_Obitos = int(dados_Covid[6])

    value = pais_Evento + "|" + str(novos_Casos) + "|" + str(novos_Obitos)

    yield data_Evento, value

def reducefn(k, v):
    print('reduce ',k)
    
    maior_Casos = 0
    maior_Obitos = 0
    pais_Casos = ''
    pais_Obitos = ''

    for value in v:
        campos = str(value).split('|')
        if int(campos[1]) > maior_Casos:
            maior_Casos = int(campos[1])
            pais_Casos = campos[0]
        
        if int(campos[2]) > maior_Obitos:
            maior_Obitos = int(campos[2])
            pais_Obitos = campos[0]

    strSaida = "Casos: " + str(maior_Casos) + " em " + pais_Casos + ". "
    strSaida += "Obitos: " + str(maior_Obitos) + " em " + pais_Obitos + "."

    
    return strSaida

s = mincemeat.Server()
print("rodando servidor!")
# The data source can be any dictionary-like object
s.datasource = source
s.mapfn = mapfn
s.reducefn = reducefn

results = s.run_server(password="changeme")

with open('RESULT.txt', 'w') as f:
    for k, v in results.items():
        f.write(f'{k} -> {v}\n')
