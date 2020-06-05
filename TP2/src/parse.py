import sys
import pandas as pd
from ftfy import fix_encoding
from math import sqrt
import unidecode

def fixerino(row, field):
    if row[field] == row[field]:
        a = unidecode.unidecode(fix_encoding(row[field]))
        return "'{}'".format(a.strip().replace("'", ''))
    else:
        return 'undefined'

def parse_list(s):
    if ',' in str(s):
            return ','.join(filter(None, str(s).split(',')))
    else:
        return s

#paragem(gid,latitude,longitude,Estado de Conservacao,Tipo de Abrigo,Abrigo com Publicidade?,Operadora,[Carreira],Codigo de Rua,Nome da Rua,Freguesia').
def parse_paragens():
    xl = pd.read_excel('paragem_autocarros_oeiras_processado_4.xlsx')
    for index, row in xl.iterrows():
        print("paragem({0},{1},{2},{3},{4},{5},{6},[{7}],{8},{9},{10}).".format(
            row['gid'],
            row['latitude'],
            row['longitude'],
            fixerino(row,'Estado de Conservacao'),
            fixerino(row, 'Tipo de Abrigo'),
            fixerino(row, 'Abrigo com Publicidade?'),
            fixerino(row,'Operadora'),
            parse_list(row['Carreira']),
            row['Codigo de Rua'],
            fixerino(row, 'Nome da Rua'),
            fixerino(row, 'Freguesia')
        ))

def distancia(row, n_row):
    x1 = row['latitude']
    y1 = row['longitude']
    x2 = n_row['latitude']
    y2 = n_row['longitude']

    d = sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    if d == d:
        return d
    else:
        return 'undefined'

# edge(gid_row, gid_next_row, carreira, distancia).
def parse_graph():
    xl = pd.ExcelFile('lista_adjacencias_paragens.xlsx')
    for sheet in xl.sheet_names:
        book = xl.parse(sheet)
        for i in range(book.shape[0]-1):
            org =  book.iloc[i]['gid']
            dest = book.iloc[i + 1]['gid']
            if org != dest:
                print("edge({0},{1},{2},{3}).".format(
                    org,
                    dest,
                    book.iloc[i]['Carreira'],
                    distancia(book.iloc[i], book.iloc[i+1])
                ))

def main():
    if len(sys.argv) == 2 and '--stops' in sys.argv[1].lower():
        parse_paragens()
    elif len(sys.argv) == 2 and '--edges' in sys.argv[1].lower():
        parse_graph()
    else:
        print('USAGE: parse [--stops|--edges]')

main()
