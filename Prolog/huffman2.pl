/* 
- Lê o arquivo inicial (in.txt) onde estará o texto para leitura 
*/
ler_arquivo(Arquivo, Texto) :-
    open(Arquivo, read, Fluxo),
    get_code(Fluxo, C1),
    ler_arquivo_codigos(Fluxo, C1, Codigos),
    close(Fluxo),
    string_codes(Texto, Codigos).

ler_arquivo_codigos(_, -1, []) :- !.
ler_arquivo_codigos(Fluxo, Codigo, [Codigo|Codigos]) :-
    get_code(Fluxo, ProximoCodigo),
    ler_arquivo_codigos(Fluxo, ProximoCodigo, Codigos).

/* 
- Lê o conteúdo que está dentro do in.txt 
*/
amostra(Texto) :-
    ler_arquivo('in.txt', Texto).

/* 
- Escreve no arquivo as informações desejadas 
*/
escrever_arquivo(Arquivo, Texto) :-
    open(Arquivo, write, Fluxo),
    escrever_codigos(Fluxo, Texto),
    close(Fluxo).

escrever_codigos(_, []).
escrever_codigos(Fluxo, [Codigo|Codigos]) :-
    put_code(Fluxo, Codigo),
    escrever_codigos(Fluxo, Codigos).

escrever_no_arquivo(Arquivo, Texto) :-
    open(Arquivo, write, Fluxo),
    write_term(Fluxo, Texto, [quoted(true)]),
    close(Fluxo).

/* 
- Calcula a frequência de caracteres em um texto e retorna uma lista de pares [Caractere, Vezes que o caractere se repetiu] 
*/
frequencia_letra(Texto, Frequencia) :-
    frequencia_letra(Texto, [], Frequencia).
frequencia_letra([], Acc, Acc).
frequencia_letra([UElemento|Texto], Acc, Frequencia) :-
    frequencia_auxilio(Texto, UElemento, 1, Contagem, RemTexto),
    frequencia_letra(RemTexto, [[UElemento, Contagem]|Acc], Frequencia).

frequencia_auxilio([Elemento|Texto], Elemento, Acc, Contagem, RemTexto) :-
    Acc1 is Acc+1,
    frequencia_auxilio(Texto, Elemento, Acc1, Contagem, RemTexto).
frequencia_auxilio(RemTexto, _, Acc, Acc, RemTexto).

/*
- Converte uma lista de frequências de caracteres em uma lista de folhas de uma árvore 
*/
frequencia_para_folhas(Frequencia, Folhas) :-
    frequencia_para_folhas(Frequencia, [], Folhas).
frequencia_para_folhas([], Acc, Acc).
frequencia_para_folhas([[Letra, Contagem]|Frequencia], Acc, Folhas) :-
    frequencia_para_folhas(Frequencia, [arvore(folha, Letra, Contagem, na, na)|Acc], Folhas).

construir_arvore([Huffman], Huffman).
construir_arvore([arvore(Tipo1, Letra1, Frequencia1, Esquerda1, Direita1), arvore(Tipo2, Letra2, Frequencia2, Esquerda2, Direita2)|Folhas], Arvore) :-
    SomaFrequencia is Frequencia1+Frequencia2,
    construir_arvore([arvore(nodo, na, SomaFrequencia, arvore(Tipo1, Letra1, Frequencia1, Esquerda1, Direita1), arvore(Tipo2, Letra2, Frequencia2, Esquerda2, Direita2))|Folhas], Arvore).

/*
- Gera códigos de Huffman para os caracteres com base na árvore de Huffman.
*/
codigo_huffman(Arvore, Codigos) :-
    codigo_huffman(Arvore, [], Codigos).
codigo_huffman(na, Acc, Acc).
codigo_huffman(arvore(folha, Letra, _, na, na), Acc, Codigos) :-
    reverse(Acc, Acc1), % Para obter o código Huffman na ordem correta
    codigo_huffman(na, [Letra, Acc1], Codigos).
codigo_huffman(arvore(nodo, na, _, Esquerda, _), Acc, Codigos) :-
    codigo_huffman(Esquerda, [0|Acc], Codigos).
codigo_huffman(arvore(nodo, na, _, _, Direita), Acc, Codigos) :-
    codigo_huffman(Direita, [1|Acc], Codigos).

/*
- Codifica um texto ASCII usando os códigos de Huffman fornecidos.
*/
codificar(Texto, CodigoHuffman, Codificado) :-
    reverse(Texto, TextoReverso),
    codificar(TextoReverso, CodigoHuffman, [], Codificado).
codificar([], _, Acc, Acc).
codificar([Letra|Texto], CodigoHuffman, Acc, Codificado) :-
    buscar_letra(Letra, CodigoHuffman, Codigo),
    append(Codigo, Acc, Acc1),
    codificar(Texto, CodigoHuffman, Acc1, Codificado).

/*
- Procura um caractere ou seu código de Huffman em uma lista de pares letra-código.
*/
buscar_letra(Letra, [[Letra, Codigo]|_], Codigo).
buscar_letra(Letra, [_|CodigoHuffman], Codigo) :-
    buscar_letra(Letra, CodigoHuffman, Codigo).

/*
- Executa um teste completo, codificando e decodificando um texto de exemplo.
*/
huffman :-
    amostra(Texto),
    string_to_list(Texto, ASCII),
    msort(ASCII, ASCIIOrdenado), % Não remove duplicatas

    frequencia_letra(ASCIIOrdenado, Frequencia),
    frequencia_para_folhas(Frequencia, Folhas),
    sort(3, @=<, Folhas, FolhasOrdenadas), % Ordenar as folhas por frequência

    % Caso especial: se houver apenas um caractere
    (length(FolhasOrdenadas, 1) -> 
        % Para o caso de um único caractere, gerar um código padrão
        FolhasOrdenadas = [arvore(folha, Letra, _, na, na)], 
        CodigoHuffman = [[Letra, [0]]]
    ;   
        construir_arvore(FolhasOrdenadas, Arvore),
        findall(Codigos, codigo_huffman(Arvore, Codigos), CodigoHuffman)
    ),
    
    % Codificar o texto
    codificar(ASCII, CodigoHuffman, Codificado),

    escrever_no_arquivo('out.txt', Codificado), % Escrever texto codificado no arquivo

    true.
