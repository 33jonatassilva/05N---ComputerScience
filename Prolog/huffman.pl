:- use_module(library(lists)).

% Entrada e saída de arquivos
huffman_encode :-
    % Ler o texto de entrada
    read_file_to_string('in.txt', InputText, []),
    atom_chars(InputText, Chars),
    write('Texto lido:\n'), write(InputText), nl, nl,

    % Gerar tabela de frequência
    frequency_table(Chars, FreqTable),
    write('Tabela de Frequência:\n'), write(FreqTable), nl, nl,

    % Construir a árvore de Huffman
    (build_huffman_tree(FreqTable, HuffmanTree) ->
        (write('Árvore de Huffman:\n'), write(HuffmanTree), nl, nl);
        (write('Erro ao construir a árvore de Huffman.'), fail)),

    % Gerar os códigos
    (generate_codes(HuffmanTree, [], Codes) ->
        (write('Códigos de Huffman:\n'), write(Codes), nl, nl);
        (write('Erro ao gerar os códigos de Huffman.'), fail)),

    % Codificar o texto
    (encode_text(Chars, Codes, EncodedText) ->
        (write('Texto codificado:\n'), write(EncodedText), nl, nl);
        (write('Erro ao codificar o texto.'), fail)),

    % Salvar o texto codificado no arquivo de saída
    (open('out.txt', write, Stream) ->
        (write(Stream, EncodedText), close(Stream));
        (write('Erro: Não foi possível abrir o arquivo de saída.'), fail)),

    % Finalizar com sucesso
    true.

% Frequência de caracteres
frequency_table(Chars, FreqTable) :-
    sort(Chars, UniqueChars),
    findall(Char-Freq, 
            (member(Char, UniqueChars), 
             count_occurrences(Char, Chars, Freq)), 
            FreqTable).

count_occurrences(Char, Chars, Count) :-
    include(=(Char), Chars, Filtered),
    length(Filtered, Count).

% Construção da árvore de Huffman
build_huffman_tree(FreqTable, HuffmanTree) :-
    % Cria a fila de prioridade (nós com frequência e caracteres)
    create_priority_queue(FreqTable, PriorityQueue),
    write('Fila de prioridade inicial:\n'), write(PriorityQueue), nl, nl,

    % Chama a construção da árvore
    construct_tree(PriorityQueue, HuffmanTree).

create_priority_queue(FreqTable, PriorityQueue) :-
    findall(node(Freq, Char), member(Char-Freq, FreqTable), PriorityQueue).

construct_tree([Tree], Tree).  % Caso base: A árvore final é o único nó restante
construct_tree(PriorityQueue, Tree) :-
    % Ordena a fila pela frequência
    sort(1, @=<, PriorityQueue, SortedQueue),
    write('Fila de prioridade ordenada:\n'), write(SortedQueue), nl,

    % Pega os dois primeiros nós da fila
    SortedQueue = [node(F1, L1), node(F2, L2) | Rest],
    
    % Cria um novo nó combinando os dois nós
    F is F1 + F2,
    NewNode = node(F, node(F1, L1), node(F2, L2)),
    write('Combinando nós:\n'), write(node(F1, L1)), write(' e '), write(node(F2, L2)), nl,
    write('Novo nó combinado: '), write(NewNode), nl,

    % Coloca o novo nó combinado de volta na fila e reordena
    merge_nodes(NewNode, Rest, MergedQueue),
    sort(1, @=<, MergedQueue, SortedMergedQueue),

    % Chama recursivamente para combinar os nós restantes
    construct_tree(SortedMergedQueue, Tree).

% Merge os nós
merge_nodes(NewNode, Queue, MergedQueue) :-
    append([NewNode], Queue, TempQueue),
    sort(1, @=<, TempQueue, MergedQueue).

% Geração de códigos de Huffman
generate_codes(node(_, Char), Code, [Char-Code]).
generate_codes(node(_, Left, Right), Code, Codes) :-
    generate_codes(Left, [0 | Code], LeftCodes),
    generate_codes(Right, [1 | Code], RightCodes),
    append(LeftCodes, RightCodes, Codes).

% Codificação do texto
encode_text([], _, []).
encode_text([Char | Chars], Codes, EncodedText) :-
    member(Char-Code, Codes),
    append(Code, Rest, EncodedText),
    encode_text(Chars, Codes, Rest).
