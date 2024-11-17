

% Felipe Gyotoku Koike - 10409640
% Bruno Viana Tripoli Barbosa  - 10409547
% Jônatas de Brito Silva - 10403674





/* 
- Função para ler o conteúdo de um arquivo e convertê-lo em uma lista de caracteres (códigos ASCII).
*/


read_file(File, Content) :-
    open(File, read, Stream),
    get_code(Stream, Char1),
    read_file_aux(Stream, Char1, Codes),
    close(Stream),
    string_codes(Content, Codes).

read_file_aux(_, -1, []).  % Condição de parada: fim do arquivo.
read_file_aux(Stream, Code, [Code|Codes]) :-
    get_code(Stream, NextChar),  % Lê o próximo caractere do arquivo.
    read_file_aux(Stream, NextChar, Codes).

/* 
- Função para ler o arquivo de amostra (in.txt) e retornar o texto como uma string.
*/


sample(Text) :-
    read_file('in.txt', Text).

/* 
- Função para escrever o conteúdo codificado no arquivo sem vírgulas e colchetes.
*/


write_file(File, Text) :-
    open(File, write, Stream),
    write_without_commas_and_brackets(Stream, Text),
    close(Stream).

write_without_commas_and_brackets(_, []).  % Condição de parada: fim da lista.
write_without_commas_and_brackets(Stream, [Code|Codes]) :-
    write(Stream, Code),  % Escreve o código sem vírgulas ou colchetes.
    write_without_commas_and_brackets(Stream, Codes).

/* 
- Função para calcular a frequência dos caracteres presentes no texto
  e retornar uma lista com o caractere e sua contagem de ocorrências.
*/


char_frequency(Text, Frequency) :-
    char_frequency(Text, [], Frequency).

char_frequency([], Acc, Acc).  % Condição de parada: texto vazio, retorna o acumulador.
char_frequency([Char|Text], Acc, Frequency) :-
    count_occurrence(Text, Char, 1, Count, RemainingText),
    char_frequency(RemainingText, [[Char, Count]|Acc], Frequency).

count_occurrence([Element|Text], Element, Counter, Count, RemainingText) :-
    NewCounter is Counter + 1,  % Incrementa o contador.
    count_occurrence(Text, Element, NewCounter, Count, RemainingText).
count_occurrence(RemainingText, _, Counter, Counter, RemainingText).  % Quando o caractere não é mais encontrado.

/* 
- Converte a lista de frequências em uma lista de folhas da árvore de Huffman.
*/


frequency_to_leaves(Frequency, Leaves) :-
    frequency_to_leaves(Frequency, [], Leaves).

frequency_to_leaves([], Acc, Acc).  % Condição de parada: não há mais frequências.
frequency_to_leaves([[Letter, Freq]|Rest], Acc, Leaves) :-
    frequency_to_leaves(Rest, [tree(leaf, Letter, Freq, na, na)|Acc], Leaves).

/* 
- Constrói a árvore de Huffman a partir da lista de folhas.
*/


build_tree([FinalTree], FinalTree).  % Quando resta apenas uma árvore, ela é a final.
build_tree([tree(Type1, Letter1, Freq1, Left1, Right1), tree(Type2, Letter2, Freq2, Left2, Right2)|Rest], FinalTree) :-
    TotalFreq is Freq1 + Freq2,  % Soma as frequências dos dois primeiros elementos.
    build_tree([tree(node, na, TotalFreq, tree(Type1, Letter1, Freq1, Left1, Right1), tree(Type2, Letter2, Freq2, Left2, Right2))|Rest], FinalTree).

/* 
- Gera os códigos de Huffman para os caracteres a partir da árvore de Huffman construída.
*/


huffman_code(Tree, Codes) :-
    huffman_code(Tree, [], Codes).

huffman_code(na, Acc, Acc).  % Condição de parada: nó 'na' (nulo), retorna o acumulador.

huffman_code(tree(leaf, Letter, _, na, na), Acc, Codes) :-  % Quando encontramos um nó folha
    reverse(Acc, Code),  % Inverte o acumulador para obter o código correto.
    huffman_code(na, [Letter, Code], Codes).

huffman_code(tree(node, na, _, Left, _), Acc, Codes) :-  % Se o nó tiver um filho à esquerda
    huffman_code(Left, [0|Acc], Codes).

huffman_code(tree(node, na, _, _, Right), Acc, Codes) :-  % Se o nó tiver um filho à direita
    huffman_code(Right, [1|Acc], Codes).

/* 
- Codifica um texto (representado por uma lista de caracteres ASCII) utilizando os códigos de Huffman fornecidos.
*/


encode(Text, HuffmanCode, EncodedText) :-
    reverse(Text, ReversedText),  % Inverte o texto para processar da direita para a esquerda.
    encode(ReversedText, HuffmanCode, [], EncodedText).

encode([], _, Acc, Acc).  % Condição de parada: quando não houver mais nada para codificar.

encode([Letter|Text], HuffmanCode, Acc, EncodedText) :-
    find_code(Letter, HuffmanCode, Code),  % Encontra o código de Huffman para o caractere.
    append(Code, Acc, NewAcc),  % Adiciona o código ao acumulador.
    encode(Text, HuffmanCode, NewAcc, EncodedText).

/* 
- Busca o código de Huffman correspondente a um caractere em uma lista de pares letra-código.
*/
find_code(Letter, [[Letter, Code]|_], Code).  % Quando encontra, retorna o código.
find_code(Letter, [_|HuffmanCode], Code) :-  % Se não encontrar, continua a busca.
    find_code(Letter, HuffmanCode, Code).

/* 
- Função para escrever o texto codificado no arquivo sem vírgulas ou colchetes.
*/
write_encoded_without_commas_and_brackets(File, EncodedText) :-
    open(File, write, Stream),
    write_without_commas_and_brackets(Stream, EncodedText),
    close(Stream).

/* 
- Função principal que realiza o processo de codificação e escreve o resultado no arquivo de saída.
*/
huffman :-
    sample(Text),  % Lê o texto do arquivo de entrada.
    string_to_list(Text, CharList),  % Converte o texto para uma lista de códigos ASCII.
    msort(CharList, SortedList),  % Ordena a lista de caracteres em ordem crescente.

    char_frequency(SortedList, Frequency),  % Calcula a frequência dos caracteres.
    frequency_to_leaves(Frequency, Leaves),  % Converte a frequência em folhas da árvore de Huffman.
    sort(3, @=<, Leaves, SortedLeaves),  % Ordena as folhas por frequência (em ordem crescente).

    % Caso especial: se houver apenas um caractere, cria um código padrão.
    (length(SortedLeaves, 1) -> 
        % Para um único caractere, gera um código padrão.
        SortedLeaves = [tree(leaf, Letter, _, na, na)], 
        HuffmanCode = [[Letter, [0]]]
    ;   
        build_tree(SortedLeaves, Tree),  % Constrói a árvore de Huffman a partir das folhas.
        findall(Codes, huffman_code(Tree, Codes), HuffmanCode)
    ),
    
    encode(CharList, HuffmanCode, EncodedText),  % Codifica o texto utilizando a árvore de Huffman.

    write_encoded_without_commas_and_brackets('out.txt', EncodedText),  % Escreve o texto codificado no arquivo de saída.

    true.
