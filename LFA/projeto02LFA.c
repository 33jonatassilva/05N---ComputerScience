#include<stdio.h>
#include<stdlib.h>
#include<string.h>  /* Incluindo a biblioteca para usar strcspn */


// Felipe Gyotoku Koike - 10409640
// Bruno Viana Tripoli Barbosa  - 10409547
// Jônatas de Brito Silva - 10403674

char lookahead;   /* Excepcionalmente variavel global */

/* Declaracoes antecipadas das funcoes */
int match(char t, char palavra[], int *pos);
int E(char palavra[], int *pos);
int S(char palavra[], int *pos);
int K(char palavra[], int *pos);
int T(char palavra[], int *pos);
int Z(char palavra[], int *pos);
int F(char palavra[], int *pos);
int D(char palavra[], int *pos);

/* 
 * Regras da gramática:
 * E -> S$
 * S -> T K
 * K -> + T K
 * K -> - T K
 * K -> e (palavra vazia)
 * T -> F Z
 * Z -> * F Z
 * Z -> / F Z
 * Z -> e (palavra vazia)
 * F -> (S)
 * F -> 1D | 2D | 3D | ... | 9D
 * D -> 0D | 1D | 2D | ... | 9D
 * D -> e (palavra vazia)
 */

int match(char t, char palavra[], int *pos){
	if (lookahead == t){
		lookahead = palavra[++(*pos)];
		return 1;
	}
	return 0;  
}

/* E -> S$ */
int E(char palavra[], int *pos){
    if (S(palavra, pos) && match('$', palavra, pos))
        return 1;
    return 0;
}

/* S -> T K */
int S(char palavra[], int *pos){
    if (T(palavra, pos) && K(palavra, pos))
        return 1;
    return 0;
}

/* K -> + T K | - T K | e */
int K(char palavra[], int *pos){
    if (lookahead == '+') {
        if (match('+', palavra, pos) && T(palavra, pos) && K(palavra, pos))
            return 1;
        return 0;
    }
    if (lookahead == '-') {
        if (match('-', palavra, pos) && T(palavra, pos) && K(palavra, pos))
            return 1;
        return 0;
    }
    return 1;  /* Caso com palavra vazia */
}

/* T -> F Z */
int T(char palavra[], int *pos){
    if (F(palavra, pos) && Z(palavra, pos))
        return 1;
    return 0;
}

/* Z -> * F Z | / F Z | e */
int Z(char palavra[], int *pos){
    if (lookahead == '*') {
        if (match('*', palavra, pos) && F(palavra, pos) && Z(palavra, pos))
            return 1;
        return 0;
    }
    if (lookahead == '/') {
        if (match('/', palavra, pos) && F(palavra, pos) && Z(palavra, pos))
            return 1;
        return 0;
    }
    return 1;  /* Caso com palavra vazia */
}

/* F -> (S) | 1D | 2D | 3D | ... | 9D */
int F(char palavra[], int *pos){
    if (lookahead == '(') {
        if (match('(', palavra, pos) && S(palavra, pos) && match(')', palavra, pos))
            return 1;
        return 0;
    }
    if (lookahead >= '1' && lookahead <= '9') {
        if (D(palavra, pos))
            return 1;
        return 0;
    }
    return 0;  /* Caso invalido */
}

/* D -> 0D | 1D | 2D | ... | 9D | e */
int D(char palavra[], int *pos){
    if (lookahead >= '0' && lookahead <= '9') {
        match(lookahead, palavra, pos);  /* Consome o digito */
        return D(palavra, pos);  /* Chama recursivamente */
    }
    return 1;  /* Caso com palavra vazia */
}

void trataErro(char palavra[], int pos){
    printf("\n\nERRO DE SINTAXE\n");
    printf("Erro na posicao %d da palavra '%s'\n", pos, palavra);
    printf("Caractere '%c' inesperado encontrado.\n", palavra[pos]);
    printf("Sugestao: Verifique a gramatica da expressao e tente corrigir a estrutura.\n");
}

int main(){
    FILE *entrada;
    char palavra[1000];  /* Buffer para armazenar a palavra lida */
    int pos = 0;
    
    /* Abrir o arquivo de entrada */
    entrada = fopen("entrada.txt", "r");
    if (entrada == NULL) {
        printf("Erro ao abrir o arquivo!\n");
        return 1;  /* Sai do programa se o arquivo nao for encontrado */
    }
    
    /* Ler o conteudo do arquivo */
    fgets(palavra, sizeof(palavra), entrada);
    fclose(entrada);
    
    /* Remover a nova linha que pode ser lida pelo fgets */
    palavra[strcspn(palavra, "\n")] = '\0';
    
    /* Inicializa lookahead com o primeiro caractere da palavra */
    lookahead = palavra[pos];
    
    /* Processa a palavra */
    if (E(palavra, &pos))
        printf("\nPalavra '%s' reconhecida\n\n", palavra);
    else
        trataErro(palavra, pos);
    
    
    system("PAUSE");
    return 0;
}

