#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <time.h>

int main() {
    int num_processos = 4;  // Quantidade de processos filhos
    pid_t pid;
    
    // Cria múltiplos processos filhos
    for (int i = 0; i < num_processos; i++) {
        pid = fork();

        if (pid == 0) {  // Código do processo filho
            srand(time(NULL) + getpid());  // Inicializa o gerador de números aleatórios
            int sleep_time = rand() % 10 + 1;  // Gera um tempo aleatório entre 1 e 10
            sleep(sleep_time);  // Faz o filho "dormir" pelo tempo gerado
            exit(sleep_time);   // O código de retorno será o tempo de sono
        }
    }

    // Código do processo pai
    for (int i = 0; i < num_processos; i++) {
        int status;
        pid_t child_pid = wait(&status);  // Espera o próximo filho terminar
        
        if (WIFEXITED(status)) {  // Verifica se o filho terminou normalmente
            int exit_status = WEXITSTATUS(status);  // Obtém o código de retorno do filho
            printf("No processo pai: processo filho: %d, status de saída do filho: %d\n", child_pid, exit_status);
        }
    }

    return 0;
}
