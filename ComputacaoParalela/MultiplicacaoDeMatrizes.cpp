{
  "nbformat": 4,
  "nbformat_minor": 0,
  "metadata": {
    "colab": {
      "provenance": []
    },
    "kernelspec": {
      "name": "python3",
      "display_name": "Python 3"
    },
    "language_info": {
      "name": "python"
    }
  },
  "cells": [
    {
      "cell_type": "markdown",
      "source": [
        "## Curso Intensivo de Threads em C++\n",
        "\n",
        "**Introdução:**\n",
        "\n",
        "Este curso intensivo visa apresentar os conceitos básicos de threads em C++ de forma concisa e prática. O objetivo é fornecer aos estudantes um entendimento sólido dos fundamentos de multithreading e suas aplicações na otimização de desempenho de programas.\n",
        "\n",
        "**O que são Threads?**\n",
        "\n",
        "Threads são unidades independentes de execução dentro de um processo. Elas compartilham a mesma memória e recursos do processo pai, mas podem executar código de forma independente. Em termos práticos, podemos visualizar threads como unidades de trabalho menores que permitem a execução simultânea de tarefas dentro de um programa.\n",
        "\n",
        "**Benefícios do Multithreading:**\n",
        "\n",
        "* **Melhor desempenho:** Permite a execução paralela de tarefas, reduzindo o tempo total de execução.\n",
        "* **Melhor utilização de recursos:** Permite que o CPU seja usado com maior eficiência, especialmente em sistemas multi-core.\n",
        "* **Melhor experiência do usuário:** Permite que programas respondam mais rapidamente e de forma mais fluida, mesmo quando executam tarefas intensivas.\n",
        "\n",
        "**Conceitos Fundamentais:**\n",
        "\n",
        "* **Criação de Threads:** A criação de threads em C++ é feita utilizando a classe `std::thread`.\n",
        "\n",
        "**Exemplo 1:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cthread\u003E\n",
        "\n",
        "void tarefa1() {\n",
        "  for (int i = 0; i \u003C 5; ++i) {\n",
        "    std::cout \u003C\u003C \"Tarefa 1: \" \u003C\u003C i \u003C\u003C std::endl;\n",
        "  }\n",
        "}\n",
        "\n",
        "void tarefa2() {\n",
        "  for (int i = 0; i \u003C 5; ++i) {\n",
        "    std::cout \u003C\u003C \"Tarefa 2: \" \u003C\u003C i \u003C\u003C std::endl;\n",
        "  }\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::thread t1(tarefa1);\n",
        "  std::thread t2(tarefa2);\n",
        "\n",
        "  t1.join();  // Aguarda a thread t1 terminar\n",
        "  t2.join();  // Aguarda a thread t2 terminar\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, duas threads são criadas: `t1` e `t2`, executando as funções `tarefa1` e `tarefa2`, respectivamente. As threads são iniciadas simultaneamente e executam de forma independente, imprimindo seus resultados na tela. A função `join()` garante que o programa principal espere as threads terminarem antes de finalizar.\n",
        "\n",
        "**Exemplo 2:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cthread\u003E\n",
        "#include \u003Cvector\u003E\n",
        "\n",
        "void calculaSoma(int inicio, int fim, int &resultado) {\n",
        "  resultado = 0;\n",
        "  for (int i = inicio; i \u003C= fim; ++i) {\n",
        "    resultado += i;\n",
        "  }\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  int n = 1000; // Número de elementos a serem somados\n",
        "  int numThreads = 4; // Número de threads a serem usadas\n",
        "\n",
        "  std::vector\u003Cstd::thread\u003E threads;\n",
        "  std::vector\u003Cint\u003E resultados(numThreads);\n",
        "\n",
        "  // Cria as threads e distribui o trabalho\n",
        "  for (int i = 0; i \u003C numThreads; ++i) {\n",
        "    int inicio = i * (n / numThreads) + 1;\n",
        "    int fim = (i + 1) * (n / numThreads);\n",
        "    threads.push_back(std::thread(calculaSoma, inicio, fim, std::ref(resultados[i])));\n",
        "  }\n",
        "\n",
        "  // Aguarda as threads terminarem\n",
        "  for (auto &t : threads) {\n",
        "    t.join();\n",
        "  }\n",
        "\n",
        "  // Calcula a soma total\n",
        "  int somaTotal = 0;\n",
        "  for (int i = 0; i \u003C numThreads; ++i) {\n",
        "    somaTotal += resultados[i];\n",
        "  }\n",
        "\n",
        "  std::cout \u003C\u003C \"A soma total é: \" \u003C\u003C somaTotal \u003C\u003C std::endl;\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, o programa calcula a soma dos números de 1 a 1000 usando múltiplas threads. O trabalho é dividido entre as threads, cada uma calculando a soma de um subconjunto dos números. No final, as somas parciais são combinadas para obter a soma total.\n",
        "\n",
        "* **Execução de Threads:** Após a criação, a execução da thread é iniciada automaticamente.\n",
        "\n",
        "* **Sincronização de Threads:**  É crucial garantir a sincronização entre threads quando elas compartilham recursos. A falta de sincronização pode levar a condições de corrida e inconsistência de dados.\n",
        "\n",
        "**Sincronização de Threads:**\n",
        "\n",
        "* **Mutexes:** Um mutex (mutual exclusion) é um mecanismo de bloqueio que garante que apenas uma thread tenha acesso a um recurso compartilhado por vez, evitando conflitos simultâneos.\n",
        "\n",
        "**Exemplo 1:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cthread\u003E\n",
        "#include \u003Cmutex\u003E\n",
        "\n",
        "std::mutex mtx; // Declaração do mutex\n",
        "int contador = 0;\n",
        "\n",
        "void incrementaContador() {\n",
        "  for (int i = 0; i \u003C 10000; ++i) {\n",
        "    std::lock_guard\u003Cstd::mutex\u003E lock(mtx); // Bloqueia o mutex\n",
        "    contador++;\n",
        "  }\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::thread t1(incrementaContador);\n",
        "  std::thread t2(incrementaContador);\n",
        "\n",
        "  t1.join();\n",
        "  t2.join();\n",
        "\n",
        "  std::cout \u003C\u003C \"Contador: \" \u003C\u003C contador \u003C\u003C std::endl;\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, um mutex é usado para proteger o acesso à variável `contador`. A função `incrementaContador` incrementa o contador 10.000 vezes, mas antes de cada incremento, ela obtém um lock do mutex. Somente uma thread pode obter o lock por vez, garantindo que o contador seja incrementado corretamente mesmo com múltiplas threads executando a função simultaneamente.\n",
        "\n",
        "**Exemplo 2:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cthread\u003E\n",
        "#include \u003Cmutex\u003E\n",
        "#include \u003Crandom\u003E\n",
        "\n",
        "std::mutex mtx;\n",
        "std::vector\u003Cint\u003E dadosCompartilhados(10);\n",
        "\n",
        "void produtor(int id) {\n",
        "  std::random_device rd;\n",
        "  std::mt19937 gen(rd());\n",
        "  std::uniform_int_distribution\u003C\u003E distrib(1, 100);\n",
        "\n",
        "  for (int i = 0; i \u003C 10; ++i) {\n",
        "    std::lock_guard\u003Cstd::mutex\u003E lock(mtx);\n",
        "    dadosCompartilhados[i] = distrib(gen);\n",
        "    std::cout \u003C\u003C \"Produtor \" \u003C\u003C id \u003C\u003C \" gerou dado: \" \u003C\u003C dadosCompartilhados[i] \u003C\u003C std::endl;\n",
        "  }\n",
        "}\n",
        "\n",
        "void consumidor(int id) {\n",
        "  for (int i = 0; i \u003C 10; ++i) {\n",
        "    std::lock_guard\u003Cstd::mutex\u003E lock(mtx);\n",
        "    std::cout \u003C\u003C \"Consumidor \" \u003C\u003C id \u003C\u003C \" consumiu dado: \" \u003C\u003C dadosCompartilhados[i] \u003C\u003C std::endl;\n",
        "  }\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::thread produtor1(produtor, 1);\n",
        "  std::thread produtor2(produtor, 2);\n",
        "  std::thread consumidor1(consumidor, 1);\n",
        "  std::thread consumidor2(consumidor, 2);\n",
        "\n",
        "  produtor1.join();\n",
        "  produtor2.join();\n",
        "  consumidor1.join();\n",
        "  consumidor2.join();\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, temos dois produtores e dois consumidores acessando um vetor de dados compartilhado. O mutex garante que as threads não interfiram umas nas outras ao acessar e modificar o vetor, evitando inconsistência de dados.\n",
        "\n",
        "* **Condition Variables:** Uma condition variable é usada para notificar threads que estão esperando por um evento específico. Isso é útil quando uma thread precisa esperar que outra thread modifique um estado específico antes de prosseguir.\n",
        "\n",
        "**Exemplo 1:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cthread\u003E\n",
        "#include \u003Cmutex\u003E\n",
        "#include \u003Ccondition_variable\u003E\n",
        "\n",
        "std::mutex mtx;\n",
        "std::condition_variable cv;\n",
        "bool dadosProntos = false;\n",
        "\n",
        "void produtor() {\n",
        "  std::unique_lock\u003Cstd::mutex\u003E lock(mtx);\n",
        "  // Simula a produção de dados...\n",
        "  dadosProntos = true;\n",
        "  cv.notify_one(); // Notifica o consumidor\n",
        "}\n",
        "\n",
        "void consumidor() {\n",
        "  std::unique_lock\u003Cstd::mutex\u003E lock(mtx);\n",
        "  cv.wait(lock, []{ return dadosProntos; }); // Espera até que dadosProntos seja true\n",
        "  // Processa os dados...\n",
        "  std::cout \u003C\u003C \"Dados processados!\" \u003C\u003C std::endl;\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::thread prod(produtor);\n",
        "  std::thread cons(consumidor);\n",
        "\n",
        "  prod.join();\n",
        "  cons.join();\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, o produtor produz dados e notifica o consumidor através da condition variable `cv`. O consumidor espera que os dados estejam prontos usando a função `wait`. A função `wait` libera o lock do mutex e entra em um estado de espera até que a condição seja satisfeita. Quando o produtor notifica o consumidor, o lock é retomado e o consumidor pode processar os dados.\n",
        "\n",
        "**Exemplo 2:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cthread\u003E\n",
        "#include \u003Cmutex\u003E\n",
        "#include \u003Ccondition_variable\u003E\n",
        "#include \u003Cqueue\u003E\n",
        "\n",
        "std::mutex mtx;\n",
        "std::condition_variable cv;\n",
        "std::queue\u003Cint\u003E filaCompartilhada;\n",
        "\n",
        "void produtor() {\n",
        "  for (int i = 1; i \u003C= 10; ++i) {\n",
        "    std::unique_lock\u003Cstd::mutex\u003E lock(mtx);\n",
        "    filaCompartilhada.push(i);\n",
        "    std::cout \u003C\u003C \"Produtor adicionou \" \u003C\u003C i \u003C\u003C \" à fila.\" \u003C\u003C std::endl;\n",
        "    cv.notify_one();\n",
        "  }\n",
        "}\n",
        "\n",
        "void consumidor() {\n",
        "  while (true) {\n",
        "    std::unique_lock\u003Cstd::mutex\u003E lock(mtx);\n",
        "    cv.wait(lock, [] { return !filaCompartilhada.empty(); });\n",
        "    int valor = filaCompartilhada.front();\n",
        "    filaCompartilhada.pop();\n",
        "    std::cout \u003C\u003C \"Consumidor consumiu \" \u003C\u003C valor \u003C\u003C \" da fila.\" \u003C\u003C std::endl;\n",
        "  }\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::thread prod(produtor);\n",
        "  std::thread cons(consumidor);\n",
        "\n",
        "  prod.join();\n",
        "  cons.join();\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, temos um produtor que adiciona elementos a uma fila compartilhada e um consumidor que remove elementos da fila. A condition variable `cv` é usada para sincronizar o produtor e o consumidor. O produtor notifica o consumidor quando adiciona um novo elemento à fila. O consumidor, por sua vez, espera que a fila não esteja vazia antes de consumir um elemento.\n",
        "\n",
        "* **Futures:** Um future é um objeto que representa o resultado de uma tarefa assíncrona. Você pode usar futures para obter o resultado de uma thread que está executando uma tarefa separada.\n",
        "\n",
        "**Exemplo 1:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cfuture\u003E\n",
        "#include \u003Cthread\u003E\n",
        "\n",
        "int calcularSoma(int inicio, int fim) {\n",
        "  int soma = 0;\n",
        "  for (int i = inicio; i \u003C= fim; ++i) {\n",
        "    soma += i;\n",
        "  }\n",
        "  return soma;\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::future\u003Cint\u003E resultado = std::async(std::launch::async, calcularSoma, 1, 100);\n",
        "\n",
        "  // Faz outras tarefas...\n",
        "\n",
        "  int soma = resultado.get(); // Obtém o resultado da thread\n",
        "  std::cout \u003C\u003C \"A soma é: \" \u003C\u003C soma \u003C\u003C std::endl;\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, a função `calcularSoma` é executada em uma thread separada usando `std::async`. A variável `resultado` do tipo `std::future` armazena o resultado da thread. O programa principal pode então usar `resultado.get()` para obter o resultado da thread quando estiver disponível.\n",
        "\n",
        "**Exemplo 2:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cfuture\u003E\n",
        "#include \u003Cthread\u003E\n",
        "#include \u003Cvector\u003E\n",
        "\n",
        "int calcularFatorial(int n) {\n",
        "  if (n == 0) {\n",
        "    return 1;\n",
        "  }\n",
        "  return n * calcularFatorial(n - 1);\n",
        "}\n",
        "\n",
        "int main() {\n",
        "  std::vector\u003Cstd::future\u003Cint\u003E\u003E resultados;\n",
        "  std::vector\u003Cint\u003E numeros = {5, 10, 3, 7};\n",
        "\n",
        "  // Cria uma tarefa para cada número\n",
        "  for (int num : numeros) {\n",
        "    resultados.push_back(std::async(std::launch::async, calcularFatorial, num));\n",
        "  }\n",
        "\n",
        "  // Imprime os resultados\n",
        "  for (auto &fut : resultados) {\n",
        "    std::cout \u003C\u003C \"Fatorial de \" \u003C\u003C fut.get() \u003C\u003C \" é \" \u003C\u003C fut.get() \u003C\u003C std::endl;\n",
        "  }\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste exemplo, o programa calcula o fatorial de vários números de forma assíncrona. A função `calcularFatorial` é executada em threads separadas, e os resultados são armazenados em objetos `std::future`. O programa principal espera que as threads terminem e, em seguida, imprime os resultados.\n",
        "\n",
        "\n",
        "**Localidade Espacial do Cache e Desempenho:**\n",
        "\n",
        "A localidade espacial de dados ocorre quando elementos de dados próximos em termos de endereço de memória são acessados sequencialmente. Explorar a localidade espacial do cache pode significativamente melhorar o desempenho de um programa.\n",
        "\n",
        "**Hierarquia de Memória:**\n",
        "\n",
        "A CPU possui acesso direto à memória principal (RAM), mas este acesso é relativamente lento. Para acelerar o acesso à memória, a CPU usa uma hierarquia de memória, que inclui caches. Os caches são pequenos blocos de memória de acesso rápido que armazenam cópias de dados recentemente usados da memória principal.\n",
        "\n",
        "**Localidade:**\n",
        "\n",
        "Localidade refere-se à tendência de que um programa acesse dados que estão próximos em termos de tempo ou espaço. Existem dois tipos de localidade:\n",
        "\n",
        "**Localidade Temporal:** Um dado que é acessado em um momento é provavelmente acessado novamente no futuro próximo.\n",
        "**Localidade Espacial:** Dados próximos na memória são provavelmente acessados sequencialmente.\n",
        "\n",
        "**Explorar Localidade Espacial:**\n",
        "\n",
        "Explorar a localidade espacial do cache é crucial para otimizar o desempenho de programas. Ao organizar os dados na memória de forma a maximizar a localidade espacial, podemos reduzir o número de cache misses, acelerando o acesso à memória.\n",
        "\n",
        "\n",
        "**Exemplo: Multiplicação de Matriz por Vetor:**\n",
        "\n",
        "**1. Percorrendo a matriz por linha:**\n",
        "\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cvector\u003E\n",
        "#include \u003Cchrono\u003E\n",
        "\n",
        "int main() {\n",
        "  const int N = 1000;\n",
        "  std::vector\u003Cstd::vector\u003Cint\u003E\u003E matriz(N, std::vector\u003Cint\u003E(N, 1));\n",
        "  std::vector\u003Cint\u003E vetor(N, 1);\n",
        "  std::vector\u003Cint\u003E resultado(N, 0);\n",
        "\n",
        "  // Multiplicação de Matriz por Vetor (Não Otimizado)\n",
        "  auto inicio = std::chrono::high_resolution_clock::now();\n",
        "  for (int i = 0; i \u003C N; ++i) {\n",
        "    for (int j = 0; j \u003C N; ++j) {\n",
        "      resultado[i] += matriz[i][j] * vetor[j];\n",
        "    }\n",
        "  }\n",
        "  auto fim = std::chrono::high_resolution_clock::now();\n",
        "  auto duracao = std::chrono::duration_cast\u003Cstd::chrono::microseconds\u003E(fim - inicio);\n",
        "  std::cout \u003C\u003C \"Tempo de execução (não otimizado): \" \u003C\u003C duracao.count() \u003C\u003C \" microsegundos\" \u003C\u003C std::endl;\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "\n",
        "\n",
        "**2. Percorrendo a matriz por coluna:**\n",
        "\n",
        "\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cvector\u003E\n",
        "#include \u003Cchrono\u003E\n",
        "\n",
        "int main() {\n",
        "  const int N = 1000;\n",
        "  std::vector\u003Cstd::vector\u003Cint\u003E\u003E matriz(N, std::vector\u003Cint\u003E(N, 1));\n",
        "  std::vector\u003Cint\u003E vetor(N, 1);\n",
        "  std::vector\u003Cint\u003E resultado(N, 0);\n",
        "\n",
        "  // Multiplicação de Matriz por Vetor (Otimizado)\n",
        "  auto inicio = std::chrono::high_resolution_clock::now();\n",
        "  for (int j = 0; j \u003C N; ++j) {\n",
        "    for (int i = 0; i \u003C N; ++i) {\n",
        "      resultado[i] += matriz[i][j] * vetor[j];\n",
        "    }\n",
        "  }\n",
        "  auto fim = std::chrono::high_resolution_clock::now();\n",
        "  auto duracao = std::chrono::duration_cast\u003Cstd::chrono::microseconds\u003E(fim - inicio);\n",
        "  std::cout \u003C\u003C \"Tempo de execução (otimizado): \" \u003C\u003C duracao.count() \u003C\u003C \" microsegundos\" \u003C\u003C std::endl;\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "\n",
        "**3. Código Utilizando Blocagem:**\n",
        "\n",
        "```cpp\n",
        "#include \u003Ciostream\u003E\n",
        "#include \u003Cvector\u003E\n",
        "#include \u003Cchrono\u003E\n",
        "\n",
        "int main() {\n",
        "  const int N = 1000;\n",
        "  const int BLOCK_SIZE = 16; // Tamanho do bloco\n",
        "  std::vector\u003Cstd::vector\u003Cint\u003E\u003E matriz(N, std::vector\u003Cint\u003E(N, 1));\n",
        "  std::vector\u003Cint\u003E vetor(N, 1);\n",
        "  std::vector\u003Cint\u003E resultado(N, 0);\n",
        "\n",
        "  // Multiplicação de Matriz por Vetor (Com Blocagem)\n",
        "  auto inicio = std::chrono::high_resolution_clock::now();\n",
        "  for (int j = 0; j \u003C N; j += BLOCK_SIZE) {\n",
        "    for (int i = 0; i \u003C N; ++i) {\n",
        "      for (int k = j; k \u003C std::min(j + BLOCK_SIZE, N); ++k) {\n",
        "        resultado[i] += matriz[i][k] * vetor[k];\n",
        "      }\n",
        "    }\n",
        "  }\n",
        "  auto fim = std::chrono::high_resolution_clock::now();\n",
        "  auto duracao = std::chrono::duration_cast\u003Cstd::chrono::microseconds\u003E(fim - inicio);\n",
        "  std::cout \u003C\u003C \"Tempo de execução (blocagem): \" \u003C\u003C duracao.count() \u003C\u003C \" microsegundos\" \u003C\u003C std::endl;\n",
        "\n",
        "  return 0;\n",
        "}\n",
        "```\n",
        "\n",
        "Neste código, a matriz é processada em blocos de tamanho `BLOCK_SIZE`. A blocagem aumenta a localidade espacial do cache ao garantir que elementos próximos sejam acessados sequencialmente. O tamanho do bloco deve ser escolhido de forma a otimizar o uso do cache.\n",
        "\n",
        "**Desabilitando Otimizações do Compilador:**\n",
        "\n",
        "Para medir o tempo de execução real do código, é necessário desabilitar otimizações do compilador que podem afetar o desempenho. Isso pode ser feito usando flags de compilação:\n",
        "\n",
        "**GCC:**\n",
        "\n",
        "* `-O0`  Desabilita todas as otimizações.\n",
        "* `-fno-tree-vectorize`  Desabilita a vetorização automática.\n",
        "* `-fno-inline`  Desabilita a inlining de funções.\n",
        "\n",
        "**Clang:**\n",
        "\n",
        "* `-O0`  Desabilita todas as otimizações.\n",
        "* `-fno-tree-vectorize`  Desabilita a vetorização automática.\n",
        "* `-fno-inline`  Desabilita a inlining de funções.\n",
        "\n",
        "**Exemplo de Compilação com GCC:**\n",
        "\n",
        "```bash\n",
        "g++ -O0 -fno-tree-vectorize -fno-inline multiplica_matriz.cpp -o multiplica_matriz\n",
        "```\n",
        "\n",
        "\n",
        "\n",
        "**Comparando PThreads e Threads C++:**\n",
        "\n",
        "* **PThreads:** Uma biblioteca de threads POSIX (Portable Operating System Interface) disponível em vários sistemas operacionais Unix-like.\n",
        "* **Threads C++:** Recursos de threading integrados na linguagem C++ a partir do C++11.\n",
        "\n",
        "**Vantagens e Desvantagens:**\n",
        "\n",
        "| Feature | PThreads | Threads C++ |\n",
        "|---|---|---|\n",
        "| **Portabilidade:** | Alta | Alta, mas pode variar entre compiladores |\n",
        "| **Facilidade de uso:** | Mais complexo, requer código mais detalhado | Mais simples e intuitivo, com recursos de alto nível |\n",
        "| **Desempenho:** | Geralmente mais rápido | Geralmente mais lento, mas a diferença diminui com a otimização do compilador |\n",
        "| **Recursos:** |  | Mais recursos modernos, como `std::future`, `std::promise` e `std::packaged_task` |\n",
        "| **Integração com C++:** |  | Integração nativa com a linguagem |\n",
        "\n",
        "**PThreads:**\n",
        "\n",
        "* **Vantagens:**\n",
        "    * Portabilidade para vários sistemas operacionais.\n",
        "    * Maior controle sobre o gerenciamento de threads.\n",
        "* **Desvantagens:**\n",
        "    * Código mais complexo e verboso.\n",
        "    * Menos recursos modernos comparado ao Threads C++.\n",
        "\n",
        "**Threads C++:**\n",
        "\n",
        "* **Vantagens:**\n",
        "    * Mais fácil de usar e integrar com o código C++.\n",
        "    * Recursos modernos para simplificar o desenvolvimento concorrente.\n",
        "* **Desvantagens:**\n",
        "    * Menos portabilidade entre compiladores, dependendo da versão C++ e das implementações.\n",
        "    * Potencialmente mais lento que PThreads em alguns casos.\n",
        "\n",
        "**Conclusão:**\n",
        "\n",
        "Este curso intensivo apresentou os conceitos básicos de threads em C++, incluindo criação, execução e sincronização, além de uma análise comparativa com PThreads e uma introdução à exploração da localidade espacial do cache. As informações aqui fornecidas servem como base para iniciar sua jornada no desenvolvimento de programas concorrentes em C++. É fundamental praticar e explorar recursos adicionais para dominar o poder do multithreading e otimizar o desempenho de seus programas.\n",
        "\n",
        "\n",
        "\n"
      ],
      "metadata": {
        "id": "kypUEeGzNf7W"
      }
    },
    {
      "cell_type": "markdown",
      "source": [
        "## Para entregar\n",
        "\n",
        "1. **Crie dois executáveis para a parte de multiplicação de matrizes: um que percorre em ordem de linha e outro que percorre em ordem de coluna.**\n",
        "\n",
        "   Faça agora a multiplicação de matrizes considerando:  \n",
        "   \n",
        "   $A * B = C$, onde:\n",
        "\n",
        "  1. Dimensões de A: M x N\n",
        "\n",
        "  2. Dimensões de B: N x X (qq dimensão razoável, mas não um vetor)\n",
        "\n",
        "  3. Consequentemente, dimensões de C: M x X\n",
        "\n",
        "  se quiser, pode utilizar matrizes quadradas.\n",
        "\n",
        "2. **Crie um terceiro executável para utilizar corretamente o cache (hierarquia de memória)**:\n",
        "\n",
        "   Para utilizar corretamente o cache (L1, L2) utilizando a abordagem de **blocagem** apresentada no [artigo da Intel](https://www.intel.com/content/www/us/en/developer/articles/technical/putting-your-data-and-code-in-order-optimization-and-memory-part-1.html).\n",
        "\n",
        "   Você deve fazer a análise da velocidade do seu algoritmo compilando o código da seguinte forma:\n",
        "\n",
        "  a. desligando todas as otimizações do compilador, como indicado acima.\n",
        "\n",
        "  b. ligando a otimização máxima.\n",
        "\n",
        "   Para cada opção, você deve medir o tempo de execução, seguindo o exemplo mostrado nos trechos de código acima.\n",
        "\n",
        "3. **Valgrind**\n",
        "\n",
        "  Faça também uma análise do padrão de acesso ao cache de todas as versões utilizando o utilitário `valgrind`. Veja abaixo um exemplo de saída que o **`valgrind`** dá:\n",
        "\n",
        "```\n",
        "$ valgrind --tool=cachegrind ./matmulcol\n",
        "==23104== Cachegrind, a cache and branch-prediction profiler\n",
        "==23104== Copyright (C) 2002-2015, and GNU GPL'd, by Nicholas Nethercote et al.\n",
        "==23104== Using Valgrind-3.11.0 and LibVEX; rerun with -h for copyright info\n",
        "==23104== Command: ./matmulcol\n",
        "==23104==\n",
        "--23104-- warning: L3 cache found, using its data for the LL simulation.\n",
        "Column order; tempo para multiplicar coluna: 0.085526 secs\n",
        "==23104==\n",
        "==23104== I   refs:      17,994,092\n",
        "==23104== I1  misses:         1,051\n",
        "==23104== LLi misses:         1,040\n",
        "==23104== I1  miss rate:       0.01%\n",
        "==23104== LLi miss rate:       0.01%\n",
        "==23104==\n",
        "==23104== D   refs:      12,641,331  (7,385,328 rd   + 5,256,003 wr)\n",
        "==23104== D1  misses:     1,117,327  (    2,580 rd   + 1,114,747 wr)\n",
        "==23104== LLd misses:       134,009  (    2,378 rd   +   131,631 wr)\n",
        "==23104== D1  miss rate:        8.8% (      0.0%     +      21.2%  )\n",
        "==23104== LLd miss rate:        1.1% (      0.0%     +       2.5%  )\n",
        "==23104==\n",
        "==23104== LL refs:        1,118,378  (    3,631 rd   + 1,114,747 wr)\n",
        "==23104== LL misses:        135,049  (    3,418 rd   +   131,631 wr)\n",
        "==23104== LL miss rate:         0.4% (      0.0%     +       2.5%  )\n",
        "\n",
        "\n",
        "$ valgrind --tool=cachegrind ./matmulrow\n",
        "==23161== Cachegrind, a cache and branch-prediction profiler\n",
        "==23161== Copyright (C) 2002-2015, and GNU GPL'd, by Nicholas Nethercote et al.\n",
        "==23161== Using Valgrind-3.11.0 and LibVEX; rerun with -h for copyright info\n",
        "==23161== Command: ./matmulrow\n",
        "==23161==\n",
        "--23161-- warning: L3 cache found, using its data for the LL simulation.\n",
        "Row order; tempo para multiplicar: 0.053935 secs\n",
        "==23161==\n",
        "==23161== I   refs:      17,993,582\n",
        "==23161== I1  misses:         1,049\n",
        "==23161== LLi misses:         1,038\n",
        "==23161== I1  miss rate:       0.01%\n",
        "==23161== LLi miss rate:       0.01%\n",
        "==23161==\n",
        "==23161== D   refs:      12,641,128  (7,385,188 rd   + 5,255,940 wr)\n",
        "==23161== D1  misses:       134,287  (    2,579 rd   +   131,708 wr)\n",
        "==23161== LLd misses:       134,009  (    2,378 rd   +   131,631 wr)\n",
        "==23161== D1  miss rate:        1.1% (      0.0%     +       2.5%  )\n",
        "==23161== LLd miss rate:        1.1% (      0.0%     +       2.5%  )\n",
        "==23161==\n",
        "==23161== LL refs:          135,336  (    3,628 rd   +   131,708 wr)\n",
        "==23161== LL misses:        135,047  (    3,416 rd   +   131,631 wr)\n",
        "==23161== LL miss rate:         0.4% (      0.0%     +       2.5%  )\n",
        "```\n",
        "\n",
        "## Entregas\n",
        "\n",
        "1. Você deve entregar um código fonte para cada versão do algoritmo implementado.\n",
        "2. Você deve entregar um relatório em PDF com as evidências de todas as **compilações** e **execuções** dos códigos.\n",
        "3. Você deve fazer uma análise da saída do **`valgrind`**  para o **item 3** discutindo os resultados."
      ],
      "metadata": {
        "id": "EwhiTFmYXrwX"
      }
    },
    {
      "cell_type": "code",
      "source": [],
      "metadata": {
        "id": "5plufhV-bHI4"
      },
      "execution_count": null,
      "outputs": []
    }
  ]
}