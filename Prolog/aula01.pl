% fibonacci

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N, R) :- N2 is N-1, N3 is N-2, fibonacci(N2, R2), fibonacci(N3, R3), R is R2 + R3.

% fatorial

fatorial(0,0).
fatorial(1,1).
fatorial(N,R) :- N2 is N-1, fatorial(N2, R2), R is N * R2.






