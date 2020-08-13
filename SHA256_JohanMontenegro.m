%%
% Proyecto - Algoritmo_Cifrado_Integridad_SHA2

%% =========== SEMINARIO ACTUALIZACIÓN II ===========
%                 -- SEGURIDADES --
% Nombre:       JOHAN MARCELO MONTENEGRO ZAMBRANO
% Tema:         ALGORITMO DE CIFRADO SHA2 (SHA256)
% Fecha:        27 - julio - 2020
% ===================================================

%% Limpieza de todo el entorno:
close all     % Cerrar todas las ventanas Figure
clear all     % Limpia el Workspace
clc           % Limpia el CommandWindow


%%  ETAPA DE RELLENO DEL MENSAJE (EXTENSIÓN A MÚLTIPLO DE 512 BITS):
% MENSAJE:
% MENSAJE:
% 'Porque de tal manera amó Dios al mundo, que ha dado a su Hijo unigénito,
% para que todo aquel que en él cree, no se pierda, mas tenga vida eterna.'
% MENSAJE PARAFRASEADO Y SIN TILDES:
% mensaje = 'Porque de tal manera amo Dios al mundo, que ha dado a su unico Hijo, para que todo aquel que en El cree, no se pierda, sino que tenga vida eterna.' 
mensaje = 'Hola mundo'
% MENSAJE A HEXADECIMALES:
% Con dec2hex obtenemos una matriz en hexadecimales, en donde la cantidad
% de filas es la cantidad de caracteres, y 2 columnas (esto porque cada
% caracter es formado por 2 valores hexadecimales)
mensaje_hex = dec2hex(mensaje); % Pasamos el mensaje a hexadecimal
% NOTA: Los valores numéricos no son pasados de decimal a hexadecimal, sino
% de ASCII a hexadecimal. En otras palabras, los valores numéricos son
% vistos como caracteres, mas no como números. Por ejemplo, 1 en
% hexadecimal no debe ser transformado como 0x1, sino como 0x31.

% MENSAJE DE HEX A BINARIO (VECTOR)
% Por cada caracter deben generarse 8 bits. En este caso, por cada valor
% hexadecimal de cada columna se forman 4 bits. Por lo que obtendremos
% una nueva matriz, en donde el número de filas es la cantidad de
% caracteres, y el número de columnas es 2x4 = 8.
% Cuando hay ceros a la izquierda, desparecen. Para garantizar 8 bits en
% cada caracter, se especifica este valor en el segundo argumento:
mensaje_bin_matriz = hexToBinaryVector(mensaje_hex,8);

% REDIMENSIONAR LA MATRIZ A VECTOR (1 SOLA FILA):
% Primero, trasponemos para poder redimensionar correctamente (si no se
% traspone, colocará en otro orden los bits):
mensaje_bin_trasp = mensaje_bin_matriz';
% Ahora, redimensionamosMsj con reshape(matriz,#Filas,#Columnas)
% Queremos un vector de 1 fila por #caracteresx8 columnas:
mensaje_bin = reshape(mensaje_bin_trasp, 1, length(mensaje)*8);

% AGREGAR EL BIT "1" AL MENSAJE:
mensaje_bin_1 = [mensaje_bin 1]; % Concatenamos un 1 a la derecha

% AGREGAR "x" CEROS AL MENSAJE:
% Los ceros a agregar cumplen la siguiente ecuación:
%         x = 448 - 1 - longitud_del _mensaje_binario  (mod 512)
x = mod(448 - 1 -length(mensaje_bin),512);
% Creamos un vector de "x" ceros para luego concatenarlos al mensaje.
vect_x_ceros = zeros(1,x);
% Agregamos (concatenamos) los ceros al mensaje:
mensaje_bin_1_xceros = [mensaje_bin_1 vect_x_ceros];

% AGREGAR LA LONGITUD DEL MENSAJE EXPRESADO EN 64 BITS AL MENSAJE:
% pasamos la longitud del mensaje a hexadecimal. Como el mensaje está en
% caracteres, y cada caracter es interpretado en 8 bits, se multiplica la
% cantidad de caracteres por 8:
longitud_hex = dec2hex(length(mensaje)*8);
% pasamos esta longitud a binario (vector). La cantidad debe contener
% 64 bits, así que completamos si falta (segundo argumento):
longitud_64bits = hexToBinaryVector(longitud_hex,64);
% Agregamos (concatenamos) estos bits en el mensaje:
mensaje_bin_1_xceros_64bits = [mensaje_bin_1_xceros longitud_64bits];
% CON ESTE PROCESO HEMOS GARANTIZADO QUE EL VECTOR SEA MÚLTIPLO DE 512
%Pasamos este último arreglo a elementos binarios (logical):
mensaje_bin_1_xceros_64bits = logical(mensaje_bin_1_xceros_64bits);

%% INICIALIZACIÓN DE LOS VALORES HASH h:
%(primeros 32 bits de la parte decimal de las raíces cuadradas de
% los primeros números primos del intervalo [2,19]:
% hextToBinaryVector acepta 1 valor hexadecimal y se especifica el número
% de bits: hextToBinaryVector(hexadecimal,No_bits_a_imprimir)
% Devuelve un arreglo de elementos binarios correspondientes.
h0 = hexToBinaryVector('0x6a09e667',32);
h1 = hexToBinaryVector('0xbb67ae85',32);
h2 = hexToBinaryVector('0x3c6ef372',32);
h3 = hexToBinaryVector('0xa54ff53a',32);
h4 = hexToBinaryVector('0x510e527f',32);
h5 = hexToBinaryVector('0x9b05688c',32);
h6 = hexToBinaryVector('0x1f83d9ab',32);
h7 = hexToBinaryVector('0x5be0cd19',32);

%% INICIALIZACIÓN DEL ARRAY CON LAS CONSTANTES k DE LAS ITERACIONES:
 %(primeros 32 bits de la parte decimal de las raíces cúbicas de los 64 
 % primeros números primos del intervalo [2,311]):
k = zeros(64,32); % INICIALIZO K A CEROS
k(1,:) = hexToBinaryVector('0x428a2f98',32);
k(2,:) = hexToBinaryVector('0x71374491',32);
k(3,:) = hexToBinaryVector('0xb5c0fbcf',32);
k(4,:) = hexToBinaryVector('0xe9b5dba5',32);
k(5,:) = hexToBinaryVector('0x3956c25b',32);
k(6,:) = hexToBinaryVector('0x59f111f1',32);
k(7,:) = hexToBinaryVector('0x923f82a4',32);
k(8,:) = hexToBinaryVector('0xab1c5ed5',32);
k(9,:) = hexToBinaryVector('0xd807aa98',32);
k(10,:) = hexToBinaryVector('0x12835b01',32);
k(11,:) = hexToBinaryVector('0x243185be',32);
k(12,:) = hexToBinaryVector('0x550c7dc3',32);
k(13,:) = hexToBinaryVector('0x72be5d74',32);
k(14,:) = hexToBinaryVector('0x80deb1fe',32);
k(15,:) = hexToBinaryVector('0x9bdc06a7',32);
k(16,:) = hexToBinaryVector('0xc19bf174',32);
k(17,:) = hexToBinaryVector('0xe49b69c1',32);
k(18,:) = hexToBinaryVector('0xefbe4786',32);
k(19,:) = hexToBinaryVector('0x0fc19dc6',32);
k(20,:) = hexToBinaryVector('0x240ca1cc',32);
k(21,:) = hexToBinaryVector('0x2de92c6f',32);
k(22,:) = hexToBinaryVector('0x4a7484aa',32);
k(23,:) = hexToBinaryVector('0x5cb0a9dc',32);
k(24,:) = hexToBinaryVector('0x76f988da',32);
k(25,:) = hexToBinaryVector('0x983e5152',32);
k(26,:) = hexToBinaryVector('0xa831c66d',32);
k(27,:) = hexToBinaryVector('0xb00327c8',32);
k(28,:) = hexToBinaryVector('0xbf597fc7',32);
k(29,:) = hexToBinaryVector('0xc6e00bf3',32);
k(30,:) = hexToBinaryVector('0xd5a79147',32);
k(31,:) = hexToBinaryVector('0x06ca6351',32);
k(32,:) = hexToBinaryVector('0x14292967',32);
k(33,:) = hexToBinaryVector('0x27b70a85',32);
k(34,:) = hexToBinaryVector('0x2e1b2138',32);
k(35,:) = hexToBinaryVector('0x4d2c6dfc',32);
k(36,:) = hexToBinaryVector('0x53380d13',32);
k(37,:) = hexToBinaryVector('0x650a7354',32);
k(38,:) = hexToBinaryVector('0x766a0abb',32);
k(39,:) = hexToBinaryVector('0x81c2c92e',32);
k(40,:) = hexToBinaryVector('0x92722c85',32);
k(41,:) = hexToBinaryVector('0xa2bfe8a1',32);
k(42,:) = hexToBinaryVector('0xa81a664b',32);
k(43,:) = hexToBinaryVector('0xc24b8b70',32);
k(44,:) = hexToBinaryVector('0xc76c51a3',32);
k(45,:) = hexToBinaryVector('0xd192e819',32);
k(46,:) = hexToBinaryVector('0xd6990624',32);
k(47,:) = hexToBinaryVector('0xf40e3585',32);
k(48,:) = hexToBinaryVector('0x106aa070',32);
k(49,:) = hexToBinaryVector('0x19a4c116',32);
k(50,:) = hexToBinaryVector('0x1e376c08',32);
k(51,:) = hexToBinaryVector('0x2748774c',32);
k(52,:) = hexToBinaryVector('0x34b0bcb5',32);
k(53,:) = hexToBinaryVector('0x391c0cb3',32);
k(54,:) = hexToBinaryVector('0x4ed8aa4a',32);
k(55,:) = hexToBinaryVector('0x5b9cca4f',32);
k(56,:) = hexToBinaryVector('0x682e6ff3',32);
k(57,:) = hexToBinaryVector('0x748f82ee',32);
k(58,:) = hexToBinaryVector('0x78a5636f',32);
k(59,:) = hexToBinaryVector('0x84c87814',32);
k(60,:) = hexToBinaryVector('0x8cc70208',32);
k(61,:) = hexToBinaryVector('0x90befffa',32);
k(62,:) = hexToBinaryVector('0xa4506ceb',32);
k(63,:) = hexToBinaryVector('0xbef9a3f7',32);
k(64,:) = hexToBinaryVector('0xc67178f2',32);

% Transformamos los valores "k" en lógicos:
k = logical(k);

%% DIVIDIMOS EL MENSAJE EN PIEZAS DE 512 BITS
% Primero hallamos la cantidad de piezas a generarse:
No_piezas = length(mensaje_bin_1_xceros_64bits)/512;
% La lógica es la siguiente:
% - el primer vector ira de 1 a 512.
% - el segundo, de 513 a 1024
% - y así sucesivamente hasta alcanzar la longitud_mensaje_binario final.
% Armamos un vector "pieza", cada fila es una pieza que contiene 512
% elementos (columnas).
% Redimensionamos, pero el arreglo se ordena verticalmente, así que creamos
% la matriz al revés: 512 filas y No.columnas = No_piezas.
pieza = reshape(mensaje_bin_1_xceros_64bits,512,No_piezas);
% Trasponemos (para poner los valores horizontalmente):
pieza = pieza';


%%  LAZO PRINCIPAL SHA-256
%   PARA CADA PIEZA DE 512 BITS:

for i = 1:No_piezas
    % Creamos un arreglo "w" de 64 palabras (filas) de 32 bits (columnas)
    % inicializado en cero. w[1... 64]
    % Para esto, es mejor una matriz de 64 filas en las que se vayan
    % almacenando los elementos (vectores de 32 bits) de la pieza. Cada 
    % elemento debe ser de 32 bits (32 columnas = 1 palabra):
    w = zeros(64,32); % 64 palabras (filas) de 32 bits (columnas).
    
    % AGREGAR EL VALOR DE UNA PIEZA EN LOS PRIMEROS 16 VECTORES:
    % Almacenamos (repartimos) los 512 elementos en las 16 primeras
    % palabras (vectores binarios) del arreglo w: w[1 ... 16]
    p = 0;  % Inicializamos "p" para el conteo.
    for n = 1:16  % para cada uno de las 16 primeras palabras (vectores binarios)
        for m = 1 : 32 % para cada uno de los 32 bits de cada palabra
            % incrementa p en 1 cada vez, para avanzar al siguiente bit.
            p = p + 1;
            %Cargamos el valor de pieza(i,p) en w(n,m)
            w(n,m) = pieza(i,p);
        end
    end
    % pasamos "w" de tipo double a tipo logical:
    w = logical(w);
    
    % EXPANDIMOS LAS PRIMERAS 16 PALABRAS HASTA COMPLETAR LAS 48 PALABRAS
    % SIGUIENTES DEL ARREGLO w: w[17 ... 64]
    %     Para "n" desde 17 a 64, en cada iteración se realiza (pseudocódigo):
    %         w[n] = s1(w[n-2]) + w[n-7] + s0(w[n-15]) + w[n-16]
    for n = 17:64
        % La suma "+" es en realidad suma de módulo 2^32:
        w(n,:) = sumaMod2_32(sumaMod2_32(sumaMod2_32(s1(w(n-2,:)),w(n-7,:)),s0(w(n-15,:))),w(n-16,:));
    end
    
    % INICIALIZAMOS LOS VARIABLES DE TRABAJO:
    % Inicialización de las variables de trabajo con los valores hash actuales:
    a = h0;
    b = h1;
    c = h2;
    d = h3;
    e = h4;
    f = h5;
    g = h6;
    h = h7;
    
    % COMPRESIÓN:
    % Bucle principal de la función de compresión:
    % Pseudocódigo:
        % Se calcula: Ch(e; f; g), Maj(a; b; c), S0(a), S1(e), and Wn
        % temp1 := h + S1(e) + Ch(e; f; g) + Kn +Wn
        % temp2 := S0(a) + Maj(a; b; c)
        % h := g
        % g := f
        % f := e
        % e := d + temp1
        % d := c
        % c := b
        % b := a
        % a := temp1 + temp2
    for n = 1:64
        temp1 = sumaMod2_32(sumaMod2_32(sumaMod2_32(sumaMod2_32(h,S1(e)),Ch(e,f,g)),k(n,:)),w(n,:));
        temp2 = sumaMod2_32(S0(a),Maj(a,b,c));
        h = g;
        g = f;
        f = e;
        e = sumaMod2_32(d, temp1);
        d = c;
        c = b;
        b = a;
        a = sumaMod2_32(temp1, temp2);
    end
    
    % Inserción del trozo comprimido al valor hash actual (pseudocódigo):
    %     h0 := h0 + a
    %     h1 := h1 + b
    %     h2 := h2 + c
    %     h3 := h3 + d
    %     h4 := h4 + e
    %     h5 := h5 + f
    %     h6 := h6 + g
    %     h7 := h7 + h
    h0 = sumaMod2_32(h0, a);
    h1 = sumaMod2_32(h1, b);
    h2 = sumaMod2_32(h2, c);
    h3 = sumaMod2_32(h3, d);
    h4 = sumaMod2_32(h4, e);
    h5 = sumaMod2_32(h5, f);
    h6 = sumaMod2_32(h6, g);
    h7 = sumaMod2_32(h7, h);
end

%%  CONCATENAMOS EN UN HASH FINAL: HASH-256
% HASH = h0(hex) h1(hex) h2(hex) h3(hex) h4(hex) h5(hex) h6(hex) h7(hex)
% binaryVectorToHex transforma arreglo de binario a hexadecimal:
HASH = binaryVectorToHex([h0 h1 h2 h3 h4 h5 h6 h7])


%% FUNCIONES:

% ==================================================
% FUNCIÓN Ch(x,y,z): realiza la siguiente operación lógica: 
% Ch(x; y; z) = (x AND y) XOR (¬ x AND z)
function [ch] = Ch(x,y,z)
    ch = xor(and(x,y),and(not(x),z));
    return
end

% ==================================================
% FUNCIÓN Maj(x,y,z): realiza la siguiente operación lógica:
% Maj(x; y; z) = (x AND y) XOR (x AND z) XOR (y AND z)
function [maj] = Maj(x,y,z)
    maj = xor(xor(and(x,y), and(x,z)), and(y,z));
    return
    % transformamos el vector de tipo double a lógico:
    maj = logical(maj);
end

% ==================================================
% FUNCIÓN SUMATORIO S0:
% Se define como:
% sumatorioS0(x) = (S2(x)) xor (S13(x)) xor (S22(x))
% Donde S = righrotate = circshift(arreglo, K_posiciones)
function [sumatorioS0] = S0(x)
    sumatorioS0 = xor(xor(circshift(x,2),circshift(x,13)),circshift(x,22));
    return
end
% ==================================================
% FUNCIÓN SUMATORIO S1:
% Se define como:
% sumatorioS1(x) = (S6(x)) xor (S11(x)) xor (S25(x))
% Donde S = righrotate = circshift(arreglo, K_posiciones)
function [sumatorioS1] = S1(x)
    sumatorioS1 = xor(xor(circshift(x,6),circshift(x,11)),circshift(x,25));
    return
end
% ==================================================
% FUNCIÓN SUMATORIO SIGMA s0:
% Se define como:
% sumatoriosigmas0(x) = (S7(x)) xor (S18(x)) xor (R3(x))
% Donde: S = rightrotate = circshift(arreglo, K_posiciones)
%        R = rightshift = roghtShift(arreglo, K_posiciones)
function [sumatoriosigmas0] = s0(x)
    sumatoriosigmas0 = xor(xor(circshift(x,7),circshift(x,18)),rightShift(x,3));
    return
end
% ==================================================
% FUNCIÓN SUMATORIO SIGMA s1:
% Se define como:
% sumatoriosigmas1(x) = (S7(x)) xor (S18(x)) xor (R3(x))
% Donde: S = rightrotate = circshift(arreglo, K_posiciones)
%        R = rightshift = roghtShift(arreglo, K_posiciones)
function [sumatoriosigmas1] = s1(x)
    sumatoriosigmas1 = xor(xor(circshift(x,17),circshift(x,19)),rightShift(x,10));
    return
end
% ==================================================
% FUNCIÓN rightShift(arreglo, K_desplazamientos):
% Acepta un arreglo y K. Desplaza los bits K espacios, ingresando ceros
% a la derecha para completar el arreglo, y elimina los últimos K bits de
% la derecha:
function [rightshift] = rightShift(arreglo, K) % K = No. desplazamientos.
    % Inicializamos rightshit con el valor del arreglo:
    rightshift = arreglo;
    % Ingresamos un contador t (lazo) que itere K veces, para agregar
    % K ceros a la izquierda y eliminar K elementos de la derecha:
    for t = 1:K
        rightshift = [0 rightshift]; % Agregamos K ceros a la izquierda.
        rightshift(:,end) = [];  % Eliminamos K elementos de la derecha.
    end
    % transformamos el vector de tipo double a lógico:
    rightshift = logical(rightshift);
    return
end

% ==================================================
% FUNCIÓN SUMA 2^32: La suma definida como "+" en las fórmulas.
% function [SumaMod2_32] = sumaMod2_32(x,y)
function [suma] = sumaMod2_32(x,y)
    % Unimos los dígitos binarios y pasamos a decimal:
    % - Usamos la función unirDigitosStr creada en otra sección más abajo:
    A = bin2dec(unirDigitosStr(x));
    B = bin2dec(unirDigitosStr(y));
    % Sumamos ambos valores en módulo 2^32 (2^32 = 4294967296):
    sumamod232 = mod(A+B,4294967296);
    % Almacenamos el resultado en un vector binario:
    % - Pasamos primero a hexadecimal:
    sumamod232_hex = dec2hex(sumamod232);
    % - de hexadecimal pasamos a un vector binario (tipo lógico):
    suma = hexToBinaryVector(sumamod232_hex,32);
    return
end
% ==================================================
% UNIRDIGITOSSTR: Función para unir los digitos de un arreglo y pasarlos a
% string. Acepta un vector de números como argumento, y une los dígitos de
% dicho arreglo.
function digitosUnidosStr = unirDigitosStr(vector)
    format longG; %Cambiamos el formato para 
    for i = 1:length(vector)% Para que opere con cada elemento del vector.
       % Agregamos el caracter convertido en string al vector
       digitosUnidosStr(1,i) = num2str(vector(1,i));
    end
    return
end