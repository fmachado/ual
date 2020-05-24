**************************************************************

              UAL - UNESA ALGORITHMIC LANGUAGE

                      Desenvolvimento: 
                 Adriana Sayuri Spallanzani
                 Andrea Teixeira de Medeiros
                   Juarez Muylaert Filho
**************************************************************

1. LICENCA
2. INTRODUCAO
3. REQUISITOS NECESSARIOS
4. INSTALACAO
   4.1 INTERPRETADOR SIMPLES
   4.2 INTERPRETADOR ANIMADO
   4.3 EDITOR UAL
5. ACESSO AO SISTEMA
6. CREDITOS
7. DUVIDAS E SUGESTOES


1. LICENCA
--------------------------------------------------------------
Ao instalar e utilizar este software, voce estara confirmando 
a aceitacao do software e dos termos desta licenca. Se nao 
concordar com estes termos, nao conclua o processo de
instalacao.

Com o intuito de difundir para a comunidade academica o
sistema desenvolvido, o mesmo encontra-se disponibilizado
gratuitamente, levando-se em consideracao o leftright, na
seguinte pagina:
http://sites.uol.com.br/ual-language/download.html 
podendo ser livremente copiado e utilizado.

Este software pode ser distribuido gratuitamente em servicos
online, BBS, ou quaisquer outras midias eletronicas, desde
que todos os arquivos sejam incluidos na distribuicao. Este
software nao pode ser distribuido em CD-ROM, disquete ou
outra midia fisica nao gratuita sem a permissao dos 
desenvolvedores.

Quando utilizado com a configuracao recomendada de hardware e
software, a linguagem funcionara em conformidade substancial
com esta documentacao. Caso contrario, nao garantimos o
correto funcionamento do UAL.

O sistema possui codigo aberto, permitindo aos interessados 
o desenvolvimento de novas funcionalidades, desde que mantida
a filosofia inicial do mesmo e mencionadas as autorias. 
Para isso, basta entrar em contato, atraves do e-mail:
ual-language@uol.com.br e solicitar os codigos fontes.

Este software e' distribuido sem quaisquer garantias de
nenhum tipo. O risco de usar este software e' totalmente
assumido pelo usuario. Este software nao e' tolerante a 
falhas e nao deve ser usado em ambientes que requerem isto. 
Em nenhum momento os desenvolvedores poderao ser acusados por 
danos indiretos, acidentais ou por consequencia, incluindo
perda de trabalho, renda, lucros, uso, dados ou outras 
vantagens economicas, mesmo que os mesmos sejam avisados 
anteriormente da possibilidade do dano.


2. INTRODUCAO
--------------------------------------------------------------
Este arquivo contem informacoes para instalacao e utilizacao
do UAL, sistema desenvolvido durante o Projeto Final do Curso 
de Informatica da Universidade Estacio de Sa, Campus Friburgo.

O UAL, interpretador para descricao de algoritmos em 
Portugues, visa auxiliar o aprendizado do aluno iniciante 
em Linguagem de Programacao atraves de uma abordagem 
diferente, possibilitando a visualizacao grafica dos 
resultados obtidos pela execucao de algoritmos. 


3. REQUISITOS NECESSARIOS
--------------------------------------------------------------
Para a instalacao do UAL, sao necessarios os seguintes 
requisistos minimos:

HARDWARE:

- Microcomputador PC compativel
- Processador 486 ou superior
- Memoria RAM de 16 MB
- 10 Mb de espaco em disco disponivel

SOFTWARE:

- Sistema Operacional Linux
- daVinci V2.1 ou superior
- X11 Versao 5 (X11R5) ou superior


4. INSTALACAO
--------------------------------------------------------------
O usuario pode escolher os aplicativos que achar convenientes
para utilizacao. O arquivo ual.tar.gz contem o executavel para 
interpretacao simples, o arquivo ualgraph.tar.gz contem o 
executavel para interpretacao com visualizacao grafica e o
editual.tar.gz contem o ambiente para edicao dos programas 
fontes, manual da linguagem, juntamente com os executaveis
(ual e ualgraph). O endereco para aquisicao dos arquivos eh
http://sites.uol.com.br/ual-language/download.html.

4.1 INTERPRETADOR SIMPLES
--------------------------------------------------------------
Faca o download do arquivo ual.tar.gz, na pagina de download 
anteriormente mencionada. Apos o download, o arquivo deve 
ser descompactado dentro do diretorio /usr/local. Copie o 
arquivo para o diretorio citado e descompacte-o, conforme
os seguintes comandos:

% cp ual.tar.gz /usr/local
% cd /usr/local
% tar zxvf ual.tar.gz

Apos estes procedimentos, o interpretador (em sua versao
simples) ja podera ser utilizado. Para informacoes sobre 
o uso do mesmo, verifique o item 5, 'Acesso ao Sistema'.

4.2 INTERPRETADOR ANIMADO
--------------------------------------------------------------
Faca o download do arquivo ualgraph.tar.gz, na pagina de 
download anteriormente mencionada. Apos o download, o arquivo
deve ser descompactado, obrigatoriamente, dentro do diretorio
/usr/local. Copie o arquivo para o diretorio citado 
e descompacte-o, conforme os seguintes comandos:

% cp ualgraph.tar.gz /usr/local
% cd /usr/local
% tar zxvf ualgraph.tar.gz

Porem, antes de executar o ualgraph, arquivo binario 
descompactado, o usuario deve verificar se os aplicativos 
daVinci e X11 estao instalados em seu computador, de modo a 
assegurar o funcionamento da linguagem. Os links para os sites 
que disponibilizam estes aplicativos estao incluidos na pagina 
de download.

Apos estes procedimentos, o interpretador (em sua versao com 
visualizacao grafica) ja podera ser utilizado. Para 
informacoes sobre o uso do mesmo, verifique o item 5,
'Acesso ao Sistema'.

4.3 EDITOR UAL
--------------------------------------------------------------
Este arquivo contem um ambiente proprio para edicao dos 
programas fontes, o manual da linguagem e tambem os 
executaveis para a interpretacao simples e animada. Com a
utilizacao do editor, o usuario podera desenvolver e executar
os seus programas de forma bastante intuitiva.

Faca o download do arquivo editual.tar.gz, na pagina de 
download anteriormente mencionada. Apos o download, o arquivo
deve ser descompactado, obrigatoriamente, dentro do diretorio
/usr/local. Copie o arquivo para o diretorio citado 
e descompacte-o, conforme os seguintes comandos:

% cp editual.tar.gz /usr/local
% cd /usr/local
% tar zxvf editual.tar.gz

Serao descompactados os arquivos binarios ual e ualgraph, 
responsaveis pela execucao simples e animada dos programas,
respectivamente, bem como os arquivos start, edit e editual,
correspondentes ao editor, e alguns exemplos de programas
escritos na linguagem UAL, no diretorio examples.

A execucao do modulo ualgraph, que esta disponivel neste
ambiente para edicao, depende dos aplicativos daVinci e X11,
como esta descrito no item 4.2 - 'Interpretador Animado'.

Para o correto funcionamento deste ambiente e' necessario
que o mesmo esteja no PATH do sistema. Isto deve ser feito
acrescentando-se o caminho /usr/local/ual ao seu profile.
No Linux Conectiva, por exemplo, voce devera editar o 
arquivo .bash_profile, do diretorio do usuario (caso voce 
acesse o Linux como root, este diretorio sera o /root)
acrescentando na linha do PATH:

:/usr/local/ual

Apos estes procedimentos, o ambiente de edicao ja podera 
ser utilizado. Para informacoes sobre o uso do mesmo, 
verifique o item 5, 'Acesso ao Sistema'.


5. ACESSO AO SISTEMA
--------------------------------------------------------------
Para acessar o UAL, pode-se executar seus arquivos binarios
ual ou ualgraph diretamente no prompt do sistema, desde que
os mesmos estejam em seu PATH ou deve-se mudar para o
diretorio /usr/local/ual antes de executa-los. 
Cada arquivo possui parametros diferentes, conforme as 
indicacoes abaixo:

- UAL:
% ual nome_arquivo_fonte

- UALGRAPH:
% ualgraph nome_arquivo_fonte tempo_animacao

onde:
- nome_arquivo_fonte e' o nome do arquivo em UAL que contem o
  programa a ser compilado, escrito pelo usuario. Este programa
  pode ser editado em qualquer ambiente ASCII.

- tempo_animacao corresponde a um numero inteiro utilizado
  para determinar o tempo de animacao do programa fonte UAL,
  compreendido entre 0 e 80000, sendo 0 o menor tempo de 
  animacao.

- EDITOR UAL:
  Tambem e' possivel executar esses arquivos atraves do editor, 
  de forma pratica e facil. Para isso, carregue o editor,
  atraves do comando:

% editual

  Esse ambiente possui menus que permitem a criacao e execucao
  dos programas UAL.

  Para maiores informacoes sobre a linguagem, consulte o ManUAL 
  on-line, no Menu Ajuda.


6. CREDITOS
--------------------------------------------------------------
Este sistema foi desenvolvido como Projeto Final do Curso de 
Informatica da Universidade Estacio de Sa, Campus Friburgo,
com a seguinte equipe:

- Juarez Muylaert Filho (prof. orientador)
  jamf@estacio.br
  Diretor de Informatica da Universidade Estacio de Sa
  
- Adriana Sayuri Spallanzani (aluna)
  spallanzani@uol.com.br
  Tecnica em Informatica, Universidade Federal de Juiz de Fora
  Analista de Sistemas, Universidade Estacio de Sa

- Andrea Teixeira de Medeiros (aluna)
  andrea@iprj.uerj.br
  Tecnica em Programacao, Faculdade de Filosofia Santa Doroteia
  Analista de Sistemas, Universidade Estacio de Sa


7. DUVIDAS E SUGESTOES
--------------------------------------------------------------
Para maiores esclarecimentos, duvidas, codigos fontes, sugestoes, 
ou mesmo notificacoes de bugs, solicitamos entrar em contato 
conosco, particularmente, ou atraves do e-mail:
ual-language@uol.com.br 

Toda sugestao sera bem-vinda.
