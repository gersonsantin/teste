/*****************************************************************************
*      Programa .....: alteraDadosContratante.p
*      Data .........: 14/05/2012
*      Area .........: TI
*      Programador ..: Diego Mullerasdasdasdas
*      Objetivo .....: API de alteracao de dados do contratante no GPL atraves de BPM
*----------------------------------------------------------------------------
* VERSAO    DATA        RESPONSAVEL    ITEM   NR-CATI  MOTIVO
* GPL1.00   14/05/2012  Diego M        Proje           Desenvolvimento
*           23/09/2014  Diego M      Migracao TOTVS 11 Alterada chamada da api-contrat para api defchsauintegration
*           06/05/2015  Diego M                        Alteracoes para atender atualizacao para versao do Totvs 12.1.3
******************************************************************************/

/* --- Definicao de usuario corrente --- */ 
def new global shared var v_cod_usuar_corren as character
                       format "x(12)":U label "Usuario Corrente"
                                      column-label "Usuario Corrente" no-undo.

/* --- Inicializa Tela --- */


/* --- Includes Padrao do Sistema --- */
{dep/defchsauintegration.i}

/* --- Tabelas Temporarias --- */


/* --- Parametros de Entrada/Saida --- */
def input  parameter st_nr-versao-par                   as int  format "99"                     init 0          no-undo.
def input  parameter st_nr-cgc-cpf-par	                as char format "x(20)"                  init ""         no-undo.
def input  parameter st_dt-nascimento-par	            like pessoa_fisic.dat_nasc_pessoa_fisic init ?          no-undo.
def input  parameter st_nr-insc-contratante-par         as int  format "99999999"               init 0          no-undo.
def input  parameter st_cd-contratante-par	            as int  format "999999999"              init 0          no-undo.
def input  parameter st_nm-contratante-par	            as char format "x(40)"                  init ""         no-undo.
def input  parameter st_in-est-civil-par                as dec  format "9"                      init 0          no-undo.
def input  parameter st_ds-sexo-par                     as char format "!"                      init ""         no-undo.
def input  parameter st_ds-orgao-emissor-ident-par	    as char format "x(30)"                  init ""         no-undo.
def input  parameter st_nm-pais-par	                    as char format "x(20)"                  init ""         no-undo.
def input  parameter st_nr-identidade-par	            as char format "x(14)"                  init ""         no-undo.
/*def input  parameter st_dt-emissao-doc-par	            like contrat.dt-emissao-doc             init ?          no-undo.*/
def input  parameter st_uf-emissor-doc                  like contrat.uf-emissor-doc             init ""         no-undo.
def input  parameter st_ds-nacionalidade-par	        as char format "x(40)"                  init ""         no-undo.
def input  parameter st_nm-email-par	                as char format "x(50)"                  init ""         no-undo.
def input  parameter st_nr-telefone1-par	            as char format "x(20)"                  init ""         no-undo.
def input  parameter st_nr-telefone2-par	            as char format "x(20)"                  init ""         no-undo.
def input  parameter st_nm-mae-par	                    as char format "x(70)"                  init ""         no-undo.
def input  parameter st_en-rua-par	                    as char format "x(40)"                  init ""         no-undo.
def input  parameter st_en-bairro-par	                as char format "x(30)"                  init ""         no-undo.
def input  parameter st_cd-cidade-par	                like contrat.cd-cidade                  init 0          no-undo.
def input  parameter st_en-cep-par	                    as char format "99.999-999"             init ""         no-undo.
def input  parameter st_en-uf-par	                    as char format "!!"                     init ""         no-undo.
def input  parameter st_nr-caixa-postal-par	            as char format "x(20)"                  init ""         no-undo.
def input  parameter st_en-rua-cob-par	                like contrat.en-rua-cob                 init ""         no-undo.
def input  parameter st_en-bairro-cob-par	            like contrat.en-bairro-cob              init ""         no-undo.
def input  parameter st_cd-cidade-cob-par	            like contrat.cd-cidade-cob              init 0          no-undo.
def input  parameter st_en-cep-cob-par	                like contrat.en-cep-cob                 init ""         no-undo.
def input  parameter st_en-uf-cob-par	                like contrat.en-uf-cob                  init ""         no-undo.
def input  parameter st_nr-caixa-postal-cob-par	        like contrat.nr-caixa-postal-cob        init ""         no-undo.
def input  parameter st_portador-par	                as char format "x(5)"                   init ""         no-undo.
/*def input  parameter st_modalidade-par	                as int  format "999"                    init 0          no-undo.*/
def input  parameter st_cod-banco-par	                as char format "x(3)"                   init ""         no-undo.
def input  parameter st_agencia-par	                    as char format "x(8)"                   init ""         no-undo.
def input  parameter st_dig-agencia-par	                as char format "x(2)"                   init ""         no-undo.
def input  parameter st_conta-corren-par	            as char format "x(20)"                  init ""         no-undo.
def input  parameter st_dig-conta-corren-par	        as char format "x(2)"                   init ""         no-undo.
def input  parameter st_nm-conjuge-par     	            as char format "x(40)"                  init ""         no-undo.
def input  parameter st_cd-vendedor-par                 as int                                  init 0          no-undo.
def input  parameter st_cd-userid-par	                as char format "x(12)"                  init ""         no-undo.
def output parameter st_ds-retorno-par                  as char format "x(20)"                  init ""         no-undo.


assign v_cod_usuar_corren = st_cd-userid-par.

/* --- Streams --- */


/* --- Definicoes de Entrada/Saida (arquivos) --- */


/* --- Acesso Tabelas do Sistema --- */


/* --- Variaveis Globais --- */


/* --- Variaveis de Zoom --- */


/* --- Variaveis Locais --- */                                       
def var h-deapi-contrat                     as handle                                               no-undo.
def var lg-sexo-aux                         as log                                                  no-undo.
def var cod-carteira-aux                    like contrat.modalidade                                 no-undo. 
def var cd-fluxo-financ-ext-aux             like tmpContratante.cd-fluxo-financ-ext init ""         no-undo.
def var nm-usuario-aux                      as char                                 init ""         no-undo.
def var nm-internacional-aux                as char                                 init ""         no-undo.
def var nm-usuario-cartao-aux               as char                                 init ""         no-undo.
def var lg-erro-aux                         as log                                  init ?          no-undo.
def var ds-mens-aux                         as char                                 init ""         no-undo.


/* --- Buffers --- */


/* --- Funcoes --- */


/* --- Querys --- */             


/* --- Browses --- */


/* --- Botoes --- */


/* --- Rectangles --- */


/* --- Frames --- */


/* --- Includes de Relatorio --- */


/* --- Laco Principal --- */


/* --- valida parametros de entrada --- */
if st_nr-versao-par <> 00
    then return error "0001-Versao da transacao invalida".

if trim(st_nr-cgc-cpf-par) = ""
    then return error "0004-Obrigatorio informar CPF/CNPJ".

if st_nr-insc-contratante-par = 0 
    then return error "0050-Obrigatorio informar Nr Inscricao Contratante".

/* --- retira caracteres do CPF e CEP --- */
assign st_nr-cgc-cpf-par = replace(replace(trim(st_nr-cgc-cpf-par),".",""),"-","")
       st_en-cep-par     = replace(replace(st_en-cep-par,".",""),"-","")
       st_en-cep-cob-par = replace(replace(st_en-cep-cob-par,".",""),"-","").

if st_dt-nascimento-par = ? 
    then return error "0038 - Data de nascimento informada e invalida".
            
find first dzcidade where dzcidade.cd-cidade = st_cd-cidade-par
                    no-lock use-index cidad1 no-error.

if not avail dzcidade
    then return error "0056 Î Codigo da cidade nao encontrado no GPL".

if st_cd-cidade-cob-par <> 0 
    then do:
            find first dzcidade where dzcidade.cd-cidade = st_cd-cidade-cob-par
                                 no-lock use-index cidad1 no-error.

            if not avail dzcidade
                then return error "0056 Î Codigo da cidade nao encontrado no GPL".
         end.

if st_in-est-civil-par > 9
or st_in-est-civil-par = 0
    then return error "0059 - Estado Civil informado invalido".

if st_in-est-civil-par <> 2
    then assign st_nm-conjuge-par = "".
      
if st_ds-sexo-par = "F"
    then assign lg-sexo-aux = no.
    else assign lg-sexo-aux = yes.

if st_agencia-par = "" 
    then assign st_agencia-par = "0000".

message "ENDERECOOOOOOOOOOOOOOO"                               skip
        "st_cd-cidade           " st_cd-cidade-par             skip  
        "st_cd-cidade-cob       " st_cd-cidade-cob-par         skip  
        "st_en-bairro           " st_en-bairro-par             skip  
        "st_en-bairro-cob       " st_en-bairro-cob-par         skip  
        "st_en-cep              " st_en-cep-par                skip  
        "st_en-cep-cob          " st_en-cep-cob-par            skip  
        "st_en-rua              " st_en-rua-par                skip  
        "st_en-rua-cob          " st_en-rua-cob-par            skip  
        "st_en-uf               " st_en-uf-par                 skip  
        "st_en-uf-cob           " st_en-uf-cob-par             skip  
        "st_nr-caixa-postal     " st_nr-caixa-postal-par       skip  
        "st_nr-caixa-postal-cob " st_nr-caixa-postal-cob-par   view-as alert-box title "Atencao !!!".

if int(st_portador-par) = 690 /*Debito em conta Unicred*/
     then assign cod-carteira-aux = 300.
     else assign cod-carteira-aux = 001.

run dep/defchsauintegration.p persistent set h-deapi-contrat.

empty temp-table tmpContratante.
empty temp-table tmpPessoaFisica.
empty temp-table tmpPessoaJuridica.
empty temp-table tmpEndereco.
empty temp-table tmpErros.

find first contrat where contrat.nr-insc-contratante = st_nr-insc-contratante-par
                   use-index contrat4 no-error.

if avail contrat
    then do:
            if contrat.cd-contratante <> st_cd-contratante-par
                then return error string("0050-Contratante cadastrado " + string(contrat.cd-contratante) + " difere do informado " + string(st_cd-contratante-par)).

            create tmpContratante.
            buffer-copy contrat except contrat.nr-telefone-banco
                                       contrat.en-uf-banco 
                                       contrat.en-cep-banco 
                                       contrat.ds-observacao to tmpContratante.

            case contrat.cod-gr-cli:
     
                when 10 then assign cd-fluxo-financ-ext-aux = "1.010".
                when 20 then assign cd-fluxo-financ-ext-aux = "1.011".
                when 30 then assign cd-fluxo-financ-ext-aux = "1.012".
                when 50 then assign cd-fluxo-financ-ext-aux = "1.011".
                otherwise assign cd-fluxo-financ-ext-aux = "".
            end case.

            assign tmpContratante.dt-atualizacao      = today
                /* tmpContratante.dt-emissao-doc      = if st_dt-emissao-doc-par <> 11/30/002 then st_dt-emissao-doc-par else ? /*tratamento para data 0000-00-00 no SOUP*/*/
                   tmpContratante.lg-emite-boleto     = yes
                   tmpContratante.modalidade          = cod-carteira-aux
                   tmpContratante.nm-contratante      = st_nm-contratante-par        
                   tmpContratante.cd-portador         = int(st_portador-par)
                   tmpContratante.cd-sit-cred         = 1
                   tmpContratante.cd-userid-analise   = v_cod_usuar_corren
                   tmpContratante.cd-userid           = v_cod_usuar_corren
                   tmpContratante.dt-analise-credito  = today
                   tmpContratante.cod-banco           = int(st_cod-banco-par)
                   tmpContratante.agencia             = st_agencia-par
                   tmpContratante.agencia-digito      = st_dig-agencia-par
                   tmpContratante.conta-corren        = st_conta-corren-par
                   tmpContratante.dig-conta-corren    = st_dig-conta-corren-par
                   tmpContratante.cd-vendedor         = st_cd-vendedor-par
                   tmpContratante.cd-fluxo-financ-ext = cd-fluxo-financ-ext-aux.

            create tmpContato.
            assign tmpContato.ds-contato       = st_nr-telefone1-par
                   /*tmpContato.ds-ramal       = */
                   tmpContato.id-contato       = 0
                   /*tmpContato.id-contato-crm = 
                   tmpContato.id-endereco      = */
                   /*tmpContato.id-pessoa      = NÆo ‚ necess rio preencher, porque na fachada j  foi preenchido.*/
                   /*tmpContato.nm-contato     = */
                   tmpContato.tp-contato       = 1.
            
            create tmpContato.
            assign tmpContato.ds-contato       = st_nr-telefone2-par
                   /*tmpContato.ds-ramal       = */
                   tmpContato.id-contato       = 0
                   /*tmpContato.id-contato-crm = 
                   tmpContato.id-endereco      = */
                   /*tmpContato.id-pessoa      = NÆo ‚ necess rio preencher, porque na fachada j  foi preenchido.*/
                   /*tmpContato.nm-contato     = */
                   tmpContato.tp-contato       = 1.
            
            create tmpContato.               
            assign tmpContato.ds-contato       = lc(st_nm-email-par)
                   /*tmpContato.ds-ramal       = */
                   tmpContato.id-contato       = 0
                   /*tmpContato.id-contato-crm = 
                   tmpContato.id-endereco      = */
                   /*tmpContato.id-pessoa      = NÆo ‚ necess rio preencher, porque na fachada j  foi preenchido.*/
                   /*tmpContato.nm-contato     = */
                   tmpContato.tp-contato       = 4.
            
            create tmpEndereco.
            assign tmpEndereco.cd-cep           = st_en-cep-par
                   tmpEndereco.cd-cidade        = st_cd-cidade-par
                   tmpEndereco.cd-uf            = st_en-uf-par
                   tmpEndereco.cd-userid        = v_cod_usuar_corren  
                   tmpEndereco.ds-bairro        = st_en-bairro-par
                   /*tmpEndereco.ds-complemento   = */
                   tmpEndereco.ds-endereco      = st_en-rua-par
                   tmpEndereco.id-endereco      = 0
                   /*tmpEndereco.id-endereco-crm  = 
                   tmpEndereco.id-pessoa        = 	/* NÆo ‚ necess rio preencher, porque na fachada j  foi preenchido. */*/
                   tmpEndereco.in-tipo-endereco = 1
                   tmpEndereco.lg-end-cobranca  = no
                   tmpEndereco.lg-principal     = yes
                   tmpEndereco.nr-caixa-postal  = st_nr-caixa-postal-par
                   /*tmpEndereco.nr-endereco      = 
                   tmpEndereco.tp-logradouro    =
                   tmpEndereco.ds-referencia    = */ .
            
            create tmpEndereco.
            assign tmpEndereco.cd-cep           = st_en-cep-cob-par
                   tmpEndereco.cd-cidade        = st_cd-cidade-cob-par
                   tmpEndereco.cd-uf            = st_en-uf-cob-par
                   tmpEndereco.cd-userid        = v_cod_usuar_corren
                   tmpEndereco.ds-bairro        = st_en-bairro-cob-par
                   /*tmpEndereco.ds-complemento   = */
                   tmpEndereco.ds-endereco      = st_en-rua-cob-par
                   tmpEndereco.id-endereco      = 0
                   /*tmpEndereco.id-endereco-crm  = 
                   tmpEndereco.id-pessoa        = 	/* NÆo ‚ necess rio preencher, porque na fachada j  foi preenchido. */*/
                   tmpEndereco.in-tipo-endereco = 1
                   tmpEndereco.lg-end-cobranca  = yes
                   tmpEndereco.lg-principal     = no
                   tmpEndereco.nr-caixa-postal  = st_nr-caixa-postal-cob-par
                   /*tmpEndereco.nr-endereco      = 
                   tmpEndereco.tp-logradouro    =
                   tmpEndereco.ds-referencia    = */  .
            
            run rtp/rtcarint.p (input  1,
                                input  st_nm-contratante-par,
                                input  no,
                                output nm-usuario-aux,
                                output nm-internacional-aux,
                                output nm-usuario-cartao-aux,
                                output lg-erro-aux,
                                output ds-mens-aux).

            if contrat.in-tipo-pessoa = "F"
                then do:
                        find first pessoa-fisica where pessoa-fisica.id-pessoa = contrat.id-pessoa
                                                 no-lock use-index pfis1 no-error.
                        
                        if avail pessoa-fisica
                            then do:
                                    create tmpPessoaFisica.
                                    buffer-copy pessoa-fisica to tmpPessoaFisica.

                                    assign tmpPessoaFisica.cd-cpf                 = st_nr-cgc-cpf-par
                                           tmpPessoaFisica.cd-userid              = v_cod_usuar_corren
                                           tmpPessoaFisica.ds-orgao-emissor-ident = st_ds-orgao-emissor-ident-par
                                           tmpPessoaFisica.dt-nascimento          = st_dt-nascimento-par
                                           tmpPessoaFisica.in-estado-civil        = st_in-est-civil-par
                                           tmpPessoaFisica.lg-sexo                = lg-sexo-aux /*(yes = M e no = F)*/
                                           tmpPessoaFisica.nm-cartao              = nm-usuario-cartao-aux
                                           tmpPessoaFisica.nm-conjuge             = st_nm-conjuge-par
                                           tmpPessoaFisica.nm-mae                 = st_nm-mae-par
                                           tmpPessoaFisica.nm-pais-emissor-ident  = if  st_nr-identidade-par <> ""
                                                                                    and st_nr-identidade-par <> ?
                                                                                    and st_nm-pais-par        = ""
                                                                                        then "BRASIL"
                                                                                        else caps(st_nm-pais-par)
                                           tmpPessoaFisica.nm-pessoa              = st_nm-contratante-par
                                           tmpPessoaFisica.nr-identidade          = st_nr-identidade-par
                                           tmpPessoaFisica.uf-emissor-ident       = st_uf-emissor-doc
                                           /*tmpPessoaFisica.dt-atualizacao       = today*/
                                           tmpPessoaFisica.lg-cadastro-homologado = yes
                                           tmpPessoaFisica.lg-valida-usuario      = yes.
                                 end.
                     end.
                else do:
                        find first pessoa-juridica where pessoa-juridica.id-pessoa = contrat.id-pessoa
                                                   no-lock use-index pessoa-juridica1 no-error.
                            
                        if avail pessoa-juridica
                            then do:
                                    create tmpPessoaJuridica.
                                    buffer-copy pessoa-juridica to tmpPessoaJuridica.

                                    assign /*tmpPessoaJuridica.cd-userid       = v_cod_usuar_corren*/
                                           tmpPessoaJuridica.cd-cnpj           = st_nr-cgc-cpf-par
                                           tmpPessoaJuridica.dt-fundacao       = st_dt-nascimento-par
                                           tmpPessoaJuridica.nm-cartao         = nm-usuario-cartao-aux
                                           tmpPessoaJuridica.nm-pessoa         = st_nm-contratante-par
                                           /*tmpPessoaJuridica.dt-atualizacao  = today*/
                                           tmpPessoaJuridica.lg-valida-usuario = yes.
                                 end.
                     end.

            run gravaContratante in h-deapi-contrat(input-output table tmpContratante,                                              
                                                    input        table tmpImpostoContratante, /*** pode ficar sem ser preenchida ***/
                                                    input        table tmpControladora,       /*** pode ficar sem ser preenchida ***/
                                                    input        table tmpRepresentanteLegal, /*** pode ficar sem ser preenchida ***/
                                                    input-output table tmpPessoaFisica,                                             
                                                    input-output table tmpPessoaJuridica,                                           
                                                    input        table tmpContato,            /*** pode ficar sem ser preenchida ***/
                                                    input        table tmpEndereco,                                                 
                                                    input-output table tmpErros).                                                   
            
            if temp-table tmpErros:has-records
                then do:
                        find first tmpErros no-lock no-error.
            
                        if avail tmpErros 
                            then return error string(tmpErros.nr-erro) + " - " + tmpErros.ds-erro.
                     end.
                else assign st_ds-retorno-par = "Alteracao realizada".
         end.

delete procedure h-deapi-contrat.
