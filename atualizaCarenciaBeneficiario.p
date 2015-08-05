/*****************************************************************************
*      Programa .....: atualizaCarenciaBeneficiario.p
*      Data .........: 23/07/2014
*      Area .........: TI
*      Programador ..: Guilherme Gregory
*      Objetivo .....: API que atualiza a carencia do beneficiario
*----------------------------------------------------------------------------
* VERSAO    DATA        RESPONSAVEL    ITEM   NR-CATI  MOTIVO
* GPL1.00   23/07/2014  Guilherme      Projeto PE1334  Desenvolvimento
*           20/10/2014  Diego M        Projeto PO1351  Alterada api da TOTVS, ao inves de api-usucaren deve ser utilizada a api defchsauintegration
*           06/05/2015  Diego M                        Alteracoes para atender atualizacao para versao do Totvs 12.1.3
******************************************************************************/
/* --- Definicao de usuario corrente --- */
def new global shared var v_cod_usuar_corren as character
                       format "x(12)":U label "Usuario Corrente"
                                      column-label "Usuario Corrente" no-undo.

assign v_cod_usuar_corren = "portalemp".

/* --- Inicializa Tela --- */


/* --- Includes Padrao do Sistema --- */
{dep/defchsauintegration.i}

/* --- Tabelas Temporarias --- */


/* --- tabela para retorno da dert0036.p --- */
def temp-table ct_tt-modulo             no-undo
    field st_cd-modulo                  like usucaren.cd-modulo         init 0
    field st_nr-dias-carencia-original  like usucaren.nr-dias           init 0
    field st_nr-dias-carencia-alterada  like usucaren.nr-dias           init 0
    index ordem1
          st_cd-modulo.

def temp-table ct_tt-coberturas         no-undo
    field st_cd-grau-parentesco         like usuario.cd-grau-parentesco
    field st_cd-modulo                  like mod-cob.cd-modulo          init 0
    field st_dt-carencia                as date                         init ?
    index ordem1
          st_cd-modulo.

def temp-table ct_tt-modulo-opcional no-undo
    field st_cd-modulo               like mod-cob.cd-modulo
    index ordem1 is primary unique
        st_cd-modulo.

def temp-table ct_tt-coberturas-dep like ct_tt-coberturas.

/* --- Parametros de Entrada/Saida --- */
def input  parameter st_nr-versao-par                   as int  format "99"                             init 0          no-undo.
def input  parameter table for ct_tt-modulo.
def input  parameter st_cd-modalidade-par               like usuario.cd-modalidade                      init 0          no-undo.
def input  parameter st_nr-ter-adesao-par               like usuario.nr-ter-adesao                      init 0          no-undo.
def input  parameter st_cd-usuario-par                  like usuario.cd-usuario                         init 0          no-undo.
def input  parameter st_cd-tipo-regra-par               as int                                          init 0          no-undo.
def output parameter st_ds-retorno-par                  as char                                         init ""         no-undo.

/* --- Streams --- */


/* --- Definicoes de Entrada/Saida (arquivos) --- */


/* --- Acesso Tabelas do Sistema --- */


/* --- Variaveis Globais --- */


/* --- Variaveis de Zoom --- */


/* --- Variaveis Locais --- */                                       
def var lg-erro-aux                                     as log                                                  no-undo.
def var nr-dias-aux                                     like usucaren.nr-dias                   init 0          no-undo.
def var lg-carencia-aux                                 like usucaren.lg-carencia               init ?          no-undo.
def var lg-bonifica-penaliza-aux                        like usucaren.lg-bonifica-penaliza      init ?          no-undo.
def var in-erro-proc-aux                                as char format "x(01)"                                  no-undo.
def var ds-mensagem-aux                                 as char format "x(18)"                                  no-undo.
def var ds-mensrelat-aux                                as char format "x(75)"                                  no-undo.
def var dt-libe-urg-aux                                 as date format "99/99/9999"                             no-undo.
def var dt-libe-ele-aux                                 as date format "99/99/9999"                             no-undo.
def var dt-ini-car-aux                                  as date format "99/99/9999"                             no-undo.
def var dt-fim-mod-aux                                  as date format "99/99/9999"                             no-undo.
def var dt-can-mod-aux                                  as date format "99/99/9999"                             no-undo.
def var nr-dias-cumpre-aux                              like usucaren.nr-dias                                   no-undo.
def var lg-boni-pena-cumpre-aux                         like usucaren.lg-bonifica-penaliza                      no-undo.
def var lg-carencia-cumpre-aux                          like usuario.lg-carencia                                no-undo.
def var nr-dias-usuario-aux                             like usucaren.nr-dias                                   no-undo.
def var lg-boni-pena-usuario-aux                        like usucaren.lg-bonifica-penaliza                      no-undo.
def var lg-carencia-usuario-aux                         like usuario.lg-carencia                                no-undo.
def var nr-dias-propla-aux                              like usucaren.nr-dias                                   no-undo.
def var lg-boni-pena-propla-aux                         like usucaren.lg-bonifica-penaliza                      no-undo.
def var lg-carencia-propla-aux                          like usuario.lg-carencia                                no-undo.
def var nr-dias-usucaren-aux                            like usucaren.nr-dias                                   no-undo.
def var lg-boni-pena-usucaren-aux                       like usucaren.lg-bonifica-penaliza                      no-undo.
def var lg-carencia-usucaren-aux                        like usuario.lg-carencia                                no-undo.
def var dt-inicio-modulo-aux                            like pro-pla.dt-inicio                                  no-undo.
def var dt-carpro-aux                                   as date                                                 no-undo.
def var h-api-usucaren                                  as handle                                               no-undo.
def var nr-idade-aux                                    as int  format 999                                      no-undo.

/* --- Buffers --- */
def buffer b-usuario for usuario.

/* --- Funcoes --- */


/* --- Querys --- */             


/* --- Browses --- */


/* --- Botoes --- */


/* --- Rectangles --- */


/* --- Frames --- */


/* --- Includes de Relatorio --- */


/* --- Laco Principal --- */


/* --- Procedures --- */
if st_nr-versao-par <> 00
    then return error "0001-Versao da transacao invalida: " + string(st_nr-versao-par).

find first usuario where usuario.cd-modalidade = st_cd-modalidade-par
                     and usuario.nr-ter-adesao = st_nr-ter-adesao-par
                     and usuario.cd-usuario    = st_cd-usuario-par
                    no-lock use-index usuari15 no-error.

if not avail usuario 
    then return error "0003-Cliente n∆o encontrado: Mod.: " + string(st_cd-modalidade-par) + " Termo: " + string(st_nr-ter-adesao-par) + " Usu†rio: " + string(st_cd-usuario-par).

if  st_cd-tipo-regra-par <> 01 
and st_cd-tipo-regra-par <> 02
and st_cd-tipo-regra-par <> 03
and st_cd-tipo-regra-par <> 04
    then return error "0037-N∆o existe tratativa de controle de carància = " + string(st_cd-tipo-regra-par).

message "DADOS INICIO ATUALIZACARENCIABENEFICIARIO" view-as alert-box title "Atencao !!!".
message "MODULOS" view-as alert-box title "Atencao !!!".

for each ct_tt-modulo no-lock:

    message "st_cd-modulo                 = " ct_tt-modulo.st_cd-modulo                 skip
            "st_nr-dias-carencia-original = " ct_tt-modulo.st_nr-dias-carencia-original skip
            "st_nr-dias-carencia-alterada = " ct_tt-modulo.st_nr-dias-carencia-alterada view-as alert-box title "Atencao !!!".
end.

message "st_cd-modalidade-par = " st_cd-modalidade-par skip
        "st_nr-ter-adesao-par = " st_nr-ter-adesao-par skip
        "st_cd-usuario-par    = " st_cd-usuario-par    skip
        "st_cd-tipo-regra-par = " st_cd-tipo-regra-par view-as alert-box title "Atencao !!!".

empty temp-table ct_tt-coberturas.
empty temp-table tmpBeneficiario.
empty temp-table tmpModuloOpcionalBeneficiario.
empty temp-table tmpCoberturaEspecialProcInsu.
empty temp-table tmpCarenciaPorProcedimento.
empty temp-table tmpCarenciaModulo.
empty temp-table tmpPessoaFisica.
empty temp-table tmpContato.
empty temp-table tmpEndereco.
empty temp-table tmpErros.

run carrega-tabelas-temporarias.

if st_cd-tipo-regra-par = 1
or st_cd-tipo-regra-par = 4
    then do:
            for each ct_tt-modulo no-lock:

                if ct_tt-modulo.st_nr-dias-carencia-original < 0 
                    then return error "0057-N£mero de dias de carància original inv†lido".
            
                if ct_tt-modulo.st_nr-dias-carencia-alterada < 0 
                    then return error "0057-N£mero de dias de carància destino inv†lido".
            
                if ct_tt-modulo.st_nr-dias-carencia-original <> ct_tt-modulo.st_nr-dias-carencia-alterada
                    then do:
                            if ct_tt-modulo.st_nr-dias-carencia-alterada > ct_tt-modulo.st_nr-dias-carencia-original
                                then assign nr-dias-aux              = ct_tt-modulo.st_nr-dias-carencia-alterada - ct_tt-modulo.st_nr-dias-carencia-original
                                            lg-carencia-aux          = true
                                            lg-bonifica-penaliza-aux = false.
                            
                            if  ct_tt-modulo.st_nr-dias-carencia-alterada <  ct_tt-modulo.st_nr-dias-carencia-original
                            and ct_tt-modulo.st_nr-dias-carencia-alterada <> 0
                                then assign nr-dias-aux              = (ct_tt-modulo.st_nr-dias-carencia-original - ct_tt-modulo.st_nr-dias-carencia-alterada) * -1
                                            lg-carencia-aux          = true
                                            lg-bonifica-penaliza-aux = true.
                            
                            if ct_tt-modulo.st_nr-dias-carencia-alterada = 0
                                then assign nr-dias-aux              = ct_tt-modulo.st_nr-dias-carencia-alterada
                                            lg-carencia-aux          = false
                                            lg-bonifica-penaliza-aux = true.

                            create tmpCarenciaModulo.
                            assign tmpCarenciaModulo.cd-modalidade        = st_cd-modalidade-par
                                   tmpCarenciaModulo.cd-modulo            = ct_tt-modulo.st_cd-modulo
                                   tmpCarenciaModulo.cd-userid            = v_cod_usuar_corren
                                   tmpCarenciaModulo.cd-usuario           = st_cd-usuario-par
                                   tmpCarenciaModulo.dt-atualizacao       = today
                                   tmpCarenciaModulo.lg-bonifica-penaliza = lg-bonifica-penaliza-aux
                                   tmpCarenciaModulo.lg-carencia          = lg-carencia-aux
                                   tmpCarenciaModulo.nr-dias              = nr-dias-aux
                                   tmpCarenciaModulo.nr-proposta          = usuario.nr-proposta.

                         end.
                    else do:
                            for each usucaren where usucaren.cd-modalidade = st_cd-modalidade-par
                                                and usucaren.nr-proposta   = usuario.nr-proposta
                                                and usucaren.cd-usuario    = usuario.cd-usuario
                                                and usucaren.cd-modulo     = ct_tt-modulo.st_cd-modulo
                                              no-lock use-index usucare1:

                                create tmpCarenciaModulo.
                                assign tmpCarenciaModulo.cd-modalidade        = st_cd-modalidade-par
                                       tmpCarenciaModulo.cd-modulo            = usucaren.cd-modulo
                                       tmpCarenciaModulo.cd-userid            = v_cod_usuar_corren
                                       tmpCarenciaModulo.cd-usuario           = st_cd-usuario-par
                                       tmpCarenciaModulo.dt-atualizacao       = today
                                       tmpCarenciaModulo.lg-bonifica-penaliza = usucaren.lg-bonifica-penaliza
                                       tmpCarenciaModulo.lg-carencia          = usucaren.lg-carencia
                                       tmpCarenciaModulo.nr-dias              = if usucaren.lg-bonifica-penaliza then usucaren.nr-dias * -1 else usucaren.nr-dias
                                       tmpCarenciaModulo.nr-proposta          = usuario.nr-proposta.


                            end.

                         end.



            end.
         end.

find first b-usuario where b-usuario.cd-modalidade = usuario.cd-modalidade
                       and b-usuario.nr-ter-adesao = usuario.nr-ter-adesao
                       and b-usuario.cd-usuario    = usuario.cd-titular
                     no-lock use-index usuari15 no-error.

if avail b-usuario 
    then do:
            if st_cd-tipo-regra-par = 3
            or (st_cd-tipo-regra-par = 2
            and b-usuario.dt-inclusao-plano = usuario.dt-inclusao-plano)
                then do:
                        run busca-coberturas(input  usuario.cd-modalidade,
                                             input  usuario.nr-ter-adesao,
                                             input  usuario.cd-usuario,
                                             output table ct_tt-coberturas-dep).
            
                        run busca-coberturas(input  usuario.cd-modalidade,
                                             input  usuario.nr-ter-adesao,
                                             input  usuario.cd-titular,
                                             output table ct_tt-coberturas).
                        
                        for each ct_tt-coberturas where ct_tt-coberturas.st_cd-grau-parentesco = 1
                                                  no-lock:
            
                            find first ct_tt-coberturas-dep where ct_tt-coberturas-dep.st_cd-grau-parentesco <> 1
                                                              and ct_tt-coberturas-dep.st_cd-modulo           = ct_tt-coberturas.st_cd-modulo
                                                              and ct_tt-coberturas-dep.st_dt-carencia        <> ct_tt-coberturas.st_dt-carencia
                                                            no-lock no-error.
            
                            if avail ct_tt-coberturas-dep 
                                then do:
                                        if ct_tt-coberturas.st_dt-carencia <> ct_tt-coberturas-dep.st_dt-carencia 
                                            then do:
                                                    if ct_tt-coberturas.st_dt-carencia <= usuario.dt-inclusao-plano
                                                        then assign nr-dias-aux              = 0
                                                                    lg-carencia-aux          = false
                                                                    lg-bonifica-penaliza-aux = true.
                                                        else do:
                                                                if ct_tt-coberturas.st_dt-carencia > ct_tt-coberturas-dep.st_dt-carencia
                                                                    then assign nr-dias-aux              = ct_tt-coberturas.st_dt-carencia - ct_tt-coberturas-dep.st_dt-carencia
                                                                                lg-carencia-aux          = true
                                                                                lg-bonifica-penaliza-aux = false.
                                                                    else assign nr-dias-aux              = ct_tt-coberturas-dep.st_dt-carencia - ct_tt-coberturas.st_dt-carencia
                                                                                lg-carencia-aux          = true
                                                                                lg-bonifica-penaliza-aux = true. 
                                                             end.
                        
                                                    create tmpCarenciaModulo.
                                                    assign tmpCarenciaModulo.cd-modalidade        = usuario.cd-modalidade
                                                           tmpCarenciaModulo.cd-modulo            = ct_tt-coberturas.st_cd-modulo
                                                           tmpCarenciaModulo.cd-userid            = v_cod_usuar_corren
                                                           tmpCarenciaModulo.cd-usuario           = usuario.cd-usuario
                                                           tmpCarenciaModulo.dt-atualizacao       = today
                                                           tmpCarenciaModulo.lg-bonifica-penaliza = lg-bonifica-penaliza-aux
                                                           tmpCarenciaModulo.lg-carencia          = lg-carencia-aux
                                                           tmpCarenciaModulo.nr-dias              = if lg-bonifica-penaliza-aux then nr-dias-aux * -1 else nr-dias-aux
                                                           tmpCarenciaModulo.nr-proposta          = usuario.nr-proposta.
            
            
                                                 end.
                                    end.
                        end.
                     end.


         end.                        

run dep/defchsauintegration.p persistent set h-api-usucaren.

for each tmpCarenciaModulo no-lock:

    message "tmpCarenciaModulo: "                                            skip
            "cd-modalidade        " tmpCarenciaModulo.cd-modalidade          skip
            "cd-modulo            " tmpCarenciaModulo.cd-modulo              skip
            "cd-userid            " tmpCarenciaModulo.cd-userid              skip
            "cd-usuario           " tmpCarenciaModulo.cd-usuario             skip
            "dt-atualizacao       " tmpCarenciaModulo.dt-atualizacao         skip
            "lg-bonifica-penaliza " tmpCarenciaModulo.lg-bonifica-penaliza   skip
            "lg-carencia          " tmpCarenciaModulo.lg-carencia            skip
            "nr-dias              " tmpCarenciaModulo.nr-dias                skip
            "nr-proposta          " tmpCarenciaModulo.nr-proposta            view-as alert-box title "Atencao !!!".
end.


for each tmpModuloOpcionalBeneficiario no-lock:

    message "tmpModuloOpcionalBeneficiario"                                         skip
            "aa-ult-fat         " tmpModuloOpcionalBeneficiario.aa-ult-fat          skip
            "cd-modalidade      " tmpModuloOpcionalBeneficiario.cd-modalidade       skip
            "cd-modulo          " tmpModuloOpcionalBeneficiario.cd-modulo           skip
            "cd-motivo-cancel   " tmpModuloOpcionalBeneficiario.cd-motivo-cancel    skip
            "cd-sit-modulo      " tmpModuloOpcionalBeneficiario.cd-sit-modulo       skip
            "cd-userid          " tmpModuloOpcionalBeneficiario.cd-userid           skip
            "cd-userid-inclusao " tmpModuloOpcionalBeneficiario.cd-userid-inclusao  skip
            "cd-usuario         " tmpModuloOpcionalBeneficiario.cd-usuario          skip
            "dt-atualizacao     " tmpModuloOpcionalBeneficiario.dt-atualizacao      skip
            "dt-cancelamento    " tmpModuloOpcionalBeneficiario.dt-cancelamento     skip
            "dt-fim             " tmpModuloOpcionalBeneficiario.dt-fim              skip
            "dt-inicio          " tmpModuloOpcionalBeneficiario.dt-inicio           skip
            "dt-mov-inclusao    " tmpModuloOpcionalBeneficiario.dt-mov-inclusao     skip
            "mm-ult-fat         " tmpModuloOpcionalBeneficiario.mm-ult-fat          skip
            "nr-proposta        " tmpModuloOpcionalBeneficiario.nr-proposta        view-as alert-box title "Atencao !!!".


end.


run gravaBeneficiario in h-api-usucaren(input-output table tmpBeneficiario,
                                        input        table tmpModuloOpcionalBeneficiario,
                                        input        table tmpCoberturaEspecialProcInsu,
                                        input        table tmpCarenciaPorProcedimento,
                                        input        table tmpCarenciaModulo,
                                        input-output table tmpPessoaFisica,
                                        input        table tmpContato,
                                        input        table tmpEndereco,
                                        input     	 no,
                                        input-output table tmpErros) no-error.

if not temp-table tmpErros:has-records
    then assign st_ds-retorno-par = "Carencia Atualizada".
    else do:
            find first tmpErros no-lock.
            
            if avail tmpErros
                then assign st_ds-retorno-par = string(tmpErros.nr-erro) + "-" + tmpErros.ds-erro.
         end.

delete procedure h-api-usucaren.


/* --- Procedures --- */
procedure busca-coberturas:

    def input parameter  cd-modalidade-par    like usuario.cd-modalidade      no-undo.
    def input parameter  nr-ter-adesao-par    like usuario.nr-ter-adesao      no-undo.
    def input parameter  cd-usuario-par       like usuario.cd-usuario         no-undo.
    def output parameter table for ct_tt-coberturas. 

    find first b-usuario where b-usuario.cd-modalidade = cd-modalidade-par
                           and b-usuario.nr-ter-adesao = nr-ter-adesao-par
                           and b-usuario.cd-usuario    = cd-usuario-par   
                         no-lock use-index usuari15 no-error. 
    
    if avail b-usuario 
        then do:
                find first propost where propost.cd-modalidade = b-usuario.cd-modalidade
                                     and propost.nr-proposta   = b-usuario.nr-proposta
                                   no-lock use-index propos1 no-error.
                
                if avail propost
                    then do:
                            if propost.cd-contratante = 0 
                                then return error "0011ŒCodigo contratante nao encontrado".

                            for each pro-pla where pro-pla.cd-modalidade = b-usuario.cd-modalidade
                                               and pro-pla.nr-proposta   = b-usuario.nr-proposta
                                             no-lock use-index pro-pla2:

                                 find first mod-cob where mod-cob.cd-modulo = pro-pla.cd-modulo
                                                    no-lock use-index mod-cob1 no-error. 
                            
                                 if not avail mod-cob
                                     then message "Nao encontrado modulo de cobertura: " pro-pla.cd-modulo view-as alert-box title "Atencao !!!".

                                 find first formpaga where formpaga.cd-forma-pagto = pro-pla.cd-forma-pagto
                                                     no-lock use-index formpag1 no-error.
                                     
                                 if not avail formpaga
                                     then message "Nao encontrada forma de pagamento: " pro-pla.cd-forma-pagto view-as alert-box title "Atencao !!!".
                                     
                                 assign dt-inicio-modulo-aux = pro-pla.dt-inicio.
                                     
                                 if dt-inicio-modulo-aux > b-usuario.dt-inclusao-plano
                                     then assign dt-carpro-aux = dt-inicio-modulo-aux.
                                     else assign dt-carpro-aux = b-usuario.dt-inclusao-plano.
                                    
                                 if b-usuario.lg-carencia
                                     then run rtp/rtcarpro.p (input  "MC",
                                                              input  dt-carpro-aux,
                                                              input  recid(propost),
                                                              input  recid(mod-cob),
                                                              input  recid(b-usuario),
                                                              input  recid(formpaga),
                                                              input  recid(pro-pla),
                                                              input  "",
                                                              input  0,
                                                              input  yes,
                                                              input  propost.lg-pea,
                                                              input  yes,
                                                              output lg-erro-aux,
                                                              output in-erro-proc-aux,
                                                              output ds-mensagem-aux,
                                                              output ds-mensrelat-aux,
                                                              output dt-libe-urg-aux,
                                                              output dt-libe-ele-aux,
                                                              output dt-ini-car-aux,
                                                              output dt-fim-mod-aux,
                                                              output dt-can-mod-aux,
                                                              output nr-dias-cumpre-aux,
                                                              output lg-boni-pena-cumpre-aux,
                                                              output lg-carencia-cumpre-aux,
                                                              output nr-dias-usuario-aux,
                                                              output lg-boni-pena-usuario-aux,
                                                              output lg-carencia-usuario-aux,
                                                              output nr-dias-propla-aux,
                                                              output lg-boni-pena-propla-aux,
                                                              output lg-carencia-propla-aux,
                                                              output nr-dias-usucaren-aux,
                                                              output lg-boni-pena-usucaren-aux,
                                                              output lg-carencia-usucaren-aux).
                                        else assign dt-libe-ele-aux = dt-carpro-aux.

                                    if b-usuario.dt-inclusao-plano = dt-libe-ele-aux
                                    or dt-libe-ele-aux             < today
                                        then do:
                                                create ct_tt-coberturas.
                                                assign ct_tt-coberturas.st_cd-grau-parentesco = b-usuario.cd-grau-parentesco
                                                       ct_tt-coberturas.st_cd-modulo          = mod-cob.cd-modulo
                                                       ct_tt-coberturas.st_dt-carencia        = b-usuario.dt-inclusao-plano.
                                             end.
                                        else do:
                                                if dt-libe-ele-aux <> ? 
                                                    then do:
                                                            create ct_tt-coberturas.
                                                            assign ct_tt-coberturas.st_cd-grau-parentesco = b-usuario.cd-grau-parentesco
                                                                   ct_tt-coberturas.st_cd-modulo          = mod-cob.cd-modulo
                                                                   ct_tt-coberturas.st_dt-carencia        = dt-libe-ele-aux.
                                                         end.
                                                    else do:
                                                            create ct_tt-coberturas.
                                                            assign ct_tt-coberturas.st_cd-grau-parentesco = b-usuario.cd-grau-parentesco
                                                                   ct_tt-coberturas.st_cd-modulo          = mod-cob.cd-modulo
                                                                   ct_tt-coberturas.st_dt-carencia        = b-usuario.dt-inclusao-plano.
                                                         end.
                                              end.
                            end.
                         end.
                    else return error "9999-Proposta nao encontrada para esse usuario".
             end.
        else return error "9999-Usuario nao encontrado".
     
end procedure.


procedure carrega-tabelas-temporarias:

    create tmpBeneficiario.
    buffer-copy usuario except ds-observacao to tmpBeneficiario.
    
    assign tmpBeneficiario.ds-observacao              = usuario.ds-observacao[1] + " " +
                                                        usuario.ds-observacao[2] + " " + 
                                                        usuario.ds-observacao[3]
           tmpBeneficiario.lg-graces                  = yes
           tmpBeneficiario.dt-atualizacao             = today
           tmpBeneficiario.cd-userid                  = v_cod_usuar_corren
           tmpBeneficiario.cd-grau-parentesco-dmed    = usuario.int-21.

        /*   tmpBeneficiario.cd-funcao                  = ?    inte 99999             Funcao                                                         */
        /*   tmpBeneficiario.cd-motivo-cancelamento     = ?    inte 999               Motivo cancel.                                                 */
        /*   tmpBeneficiario.cd-uf-ctps                 = ?    char x(2)              UF CTPS                                                        */
        /*   tmpBeneficiario.cd-unidade-destino         = ?    inte 9999              Unid.dest.                                                     */
        /*   tmpBeneficiario.cd-usuario-inclusao        = ?    char x(12)             Usuario inclusao                                               */
        /*   tmpBeneficiario.ds-obs-contratante         = ?    char x(230)            Obs Contratante                                                */
        /*   tmpBeneficiario.dt-devolucao               = ?    date 99/99/9999        Dt Devolucao Doc                                               */
        /*   tmpBeneficiario.dt-emissao                 = ?    date 99/99/9999        Dt Emissao Doc                                                 */
        /*   tmpBeneficiario.dt-inclusao-sistema        = ?    date 99/99/9999        Dt Incl.sistema                                                */
        /*   tmpBeneficiario.dt-intercambio             = ?    date 99/99/9999        Dt Intercambio             Obrigat¢rio caso for repasse        */
        /*   tmpBeneficiario.dt-intercambio-atendimento = ?    date 99/99/9999        Dt Intercambio Atend       Obrigat¢rio caso for repasse        */
        /*   tmpBeneficiario.dt-mudanca-funcao-setor    = ?    date 99/99/9999        Dt Mudanca Setor                                               */
        /*   tmpBeneficiario.dt-retorno-trabalho        = ?    date 99/99/9999        Dt. Retorno Trab                                               */
        /*   tmpBeneficiario.dt-validade-carteira       = ?    date 99/99/9999        Dt. Validade Carteira                                          */
        /*   tmpBeneficiario.in-parto-coberto           = ?    int  9                 Indicador de parto cobert                                      */
        /*   tmpBeneficiario.lg-cobertura-padrao        = ?    log  Sim/Nao           Cob Padrao                                                     */
        /*   tmpBeneficiario.lg-cob-especial-padrao     = ?    log  Sim/Nao           Cob Esp                                                        */
        /*   tmpBeneficiario.lg-demitido-aposentado     = ?    log  Sim/Nao           Demitido Aposentado                                            */
        /*   tmpBeneficiario.lg-formador-opiniao        = ?    log  Sim/Nao           Formador Opiniao                                               */
        /*   tmpBeneficiario.lg-med-ocup                = ?    log  Sim/Nao           Medicina Ocupacional                                           */
        /*   tmpBeneficiario.log-permite-quest          = ?    log  Sim/Nao           Permite Questionario                                           */
        /*   tmpBeneficiario.nr-serie-ctps              = ?    char X(100)            Serie CTPS                                                     */
        /*   tmpBeneficiario.nr-via-carteira            = ?    inte 99                Numero Via Carteira .                                          */ 

    find first pessoa-fisica where pessoa-fisica.id-pessoa = usuario.id-pessoa
                             no-lock use-index pfis1 no-error.
    
    if avail pessoa-fisica
        then do:
                create tmpPessoaFisica.
                buffer-copy pessoa-fisica to tmpPessoaFisica.

                assign tmpPessoaFisica.cd-userid              = v_cod_usuar_corren
                       tmpPessoaFisica.lg-cadastro-homologado = pessoa-fisica.log-3
                       tmpPessoaFisica.lg-valida-usuario      = yes.

                /*
                assign tmpPessoaFisica.des-alerg                  =   char   x(100)            Alergico                          
                       tmpPessoaFisica.des-cirurgia-realzda       =   char   x(200)            Cirurgia Realizada                
                       tmpPessoaFisica.des-doenca                 =   char   x(255)            Descricao doenªa                  
                       tmpPessoaFisica.des-hos-atendim            =   char   x(200)            Hospital Atendimento              
                       tmpPessoaFisica.des-medic-atendim          =   char   x(200)            Medico Atendimento                
                       tmpPessoaFisica.des-medicto-uso            =   char   x(200)            Medicamento Cont Uso              
                       tmpPessoaFisica.des-obs                    =   char   x(2000)           Observaá∆o
                       tmpPessoaFisica.in-tipo-dependencia        =   int    9                 Tipo de dependencia               
                       tmpPessoaFisica.lg-exerce-atividade        =   log    Sim/Nao           Exerce atividade                  
                       tmpPessoaFisica.lg-port-necessid-especiais =   log    Sim/Nao           Portador de necessidades especiais
                       tmpPessoaFisica.val-alt-pfis               =   deci-3 99.99             Altura Pessoa fisica              
                       tmpPessoaFisica.val-peso-pfis              =   deci-3 999999			  Peso Pessoa fisica                */
             end.
                    
    for each endereco where endereco.id-pessoa = usuario.id-pessoa
                      no-lock use-index endereco2:
    
        create tmpEndereco.
        buffer-copy endereco to tmpEndereco.

        assign tmpEndereco.cd-userid     = v_cod_usuar_corren
               tmpEndereco.ds-referencia = endereco.char-1.

        /*assign tmpEndereco.id-endereco-crm =     int  999999999 Endereco no Crm             */
    end.
    
    for each contato-pessoa where contato-pessoa.id-pessoa = usuario.id-pessoa
                            no-lock use-index contato2:
    
        create tmpContato.
        buffer-copy contato-pessoa to tmpContato.


        /*assign tmpContato.id-contato-crm =   inte    99999999    Endereco no Crm*/
    end.

    run dep/dert0028.p(input  usuario.dt-nascimento,
                       input  usuario.dt-inclusao-plano,
                       output nr-idade-aux,
                       output lg-erro-aux).
                    
    if lg-erro-aux
        then return error "0050-Nao foi possivel calcular idade do usuario na inclusao do modulo. dtNasc: " + string(usuario.dt-nascimento,"99/99/9999").

    if nr-idade-aux < 66
        then do:
                create ct_tt-modulo-opcional.
                assign ct_tt-modulo-opcional.st_cd-modulo = 608.

                create ct_tt-modulo-opcional.
                assign ct_tt-modulo-opcional.st_cd-modulo = 611.
             end.
        else do:
                create ct_tt-modulo-opcional.
                assign ct_tt-modulo-opcional.st_cd-modulo = 609.

                create ct_tt-modulo-opcional.
                assign ct_tt-modulo-opcional.st_cd-modulo = 612.
             end.

    create ct_tt-modulo-opcional.
    assign ct_tt-modulo-opcional.st_cd-modulo = 800.

    for each ct_tt-modulo-opcional no-lock:
    
        find first pro-pla where  pro-pla.cd-modalidade   = usuario.cd-modalidade
                             and  pro-pla.nr-proposta     = usuario.nr-proposta
                             and  pro-pla.cd-modulo       = ct_tt-modulo-opcional.st_cd-modulo
                             and (pro-pla.dt-cancelamento = ?
                              or  pro-pla.dt-cancelamento > today)
                           no-lock use-index pro-pla3 no-error.
        
        if avail pro-pla
            then do:
                    create tmpModuloOpcionalBeneficiario.
                    assign /*tmpModuloOpcionalBeneficiario.aa-ult-fat       = */
                           tmpModuloOpcionalBeneficiario.cd-modalidade      = usuario.cd-modalidade
                           tmpModuloOpcionalBeneficiario.cd-modulo          = pro-pla.cd-modulo
                           /*tmpModuloOpcionalBeneficiario.cd-motivo-cancel = */
                           tmpModuloOpcionalBeneficiario.cd-sit-modulo      = 0
                           tmpModuloOpcionalBeneficiario.cd-userid          = v_cod_usuar_corren
                           tmpModuloOpcionalBeneficiario.cd-userid-inclusao = v_cod_usuar_corren
                           tmpModuloOpcionalBeneficiario.cd-usuario         = usuario.cd-usuario
                           tmpModuloOpcionalBeneficiario.dt-atualizacao     = today
                           /*tmpModuloOpcionalBeneficiario.dt-cancelamento  =*/ 
                           /*tmpModuloOpcionalBeneficiario.dt-fim           = 05/31/2013 */
                           tmpModuloOpcionalBeneficiario.dt-inicio          = usuario.dt-inclusao-plano
                           tmpModuloOpcionalBeneficiario.dt-mov-inclusao    = today
                           /*tmpModuloOpcionalBeneficiario.mm-ult-fat       = */
                           tmpModuloOpcionalBeneficiario.nr-proposta        = usuario.nr-proposta.
                end.
    end.

    /*
    tmpCoberturaEspecialProcInsu. 
    tmpCarenciaPorProcedimento.
    */

end procedure.
