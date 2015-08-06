/********************************************************************************
HOJE
*******************************************************************************/
{include/i-prgvrs.i AU0101A 2.00.00.055 } /*** 010055 ***/
DS\FASDFAF
&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i au0101a MAU}
&ENDIF

/**********************************************************************************
**
**    AU0101A.P - Programa de geraá∆o de REPLICATION TRIGGER WRITE/DELETE 
**
**********************************************************************************/
{include/i_dbvers.i}
{include/i_dbtype.i}

define temp-table tt-atributo NO-UNDO
    field cod-tabela   as char    label "Tabela"    format "x(32)":U
    field cod-atributo as char    label "Atributo"  format "x(32)":U
    field des-atributo as char    label "Descri?ío" format "x(72)"
    field log-monitora as logical label "Monitora"
    field log-alterado as logical label "Alterado"
    field des-monitora as char    label "" format "x(03)"
    field cod-datatype as char    label "" format "x(20)"
    field formato      as char    label ""
    field field-npos   as Integer Label ""
    field iextent      as Integer Label ""
    field cinicial     as char    Label ""
    field iOrder       as Integer Label ""
    index i-atrib is PRIMARY UNIQUE cod-tabela cod-atributo ASCENDING.

define temp-table tt-tabela NO-UNDO
    field cod-tabela     as char    label "Tabela"     format "x(32)":U
    field cod-dump       as char    label "Dump"       format "x(8)":U 
    field des-tabela     as char    label "Descri?ío"  format "x(72)":U
    field cod-crc        as Integer label "CRC"
    field log-monitora   as logical label "Monitora"
    field log-alterado   as logical label "Alterado"
    field log-create     as logical label "Create"
    field log-write      as logical label "Write"
    field log-delete     as logical label "Delete"
    FIELD log-full       AS LOGICAL LABEL "Completa"
    FIELD log-select     AS LOGICAL LABEL "Completa"
    FIELD log-rpos       AS LOGICAL LABEL "RPos"
    index i-tab is PRIMARY UNIQUE cod-tabela ASCENDING.

define temp-table tt-exc NO-UNDO
    field cod-tabela   as char  label "Tabela"     format "x(32)":U
    field cod-evento   as char  label "Evento"     format "X":U
    field num-seq      as int   label "Sequencia"  format ">>>>>>>>9":U
    field des-exc      as char  label "Exce?ío"    format "x(2000)":U
    field log-elimina  as log   label "Elimina"
    index i-exc is PRIMARY UNIQUE cod-tabela num-seq cod-evento ASCENDING.

define temp-table tt-alerta NO-UNDO
    field cod-tabela        as char  label "Tabela"      format "x(32)":U
    field cod-atributo      as char  label "Atributo"    format "x(32)":U
    field tip-alerta        as char  label "Tipo Alerta" format "X":U
    field num-seq           as int   label "Sequencia"   format ">>>>>>>>9":U
    field des-criterio      as char  label "Crit?rio"    format "x(2000)":U
    field log-elimina       as log   label "Elimina"
    field cod-usuario       as char  label "Usuˇrio"     format "x(12)":U
    field log_email_local   as log label "E-mail Local"
    field log_email_celular as log label "E-mail Celular"
    field cod_grp_usuar     as char label "Grp Usuar" format "x(03)":U
    FIELD log-desativo      AS LOG LABEL "Desativar Alerta"
    index i-alerta is PRIMARY UNIQUE cod-tabela cod-atributo num-seq tip-alerta ASCENDING.


DEFINE INPUT    PARAMETER p-base       AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER p-nom-log    AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER p-gera-trig  AS LOGICAL NO-UNDO.
DEFINE INPUT    PARAMETER p-dir-win    AS CHARACTER NO-UNDO.
DEFINE INPUT    PARAMETER p-dir-unix   AS CHARACTER NO-UNDO.

/* --- FO 1286.314 - Repasse do Notista - INI - */
{include/i-epc200.i au0101a}
/* --- FO 1286.314 - Repasse do Notista - FIM - */

DEFINE VARIABLE c-arq             AS CHARACTER FORMAT 'x(40)':U.
DEFINE VARIABLE c-arq-r           AS CHARACTER FORMAT 'x(40)':U.
DEFINE VARIABLE c-arq-e           AS CHARACTER FORMAT 'x(40)':U.
DEFINE VARIABLE l-monitora        AS LOG       NO-UNDO.
DEFINE VARIABLE l-create          AS LOG       NO-UNDO.
DEFINE VARIABLE l-write           AS LOG       NO-UNDO.
DEFINE VARIABLE p-dir-trig        AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-error           AS INTEGER   NO-UNDO.
DEFINE VARIABLE c_indice          AS CHARACTER NO-UNDO.
DEFINE VARIABLE c_field           AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-ext             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE c-des-campo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-des-desc        AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-val-new         AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-val-ant         AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-lit-wri         AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-lit-del         AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-lit-alert       AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-clausula        AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-var-user        AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-list-rpos       AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-conteudo        AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-progress        AS LOG       NO-UNDO.
DEFINE VARIABLE l-erro-comp       AS LOG       NO-UNDO.
DEFINE VARIABLE l-magnus          AS LOG       NO-UNDO.
DEFINE VARIABLE l-oracle          AS LOG       NO-UNDO.
DEFINE VARIABLE l-full            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i-nr-atrib        AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-usuar-espec     AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-cont-def        AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-def-aux         AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-adic-base-dados AS LOGICAL   NO-UNDO.
DEFINE VARIABLE p-retorno         AS CHARACTER.
DEFINE VARIABLE v-base-ant        AS CHARACTER.

DEFINE VARIABLE h-au0108r2        AS HANDLE    NO-UNDO.
DEFINE VARIABLE cDatabase         AS CHARACTER NO-UNDO.

/* --- FO 1286.314 - Repasse do Notista - INI - */
EMPTY TEMP-TABLE tt-epc.
CREATE  tt-epc.
ASSIGN  
    tt-epc.cod-event     = "Variable_Definition"
    tt-epc.cod-parameter = "Definicao_Variaveis"
    tt-epc.val-parameter = "".

{include/i-epc201.i "variable_definition"}
/* --- FO 1286.314 - Repasse do Notista - FIM - */


DEFINE TEMP-TABLE tt-list-new NO-UNDO
    FIELD num-seq  AS INTEGER
    FIELD conteudo AS CHARACTER
    INDEX id num-seq ASCENDING.
DEFINE VARIABLE i-seq-list-new AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE tt-val-ant NO-UNDO
    FIELD num-seq  AS INTEGER
    FIELD conteudo AS CHARACTER
    INDEX id num-seq ASCENDING.
DEFINE        VARIABLE i-seq-val-ant AS INTEGER NO-UNDO.

DEFINE SHARED VARIABLE h-acomp       AS HANDLE  NO-UNDO.
DEFINE STREAM s-alerta.
DEFINE STREAM s-trigger.
DEFINE STREAM s-arq-df.
DEFINE STREAM s-erro-comp.
DEFINE STREAM st-error.


/********* MAIN BLOCK *********/

/* --- Comentado pela necessidade da FO 1286.314 que necessita desta chamada 
       no in°cio do programa -
{include/i-epc200.i au0101a} /** Upc **/
*/

ASSIGN 
    l-adic-base-dados = NO.                             
FIND mgadt.base_dados NO-LOCK
    WHERE base_dados.cod_base_dados = p-base NO-ERROR.

IF AVAILABLE base_dados THEN 
DO:
    IF base_dados.log-1 = YES THEN ASSIGN l-adic-base-dados = YES.
END.

IF OPSYS = "WIN32" THEN 
DO:
    /*     if p-dir-win <> "":U then do: */
    IF l-adic-base-dados = YES THEN 
    DO:
        ASSIGN 
            p-dir-win = p-dir-win + "/":U + TRIM(p-base).
        OS-CREATE-DIR value(p-dir-win).
    END.
    OS-CREATE-DIR value(p-dir-win + '/tgrw':U).
    OS-CREATE-DIR value(p-dir-win + '/tgrd':U).
    ASSIGN 
        p-dir-trig = p-dir-win.
/*     end. */
END.
ELSE 
DO:
    /*     if p-dir-unix <> "":U then do: */
    IF l-adic-base-dados = YES THEN 
    DO:
        ASSIGN 
            p-dir-unix = p-dir-unix + "/":U + TRIM(p-base).
        OS-CREATE-DIR value(p-dir-unix).
    END.
    OS-CREATE-DIR value(p-dir-unix + '/tgrw':U).
    OS-CREATE-DIR value(p-dir-unix + '/tgrd':U).
    ASSIGN 
        p-dir-trig = p-dir-unix.
/*     end. */
END.

IF PROGRESS = "FULL":U THEN
    ASSIGN l-progress = YES.
ELSE 
DO:
    ASSIGN 
        l-progress = NO.
    OUTPUT stream s-arq-df to value(SESSION:TEMP-DIRECTORY + p-base + ".df":U).
END.

FIND FIRST mgadt.param_audit NO-LOCK NO-ERROR.
IF AVAILABLE param_audit 
    AND param_audit.char-1 <> "" THEN 
DO:
    IF INDEX(param_audit.char-1,".":U) = 0 
        THEN ASSIGN c-usuar-espec = param_audit.char-1 + ".":U.
    ELSE ASSIGN c-usuar-espec = param_audit.char-1.
END.

IF AVAILABLE base_dados THEN 
DO:

    IF base_dados.cod_produto = "Magnus" THEN 
        ASSIGN c-var-user = 'userid("mgadm":U),':U
            l-magnus   = YES.
    ELSE 
    DO:
        IF base_dados.cod_produto = "Outros" THEN 
        DO:
            IF c-usuar-espec = "" THEN
                ASSIGN c-var-user = 'global_userid,':U
                    l-magnus   = NO.
            ELSE 
            DO:
                ASSIGN  
                    c-def-aux = REPLACE(c-usuar-espec," ":U,CHR(254))
                    l-magnus  = NO.
                DO i-cont-def = 1 TO NUM-ENTRIES(c-def-aux,CHR(254)):
                    IF ENTRY(i-cont-def,c-def-aux,CHR(254)) BEGINS "VAR":U THEN 
                        ASSIGN c-var-user = ENTRY(i-cont-def + 1,c-def-aux,CHR(254)) + ",".
                END.
            END.
        END.
        ELSE 
            ASSIGN c-var-user = 'v_cod_usuar_corren,':U
                l-magnus   = NO.
    END.
END.
IF AVAILABLE base_dados THEN 
DO:
    IF base_dados.log_exec_base_progress = YES THEN
        ASSIGN l-oracle = NO.
    ELSE
        ASSIGN l-oracle = YES.
END.


OUTPUT stream s-erro-comp to value(SESSION:TEMP-DIRECTORY + p-base + ".err":U) UNBUFFERED.
ASSIGN 
    l-erro-comp = NO.

{utp/ut-liter.i Erro_na_compilacao_da_trigger_REPLICATION-WRITE_da_tabela * R}
ASSIGN 
    c-lit-wri = TRIM(RETURN-VALUE).
{utp/ut-liter.i Erro_na_compilacao_da_trigger_REPLICATION-DELETE_da_tabela * R}
ASSIGN 
    c-lit-del = TRIM(RETURN-VALUE).
{utp/ut-liter.i Erro_na_compilacao_do_ALERTA_do_atributo * R}
ASSIGN 
    c-lit-alert = TRIM(RETURN-VALUE).

FOR EACH dictdb._file NO-LOCK
    WHERE dictdb._file._dump-name <> ?
    AND NOT dictdb._file._dump-name BEGINS "_":U:

    /* INICIO funá∆o versionamento temporariamente desativada 
    /**************************************************************************************************/
    /**************************************************************************************************/
    /**************************************************************************************************/
    /*Ajusta o rpos das tabelas para nío ocorrer erro de raw-transfer no monitoramento tabela completa*/
    Assign l-full = No.
    Find  Last tabela_versao_raw
         Where tabela_versao_raw.cod-tabela  = dictdb._file._file-name
           And tabela_versao_raw.log-livre-1 = Yes
        No-lock No-error.
    If Avail tabela_versao_raw
    Then Do:
        If dictdb._file._CRC <> tabela_versao_raw.num-livre-1 /*CRC*/
        Then Do:
            Run utp/ut-msgs.p ( Input "show",
                                Input 17006,
                                Input "Alteraá∆o de Dicion†rio~~A estrutura de algumas tabelas est∆o incorretas. Para gerar as trigger "
                                    + "ser† necess†rio executar o programa (aup/au0101g.p) mono-usu†rio para atualizaá∆o do dicion†rio.").
            Return "NOK".
        End.
    End.
    /**************************************************************************************************/
    /**************************************************************************************************/
    /**************************************************************************************************/
    FIM funá∆o versionamento temporariamente desativada */

    {utp/ut-liter.i Gerando_Triggers:_ *}
    RUN pi-acompanhar IN h-acomp (RETURN-VALUE + lc(dictdb._file._file-name)).

    IF _dump-name = "":U OR _dump-name = ? 
        THEN NEXT.

    /* Grava ordem dos atributos da tabela para pesquisa do RAW */
    ASSIGN 
        c-list-rpos = "":U.
    IF l-oracle THEN 
    DO:
        FOR EACH dictdb._field OF dictdb._file NO-LOCK
            /* - FO 1245.757 - Hexis -> No ambiente Oracle a consulta dos registros 
                 dos monitoramentos completos n∆o era apresentada corretamente (Ordenaá∆o no Browser).*/
            BY dictdb._field._order:
            ASSIGN 
                c-list-rpos = c-list-rpos + dictdb._field._field-name + ',':U.
        END.
    END.
    ELSE 
    DO:
        FOR EACH dictdb._field OF dictdb._file NO-LOCK
            BY dictdb._field._field-rpos:
            ASSIGN 
                c-list-rpos = c-list-rpos + dictdb._field._field-name + ',':U.
        END.
    END.
    ASSIGN 
        c-list-rpos = SUBSTRING(c-list-rpos,1,LENGTH(c-list-rpos) - 1).

    ASSIGN 
        l-monitora = NO
        l-create   = NO
        l-write    = NO
        l-full     = NO.

    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_1"
        tt-epc.cod-parameter = "Ponto_Execucao_1"
        tt-epc.val-parameter = "".

    {include/i-epc201.i "execution_point_1"}
    /* --- FO 1286.314 - Repasse do Notista - FIM - */

    FIND FIRST mgadt.tabela_monitor EXCLUSIVE-LOCK
        WHERE tabela_monitor.cod_base_dados = p-base
        AND tabela_monitor.cod_tabela     = dictdb._file._file-name 
        AND tabela_monitor.log_create     = YES NO-ERROR.
    IF AVAILABLE tabela_monitor
        THEN 
    DO:
        ASSIGN 
            l-monitora = YES
            l-create   = YES
            l-full     = &IF '{&mgadt_version}' < '2.06' &THEN
                               tabela_monitor.log-1
                            &ELSE
            tabela_monitor.log_tab_complet 
                            &ENDIF .
        /* Begin Amarildo */
        ASSIGN 
            v-base-ant = LDBNAME ("dictdb").

        FIND base_dados NO-LOCK
            WHERE base_dados.cod_base_dados = p-base NO-ERROR.
        CREATE ALIAS dictdb FOR DATABASE VALUE(LDBNAME(base_dados.nom_logic_base)).

        RUN aup/au0002c.p (INPUT v-base-ant,
            INPUT tabela_monitor.cod_tabela,
            OUTPUT p-retorno).
      
        IF p-retorno = "OK" 
            THEN tabela_monitor.log-2 = NO.
        ELSE tabela_monitor.log-2 = YES.
    /* End Amarildo */
    
    END.
    FIND FIRST tabela_monitor NO-LOCK
        WHERE tabela_monitor.cod_base_dados = p-base
        AND tabela_monitor.cod_tabela     = dictdb._file._file-name 
        AND tabela_monitor.log_write      = YES NO-ERROR.
    IF AVAILABLE tabela_monitor
        THEN ASSIGN l-monitora = YES
            l-write    = YES
            l-full     = &IF '{&mgadt_version}' < '2.06' &THEN
                                     tabela_monitor.log-1
                                &ELSE
            tabela_monitor.log_tab_complet 
                                &ENDIF .
    /**************************************************************************************************/
    /**************************************************************************************************/
    /**************************************************************************************************/
    /*Caso a tabela seja monitorada de forma completa ser† gerado um versionamento da mesma caso n∆o tenha*/
    IF l-full THEN 
    DO:
        DEFINE VARIABLE iVersionAnt      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iVersionNew      AS INTEGER NO-UNDO.
        DEFINE VARIABLE tgCreate         AS LOGICAL NO-UNDO.
        DEFINE VARIABLE iCRCSchema       AS INTEGER NO-UNDO.
        DEFINE VARIABLE iCRC             AS INTEGER NO-UNDO.
        DEFINE VARIABLE tgCreateAtributo AS LOGICAL NO-UNDO.
        DEFINE VARIABLE rwTabela         AS ROWID   NO-UNDO.
    
        FIND  LAST mgadt.tabela_versao_raw
            WHERE tabela_versao_raw.cod-tabela = tabela_monitor.cod_tabela
            AND tabela_versao_raw.log-livre-1 = YES  /*Vers∆o Ativa*/
            NO-LOCK NO-ERROR.
        IF AVAILABLE tabela_versao_raw
            THEN 
        DO:
            ASSIGN  
                iVersionAnt = tabela_versao_raw.num-vers 
                iVersionNew = iVersionAnt
                iCRC        = tabela_versao_raw.num-livre-1 /*CRC*/
                rwTabela    = ROWID(tabela_versao_raw).
        END.
        ELSE 
        DO:
            ASSIGN  
                iVersionAnt = 0
                iVersionNew = 0
                iCRC        = 0.
        END.
        IF AVAILABLE dictdb._file THEN
            ASSIGN iCRCSchema = dictdb._file._CRC.
        ELSE 
            ASSIGN iCRCSchema = 0.
    
        IF  iCRCSchema <> 0
            AND iCRCSchema <> iCRC
            THEN 
        DO:
            FOR EACH tt-atributo:
                DELETE tt-atributo.
            END.
            FOR EACH tt-alerta:
                DELETE tt-alerta.
            END.
            RUN aup/au0002b.p ( INPUT p-base,
                INPUT tabela_monitor.cod_tabela,
                INPUT-OUTPUT Table tt-atributo,
                INPUT-OUTPUT Table tt-alerta).
    
            ASSIGN  
                tgCreate    = NO
                iVersionNew = iVersionAnt + 1.
    
            FOR  EACH tt-atributo
                WHERE tt-atributo.cod-tabela = dictdb._file._file-name:
    
                CREATE  mgadt.atributo_versao_raw.
                ASSIGN  
                    atributo_versao_raw.cod-tabela       = tt-atributo.cod-tabela
                    atributo_versao_raw.num-vers         = iVersionNew
                    atributo_versao_raw.cod-atributo     = tt-atributo.cod-atributo
                    atributo_versao_raw.cod-format-campo = tt-atributo.formato
                    atributo_versao_raw.cod-tip-dado     = tt-atributo.cod-datatype
                    atributo_versao_raw.nom-atributo     = tt-atributo.des-atributo
                    atributo_versao_raw.nom-label        = tt-atributo.des-atributo
                    atributo_versao_raw.cod-livre-1      = p-base
                    atributo_versao_raw.cod-livre-2      = tt-atributo.cinicial
                    atributo_versao_raw.num-livre-1      = tt-atributo.field-npos
                    atributo_versao_raw.num-livre-2      = tt-atributo.iextent
                    atributo_versao_raw.val-livre-1      = tt-atributo.iOrder
                    tgCreate                             = YES.
            END.
            IF tgCreate = YES
                THEN 
            DO:
                IF iVersionNew > 1
                    THEN 
                DO:
                    FIND tabela_versao_raw WHERE ROWID(tabela_versao_raw) = rwTabela
                        EXCLUSIVE-LOCK NO-ERROR.
                    ASSIGN 
                        tabela_versao_raw.log-livre-1 = NO.
                    RELEASE tabela_versao_raw.
                END.
                CREATE  tabela_versao_raw.
                ASSIGN  
                    tabela_versao_raw.cod-tabela  = dictdb._file._file-name
                    tabela_versao_raw.nom-tabela  = SUBSTRING(dictdb._file._desc,1,72)
                    tabela_versao_raw.cod-livre-1 = p-base
                    tabela_versao_raw.num-livre-1 = iCRCSchema
                    tabela_versao_raw.num-vers    = iVersionNew
                    tabela_versao_raw.log-livre-1 = YES.
            END.
        END.
    END.
    /**************************************************************************************************/
    /**************************************************************************************************/
    /**************************************************************************************************/

    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_2"
        tt-epc.cod-parameter = "Ponto_Execucao_2"
        tt-epc.val-parameter = p-base + ";" + dictdb._file._file-name.

    {include/i-epc201.i "execution_point_2"}

    FIND tt-epc NO-LOCK
        WHERE tt-epc.cod-event = "Execution_Point_2" NO-ERROR.
    IF  tt-epc.val-parameter = 'yes' THEN
        ASSIGN l-monitora = YES.
    /* --- FO 1286.314 - Repasse do Notista - FIM - */
    
    IF l-adic-base-dados = NO THEN 
    DO:
        ASSIGN 
            c-arq-e = "tgrw/rw":U + _dump-name + ".p":U.
    END.
    ELSE 
    DO:
        ASSIGN 
            c-arq-e = TRIM(p-base) + "/":U + "tgrw/rw":U + _dump-name + ".p":U.
    END.
    ASSIGN 
        c-arq   = p-dir-trig + "/tgrw/rw":U + _dump-name + ".p":U
        c-arq-r = p-dir-trig + "/tgrw/rw":U + _dump-name + ".r":U.

    /* conforme chamado THXBEL - Dump-name em Mai£sculos causando problemas em linux */

    RUN convert2lowercase ( INPUT-OUTPUT c-arq  ).
    RUN convert2lowercase ( INPUT-OUTPUT c-arq-r).

    IF l-progress = NO THEN
        PUT STREAM s-arq-df UNFORMATTED 'UPDATE TABLE "' dictdb._file._file-name '"':U SKIP.

    IF  (p-gera-trig OR SEARCH(c-arq-r) = ?) AND l-monitora = YES THEN 
    DO:
        RUN pi-cria-prog-write (c-arq).

        /**
         * corp40362 -> Daniel Kasemodel
         * Caso a vers∆o do Progress j† seja FUll, compilar† as triggers
         * em tempo de execuá∆o normalmente, somente ir† executar uma nova
         * sess∆o com o parÉmetro -rx, quando o cliente n∆o possuir licenáa
         * de desenvolvimento para o Progress
         */
        /*         IF PROGRESS = "FULL" OR                    */
        /*            INTEGER(ENTRY(1, PROVERSION, ".")) < 10 */
        /*         THEN DO:                                   */
        COMPILE VALUE(c-arq) SAVE NO-ERROR.

        IF COMPILER:ERROR THEN 
        DO:
            PUT STREAM s-erro-comp UNFORMATTED "***** ":U c-lit-wri " ":U CAPS(dictdb._file._file-name) " *****":U SKIP.
            DO  i-error = 1 TO ERROR-STATUS:NUM-MESSAGES:
                PUT STREAM s-erro-comp UNFORMATTED ERROR-STATUS:GET-MESSAGE(i-error) SKIP.
            END.
            PUT STREAM s-erro-comp UNFORMATTED "":U SKIP(1).
            ASSIGN 
                l-erro-comp = YES.
        END.

        
        /*         END.                                                                                   */
        /*         ELSE DO:                                                                               */
        /*             /**                                                                                */
        /*              * corp40362                                                                       */
        /*              * Cria sess∆o com parÉmetro -rx, para compilar                                    */
        /*              * as triggers                                                                     */
        /*              *                                                                                 */
        /*              * compile value(c-arq) save no-error.                                             */
        /*              */                                                                                */
        /*             RUN aup/au0108r2.p PERSISTENT SET h-au0108r2.                                      */
        /*                                                                                                */
        /*             RUN pi-criptografa-trigger IN h-au0108r2 ( INPUT c-arq      ,                      */
        /*                                                        INPUT p-dir-win ,                       */
        /*                                                        INPUT p-dir-unix ) .                    */
        /*                                                                                                */
        /*             IF NOT RETURN-VALUE = "ok":U THEN DO:                                              */
        /*                 run utp/ut-msgs.p(input "show":U ,                                             */
        /*                                   input 34483    ,                                             */
        /*                                   input "":U ) .                                               */
        /*             END.                                                                               */
        /*             ELSE DO:                                                                           */
        /*                 RUN pi-compile-trigger IN h-au0108r2 ( INPUT c-arq     ,                       */
        /*                                                        INPUT c-lit-wri ,                       */
        /*                                                        INPUT dictdb._file._file-name ,         */
        /*                                                        INPUT c-arq-e   ,                       */
        /*                                                        INPUT p-dir-trig ) .                    */
        /*                                                                                                */
        /*                 IF VALID-HANDLE(h-au0108r2) THEN DO:                                           */
        /*                     DELETE PROCEDURE h-au0108r2.                                               */
        /*                 END.                                                                           */
        /*                                                                                                */
        /*                 /**                                                                            */
        /*                  * Verifica se ocorreram erros durante a compilaá∆o da Triiger.                */
        /*                  */                                                                            */
        /*                 IF SEARCH(SESSION:TEMP-DIRECTORY + "compile-error-triggers.err") <> ? THEN DO: */
        /*                     ASSIGN                                                                     */
        /*                         l-erro-comp = YES                                                      */
        /*                         .                                                                      */
        /*                 END.                                                                           */
        /*             END.                                                                               */
        /*         END.                                                                                   */

        /*****************************************************************************/    
        /** Chamada UPC no inicio da Atualizaªío                                    **/
        /*****************************************************************************/

        FOR EACH tt-epc:
            DELETE tt-epc.
        END.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "write":U
            tt-epc.cod-parameter = "nome-arq":U
            tt-epc.val-parameter = c-arq-e.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "write":U
            tt-epc.cod-parameter = "nome-caminho":U
            tt-epc.val-parameter = p-dir-trig.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "write":U
            tt-epc.cod-parameter = "caminho":U
            tt-epc.val-parameter = c-arq.

        {include/i-epc201.i "write":u}

        IF RETURN-VALUE = "NOK":U THEN 
        DO:
            UNDO, NEXT.
        END.

        /*************************** FIM CHAMADA EPC ************************************/

        IF p-dir-unix <> "":U AND
            p-dir-unix <> p-dir-trig 
            THEN OS-COPY value(c-arq-r) value(p-dir-unix + "/tgrw":U).
    END.

    ELSE IF SEARCH(c-arq-r) <> ? 
            AND  l-monitora = NO THEN 
        DO:
            OS-DELETE value(c-arq-r) no-error.
            IF  p-dir-unix <> "":U 
                AND p-dir-unix <> p-dir-trig THEN 
            DO:
                OS-DELETE value(p-dir-unix + "/tgrw/rw":U + _dump-name + ".r":U).
            END.
        END.
    RUN convert2lowercase ( INPUT-OUTPUT c-arq-e  ).

    RUN pi-cria-evento ("REPLICATION-WRITE":U,c-arq-e). 

    IF l-adic-base-dados = NO THEN 
    DO:
        c-arq-e = "tgrd/rd":U + _dump-name + ".p":U.
    END.
    ELSE 
    DO:
        ASSIGN 
            c-arq-e = TRIM(p-base) + "/" + "tgrd/rd":U + _dump-name + ".p":U.
    END.

    ASSIGN 
        c-arq   = p-dir-trig + "/tgrd/rd":U + _dump-name + ".p":U
        c-arq-r = p-dir-trig + "/tgrd/rd":U + _dump-name + ".r":U.

    RUN convert2lowercase ( INPUT-OUTPUT c-arq  ).
    RUN convert2lowercase ( INPUT-OUTPUT c-arq-r).

    FIND FIRST tabela_monitor NO-LOCK
        WHERE tabela_monitor.cod_base_dados = p-base
        AND tabela_monitor.cod_tabela     = dictdb._file._file-name 
        AND tabela_monitor.log_delete     = YES  NO-ERROR.
    IF AVAILABLE tabela_monitor THEN 
    DO: 
        ASSIGN  
            l-monitora = YES
            l-full     = &IF '{&mgadt_version}' < '2.06' &THEN
                                 tabela_monitor.log-1
                             &ELSE
            tabela_monitor.log_tab_complet 
                             &ENDIF .
    END.
    ELSE ASSIGN l-monitora = NO.


    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY   TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_3"
        tt-epc.cod-parameter = "Ponto_Execucao_3"
        tt-epc.val-parameter = "".

    {include/i-epc201.i "execution_point_3"}

    FIND tt-epc NO-LOCK 
        WHERE tt-epc.cod-event = "Execution_Point_3" NO-ERROR.
    IF   tt-epc.val-parameter <> '' THEN
        ASSIGN l-monitora = YES.
    /* --- FO 1286.314 - Repasse do Notista - FIM - */


    IF  (p-gera-trig OR SEARCH(c-arq-r) = ?) AND l-monitora = YES THEN 
    DO:
        RUN pi-cria-prog-delete (c-arq).

        /**
         * corp40362 -> Daniel Kasemodel
         * Caso a vers∆o do Progress j† seja FUll, compilar† as triggers
         * em tempo de execuá∆o normalmente, somente ir† executar uma nova
         * sess∆o com o parÉmetro -rx, quando o cliente n∆o possuir licenáa
         * de desenvolvimento para o Progress
         */
        /*         IF PROGRESS = "FULL" THEN DO: */
        COMPILE value(c-arq) SAVE NO-ERROR.

        IF COMPILER:ERROR THEN 
        DO:
            PUT STREAM s-erro-comp UNFORMATTED "***** ":U c-lit-del " ":U CAPS(dictdb._file._file-name) " *****":U SKIP.
            DO  i-error = 1 TO ERROR-STATUS:NUM-MESSAGES:
                PUT STREAM s-erro-comp UNFORMATTED ERROR-STATUS:GET-MESSAGE(i-error) SKIP.
            END.
            PUT STREAM s-erro-comp UNFORMATTED "":U SKIP(1).
            ASSIGN 
                l-erro-comp = YES.
        END.
        /*         END.                                                                                   */
        /*         ELSE DO:                                                                               */
        /*             /**                                                                                */
        /*              * corp40362                                                                       */
        /*              * Cria sess∆o com parÉmetro -rx, para compilar                                    */
        /*              * as triggers                                                                     */
        /*              *                                                                                 */
        /*              * compile value(c-arq) save no-error.                                             */
        /*              */                                                                                */
        /*             RUN aup/au0108r2.p PERSISTENT SET h-au0108r2.                                      */
        /*                                                                                                */
        /*             RUN pi-criptografa-trigger IN h-au0108r2 ( INPUT c-arq     ,                       */
        /*                                                        INPUT p-dir-win ,                       */
        /*                                                        INPUT p-dir-unix ) .                    */
        /*             IF NOT RETURN-VALUE = "ok":U THEN DO:                                              */
        /*                 run utp/ut-msgs.p(input "show":U ,                                             */
        /*                                   input 34483    ,                                             */
        /*                                   input "":U ) .                                               */
        /*             END.                                                                               */
        /*             ELSE DO:                                                                           */
        /*                 RUN pi-compile-trigger IN h-au0108r2 ( INPUT c-arq     ,                       */
        /*                                                        INPUT c-lit-wri ,                       */
        /*                                                        INPUT dictdb._file._file-name ,         */
        /*                                                        INPUT c-arq-e   ,                       */
        /*                                                        INPUT p-dir-trig ) .                    */
        /*                                                                                                */
        /*                 IF VALID-HANDLE(h-au0108r2) THEN DO:                                           */
        /*                     DELETE PROCEDURE h-au0108r2.                                               */
        /*                 END.                                                                           */
        /*                                                                                                */
        /*                 /**                                                                            */
        /*                  * Verifica se ocorreram erros durante a compilaá∆o da Triiger.                */
        /*                  */                                                                            */
        /*                 IF SEARCH(SESSION:TEMP-DIRECTORY + "compile-error-triggers.err") <> ? THEN DO: */
        /*                     ASSIGN                                                                     */
        /*                         l-erro-comp = YES                                                      */
        /*                         .                                                                      */
        /*                 END.                                                                           */
        /*             END.                                                                               */
        /*         END.                                                                                   */

        /*****************************************************************************/    
        /** Chamada UPC no inicio da Atualizaªío                                    **/
        /*****************************************************************************/

        FOR EACH tt-epc:
            DELETE tt-epc.
        END.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "delete":U
            tt-epc.cod-parameter = "nome-arq":U
            tt-epc.val-parameter = c-arq-e.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "delete":U
            tt-epc.cod-parameter = "nome-caminho":U
            tt-epc.val-parameter = p-dir-trig.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "delete":U
            tt-epc.cod-parameter = "caminho":U
            tt-epc.val-parameter = c-arq.

        {include/i-epc201.i "delete":u}

        IF RETURN-VALUE = "NOK":U THEN 
        DO:
            UNDO, NEXT.
        END.

        /*************************** FIM CHAMADA EPC ************************************/

        

        IF p-dir-unix <> "":U AND
            p-dir-unix <> p-dir-trig 
            THEN OS-COPY value(c-arq-r) value(p-dir-unix + "/tgrd":U).
    END.
    ELSE IF SEARCH(c-arq-r) <> ? 
            AND l-monitora = NO THEN 
        DO:
            OS-DELETE value(c-arq-r) no-error.
            IF  p-dir-unix <> "":U 
                AND p-dir-unix <> p-dir-trig THEN
                OS-DELETE value(p-dir-unix + "/tgrd/rd":U + _dump-name + ".r":U).
        END.

    RUN pi-cria-evento ("REPLICATION-DELETE":U,c-arq-e). 

    IF l-progress = NO THEN
        PUT STREAM s-arq-df UNFORMATTED '':U SKIP(1).
END.

OUTPUT stream s-erro-comp close.

/* Finaliza geraá∆o DF */
IF l-progress = NO THEN 
DO:
    PUT STREAM s-arq-df UNFORMATTED '.':U SKIP
        'PSC':U SKIP
        'cpstream=ibm850':U SKIP
        '.':U SKIP(1).
    OUTPUT stream s-arq-df close.
END.

/* corp40362 */
IF l-erro-comp THEN 
DO:
    RETURN "NOK":U.
END.
ELSE 
DO:
    RETURN "OK":U.
END.


/******* END MAIN BLOCK *******/


/********* PROCEDURES *********/
PROCEDURE pi-cria-prog-write:

    DEFINE INPUT PARAMETER nome     AS CHARACTER.

    DEFINE VARIABLE c-list-campo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-list-desc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-list-new     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-list-ant     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-des-resumo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE l-atrib        AS LOG       NO-UNDO INITIAL NO.

    DEFINE VARIABLE i-cont-atrib   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i-variavel     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE l-tem-atributo AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cDia           AS CHARACTER.
    DEFINE VARIABLE cMes           AS CHARACTER.
    DEFINE VARIABLE cAno           AS CHARACTER.
    FOR EACH tt-list-new:
        DELETE tt-list-new.
    END.

    OUTPUT stream s-trigger to value(nome).

    PUT STREAM s-trigger UNFORMATTED "TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ":U p-nom-log ".":U dictdb._file._file-name " OLD BUFFER old_":U SUBSTRING(dictdb._file._file-name,1,28) ".":U SKIP(1)
        "def new global shared var v_cod_usuar_corren as character format 'x(12)' label 'Usu†rio Corrente' column-label 'Usu†rio Corrente' no-undo.":U SKIP
        "def new global shared var global_userid as character format 'x(12)' label 'Usu†rio Corrente'.":U SKIP
        "def new global shared var v_cod_aplicat_dtsul_corren as character format 'x(3)' no-undo.":U SKIP
        "def new global shared var v_cod_empres_usuar as character format 'x(3)' label 'Empresa' column-label 'Empresa' no-undo.":U SKIP
                    /*Tratamento para Datasul 10 e 11 para a variavel de empresa do ems2*/
                    /*Tipo do campo muda conforme a vers∆o do banco emsfnd*/
                    &IF "{&EMSFND_VERSION}" = "1.00" &THEN
        "def new global shared var i-ep-codigo-usuario As Integer no-undo.":U SKIP
                    &ELSEIF '{&emsfnd_version}' >= '1.01' &THEN 
        "def new global shared var i-ep-codigo-usuario As Character no-undo.":U SKIP
                    &ENDIF
        c-usuar-espec SKIP
        "def var c-prog-atualiz     as char no-undo.":U SKIP
        "def var c-evento           as char no-undo.":U SKIP
        "def var c_chave            as char no-undo.":U SKIP
        "def var v-des-resumo       as char no-undo.":U SKIP
        "def var v-des-campo        as char no-undo.":U SKIP
        "def var v-des-desc         as char no-undo.":U SKIP
        "def var v-list-rpos        as char no-undo.":U SKIP
        "def var v-val-ant          as char no-undo.":U SKIP
        "def var v-val-new          as char no-undo.":U SKIP
        "def var c-user-prog        as char no-undo.":U SKIP
        "def var c-terminal         as char no-undo.":U SKIP
        "def var i-transid          as int  no-undo.":U SKIP
        "def var i-cont             as int  no-undo.":U SKIP
        "def var c-list-alert       as char no-undo.":U SKIP
        "def var c-list-user        as char no-undo.":U SKIP
        "def var c-raw-ant          as raw  no-undo.":U SKIP
        "def var c-raw-new          as raw  no-undo.":U SKIP
        "def var c-prog-name-exc    as char no-undo.":U SKIP
        "def var l-envia-alerta     as log initial no no-undo.":U SKIP(1)
        "def var c-prog-atualiz-aux as char no-undo.":U SKIP
        "def var c-file-name        as char no-undo.":U SKIP
        "def var c-prog-aux         as char no-undo.":U SKIP
        "def var l-processa         as log  no-undo.":U SKIP
        "def var h-prog-name        as widget-handle no-undo.":U SKIP
        "def var c-dictdb           as char no-undo.":U SKIP(1).

    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY   TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_4"
        tt-epc.cod-parameter = "Ponto_Execucao_4"
        tt-epc.val-parameter = "".

    {include/i-epc201.i "execution_point_4"}
    /* --- FO 1286.314 - Repasse do Notista - FIM - */

    &IF '{&mgadt_dbtype}' <> "Progress":U &THEN
    PUT STREAM s-trigger UNFORMATTED "def temp-table tt-old_":U SUBSTRING(dictdb._file._file-name,1,25) " no-undo like ":U p-nom-log ".":U SUBSTRING(dictdb._file._file-name,1,28) ".":U SKIP
        "def temp-table tt-":U dictdb._file._file-name " no-undo like ":U p-nom-log ".":U dictdb._file._file-name ".":U SKIP(1).
    &ENDIF


    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY   TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_5"
        tt-epc.cod-parameter = "Ponto_Execucao_5"
        tt-epc.val-parameter = p-base + ";" + String(ROWID(dictdb._file)).

    {include/i-epc201.i "execution_point_5"}
    /* --- FO 1286.314 - Repasse do Notista - FIM - */


    FIND FIRST mgadt.atrib_monitor NO-LOCK
        WHERE atrib_monitor.cod_base_dados = p-base
        AND atrib_monitor.cod_tabela     = dictdb._file._file-name NO-ERROR.
    IF AVAILABLE atrib_monitor THEN 
    DO:
        PUT STREAM s-trigger UNFORMATTED "if new ":U p-nom-log ".":U dictdb._file._file-name.
        ASSIGN 
            l-atrib = YES.
    END.

    DEFINE VARIABLE l-entrei AS LOGICAL NO-UNDO.

    ASSIGN 
        i-cont-atrib = 0.
    FOR EACH atrib_monitor NO-LOCK
        WHERE atrib_monitor.cod_base_dados = p-base
        AND atrib_monitor.cod_tabela     = dictdb._file._file-name:
        FIND dictdb._field NO-LOCK
            WHERE dictdb._field._file-recid = recid(dictdb._file) 
            AND dictdb._field._field-name = atrib_monitor.cod_atributo NO-ERROR.
        IF NOT AVAILABLE dictdb._field THEN NEXT.
        IF AVAILABLE dictdb._field THEN 
        DO:
            ASSIGN  
                i-cont-atrib = i-cont-atrib + 1
                l-entrei     = NO.

            IF (i-cont-atrib MOD 20) = 0 THEN 
            DO:
                PUT STREAM s-trigger " then do: ":U SKIP
                    "    run pi-cod-principal.":U SKIP
                    "end.":U SKIP (2)
                    "else":U SKIP
                    "if ":U.
                ASSIGN 
                    l-entrei = YES.
            END.

            IF  dictdb._field._data-type = 'date':U THEN
                ASSIGN c_field = ",~"":U + dictdb._field._format + "~"":U.
            ELSE
                ASSIGN c_field = "":U.

            ASSIGN 
                l-tem-atributo = NO.

            IF dictdb._field._extent <> 0
                THEN 
            DO:
                /*INI Luciano 19/03/2002*/
                IF i-cont-atrib = 1 
                    THEN ASSIGN  c-list-new = c-list-new + '@':U.
                ELSE ASSIGN  c-list-new = c-list-new + chr(160) + '@':U + " + '|' + ":U.
                /*FIM Luciano 19/03/2002*/
                DO i-ext = 1 TO dictdb._field._extent:
                    IF (i-cont-atrib MOD 20) = 0
                        AND NOT l-entrei
                        THEN 
                    DO:
                        ASSIGN 
                            l-tem-atributo = NO.
                        PUT STREAM s-trigger " then do: ":U SKIP
                            "    run pi-cod-principal.":U SKIP
                            "end.":U SKIP (2)
                            "else":U SKIP
                            "if ":U.
                    END.
                    ASSIGN  
                        c-list-new = c-list-new + "(if ":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + " <> ? then string(":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + c_field + ") else '') + chr(254) + ":U.

                    /*INI Luciano 19/03/2002*/
                    IF  (i-ext MOD 20) = 0
                        THEN ASSIGN  c-list-new = SUBSTRING(c-list-new,1,LENGTH(c-list-new) - 14)
                            c-list-new = c-list-new + chr(160) + '@':U + " + chr(254) + ":U.
                    /*FIM Luciano 19/03/2002*/
                    IF i-ext = dictdb._field._extent
                        THEN ASSIGN c-list-new = (  IF  (dictdb._field._extent MOD 20) = 0
                                                THEN SUBSTRING(c-list-new,1,LENGTH(c-list-new) - 14)
                                                ELSE SUBSTRING(c-list-new,1,LENGTH(c-list-new) - 14))
                                c-list-new = c-list-new + chr(160) + '@':U.
                    
                    /* Amarildo - FO 1.868.131 */
                    /* else assign c-list-ant = c-list-ant + (if dictdb._field._initial = ? then '?':U else dictdb._field._initial) + chr(254). */
                    ELSE 
                    DO:
                        IF dictdb._field._initial = ? THEN 
                            ASSIGN c-list-ant = c-list-ant + '?':U  + chr(254).
                        ELSE 
                        DO:
                            IF  _field._data-type = "date"      AND
                                SESSION:DATE-FORMAT = "dmy"     AND
                                   INDEX(_field._initial,"/") <> 0 AND
                                dictdb._field._initial <> ""
                                THEN 
                            DO:                      
                                ASSIGN 
                                    cDia = SUBSTRING(_field._initial,4,2)
                                    cMes = SUBSTRING(_field._initial,1,2)
                                    cAno = SUBSTRING(_field._initial,7,LENGTH(_field._initial)).                    
                                ASSIGN 
                                    c-list-ant = c-list-ant + string(DATE(cDia + cMes + cAno),"99/99/9999")  + chr(254).
                            END.
                            ELSE ASSIGN c-list-ant = c-list-ant + dictdb._field._initial  + chr(254).
                        END.
                    END.

                    PUT STREAM s-trigger UNFORMATTED IF ((i-cont-atrib MOD 20) = 0 AND NOT l-tem-atributo) THEN "":U ELSE " or":U SKIP
                                                     "   ":U p-nom-log ".":U atrib_monitor.cod_tabela ".":U atrib_monitor.cod_atributo "[":U i-ext "] <> old_":U substring(atrib_monitor.cod_tabela,1,28) ".":U atrib_monitor.cod_atributo "[":U i-ext "]":U.
                    ASSIGN  
                        l-tem-atributo = YES
                        i-cont-atrib   = i-cont-atrib + 1.
                END.

            END.
            ELSE 
            DO:
                ASSIGN 
                    c-list-new = c-list-new  + "(if ":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + " <> ? then string(":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + c_field + ") else '')":U.
                PUT STREAM s-trigger UNFORMATTED IF (i-cont-atrib MOD 20) = 0 THEN "":U ELSE " or":U skip 
                                                 "   ":U p-nom-log ".":U atrib_monitor.cod_tabela ".":U atrib_monitor.cod_atributo " <> old_":U substring(atrib_monitor.cod_tabela,1,28) ".":U atrib_monitor.cod_atributo.
            END.
        END.

        /* Nakamura */
        IF LENGTH(c-list-new) > 25000 THEN 
        DO:
            FIND LAST tt-list-new NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-list-new THEN
                ASSIGN i-seq-list-new = 1.
            ELSE
                ASSIGN i-seq-list-new = tt-list-new.num-seq + 1.

            CREATE tt-list-new.
            ASSIGN 
                tt-list-new.num-seq  = i-seq-list-new
                tt-list-new.conteudo = c-list-new
                c-list-new           = "".
        END.

        ASSIGN 
            c-list-campo = c-list-campo + atrib_monitor.cod_atributo + ',':U
            c-list-desc  = c-list-desc + (IF dictdb._field._label <> "" AND dictdb._field._label <> ? THEN REPLACE(dictdb._field._label,"'","") ELSE "") 
                                          + (IF dictdb._field._desc <> "" THEN " - ":U + replace(dictdb._field._desc,"'","") ELSE (IF dictdb._field._help <> "" THEN " - ":U + REPLACE(dictdb._field._help,"'","") ELSE "")) + ",":U
            c-list-new   = c-list-new + chr(160) + " + '|' + ":U.
        /* Amarildo - FO 1.868.131 */
        /* c-list-ant   = c-list-ant + (if dictdb._field._initial = ? then '?':U else dictdb._field._initial) + '|':U. */
        IF dictdb._field._initial = ? THEN 
            ASSIGN c-list-ant = c-list-ant + '?':U  + '|':U.
        ELSE 
        DO:
            IF  _field._data-type = "date"      AND
                SESSION:DATE-FORMAT = "dmy"     AND
                       INDEX(_field._initial,"/") <> 0 AND
                dictdb._field._initial <> ""
                THEN 
            DO:                      
                ASSIGN 
                    cDia = SUBSTRING(_field._initial,4,2)
                    cMes = SUBSTRING(_field._initial,1,2)
                    cAno = SUBSTRING(_field._initial,7,LENGTH(_field._initial)).                    
                ASSIGN 
                    c-list-ant = c-list-ant + string(DATE(cDia + cMes + cAno),"99/99/9999") + '|':U.
            END.
            ELSE ASSIGN c-list-ant = c-list-ant + dictdb._field._initial  + '|':U.
        END.



    END. /* for each atrib_monitor */

    ASSIGN 
        c-list-campo = SUBSTRING(c-list-campo,1,LENGTH(c-list-campo) - 1)
        c-list-desc  = SUBSTRING(c-list-desc,1,LENGTH(c-list-desc) - 1)
        c-list-new   = SUBSTRING(c-list-new,1,LENGTH(c-list-new) - 9)
        c-list-ant   = SUBSTRING(c-list-ant,1,LENGTH(c-list-ant) - 1).

    /* Nakamura */
    FIND LAST tt-list-new NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-list-new THEN
        ASSIGN i-seq-list-new = 1.
    ELSE
        ASSIGN i-seq-list-new = tt-list-new.num-seq + 1.

    CREATE tt-list-new.
    ASSIGN 
        tt-list-new.num-seq  = i-seq-list-new
        tt-list-new.conteudo = c-list-new.

    IF  i-cont-atrib <> 0 THEN 
    DO:
        PUT STREAM s-trigger " then do: ":U SKIP
            "    run pi-cod-principal.":U SKIP
            "end.":U SKIP (2).
    END.

    PUT STREAM s-trigger UNFORMATTED SKIP(2) "PROCEDURE pi-cod-principal:":U SKIP(1).

    RUN aup/au0101b.p (INPUT 2, /* retorna indice */
        INPUT dictdb._file._file-name,
        INPUT '':U,
        OUTPUT c_indice, /* Nome da Chave ou NOK */
        INPUT YES). /* Procura chave prim†ria */                         

    IF  c_indice = 'NOK':U THEN 
    DO:
        RETURN.
    END.

    IF  c_indice <> ""
        AND ENTRY(2,c_indice) <> 'DEFAULT' THEN 
    DO:
        FIND dictdb._index WHERE 
            dictdb._index._file-recid = recid(dictdb._file) AND
            dictdb._index._index-name = entry(2,c_indice) NO-LOCK NO-ERROR.     

        FOR EACH dictdb._index-field OF dictdb._index NO-LOCK
            BREAK BY dictdb._index-field._index-recid:
            FIND dictdb._field OF dictdb._index-field
                NO-LOCK NO-ERROR.

            IF  dictdb._field._data-type = 'date':U THEN
                c_field = ",~"":U + dictdb._field._format + "~"":U.
            ELSE
                c_field = "":U.

            ASSIGN 
                c-des-resumo = c-des-resumo + "'# ":U +  (IF dictdb._field._label = ? THEN dictdb._field._field-name ELSE REPLACE(dictdb._field._label,"'",""))
                                  + " (":U + p-nom-log + ".":U + dictdb._file._file-name + ".":U + dictdb._field._field-name + ")":U
                                  + "' + ' = ' + (if ":U + p-nom-log + ".":U + dictdb._file._file-name + ".":U + dictdb._field._field-name + " <> ? then string(":U + p-nom-log + ".":U + dictdb._file._file-name + ".":U + dictdb._field._field-name + c_field + ") else '?') + chr(10) + ":U + chr(10) + '                          ':U.

            IF  FIRST-OF(dictdb._index-field._index-recid) THEN 
                PUT STREAM s-trigger UNFORMATTED "    assign c_chave = (if ":U p-nom-log ".":U dictdb._file._file-name ".":U dictdb._field._field-name " <> ? then string(":U p-nom-log ".":U dictdb._file._file-name ".":U dictdb._field._field-name c_field ") else '?') + '|'":U.
            ELSE
                PUT STREAM s-trigger UNFORMATTED SKIP
                    "                   + (if ":U p-nom-log ".":U dictdb._file._file-name ".":U dictdb._field._field-name " <> ? then string(":U + p-nom-log + ".":U + dictdb._file._file-name ".":U dictdb._field._field-name c_field ") else '?') + '|'":U.
        END.     
    END.

    IF l-magnus = NO THEN
        PUT STREAM s-trigger UNFORMATTED ".":U SKIP(1)
            "    assign h-prog-name = session:last-procedure.":U SKIP
            "    repeat while valid-handle(h-prog-name):":U SKIP
            "        assign c-file-name = replace(h-prog-name:file-name,'":U + Chr(126) + "~\','/').":U SKIP
            "        if  num-entries(c-file-name,'/') = 2 then do:":U SKIP
            "            if  entry(1,c-file-name,'/') <> 'utp'   and":U SKIP
            "                entry(1,c-file-name,'/') <> 'panel' and":U SKIP
            "                index(entry(1,c-file-name,'/'),'vwr') = 0 and":U SKIP
            "                index(entry(1,c-file-name,'/'),'brw') = 0 and":U SKIP
            "                index(entry(1,c-file-name,'/'),'qry') = 0 and":U SKIP
            "                entry(2,c-file-name,'/') <> 'men905za.p'  and":U SKIP
            "                entry(2,c-file-name,'/') <> 'cdapi524.p'  and":U SKIP
            "                index(entry(2,c-file-name,'/'),'-b')  = 0 and":U SKIP
            "                index(entry(2,c-file-name,'/'),'-q')  = 0 and":U SKIP
            "                index(entry(2,c-file-name,'/'),'-v')  = 0 then do:":U SKIP
            "                assign c-prog-atualiz-aux = c-prog-atualiz-aux + caps(entry(2,c-file-name,'/')) + ','.":U SKIP
            "            end.":U SKIP
            "        end.":U SKIP(1)
            "        if  num-entries(c-file-name,'/') >= 3 then do:":U SKIP
            "            if  entry(1,c-file-name,'/') <> 'adm'        and":U SKIP
            "                entry(3,c-file-name,'/') <> 'brw'        and":U SKIP
            "                entry(3,c-file-name,'/') <> 'qry'        and":U SKIP
            "                entry(3,c-file-name,'/') <> 'vwr'        and":U SKIP
            "                entry(3,c-file-name,'/') <> 'fpapi524.p' then do:":U SKIP
            "                assign c-prog-atualiz-aux = c-prog-atualiz-aux + caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ','.":U SKIP
            "            end.":U SKIP
            "        end.":U SKIP(1)
            "        assign h-prog-name = h-prog-name:prev-sibling.":U SKIP(1)
            "        if  h-prog-name = ? or c-prog-atualiz-aux <> '' then":U SKIP
            "            leave.":U SKIP
            "    end. /* end repeat */":U SKIP(1)                                     
            "    do  i-cont = 2 to 20:":U SKIP
            "        assign c-file-name = replace(program-name(i-cont),'":U + Chr(126) + "~\','/').":U SKIP
            "        if  c-file-name = ? or c-file-name = '' then do:":U SKIP
            "            assign c-prog-atualiz-aux = c-prog-atualiz-aux + ','.":U SKIP
            "            next.":U SKIP
            "        end.":U SKIP(1)
            "        if  num-entries(c-file-name,'/') = 2 then do:":U SKIP
            "            if  index(c-file-name,'panel') <> 0 or":U SKIP
            "                index(c-file-name,'utp')   <> 0 then do:":U SKIP
            "                if num-entries(entry(1,c-file-name,'/'),' ') >= 2 then do:":U SKIP
            "                    assign c-prog-aux = entry(2,entry(1,c-file-name,'/'),' ').":U SKIP
            "                end. /* if */ ":U SKIP
            "                else do:":U SKIP
            "                    assign c-prog-aux = entry(1,c-file-name,'/').":U SKIP   
            "                end. /* else */ ":U SKIP 
            "                if  c-prog-aux = 'panel' or":U SKIP
            "                    c-prog-aux = 'utp' then next.":U SKIP
            "            end.":U SKIP
            "        end.":U SKIP
            "        if  entry(num-entries(c-file-name,'/'),c-file-name,'/') = 'cdapi524.p' or":U SKIP
            "            entry(num-entries(c-file-name,'/'),c-file-name,'/') = 'fpapi524.p' or":U SKIP
            "            entry(num-entries(c-file-name,'/'),c-file-name,'/') = 'broker.p' then":U SKIP
            "            next.":U SKIP(1)
            "        if  i-cont = 2 then":U SKIP
            "            assign c-prog-atualiz-aux = caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ',' + c-prog-atualiz-aux.":U SKIP
            "        else":U SKIP
            "            assign c-prog-atualiz-aux = c-prog-atualiz-aux + caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ','.":U SKIP
            "    end.":U SKIP(1)
            "    do  i-cont = 1 to 10:":U SKIP
            "        assign c-prog-atualiz = c-prog-atualiz + entry(i-cont,c-prog-atualiz-aux) + ','.":U SKIP
            "    end.":U SKIP.
    ELSE 
        PUT STREAM s-trigger UNFORMATTED ".":U SKIP(1)
            "    do  i-cont = 1 to 10:":U SKIP
            "        assign c-file-name = replace(program-name(i-cont),'":U + Chr(126) + "~\','/').":U SKIP
            "        if  c-file-name = ? or c-file-name = '' then do:":U SKIP
            "            assign c-prog-atualiz = c-prog-atualiz + ','.":U SKIP
            "            next.":U SKIP
            "        end.":U SKIP
            "        assign c-prog-atualiz = c-prog-atualiz + caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ','.":U SKIP
            "    end.":U SKIP.

    PUT STREAM s-trigger UNFORMATTED "    assign c-prog-atualiz  = substring(c-prog-atualiz,1,length(c-prog-atualiz) - 1)":U SKIP
        "           c-prog-name-exc = entry(2,c-prog-atualiz)":U SKIP
        "           c_chave         = substring(c_chave,1,length(c_chave) - 1)":U SKIP
        "           v-list-rpos     = '":U c-list-rpos "'.":U SKIP(1).

    PUT STREAM s-trigger UNFORMATTED "    if new ":U p-nom-log ".":U dictdb._file._file-name " then do:":U SKIP.

    IF l-create = YES THEN 
    DO:

        FOR EACH mgadt.tabela_monitor_exc NO-LOCK
            WHERE tabela_monitor_exc.cod_base_dados = p-base
            AND tabela_monitor_exc.cod_tabela     = dictdb._file._file-name
            AND tabela_monitor_exc.cod_evento     = 'C':U:
            PUT STREAM s-trigger UNFORMATTED '        ':U REPLACE(tabela_monitor_exc.des_exc,CHR(10),CHR(10) + '        ':U) SKIP(1). 
        END.                                                                                                           

        FOR EACH atrib_monitor NO-LOCK
            WHERE atrib_monitor.cod_base_dados = p-base
            AND atrib_monitor.cod_tabela     = dictdb._file._file-name:

            RUN pi-cria-alerta (INPUT atrib_monitor.cod_tabela,
                INPUT atrib_monitor.cod_atributo,
                INPUT "C":U).

        END.                                     

        PUT STREAM s-trigger UNFORMATTED "        assign  v-des-campo = '":U c-list-campo "'.":U SKIP
            "        assign  v-des-desc  = '":U c-list-desc "'.":U SKIP
            "        assign  v-val-ant   = '":U c-list-ant "'.":U SKIP
            "        assign  c-evento    = 'C'.":U SKIP.

        IF TRIM(c-list-new) = "":U THEN
            PUT STREAM s-trigger UNFORMATTED " assign  v-val-new   =  ~'~'":U.

        /* Nakamura */
        FOR EACH tt-list-new:
            ASSIGN  
                c-list-new = tt-list-new.conteudo
                c-conteudo = "".
            DO i-variavel = 1 TO NUM-ENTRIES(c-list-new,CHR(160)):
                /*INI Luciano 19/03/2002*/
                ASSIGN  
                    c-conteudo = ENTRY(i-variavel,c-list-new,CHR(160)).
                IF  SUBSTRING(c-conteudo,1,1) = "@"
                    THEN ASSIGN  c-conteudo = SUBSTRING(c-conteudo,2).
                ELSE IF  c-conteudo = " + '|' + ":U
                        THEN ASSIGN  c-conteudo = "".
                /*FIM Luciano 19/03/2002*/

                IF  c-conteudo <> ""
                    THEN 
                DO:
                    IF (i-variavel MOD 10) = 0 OR
                        i-variavel = 1 OR
                        (SUBSTRING(ENTRY(i-variavel,c-list-new,CHR(160)),1,1) = "@") THEN 
                    DO:
                        IF i-variavel = 1 AND tt-list-new.num-seq = 1 THEN
                            PUT STREAM s-trigger UNFORMATTED "        assign  v-val-new   =   ":U.
                        ELSE 
                        DO:
                            PUT STREAM s-trigger UNFORMATTED ".":U SKIP.
                            PUT STREAM s-trigger UNFORMATTED "        assign  v-val-new   =  v-val-new ":U.
                        END.
                    END.

                    /*INI Luciano 19/03/2002*/
                    PUT STREAM s-trigger UNFORMATTED c-conteudo.
                /*FIM Luciano 19/03/2002*/
                END.
            END.
        END.
        PUT STREAM s-trigger UNFORMATTED ".":U SKIP.
    END. /* if l-create */
    ELSE
        PUT STREAM s-trigger UNFORMATTED "        return RETURN-VALUE.":U SKIP.


    PUT STREAM s-trigger UNFORMATTED "    end.":U SKIP
        "    else do:":U SKIP.

    IF l-write = YES THEN 
    DO:

        ASSIGN 
            i-cont-atrib = 10.

        FOR EACH tabela_monitor_exc NO-LOCK
            WHERE tabela_monitor_exc.cod_base_dados = p-base
            AND tabela_monitor_exc.cod_tabela     = dictdb._file._file-name
            AND tabela_monitor_exc.cod_evento     = 'W':U:
            PUT STREAM s-trigger UNFORMATTED '        ':U REPLACE(tabela_monitor_exc.des_exc,CHR(10),CHR(10) + '        ':U) SKIP(1).
        END.

        FOR EACH atrib_monitor NO-LOCK
            WHERE atrib_monitor.cod_base_dados = p-base
            AND atrib_monitor.cod_tabela     = dictdb._file._file-name:

            ASSIGN 
                c-des-campo = "":U
                c-des-desc  = "":U
                c-val-new   = "":U
                c-val-ant   = "":U.

            IF (i-cont-atrib MOD 10) = 0 THEN 
            DO:
                PUT STREAM s-trigger UNFORMATTED "        run pi_atrib_":U + STRING(i-cont-atrib) + ".":U SKIP(1).
            END.

            ASSIGN 
                i-cont-atrib = i-cont-atrib + 1.
        END.
        PUT STREAM s-trigger UNFORMATTED "        assign c-evento    = 'W'":U SKIP
            "               v-des-campo = substring(v-des-campo,1,length(v-des-campo) - 1)":U SKIP
            "               v-des-desc  = substring(v-des-desc,1,length(v-des-desc) - 1)":U SKIP
            "               v-val-new   = substring(v-val-new,1,length(v-val-new) - 1)":U SKIP
            "               v-val-ant   = substring(v-val-ant,1,length(v-val-ant) - 1).":U SKIP(1).
    END. /* if l-write */
    ELSE PUT STREAM s-trigger UNFORMATTED "        return RETURN-VALUE.":U SKIP. 

    PUT STREAM s-trigger UNFORMATTED "    end.":U SKIP(1).
           
    IF l-oracle THEN
        cDatabase = SDBNAME (p-nom-log).    
    ELSE
        cDatabase = p-nom-log.    
    
    IF INTEGER(ENTRY(1,DBVERSION(cDatabase),".":U)) >= 9 THEN    
        PUT STREAM s-trigger UNFORMATTED "    find first ":U cDatabase "._myconnection no-lock no-error.":U SKIP
            "    find first ":U cDatabase "._connect no-lock":U SKIP
            "        where ":U cDatabase "._connect._connect-usr = ":U cDatabase "._myconnection._myconn-userid no-error.":U SKIP
            "    assign c-user-prog  = ":U cDatabase "._connect._connect-name":U SKIP
            "           c-terminal   = ":U cDatabase "._connect._connect-device":U SKIP
            "           i-transid    = ":U cDatabase "._connect._connect-transid.":U SKIP.
    ELSE
        PUT STREAM s-trigger UNFORMATTED "    assign c-user-prog  = ''":U SKIP
            "           c-terminal   = ''":U SKIP
            "           i-transid    = 0.":U SKIP.

    PUT STREAM s-trigger UNFORMATTED "    assign v-des-resumo = ":U c-des-resumo " chr(10).":U SKIP(1)
        "    if c-list-alert <> '' then assign c-list-alert = substring(c-list-alert,1,length(c-list-alert) - 1).":U SKIP
        "    if c-list-user  <> '' then assign c-list-user  = substring(c-list-user,1,length(c-list-user) - 1).":U SKIP(1).
&IF '{&mgadt_dbtype}' <> "Progress":U &THEN
    PUT STREAM s-trigger UNFORMATTED "    create tt-old_":U SUBSTRING(dictdb._file._file-name,1,25) ".":U SKIP
        "    create tt-":U dictdb._file._file-name ".":U SKIP(1)
        "    buffer-copy ":U p-nom-log ".old_":U SUBSTRING(dictdb._file._file-name,1,28) " to tt-old_":U SUBSTRING(dictdb._file._file-name,1,25) ".":U SKIP
        "    buffer-copy ":U p-nom-log ".":U dictdb._file._file-name " to tt-":U dictdb._file._file-name ".":U SKIP(1)
        "    raw-transfer tt-old_":U SUBSTRING(dictdb._file._file-name,1,25) " to c-raw-ant.":U SKIP
        "    raw-transfer tt-":U dictdb._file._file-name " to c-raw-new.":U SKIP(1).
&ELSE
    PUT STREAM s-trigger UNFORMATTED "    raw-transfer ":U p-nom-log ".old_":U SUBSTRING(dictdb._file._file-name,1,28) " to c-raw-ant.":U SKIP
        "    raw-transfer ":U p-nom-log ".":U dictdb._file._file-name " to c-raw-new.":U SKIP(1).
&ENDIF
    /* Amarildo - Ref FO 1.730.937 (EMS2) e 1.725.504 (EMS5) */
    PUT STREAM s-trigger UNFORMATTED "    IF LDBNAME('dictdb') <> ?":u SKIP
        "       THEN assign c-dictdb = LDBNAME('dictdb').":u SKIP
        "       ELSE DO:":u SKIP
        "          assign c-dictdb = LDBNAME(1).":u SKIP
        "          Create Alias dictdb For Database value(c-dictdb).":u SKIP
        "       END.":u SKIP
        "    IF ":u l-full " THEN DO:":u SKIP
        "       Find First base_dados":u SKIP
        "          Where base_dados.cod_base_dados = '" p-base "'" SKIP
        "          No-lock No-error.":u SKIP
        "       If Avail base_dados Then Do:":u SKIP
        "          Create Alias dictdb For Database Value(base_dados.nom_logic_base).":u SKIP
        "       End.":u SKIP
        "       Else Do:":u SKIP
        "          Create Alias dictdb For Database Value(Ldbname('":u p-base "')).":u SKIP
        "       End.":u SKIP
        "    END.":u SKIP.

    PUT STREAM s-trigger UNFORMATTED "    run aup/auapi003.p (input '":u p-base "',":u SKIP
        "                        input '":U dictdb._file._file-name "',":U SKIP
        "                        input ":U l-full ",":U SKIP
        "                        input c-evento,":U SKIP
        "                        input c_chave,":U  SKIP
        "                        input ":U c-var-user SKIP
        "                        input c-user-prog,":U SKIP
        "                        input c-terminal,":U SKIP
        "                        input c-prog-atualiz,":U SKIP
        "                        input i-transid,":U SKIP
        "                        input v-des-resumo,":U SKIP
        "                        input v-des-campo,":U SKIP
        "                        input v-des-desc,":U SKIP
        "                        input '":U  IF dictdb._file._desc <> "" THEN REPLACE(SUBSTRING(dictdb._file._desc,1,72),"'","") ELSE dictdb._file._file-label "',":U SKIP
        "                        input v-list-rpos,":U SKIP
        "                        input v-val-new,":U SKIP
        "                        input v-val-ant,":U SKIP
        "                        input c-list-alert,":U SKIP
        "                        input c-list-user,":U SKIP
        "                        input c-raw-ant,":U SKIP
        "                        input c-raw-new).":U SKIP.

    /* Amarildo - Ref FO 1.730.937 (EMS2) e 1.725.504 (EMS5) */
    PUT STREAM s-trigger UNFORMATTED "    Create Alias dictdb For Database value(c-dictdb).":u SKIP.

    PUT STREAM s-trigger "END PROCEDURE.":U SKIP (2).

    IF l-write = YES THEN 
    DO:

        ASSIGN 
            i-cont-atrib   = 10
            l-tem-atributo = NO.
        FOR EACH atrib_monitor NO-LOCK
            WHERE atrib_monitor.cod_base_dados = p-base
            AND atrib_monitor.cod_tabela     = dictdb._file._file-name:

            ASSIGN 
                c-des-campo    = "":U
                c-des-desc     = "":U
                c-val-new      = "":U
                c-val-ant      = "":U
                i-nr-atrib     = 0
                l-tem-atributo = YES.

            FIND dictdb._field NO-LOCK
                WHERE dictdb._field._file-recid = recid(dictdb._file) 
                AND dictdb._field._field-name = atrib_monitor.cod_atributo NO-ERROR.

            IF NOT AVAILABLE dictdb._field THEN NEXT.

            IF (i-cont-atrib MOD 10) = 0 THEN 
            DO:

                IF i-cont-atrib <> 10 THEN 
                DO:
                    PUT STREAM s-trigger UNFORMATTED "END PROCEDURE.":U SKIP(1).
                END.

                PUT STREAM s-trigger UNFORMATTED "PROCEDURE pi_atrib_":U + STRING(i-cont-atrib) + ":":U SKIP(1).
            END.

            PUT STREAM s-trigger UNFORMATTED "    assign l-processa = no. ":U SKIP.
            PUT STREAM s-trigger UNFORMATTED "    if ":U.

            IF AVAILABLE dictdb._field THEN 
            DO:
                IF  dictdb._field._data-type = 'date':U THEN
                    c_field = ",~"":U + dictdb._field._format + "~"":U.
                ELSE
                    c_field = "":U.

                IF dictdb._field._extent <> 0
                    THEN 
                DO:
                    DO i-ext = 1 TO dictdb._field._extent:
                        ASSIGN  
                            c-val-new = c-val-new + "(if ":U + p-nom-log + ".":U            +           atrib_monitor.cod_tabela       + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + " <> ? then string(":U + p-nom-log + ".":U            +           atrib_monitor.cod_tabela       + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + c_field + ") else '') + chr(254) + ":U
                            c-val-ant = c-val-ant + "(if ":U + p-nom-log + ".":U + "old_":U + substring(atrib_monitor.cod_tabela,1,28) + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + " <> ? then string(":U + p-nom-log + ".":U + "old_":U + substring(atrib_monitor.cod_tabela,1,28) + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + c_field + ") else '') + chr(254) + ":U.

                        IF i-ext = dictdb._field._extent
                            THEN ASSIGN c-val-new  = SUBSTRING(c-val-new,1,LENGTH(c-val-new) - 11)
                                c-val-ant  = SUBSTRING(c-val-ant,1,LENGTH(c-val-ant) - 11)
                                c-clausula = "then assign l-processa = yes.":U + chr(10).
                        ELSE 
                        DO:
                            IF  (i-ext MOD 20) = 0
                                THEN 
                            DO:
                                /*INI Luciano 19/03/2002*/
                                ASSIGN  
                                    c-val-new = SUBSTRING(c-val-new,1,LENGTH(c-val-new) - 14)
                                    c-val-new = c-val-new + "@":U + " chr(254) + ":U
                                    c-val-ant = SUBSTRING(c-val-ant,1,LENGTH(c-val-ant) - 14)
                                    c-val-ant = c-val-ant + "@":U + " chr(254) + ":U.
                                /*FIM Luciano 19/03/2002*/
                                ASSIGN  
                                    c-clausula = "then assign l-processa = yes.":U + chr(10)
                                    c-clausula = c-clausula + "    if ":U.
                            END.
                            ELSE 
                                ASSIGN c-clausula = "or":U + chr(10) + "           ":U.
                        END.

                        PUT STREAM s-trigger UNFORMATTED p-nom-log ".":U atrib_monitor.cod_tabela ".":U atrib_monitor.cod_atributo "[":U i-ext "] <> ":U p-nom-log ".":U "old_":U SUBSTRING(atrib_monitor.cod_tabela,1,28) ".":U atrib_monitor.cod_atributo "[":U i-ext "] ":U c-clausula.
                    END.
                END.
                ELSE 
                DO:
                    ASSIGN  
                        c-val-new = c-val-new + "(if ":U + p-nom-log + ".":U +                      atrib_monitor.cod_tabela       + ".":U + atrib_monitor.cod_atributo + " <> ? then string(":U + p-nom-log + ".":U +                      atrib_monitor.cod_tabela       + ".":U + atrib_monitor.cod_atributo + c_field + ") else '') + ":U
                        c-val-ant = c-val-ant + "(if ":U + p-nom-log + ".":U + "old_":U + substring(atrib_monitor.cod_tabela,1,28) + ".":U + atrib_monitor.cod_atributo + " <> ? then string(":U + p-nom-log + ".":U + "old_":U + substring(atrib_monitor.cod_tabela,1,28) + ".":U + atrib_monitor.cod_atributo + c_field + ") else '') + ":U.

                    PUT STREAM s-trigger UNFORMATTED p-nom-log ".":U atrib_monitor.cod_tabela ".":U atrib_monitor.cod_atributo " <> ":U p-nom-log ".":U "old_":U SUBSTRING(atrib_monitor.cod_tabela,1,28) ".":U atrib_monitor.cod_atributo " then assign l-processa = yes.":U SKIP(1).
                END.
                ASSIGN 
                    c-des-desc = c-des-desc + (IF dictdb._field._label <> "" AND dictdb._field._label <> ? THEN REPLACE(dictdb._field._label,"'","") ELSE "") 
                                               + (IF dictdb._field._desc <> "" THEN " - ":U + replace(dictdb._field._desc,"'","") ELSE (IF dictdb._field._help <> "" THEN " - ":U + REPLACE(dictdb._field._help,"'","") ELSE "")) + ",":U.
            END. /* avail dictdb._field */

            ASSIGN 
                c-des-campo = atrib_monitor.cod_atributo + ',':U
                c-val-new   = c-val-new + "'|' @":U
                c-val-ant   = c-val-ant + "'|' @":U.

            PUT STREAM s-trigger UNFORMATTED "    assign l-envia-alerta = no.":U SKIP
                "    if  l-processa = yes then do:":U SKIP.

            RUN pi-cria-alerta (INPUT atrib_monitor.cod_tabela,
                INPUT atrib_monitor.cod_atributo,
                INPUT "W":U).

            PUT STREAM s-trigger UNFORMATTED "        assign v-des-campo = v-des-campo + '":U c-des-campo "'.":U SKIP
                "        assign v-des-desc  = v-des-desc  + '":U c-des-desc  "'.":U SKIP.


            DO i-ext = 1 TO NUM-ENTRIES(c-val-new,"@"):
                IF  ENTRY(i-ext,c-val-new,"@") <> "" THEN
                    PUT STREAM s-trigger UNFORMATTED "        assign v-val-new   = v-val-new   + ":U ENTRY(i-ext,c-val-new,"@")
                        ".":U SKIP.
            END.
            DO i-ext = 1 TO NUM-ENTRIES(c-val-ant,"@"):
                IF  ENTRY(i-ext,c-val-ant,"@") <> "" THEN
                    PUT STREAM s-trigger UNFORMATTED "        assign v-val-ant   = v-val-ant   + ":U ENTRY(i-ext,c-val-ant,"@")
                        ".":U SKIP.
            END.
            PUT STREAM s-trigger UNFORMATTED "    end.":U SKIP(1).

            ASSIGN 
                i-cont-atrib = i-cont-atrib + 1.

        END. /* for each atrib_monitor */

        IF l-tem-atributo THEN
            PUT STREAM s-trigger UNFORMATTED "END PROCEDURE.":U SKIP(1).
    END. /* l-write */

    OUTPUT stream s-trigger close.

END PROCEDURE.

PROCEDURE pi-cria-prog-delete:

    DEFINE INPUT PARAMETER nome AS CHARACTER.

    DEFINE VARIABLE c-des-resumo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i-variavel   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i-cont-atrib AS INTEGER   NO-UNDO.

    ASSIGN 
        c-des-campo = '':U
        c-des-desc  = '':U
        c-val-ant   = '':U
        c-val-new   = '':U.

    FOR EACH tt-val-ant:
        DELETE tt-val-ant.
    END.

    OUTPUT stream s-trigger to value(nome).

    PUT STREAM s-trigger UNFORMATTED "TRIGGER PROCEDURE FOR REPLICATION-DELETE OF ":U p-nom-log ".":U dictdb._file._file-name ".":U SKIP(1)
        "def new global shared var v_cod_usuar_corren as character format 'x(12)' label 'Usu†rio Corrente' column-label 'Usu†rio Corrente' no-undo.":U SKIP
        "def new global shared var global_userid as character format 'x(12)' label 'Usu†rio Corrente'.":U SKIP
        "def new global shared var v_cod_aplicat_dtsul_corren as character format 'x(3)' no-undo.":U SKIP
        "def new global shared var v_cod_empres_usuar as character format 'x(3)' label 'Empresa' column-label 'Empresa' no-undo.":U SKIP
                    /*Tratamento para Datasul 10 e 11 para a variavel de empresa do ems2*/
                    /*Tipo do campo muda conforme a vers∆o do banco emsfnd*/
                    &IF "{&EMSFND_VERSION}" = "1.00" &THEN
        "def new global shared var i-ep-codigo-usuario As Integer no-undo.":U SKIP
                    &ELSEIF '{&emsfnd_version}' >= '1.01' &THEN 
        "def new global shared var i-ep-codigo-usuario As Character no-undo.":U SKIP
                    &ENDIF
        c-usuar-espec SKIP
        "def var c-prog-atualiz     as char no-undo.":U SKIP
        "def var c-evento           as char no-undo.":U SKIP
        "def var c_chave            as char no-undo.":U SKIP
        "def var v-des-resumo       as char no-undo.":U SKIP
        "def var v-des-campo        as char no-undo.":U SKIP
        "def var v-des-desc         as char no-undo.":U SKIP
        "def var v-list-rpos        as char no-undo.":U SKIP
        "def var v-val-ant          as char no-undo.":U SKIP
        "def var v-val-new          as char no-undo.":U SKIP
        "def var c-user-prog        as char no-undo.":U SKIP
        "def var c-terminal         as char no-undo.":U SKIP
        "def var i-transid          as int  no-undo.":U SKIP
        "def var i-cont             as int  no-undo.":U SKIP
        "def var c-list-alert       as char no-undo.":U SKIP
        "def var c-list-user        as char no-undo.":U SKIP
        "def var c-raw-ant          as raw  no-undo.":U SKIP
        "def var c-raw-new          as raw  no-undo.":U SKIP
        "def var c-prog-name-exc    as char no-undo.":U SKIP
        "def var l-envia-alerta     as log initial no no-undo.":U SKIP(1)
        "def var c-prog-atualiz-aux as char no-undo.":U SKIP
        "def var c-file-name        as char no-undo.":U SKIP
        "def var c-prog-aux         as char no-undo.":U SKIP
        "def var h-prog-name        as widget-handle no-undo.":U SKIP
        "def var c-dictdb           as char no-undo.":U SKIP(1).


    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY   TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_6"
        tt-epc.cod-parameter = "Ponto_Execucao_6"
        tt-epc.val-parameter = "".

    {include/i-epc201.i "execution_point_6"}
    /* --- FO 1286.314 - Repasse do Notista - FIM - */


    RUN aup/au0101b.p (INPUT 2, /* retorna indice */
        INPUT dictdb._file._file-name,
        INPUT '':U,
        OUTPUT c_indice, /* Nome da Chave ou NOK */
        INPUT YES). /* Procura chave prim†ria */

    IF  c_indice = 'NOK':U THEN 
    DO:
        RETURN.
    END.

    IF  c_indice <> ""
        AND ENTRY(2,c_indice) <> 'DEFAULT' THEN 
    DO:                
        FIND dictdb._index WHERE 
            dictdb._index._file-recid = recid(dictdb._file) AND
            dictdb._index._index-name = entry(2,c_indice) NO-LOCK NO-ERROR.

        FOR EACH dictdb._index-field OF dictdb._index NO-LOCK
            BREAK BY dictdb._index-field._index-recid:
            FIND dictdb._field OF dictdb._index-field
                NO-LOCK NO-ERROR.

            IF  dictdb._field._data-type = 'date':U THEN
                c_field = ",~"":U + dictdb._field._format + "~"":U.
            ELSE
                c_field = "":U.

            ASSIGN 
                c-des-resumo = c-des-resumo + "'# ":U + (IF dictdb._field._label = ? THEN dictdb._field._field-name ELSE REPLACE(dictdb._field._label,"'",""))
                                  + " (":U + p-nom-log + ".":U + dictdb._file._file-name + ".":U + dictdb._field._field-name + ")":U
                                  + "' + ' = ' + (if ":U + p-nom-log + ".":U + dictdb._file._file-name + ".":U + dictdb._field._field-name + " <> ? then string(":U + p-nom-log + ".":U + dictdb._file._file-name + ".":U + dictdb._field._field-name + c_field + ") else '?') + chr(10) + ":U + chr(10) + '                      ':U.

            IF  FIRST-OF(dictdb._index-field._index-recid) THEN 
                PUT STREAM s-trigger UNFORMATTED "assign c_chave = (if ":U dictdb._file._file-name ".":U dictdb._field._field-name " <> ? then string(":U dictdb._file._file-name ".":U dictdb._field._field-name c_field ") else '?') + '|'":U. 
            ELSE
                PUT STREAM s-trigger UNFORMATTED SKIP
                    "               + (if ":U dictdb._file._file-name ".":U dictdb._field._field-name " <> ? then string(":U dictdb._file._file-name ".":U dictdb._field._field-name c_field ") else '?') + '|'":U.
        END.     
    END.

    IF l-magnus = NO THEN 
        PUT STREAM s-trigger UNFORMATTED ".":U SKIP(1)
            "assign h-prog-name = session:last-procedure.":U SKIP
            "repeat while valid-handle(h-prog-name):":U SKIP
            "    assign c-file-name = replace(h-prog-name:file-name,'":U + Chr(126) + "~\','/').":U SKIP
            "    if  num-entries(c-file-name,'/') = 2 then do:":U SKIP
            "        if  entry(1,c-file-name,'/') <> 'utp'   and":U SKIP
            "            entry(1,c-file-name,'/') <> 'panel' and":U SKIP
            "            index(entry(1,c-file-name,'/'),'vwr') = 0 and":U SKIP
            "            index(entry(1,c-file-name,'/'),'brw') = 0 and":U SKIP
            "            index(entry(1,c-file-name,'/'),'qry') = 0 and":U SKIP
            "            entry(2,c-file-name,'/') <> 'men905za.p'  and":U SKIP
            "            entry(2,c-file-name,'/') <> 'cdapi524.p'  and":U SKIP
            "            index(entry(2,c-file-name,'/'),'-b')  = 0 and":U SKIP
            "            index(entry(2,c-file-name,'/'),'-q')  = 0 and":U SKIP
            "            index(entry(2,c-file-name,'/'),'-v')  = 0 then do:":U SKIP
            "            assign c-prog-atualiz-aux = c-prog-atualiz-aux + caps(entry(2,c-file-name,'/')) + ','.":U SKIP
            "        end.":U SKIP
            "    end.":U SKIP(1)
            "    if  num-entries(c-file-name,'/') >= 3 then do:":U SKIP
            "        if  entry(1,c-file-name,'/') <> 'adm'        and":U SKIP
            "            entry(2,c-file-name,'/') <> 'brw'        and":U SKIP
            "            entry(2,c-file-name,'/') <> 'qry'        and":U SKIP
            "            entry(2,c-file-name,'/') <> 'vwr'        and":U SKIP
            "            entry(3,c-file-name,'/') <> 'fpapi524.p' then do:":U SKIP
            "            assign c-prog-atualiz-aux = c-prog-atualiz-aux + caps(entry(2,c-file-name,'/')) + ','.":U SKIP
            "        end.":U SKIP
            "    end.":U SKIP(1)
            "    assign h-prog-name = h-prog-name:prev-sibling.":U SKIP(1)
            "    if  h-prog-name = ? or c-prog-atualiz-aux <> '' then":U SKIP
            "        leave.":U SKIP
            "end. /* end repeat */":U SKIP(1)                                     
            "do  i-cont = 2 to 20:":U SKIP
            "    assign c-file-name = replace(program-name(i-cont),'":U + Chr(126) + "~\','/').":U SKIP
            "    if  c-file-name = ? or c-file-name = '' then do:":U SKIP
            "        assign c-prog-atualiz-aux = c-prog-atualiz-aux + ','.":U SKIP
            "        next.":U SKIP
            "    end.":U SKIP(1)
            "    if  num-entries(c-file-name,'/') = 2 then do:":U SKIP
            "        if  index(c-file-name,'panel') <> 0 or":U SKIP
            "            index(c-file-name,'utp')   <> 0 then do:":U SKIP
            "            if num-entries(entry(1,c-file-name,'/'),' ') >= 2 then do:":U SKIP
            "               assign c-prog-aux = entry(2,entry(1,c-file-name,'/'),' ').":U SKIP
            "            end. /* if */ ":U SKIP
            "            else do:":U SKIP
            "               assign c-prog-aux = entry(1,c-file-name,'/').":U SKIP   
            "            end. /* else */ ":U SKIP 
            "            if  c-prog-aux = 'panel' or":U SKIP
            "                c-prog-aux = 'utp' then next.":U SKIP
            "        end.":U SKIP
            "    end.":U SKIP
            "    if  entry(num-entries(c-file-name,'/'),c-file-name,'/') = 'cdapi524.p' or":U SKIP
            "        entry(num-entries(c-file-name,'/'),c-file-name,'/') = 'fpapi524.p' or":U SKIP
            "        entry(num-entries(c-file-name,'/'),c-file-name,'/') = 'broker.p' then":U SKIP
            "        next.":U SKIP(1)
            "    if  i-cont = 2 then":U SKIP
            "        assign c-prog-atualiz-aux = caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ',' + c-prog-atualiz-aux.":U SKIP
            "    else":U SKIP
            "        assign c-prog-atualiz-aux = c-prog-atualiz-aux + caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ','.":U SKIP
            "end.":U SKIP(1)
            "do  i-cont = 1 to 10:":U SKIP
            "    assign c-prog-atualiz = c-prog-atualiz + entry(i-cont,c-prog-atualiz-aux) + ','.":U SKIP
            "end.":U SKIP.
    ELSE 
        PUT STREAM s-trigger UNFORMATTED ".":U SKIP(1)
            "do  i-cont = 1 to 10:":U SKIP
            "    assign c-file-name = replace(program-name(i-cont),'":U + Chr(126) + "~\','/').":U SKIP
            "    if  c-file-name = ? or c-file-name = '' then do:":U SKIP
            "        assign c-prog-atualiz = c-prog-atualiz + ','.":U SKIP
            "        next.":U SKIP
            "    end.":U SKIP
            "    assign c-prog-atualiz = c-prog-atualiz + caps(entry(num-entries(c-file-name,'/'),c-file-name,'/')) + ','.":U SKIP
            "end.":U SKIP.

    PUT STREAM s-trigger UNFORMATTED "assign c-prog-atualiz  = substring(c-prog-atualiz,1,length(c-prog-atualiz) - 1)":U SKIP
        "       c-prog-name-exc = entry(2,c-prog-atualiz)":U SKIP
        "       c_chave         = substring(c_chave,1,length(c_chave) - 1)":U SKIP
        "       v-list-rpos     = '":U c-list-rpos "'.":U SKIP(1).

    FOR EACH mgadt.tabela_monitor_exc NO-LOCK
        WHERE tabela_monitor_exc.cod_base_dados = p-base
        AND tabela_monitor_exc.cod_tabela     = dictdb._file._file-name
        AND tabela_monitor_exc.cod_evento     = 'D':U:
        PUT STREAM s-trigger UNFORMATTED tabela_monitor_exc.des_exc SKIP(1).
    END.

    ASSIGN 
        i-cont-atrib = 0.
    FOR EACH mgadt.atrib_monitor NO-LOCK
        WHERE atrib_monitor.cod_base_dados = p-base
        AND atrib_monitor.cod_tabela     = dictdb._file._file-name:

        FIND dictdb._field NO-LOCK
            WHERE dictdb._field._file-recid = recid(dictdb._file) 
            AND dictdb._field._field-name = atrib_monitor.cod_atributo NO-ERROR.

        IF NOT AVAILABLE dictdb._field THEN NEXT.
        IF AVAILABLE dictdb._field THEN 
        DO:

            ASSIGN 
                i-cont-atrib = i-cont-atrib + 1.

            IF  dictdb._field._data-type = 'date':U THEN
                c_field = ",~"":U + dictdb._field._format + "~"":U.
            ELSE
                c_field = "":U.

            IF dictdb._field._extent <> 0 THEN 
            DO:
                /*INI Luciano 19/03/2002*/
                IF i-cont-atrib = 1 
                    THEN ASSIGN  c-val-ant = c-val-ant + '@':U.
                ELSE ASSIGN  c-val-ant = c-val-ant + chr(160) + '@':U + " + '|' + ":U.
                /*FIM Luciano 19/03/2002*/
                DO i-ext = 1 TO dictdb._field._extent:
                    ASSIGN 
                        c-val-ant = c-val-ant + "(if ":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + " <> ? then string(":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + "[":U + string(i-ext) + "]":U + c_field + ") else '') + chr(254) + ":U.

                    /*INI Luciano 19/03/2002*/
                    IF  (i-ext MOD 10) = 0
                        THEN ASSIGN  c-val-ant = SUBSTRING(c-val-ant,1,LENGTH(c-val-ant) - 14)
                            c-val-ant = c-val-ant  + chr(160) + '@':U + ' + chr(254) + ':U.

                    /*FIM Luciano 19/03/2002*/

                    IF i-ext = dictdb._field._extent
                        THEN ASSIGN c-val-ant = (IF  (dictdb._field._extent MOD 10) = 0
                                             THEN SUBSTRING(c-val-ant,1,LENGTH(c-val-ant) - 14)
                                             ELSE SUBSTRING(c-val-ant,1,LENGTH(c-val-ant) - 14))
                                c-val-ant  = c-val-ant  + chr(160) + '@':U.
                    ELSE ASSIGN c-val-new = c-val-new + '':U + chr(254).
                END.
            END.
            ELSE 
            DO:
                ASSIGN 
                    c-val-ant = c-val-ant + "(if ":U + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + " <> ? then string(":U     + p-nom-log + ".":U + atrib_monitor.cod_tabela + ".":U + atrib_monitor.cod_atributo + c_field + ") else '')":U.
            END.

            /* Nakamura */
            IF LENGTH(c-val-ant) > 25000 THEN 
            DO:
                FIND LAST tt-val-ant NO-LOCK NO-ERROR.
                IF NOT AVAILABLE tt-val-ant THEN
                    ASSIGN i-seq-val-ant = 1.
                ELSE
                    ASSIGN i-seq-val-ant = tt-val-ant.num-seq + 1.

                CREATE tt-val-ant.
                ASSIGN 
                    tt-val-ant.num-seq  = i-seq-val-ant
                    tt-val-ant.conteudo = c-val-ant
                    c-val-ant           = "".
            END.

            ASSIGN 
                c-des-campo = c-des-campo + atrib_monitor.cod_atributo + ',':U
                c-des-desc  = c-des-desc + (IF dictdb._field._label <> "" AND dictdb._field._label <> ? THEN REPLACE(dictdb._field._label,"'","") ELSE "") 
                                            + (IF dictdb._field._desc <> "" THEN " - ":U + replace(dictdb._field._desc,"'","") ELSE (IF dictdb._field._help <> "" THEN " - ":U + REPLACE(dictdb._field._help,"'","") ELSE "")) + ",":U
                c-val-ant   = c-val-ant + chr(160) + " + '|' + ":U
                c-val-new   = c-val-new + '':U + '|':U.

        END /* avail dictdb._field */. 

        RUN pi-cria-alerta (INPUT atrib_monitor.cod_tabela,
            INPUT atrib_monitor.cod_atributo,
            INPUT "D":U).
    END /* for each atrib_monitor */.

    PUT STREAM s-trigger UNFORMATTED "assign v-des-campo  = '":U SUBSTRING(c-des-campo,1,LENGTH(c-des-campo) - 1) "'.":U SKIP
        "assign v-des-desc   = '":U SUBSTRING(c-des-desc,1,LENGTH(c-des-desc) - 1) "'.":U SKIP
        "assign v-val-new    = '":U SUBSTRING(c-val-new,1,LENGTH(c-val-new) - 1) "'.":U SKIP
        "assign c-raw-new    = ?.":U SKIP
        "assign c-evento     = 'D'.":U SKIP
        "assign v-des-resumo = ":U c-des-resumo " chr(10).":U SKIP(1).

    IF TRIM(c-val-ant) = "":U THEN
        PUT STREAM s-trigger UNFORMATTED "assign v-val-ant    =  ~'~'":U.

    ASSIGN 
        c-val-ant = SUBSTRING(c-val-ant,1,LENGTH(c-val-ant) - 9).

    /* Nakamura */
    FIND LAST tt-val-ant NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-val-ant THEN
        ASSIGN i-seq-val-ant = 1.
    ELSE
        ASSIGN i-seq-val-ant = tt-val-ant.num-seq + 1.

    CREATE tt-val-ant.
    ASSIGN 
        tt-val-ant.num-seq  = i-seq-val-ant
        tt-val-ant.conteudo = c-val-ant.

    FOR EACH tt-val-ant:
        ASSIGN  
            c-val-ant  = tt-val-ant.conteudo
            c-conteudo = "".
        DO i-variavel = 1 TO NUM-ENTRIES(c-val-ant,CHR(160)):
            /*INI Luciano 19/03/2002*/
            ASSIGN  
                c-conteudo = ENTRY(i-variavel,c-val-ant,CHR(160)).
            IF  SUBSTRING(c-conteudo,1,1) = "@"
                THEN ASSIGN  c-conteudo = SUBSTRING(c-conteudo,2).
            ELSE IF  c-conteudo = " + '|' + "
                    THEN ASSIGN  c-conteudo = "".
            /*FIM Luciano 19/03/2002*/

            IF  c-conteudo <> ""
                THEN 
            DO:
                IF (i-variavel MOD 10) = 0
                    OR i-variavel = 1
                    OR (SUBSTRING(ENTRY(i-variavel,c-val-ant,CHR(160)),1,1) = "@")
                    THEN 
                DO:
                    IF i-variavel = 1 AND tt-val-ant.num-seq = 1 THEN
                        PUT STREAM s-trigger UNFORMATTED "assign v-val-ant    =  ":U.
                    ELSE 
                    DO:
                        PUT STREAM s-trigger UNFORMATTED ".":U SKIP.
                        PUT STREAM s-trigger UNFORMATTED "assign v-val-ant    = v-val-ant ":U.
                    END.
                END.

                /*INI Luciano 19/03/2002*/
                PUT STREAM s-trigger UNFORMATTED c-conteudo.
            /*FIM Luciano 19/03/2002*/
            END.
        END.
    END.
    PUT STREAM s-trigger UNFORMATTED ".":U SKIP(1).

    IF l-oracle THEN
        cDatabase = SDBNAME (p-nom-log).    
    ELSE
        cDatabase = p-nom-log.
        
    IF INTEGER(ENTRY(1,DBVERSION(cDatabase),".":U)) >= 9 THEN
        PUT STREAM s-trigger UNFORMATTED "find first ":U cDatabase "._myconnection no-lock no-error.":U SKIP
            "find first ":U cDatabase "._connect no-lock":U SKIP
            "    where ":U cDatabase "._connect._connect-usr = ":U cDatabase "._myconnection._myconn-userid no-error.":U SKIP
            "assign c-user-prog  = ":U cDatabase "._connect._connect-name":U SKIP
            "       c-terminal   = ":U cDatabase "._connect._connect-device":U SKIP
            "       i-transid    = ":U cDatabase "._connect._connect-transid.":U SKIP(1).
    ELSE
        PUT STREAM s-trigger UNFORMATTED "assign c-user-prog  = ''":U SKIP
            "       c-terminal   = ''":U SKIP
            "       i-transid    = 0.":U SKIP(1).

    PUT STREAM s-trigger UNFORMATTED "if c-list-alert <> '' then assign c-list-alert = substring(c-list-alert,1,length(c-list-alert) - 1).":U SKIP
        "if c-list-user  <> '' then assign c-list-user  = substring(c-list-user,1,length(c-list-user) - 1).":U SKIP(1).
&IF '{&mgadt_dbtype}' <> "Progress":U &THEN
    PUT STREAM s-trigger UNFORMATTED "def temp-table tt-ant_":U SUBSTRING(dictdb._file._file-name,1,25) " no-undo like ":U p-nom-log ".":U dictdb._file._file-name ".":U SKIP
        "create tt-ant_":U SUBSTRING(dictdb._file._file-name,1,25) ".":U SKIP
        "buffer-copy ":U p-nom-log ".":U dictdb._file._file-name " to tt-ant_":U SUBSTRING(dictdb._file._file-name,1,25) ".":U SKIP
        "raw-transfer tt-ant_":U SUBSTRING(dictdb._file._file-name,1,25) " to c-raw-ant.":U SKIP(1).
&ELSE
    PUT STREAM s-trigger UNFORMATTED "raw-transfer ":U p-nom-log ".":U dictdb._file._file-name " to c-raw-ant.":U SKIP(1).
&ENDIF

    /* Amarildo - Ref FO 1.730.937 (EMS2) e 1.725.504 (EMS5) */
    PUT STREAM s-trigger UNFORMATTED "    IF LDBNAME('dictdb') <> ?":u SKIP
        "       THEN assign c-dictdb = LDBNAME('dictdb').":u SKIP
        "       ELSE DO:":u SKIP
        "          assign c-dictdb = LDBNAME(1).":u SKIP
        "          Create Alias dictdb For Database value(c-dictdb).":u SKIP
        "       END.":u SKIP
        "    IF ":u l-full " THEN DO:":u SKIP
        "       Find First base_dados":u SKIP
        "          Where base_dados.cod_base_dados = '" p-base "'" SKIP
        "          No-lock No-error.":u SKIP
        "       If Avail base_dados Then Do:":u SKIP
        "          Create Alias dictdb For Database Value(base_dados.nom_logic_base).":u SKIP
        "       End.":u SKIP
        "       Else Do:":u SKIP
        "          Create Alias dictdb For Database Value(Ldbname('":u p-base "')).":u SKIP
        "       End.":u SKIP
        "    END.":u SKIP.

    PUT STREAM s-trigger UNFORMATTED "run aup/auapi003.p (input '":u p-base "',":u SKIP
        "                    input '":U dictdb._file._file-name "',":U SKIP
        "                    input ":U l-full ",":U SKIP
        "                    input c-evento,":U SKIP
        "                    input c_chave,":U  SKIP
        "                    input ":U c-var-user SKIP
        "                    input c-user-prog,":U SKIP
        "                    input c-terminal,":U SKIP
        "                    input c-prog-atualiz,":U SKIP
        "                    input i-transid,":U SKIP
        "                    input v-des-resumo,":U SKIP
        "                    input v-des-campo,":U SKIP
        "                    input v-des-desc,":U SKIP
        "                    input '":U  IF dictdb._file._desc <> "" THEN REPLACE(SUBSTRING(dictdb._file._desc,1,72),"'","") ELSE dictdb._file._file-label "',":U SKIP
        "                    input v-list-rpos,":U SKIP
        "                    input v-val-new,":U SKIP
        "                    input v-val-ant,":U SKIP
        "                    input c-list-alert,":U SKIP
        "                    input c-list-user,":U SKIP
        "                    input c-raw-ant,":U SKIP
        "                    input c-raw-new).":U SKIP.

    /* Amarildo - Ref FO 1.730.937 (EMS2) e 1.725.504 (EMS5) */
    PUT STREAM s-trigger UNFORMATTED "    Create Alias dictdb For Database value(c-dictdb).":u SKIP.

    /* --- FO 1286.314 - Repasse do Notista - INI - */
    EMPTY   TEMP-TABLE tt-epc.
    CREATE  tt-epc.
    ASSIGN  
        tt-epc.cod-event     = "Execution_Point_7"
        tt-epc.cod-parameter = "Ponto_Execucao_7"
        tt-epc.val-parameter = p-base + ";" + String(dictdb._file._file-name).

    {include/i-epc201.i "execution_point_7"}
    /* --- FO 1286.314 - Repasse do Notista - FIM - */

    OUTPUT stream s-trigger close.

END PROCEDURE.

PROCEDURE pi-cria-evento:

    DEFINE INPUT PARAMETER evento AS CHARACTER.
    DEFINE INPUT PARAMETER nome   AS CHARACTER.

    IF l-progress = YES THEN 
    DO:
        FIND dictdb._file-trig
            WHERE dictdb._file-trig._file-recid = recid(dictdb._file)
            AND   dictdb._file-trig._event      = evento
            EXCLUSIVE-LOCK NO-ERROR.    

        IF  AVAILABLE dictdb._file-trig THEN 
        DO:
            IF  NOT l-monitora THEN 
            DO: 
                DELETE dictdb._file-trig.
            END. /* if */
            ELSE 
            DO:
                IF dictdb._file-trig._proc-name <> nome THEN 
                DO: /* caso tenha selecionado ou n∆o desmarcado para adicionar codigo da base de dados no caminho da trigger */
                    DELETE dictdb._file-trig.
                END. /* if */
            END. /* else */
        END. /* if */

        IF l-monitora = YES
            AND NOT AVAILABLE dictdb._file-trig THEN 
        DO:
            CREATE dictdb._file-trig.
            ASSIGN 
                dictdb._file-trig._override   = NO
                dictdb._file-trig._file-recid = RECID(dictdb._file)
                dictdb._file-trig._trig-crc   = ?
                dictdb._file-trig._event      = evento
                dictdb._file-trig._proc-name  = nome.
        END.
    END.
    ELSE 
    DO:
        IF l-monitora = YES THEN
            PUT STREAM s-arq-df UNFORMATTED '  TABLE-TRIGGER "' evento '" NO-OVERRIDE PROCEDURE "' nome '" CRC "?"':U SKIP.
        ELSE 
            PUT STREAM s-arq-df UNFORMATTED '  TABLE-TRIGGER "' evento '" DELETE':U SKIP.
    END.
END PROCEDURE.

PROCEDURE pi-cria-alerta:
    DEFINE INPUT PARAMETER p-tabela   AS CHARACTER.
    DEFINE INPUT PARAMETER p-atributo AS CHARACTER.
    DEFINE INPUT PARAMETER p-evento   AS CHARACTER.

    DEFINE VARIABLE c-space AS CHARACTER.
    DEFINE VARIABLE c-linha AS CHARACTER NO-UNDO.

    CASE p-evento:
        WHEN 'D':U THEN 
            ASSIGN 
                c-space = '':U.
        WHEN 'C':U THEN 
            ASSIGN 
                c-space = '        ':U.
        WHEN 'W':U THEN 
            ASSIGN 
                c-space = '            ':U.
    END.

    FOR EACH mgadt.atrib_alerta NO-LOCK
        WHERE atrib_alerta.cod_base_dados = p-base
        AND atrib_alerta.cod_tabela     = p-tabela
        AND atrib_alerta.cod_atributo   = p-atributo
        AND atrib_alerta.cod_evento     = p-evento
        AND &IF '{&mgadt_version}' < '2.05' &THEN atrib_alerta.log-2
              &ELSE atrib_alerta.log_desativ
              &ENDIF = NO:

        OUTPUT stream s-alerta to value(SESSION:TEMP-DIRECTORY + "alerta.p":U).
        PUT STREAM s-alerta UNFORMATTED
            "def var l-envia-alerta as log initial no.":U SKIP(1)
            "def new global shared var v_cod_empres_usuar as character format 'x(3)' label 'Empresa' column-label 'Empresa' no-undo.":U SKIP(1)
                /*Tratamento para Datasul 10 e 11 que utilizam o banco emsfnd para obter as informaá‰es*/
                /*De acordo com a vers∆o do banco do emsfnd muda o tipo do campo*/
                &IF "{&EMSFND_VERSION}" = "1.00" &THEN
            "def new global shared var i-ep-codigo-usuario As Integer no-undo.":U
            SKIP(1)
            "def new global shared var v_cod_usuar_corren like emsfnd.usuar_mestre.cod_usuario no-undo.":U
                &ELSEIF '{&EMSFND_VERSION}' >= '1.01' &THEN
            "def new global shared var i-ep-codigo-usuario   As Character    no-undo.":U
            SKIP(1)
            "def new global shared var v_cod_usuar_corren like emsfnd.usuar_mestre.cod_usuario no-undo.":U
                &ELSE
            "def new global shared var v_cod_usuar_corren like mguni.usuar_mestre.cod_usuario no-undo.":U
                &ENDIF
            SKIP(1)
            c-usuar-espec SKIP
            "def buffer OLD_":U SUBSTRING(p-tabela,1,28) " for ":U p-nom-log ".":U p-tabela ".":U SKIP(1)
            "find first OLD_":U SUBSTRING(p-tabela,1,28) " no-lock no-error.":U SKIP(1)
            "find first ":U p-nom-log ".":U p-tabela " no-lock no-error.":U SKIP(1)
            atrib_alerta.des_criterio.

        OUTPUT stream s-alerta close.

        /* corp40362 */
        /**
         * Cria sess∆o com parÉmetro -rx, para compilar
         * as triggers
         *
         * compile value(c-arq) save no-error.
         */
        /*         IF PROGRESS = "FULL" THEN DO: */
        COMPILE value(SESSION:TEMP-DIRECTORY + "alerta.p":U) NO-ERROR.

        IF COMPILER:ERROR THEN 
        DO:
            PUT STREAM s-erro-comp UNFORMATTED "***** ":U c-lit-alert " ":U CAPS(p-tabela) ".":U CAPS(p-atributo) " *****":U SKIP.
            DO  i-error = 1 TO ERROR-STATUS:NUM-MESSAGES:
                PUT STREAM s-erro-comp UNFORMATTED ERROR-STATUS:GET-MESSAGE(i-error) SKIP.
            END.
            PUT STREAM s-erro-comp UNFORMATTED "":U SKIP(1).
            ASSIGN 
                l-erro-comp = YES.
        END.
        ELSE PUT STREAM s-trigger UNFORMATTED c-space "/* Validacao para envio do Alerta */ ":U   SKIP
                c-space REPLACE(atrib_alerta.des_criterio,CHR(10),CHR(10) + c-space) SKIP
                c-space "if l-envia-alerta = yes then do:":U SKIP
                c-space "    assign c-list-alert   = c-list-alert + '":U p-atributo ",'":U SKIP
                c-space "           c-list-user    = c-list-user  + '":U /* atrib_alerta.cod_usuario */ STRING(atrib_alerta.num_seq) ",'.":U SKIP
                c-space "end.":U SKIP
                /* --- FO 1383.622 - */
                c-space "assign l-envia-alerta = no." SKIP(1).
        /*         END.                                                                                                                                                  */
        /*         ELSE DO:                                                                                                                                              */
        /*             RUN aup/au0108r2.p PERSISTENT SET h-au0108r2.                                                                                                     */
        /*                                                                                                                                                               */
        /*             RUN pi-criptografa-trigger IN h-au0108r2 ( INPUT session:temp-directory + "alerta.p":U ,                                                          */
        /*                                                        INPUT session:TEMP-DIRECTORY                ,                                                          */
        /*                                                        INPUT session:temp-directory  ) .                                                                      */
        /*                                                                                                                                                               */
        /*             IF NOT RETURN-VALUE = "ok":U THEN DO:                                                                                                             */
        /*                 run utp/ut-msgs.p(input "show":U ,                                                                                                            */
        /*                                   input 34483    ,                                                                                                            */
        /*                                   input "":U ) .                                                                                                              */
        /*             END.                                                                                                                                              */
        /*             ELSE DO:                                                                                                                                          */
        /*                 RUN pi-compile-trigger IN h-au0108r2 ( INPUT session:temp-directory + "alerta.p":U ,                                                          */
        /*                                                        INPUT c-lit-alert                           ,                                                          */
        /*                                                        INPUT caps(p-tabela)                        ,                                                          */
        /*                                                        INPUT c-arq-e                               ,                                                          */
        /*                                                        INPUT session:temp-directory ) .                                                                       */
        /*                                                                                                                                                               */
        /*                 IF VALID-HANDLE(h-au0108r2) THEN DO:                                                                                                          */
        /*                     DELETE PROCEDURE h-au0108r2.                                                                                                              */
        /*                 END.                                                                                                                                          */
        /*                                                                                                                                                               */
        /*                                                                                                                                                               */
        /*                 IF SEARCH(SESSION:TEMP-DIRECTORY + "compile-error-triggers.err") <> ? THEN DO:                                                                */
        /*                     INPUT STREAM st-error FROM VALUE(SESSION:TEMP-DIRECTORY + "compile-error-triggers.err").                                                  */
        /*                     REPEAT :                                                                                                                                  */
        /*                         IMPORT STREAM st-error UNFORMATTED c-linha.                                                                                           */
        /*                         put stream s-erro-comp unformatted error-status:get-message(i-error) skip.                                                            */
        /*                     END.                                                                                                                                      */
        /*                 END.                                                                                                                                          */
        /*                 ELSE DO:                                                                                                                                      */
        /*                     put stream s-trigger unformatted c-space "/* Validacao para envio do Alerta */ ":U   skip                                                 */
        /*                             c-space replace(atrib_alerta.des_criterio,chr(10),chr(10) + c-space) skip                                                         */
        /*                             c-space "if l-envia-alerta = yes then do:":U skip                                                                                 */
        /*                             c-space "    assign c-list-alert   = c-list-alert + '":U p-atributo ",'":U skip                                                   */
        /*                             c-space "           c-list-user    = c-list-user  + '":U /* atrib_alerta.cod_usuario */ string(atrib_alerta.num_seq) ",'.":U skip */
        /*                             c-space "end.":U skip                                                                                                             */
        /*                             /* --- FO 1383.622 - */                                                                                                           */
        /*                             c-space "assign l-envia-alerta = no." Skip(1).                                                                                    */
        /*                 END.                                                                                                                                          */
        /*             END.                                                                                                                                              */
        /*         END.                                                                                                                                                  */


        /*****************************************************************************/    
        /** Chamada UPC no inicio da Atualizaªío                                    **/
        /*****************************************************************************/

        FOR EACH tt-epc:
            DELETE tt-epc.
        END.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "alerta":U
            tt-epc.cod-parameter = "nome-arq":U
            tt-epc.val-parameter = "alerta.p":U.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "alerta":U
            tt-epc.cod-parameter = "nome-caminho":U
            tt-epc.val-parameter = SESSION:TEMP-DIRECTORY.

        CREATE tt-epc.
        ASSIGN 
            tt-epc.cod-event     = "alerta":U
            tt-epc.cod-parameter = "caminho":U
            tt-epc.val-parameter = SESSION:TEMP-DIRECTORY + "alerta.p":U.

        {include/i-epc201.i "alerta":u}

        IF RETURN-VALUE = "NOK":U THEN 
        DO:
            UNDO, NEXT.
        END.

        /*************************** FIM CHAMADA EPC ************************************/

        OS-DELETE value(SESSION:TEMP-DIRECTORY + "alerta.p":U) no-error.
        OS-DELETE value(SESSION:TEMP-DIRECTORY + "alerta.r":U) no-error.

    END.
END PROCEDURE.

PROCEDURE convert2lowercase:
    DEFINE INPUT-OUTPUT PARAMETER parquivo AS CHARACTER   NO-UNDO.

    parquivo = LC(parquivo).

END.
