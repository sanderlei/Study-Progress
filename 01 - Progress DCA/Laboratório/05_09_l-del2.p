 DEFINE VARIABLE l-resp AS LOGICAL.
 FOR EACH customer:
    DISPLAY customer WITH 1 COLUMN.
    ASSIGN l-resp = NO.
    MESSAGE "Deseja excluir este cliente? " UPDATE l-resp.
    IF l-resp = YES THEN DO:
        FIND FIRST order OF customer NO-ERROR.
        IF AVAILABLE order THEN DO:
           MESSAGE "o Cliente possui Pedido".
           /* poderia usar o comando NEXT para pular para a 
           proxima interacao e nao precisar usar o ELSE como o anunciado pede.
           */
        END.
        ELSE DO:
            DELETE customer.
            MESSAGE "o Cliente foi excluido!".
        END.
    END.
END.
