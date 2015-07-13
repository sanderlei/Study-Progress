 DEFINE VARIABLE l-resp AS LOGICAL.
 FOR EACH customer:
    DISPLAY customer WITH 1 COLUMN.
    ASSIGN l-resp = NO.
    MESSAGE "Deseja excluir este cliente? " UPDATE l-resp.
    IF l-resp = YES THEN DO:
        FIND FIRST order OF customer NO-ERROR.
        IF AVAILABLE order THEN DO:                                   
             ASSIGN l-resp = NO.
            MESSAGE "O cliente possui Pedido. Deseja excluir o Cliente e seus Pedidos? " UPDATE l-resp.
            IF l-resp = YES THEN DO:
               FOR EACH order OF customer:
                    FOR EACH order-line OF order:
                        DELETE order-line.
                    END.
                    DELETE order.
               END.
                DELETE customer.
                MESSAGE "o Cliente foi excluido e seus pedidos!".
            END.
        END.
        ELSE DO:
            DELETE customer.
            MESSAGE "o Cliente foi excluido!".
        END.
    END.
END.
