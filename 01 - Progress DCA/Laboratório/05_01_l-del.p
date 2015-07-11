 DEFINE VARIABLE l-resp AS LOGICAL.
 FOR EACH customer:
    DISPLAY customer WITH 1 COLUMN.
    ASSIGN l-resp = NO.
    MESSAGE "Deseja excluir este cliente? " UPDATE l-resp.
    IF l-resp = YES THEN DO:
        DELETE customer.
        MESSAGE "o Cliente foi excluido!".
    END.
END.
