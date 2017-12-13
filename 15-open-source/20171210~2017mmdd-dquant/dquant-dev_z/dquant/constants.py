class Constants():
    BITMEX_APIKEY = "bitmex_apikey"
    BITMEX_APISEC = "bitmex_apisec"
    BITMEX_FEE = "bitmex_fee"
    BITMEX_FUTURE_CLOSE_POSITION = "https://www.bitmex.com/api/v1/order/closePosition"
    BITMEX_FUTURE_LEVER = "https://www.bitmex.com/api/v1/position/leverage"
    BITMEX_FUTURE_ORDER = "https://www.bitmex.com/api/v1/order"
    BITMEX_FUTURE_WS_BASE = "wss://www.bitmex.com/realtime"

    DEV = "dev"
    DQUANT_ENV = "DQUANTENV"

    OKEX_APIKEY = "okex_apikey"
    OKEX_APISEC = "okex_apisec"
    OKEX_FEE = "okex_fee"
    OKEX_FUTURE_DELETE_ORDER_REST = "/api/v1/future_cancel.do"
    OKEX_FUTURE_DELETE_ORDER_WS = "ok_futureusd_cancel_order"
    OKEX_FUTURE_DEPTH_RESOURCE_REST = "/api/v1/future_depth.do"
    OKEX_FUTURE_FOLLOW_PRICE = "okex_future_follow_price"
    OKEX_FUTURE_GET_ORDER_WS = "ok_futureusd_orderinfo"
    OKEX_FUTURE_REST_BASE = "https://www.okex.com"
    OKEX_FUTURE_TRADE_REST = "/api/v1/future_trade.do?"
    OKEX_FUTURE_TRADE_WS = "ok_futureusd_trade"
    OKEX_FUTURE_WS_BASE = "wss://real.okex.com:10440/websocket/okexapi"
    OKEX_FUTURE_USERINFO_WS = 'ok_futureusd_userinfo'

    OKEX_FUTURE_LOGIN = 'login'
    OKEX_FUTURE_SUB_USERINFO = 'ok_sub_futureusd_userinfo'
    OKEX_FUTURE_SUB_TRADES = 'ok_sub_futureusd_trades'
    OKEX_FUTURE_SUB_POSITIONS = 'ok_sub_futureusd_positions'

    OK_HTTP_TIMEOUT = 2000

    PRO = "PRO"

    REDIS_HOST = "redis_host"
    REDIS_PORT = "redis_port"
