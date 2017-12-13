from threading import Thread

import websockets, queue


class Market(Thread):
    def __init__(self, market_currency, base_currency, meta_code, fee_rate):
        Thread.__init__(self)
        self.daemon = True
        self.base_url = None
        self._name = None
        self.market_currency = market_currency
        self.base_currency = base_currency
        self.meta_code = meta_code
        self.fee_rate = fee_rate
        self.update_flags = {"depth": False}
        self.methods = {}
        self.q = queue.Queue()

    # 将所有flag设置为false
    def unset_flags(self, flags):
        for name in self.update_flags:
            self.update_flags[name] = True
        for name in flags:
            self.update_flags[name] = False

    # 逐个检查flag，只有全部为真时（全部更新完毕），返回True
    def check_flags(self, flags):
        status = True
        for name in flags:
            status = (status and (self.update_flags[name]))
        return status

    async def sub_channel(self):
        # sub depth
        pass

    # 保持连接
    async def keep_connect(self):
        if self.websocket == None:
            self.websocket = await websockets.connect(self.base_url)
        else:
            if not self.websocket.open:
                self.websocket.close()
                self.websocket = await websockets.connect(self.base_url)
        await self.sub_channel()

    # 获取ticker（买一卖一报价）
    def get_ticker(self):
        depth = self.get_depth()
        if not depth:
            return None
        res = {'ask': {'price': 0, 'amount': 0}, 'bid': {'price': 0, 'amount': 0}}
        if len(depth['asks']) > 0:
            res['ask'] = depth['asks'][0]
        if len(depth["bids"]) > 0:
            res['bid'] = depth['bids'][0]
        return res

    def update(self,update_flags):
        """
        all update must ends with barrier.
        :argument update_flags {"depth": True}
        :return:
        """
        pass

    def get_depth(self):
        pass

    async def long(self, price, amount):
        pass

    async def short(self, price, amount):
        pass

    async def close_long(self, price, amount):
        pass

    async def close_short(self, price, amount):
        pass
