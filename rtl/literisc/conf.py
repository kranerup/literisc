from myhdl import Struct
from modules.common.signal import signal
import re

class Conf(Struct):
    def __init__(self):

        self.slave_request_address = signal(16)
        self.slave_request_data = signal(32)
        self.slave_request_id = signal(2)
        self.slave_request_type = signal(2)
        self.slave_request_re = signal()
        self.slave_request_we = signal()
        self.slave_reply_data = signal(32)
        self.slave_reply_id = signal()
        self.slave_reply_status = signal(2)
        self.master_request_address = signal(16)
        self.master_request_data = signal(32)
        self.master_request_id = signal(2)
        self.master_request_type = signal(2)
        self.master_request_we = signal()
        self.master_request_re = signal()
        self.master_reply_id = signal()
        self.master_reply_status = signal(2)
        self.master_reply_data = signal(32)
        self.ticks = signal(5)
        self.irq = signal()
