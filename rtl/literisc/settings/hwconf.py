class hwconf_fake(object):
    def __init__(self):
        self.epp_conf_input_flop = 1
        self.epp_conf_output_flop = 1
        self.ipp_conf_input_flop = 1
        self.ipp_conf_output_flop = 1
        self.block_conf_flops = 1
        self.conf_collector_ppp_inp_flops = 1
        self.conf_data_width = 32
        self.cam_model = None
        self.memory_flop_limit = 20000
        self.memory_flop_dlimit = 20000
        self.memory_pre_load = None
        self.memory_mode = 'inferred'
        self.memory_input_flops = 0
        self.memory_output_flops = 0
        self.memory_force_output_flop = 0
        self.memory_force_input_flop = 0
        self.memory_force_in_or_out_flop = 0
        self.conf_collector_inp_flops = 0
        self.conf_bus_collector_output_flops = [("pa.top.switch.ipp\d+.ippp.ippp.collector", 1)]
        self.conf_collector_combinational = False
        self.conf_interface        = 'conf'
        self.nr_of_id = 1
        self.start_address_space  = 0
        self.nr_of_pb = 1
        self.sparsly_table_address_space = False
        self.maximum_address_space = None
        self.memory_max_width = 1000
        self.memory_max_depth = 2000000
        self.statistics_config = {'conf_width':32,'conf_access':'rw','debug_width':16,'debug_access':'rw'}

    def regTab_check_exist_address(self,n):
        return False
    def regTab_check_extra_address_space(self,n):
        return False

def create_hwconf():
    global hwconf
    if "hwconf" not in globals():
        hwconf = hwconf_fake()
