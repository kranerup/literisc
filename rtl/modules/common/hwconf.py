"""
Provide real hwconf if it exists, else a fake one
Allow local overrides.
"""

import sys
import os
#import yaml
from contextlib import contextmanager
from pprint import pprint


@contextmanager
def redirect_stdout(filename):
    so = sys.stdout
    so.flush()
    sys.stdout = open(filename, "w")
    yield None
    sys.stdout.close()
    sys.stdout = so


_defaults = {}


_no_mem_ff = dict(
    memory_flop_limit=None,
    memory_flop_dlimit=None,
    memory_force_output_flop=None,
    memory_force_input_flop=None,
    memory_force_in_or_out_flop=None,
)


def read_config_yaml( config_yml ):
    with open(config_yml, 'r') as F:
        y = yaml.full_load(F.read())
        h = y['hwconf']
        d = y['defines']
        # Copy all hwconf parameters to defines but check that there is no
        # conflict.
        # The purpose is to allow refmod and other places simple access to
        # hwconf where defines is kept in 'C' and there is no hwconf.
        for name,val in h.items():
            if name in d:
                assert  val == d[name], f"HwConf parameter overrides 'defines' {name} {val} {d[name]}"
            d[name] = val

    return d,h

# HwconfWrapper is a wrapper around
# - HwConf instance
# - hwconf values read from config.yml
# When hwconf is based on reading in a config.yml file it will
# make all attribute accesses to dicts based on the config.yml content.
# If there is a method call done on the hwconf object then the
# method in the HwConf class will be called but the class object will
# be the HwconfWrapper instance.

class HwconfWrapper:
    def __getattr__(self, x):
        #print("HwconfWrapper __getattr__",x)
        if x in self._override:
            return self._override[x]
        if not self._hwconf:
            # this is the first attribute access so load a hwconf
            self._init()

        if self._hwconf_dict: # only when HwconfWrapper is based on reading config.yml
            # --------- handle config.yml access --------------
            #print("HwconfWrapper __getattr__ access to _hwconf_dict",x)
            if x == 'defines':
                return self._defines
            if x in self._hwconf_dict:
                #print(f"found {x} in _hwconf_dict")
                return self._hwconf_dict[ x ]
            else:
                # it can be a function call to a method in HwConf
                #print(f"{x} not in _hwconf_dict, try a function in HwConf")
                from modules.top.HwConf import HwConf
                if hasattr( HwConf, x ):
                    #print("HwConf has",x)
                    HwConf_func = getattr(HwConf,x)
                    #print("HwConf func = ",HwConf_func)
                    def new_call( *args, **kwargs):
                        #print(f"Before calling {x}")
                        result = HwConf_func(self,*args, **kwargs)
                        #print(f"After calling {x}")
                        return result
                    return new_call
                else:
                    raise AttributeError
        else:
            # --------- handle real HwConf instance access --------------
            return getattr(self._hwconf, x)

    def __init__(self, override, defaults={}):
        self._override = override
        self._defaults = {**_defaults, **defaults}
        hwc = sys.modules.get("settings.hwconf")
        self._hwconf = None
        self.hwconf = self
        self._hwconf_dict = None
        self._defines = None
        self.faked = None

    def _init(self):
        """Try to find/load real hwconf, else create fake one"""
        hwc = sys.modules.get("settings.hwconf")
        if not hwc:
            print("HwconfWrapper no settings.hwconf module has been imported")
            # first try to create hwconf from config.yml
            from modules.common.Common import hwdir
            config_yml = hwdir() + "/config.yml"
            if os.path.exists( config_yml ):
                print("HwconfWrapper: read from yml:",config_yml)
                d, h = read_config_yaml( config_yml )

                self._hwconf_dict = h
                self._defines = d

                sys.modules["settings.hwconf"] = self
                hwc = self
                self.faked = False
            else:
                print("HwconfWrapper: no config.yml, instead try import settings.hwconf")
                # there is no config.yml so try to import from settings.hwconf
                from modules.common.Common import hwdir  # noqa: F401

                try:
                    sys.path.append(hwdir())
                    import settings.hwconf  # noqa: F401

                    hwc = settings.hwconf
                    print("HwconfWrapper import settings.hwconf succeed")
                except ImportError:
                    print("HwconfWrapper import settings.hwconf failed")
                    pass
        if hwc:
            # hwconf is imported, create hwconf object if not already done:
            hwconf = getattr(hwc, "hwconf", None)
            if not hwconf:
                print("HwconfWrapper hwconf module exists but no hwconf instance, create_hwconf")
                with redirect_stdout("hwconf.log"):
                    hwc.create_hwconf()
                hwconf = getattr(hwc, "hwconf")
                # must also export config.yml in case it's a block tb. For top
                # tb it's done in runFlexSwitch_synt
                hwconf.exportSettings( top_level_flow = True )
            self._hwconf = hwconf
            self.faked = False
        else:
            print("HwconfWrapper: create fake hwconf")
            # Create fake hwconf. Import here to avoid circular imports in some cases
            from modules.top.HwConf import HwConf  # noqa: F401

            with redirect_stdout("hwconf.log"):
                self._hwconf = HwConf()
            #print("HwConf instantiating from get_hwconf done")
            sys.modules["settings.hwconf"] = self
            self.faked = True

    def create_hwconf(self):
        pass





def get_hwconf(no_mem_flops=False, **overrides):
    """
    Return real hwconf if exists, else a fake one.
    Override default settings with keyword arguments.
    """
    if no_mem_flops:
        overrides.update(_no_mem_ff)
    return HwconfWrapper(overrides)
