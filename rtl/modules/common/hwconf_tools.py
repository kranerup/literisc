#import yaml
from modules.top.HwConf import HwConf
import types
import sys

class HwConfFromConfig:
    def __getattr__(self, x):
        #print(f"getattr x:{x}")
        if x == 'defines':
            return self._defines
        elif x in self._hwconf:
            #print("x is ",self._hwconf[x])
            return self._hwconf[ x ]
        else:
            assert f"HwConf attribute {x} doesn't exist."

    def __getattribute__(self, item):
        try:
            original_attribute = object.__getattribute__(self, item)
        except AttributeError:
            #print("exception")
            if hasattr( HwConf, item ):
                HwConf_func = getattr(HwConf,item)
                #print("HwConf func = ",HwConf_func)
                def new_call( *args, **kwargs):
                    #print(f"Before calling {item}")
                    result = HwConf_func(self,*args, **kwargs)
                    #print(f"After calling {item}")
                    return result
                return new_call
            raise AttributeError
        return original_attribute

    def import_hwconf(self):
        #========================
        # fake the hwconf so that the normal code to import hwconf works but
        # instead use the hwconf imported from config.yml file.
        # >>> this code will still work:
        #   from modules.common.hwconf import get_hwconf; hwconf = get_hwconf()

        # make it look like we already imported settings.hwconf
        mod = types.ModuleType("settings.hwconf")
        sys.modules["settings.hwconf"] = mod

        # this function needs to exist in the imported fake module but doesn't
        # have to do anything
        def create_hwconf():
            pass # print("fake create_hwconf")
        setattr( mod, "create_hwconf", create_hwconf )

        # make this object available for import in settings.hwconf
        hwconf = self
        setattr( mod, "hwconf", hwconf )

    def __init__(self, yml_config):
        #print("HwConfFromConfig init:",yml_config)
        with open(yml_config, 'r') as F:
            c = yaml.full_load(F.read())
            self._hwconf = c['hwconf']
            self._defines = c['defines']
            #import pprint
            #pprint.pprint(self._hwconf)
        

