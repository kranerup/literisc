import sys
from math import ceil

class tsmc_cln16fpll_sram(object):
    # Note that the banking feature is not used in this model, and that the RF generator is used only sparingly
    def __init__(self):
        self.mux_list  = {
            'ram': [8, 4, 2],
#            'rf':  [8, 4, 2, 1]
            'rf':  [1]
        }
        self.max_width_list  = {
            'ram': [80, 160, 160],
#            'rf':  [40, 80, 160, 320]
            'rf':  [320]
        }
        self.min_width_list  = {
            'ram': [4, 4, 8],
#            'rf':  [12, 6, 4, 4]
            'rf':  [12]
        }
        self.max_depth_list  = {
            'ram': [2048, 1024, 512],
#            'rf':  [1024,  512,  256, 128]
            'rf':  [64]
        }
        self.min_depth_list  = {
            'ram': [128, 64, 16],
#            'rf':  [128, 64, 32, 8]
            'rf':  [8]
        }
        self.depth_step_list = {
            'ram': [16, 8, 4],
#            'rf':  [2, 2, 2, 2]
            'rf':  [2]
        }
        self.width_step_list = {
            'ram': [1, 1, 2],
#            'rf':  [2, 2, 2, 2]
            'rf':  [2]
        }
        self.generator_cmd = {
            'ram': '/opt/ip/arm/tsmc/cln16fpll001/sram_2p_uhde_svt_mvt/r21p1/bin/sram_2p_uhde_svt_mvt',
            'rf':  '/opt/ip/arm/tsmc/cln16fpll001/rf_2p_hsc_svt_mvt/r22p0/bin/rf_2p_hsc_svt_mvt',
        }
        self.targets = {
            'lib':'liberty -libertyviewstyle nldm',
            'txt':'ascii',
            'model':'verilog'
        }
        self.max_depth = self.max_depth_list['ram'][0]
        
    def is_rf(self, d, w):
        if d<8:
            print("Warning! Depths below 8 not supported, there will be unused addresses. d=%s, w=%s" % (d, w))
            return 1
        elif d<32:
            return 1
        elif d<64 and w > 160:
            return 1
        else:
            return 0

    def depth_step(self, m, type):
        #print "depth_step mux", m, "type", type, "step", self.depth_step_list[type][self.mux_index({'mux':m, 'type':type})] 
        return self.depth_step_list[type][self.mux_index({'mux':m, 'type':type})]
    def max_width(self, mux, type):
        for n in range(len(self.mux_list[type])):
            if mux==self.mux_list[type][n]:
                return self.max_width_list[type][n]
        raise ValueError("A mux value of %s is not supported. supported values are %s" %( mux, self.mux_list))


    def conf(self, depth, width):
        c = {}
        # Decide RAM or RF
        if self.is_rf(depth, width):
            c['type'] = 'rf'
        else:
            c['type'] = 'ram'
        # Depth
        if depth > self.max_depth_list[c['type']][0]:
            #print "conf depth", depth, ">", self.max_depth_list[c['type']][0]
            c['rows'] = int(ceil(depth / (self.max_depth_list[c['type']][0]*1.0)))
        else:
            c['rows'] = 1
        c['md']   = int(ceil((depth*1.0) / c['rows'])) 
        c['mux']  = self.mux_d( c['md'], c['type'] ) 
        while c['md'] % self.depth_step(c['mux'], c['type']) != 0:
            c['md'] =  int(ceil((c['md']*1.0) / self.depth_step(c['mux'], c['type'])) * self.depth_step(c['mux'], c['type']))
            c['mux']  = self.mux_d( c['md'], c['type'] ) 
        if c['md'] < self.min_depth_list[ c['type'] ][ self.mux_index(c) ]:
            c['md'] = self.min_depth_list[ c['type'] ][ self.mux_index(c) ]
            
        # Width
        #print "conf: ", c
        if width > self.max_width(c['mux'], c['type']): 
            c['cols'] = int(ceil(width / (self.max_width(c['mux'], c['type'])*1.0)))
        else:
            c['cols'] = 1
        #print "conf: after cols ", c
        c['mw']   = int(ceil((width*1.0) / c['cols'])) 
        if c['mw'] % self.width_step_list[ c['type'] ][ self.mux_index(c) ] != 0:
            c['mw'] = ((c['mw'] / self.width_step_list[c['type']][ self.mux_index(c) ]) + 1) * (self.width_step_list[c['type']][ self.mux_index(c) ])
        if c['mw'] < self.min_width_list[ c['type'] ][ self.mux_index(c) ]:
            c['mw'] = self.min_width_list[ c['type'] ][ self.mux_index(c) ]
        #print "baba ", c
            
        return c
            
    def mux_index(self, c):
        #print "mux_index", c['type'], self.mux_list[c['type']]
        for n in range(len( self.mux_list[c['type']] )):
            if self.mux_list[c['type']][n] == c['mux']:
                #print "mux_index", c, "->", n
                return n
        raise ValueError()
    
    def mux_d(self, depth, type):
        mi = 0
        for n in reversed(list(range(len(self.max_depth_list[type])))):
            if depth<=self.max_depth_list[type][n]:
                break
        while depth < self.min_depth_list[type][n]:
            print("Depth", depth, "is less than the min", self.min_depth_list[type][n]) 
            if n==len(self.max_depth_list[type])-1:
                break
            else:
                n = n+1
        if depth <= self.max_depth_list[type][n]:
            if depth < self.min_depth_list[type][n]:
                print("Warning! Depth %s < %s and thus causes unused addresses" % (depth, self.min_depth_list[type][n]))
            return self.mux_list[type][n]
        else:
            raise ValueError("The depth=%s is out of range %s-%s"%(depth, self.min_depth_list[type][0], self.max_depth_list[type][-1]))

    def mux_val(self, depth, width, rf=0):
        if rf==0:
            if depth>4096:
                mux = 16
                if width > 36:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
            elif depth>2048:
                mux = 8
                if width > 72:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
            else:
                mux = 4
                if width>144:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
        else:
            if depth>512:
                mux = 4
                if width > 72:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
            elif depth>256:
                mux = 2
                if width > 144:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
            else:
                mux = 1
                if width > 288:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
        return mux
        
    def cmd(self, depth, width, type):
        return self.cmd_mux(depth, width, self.mux_d(int(depth), type), type)

    def name(self, depth, width, mux, type):
        iname = 'dp_d%s_w%s_m%s'%(depth, width, mux)
        return iname
        

    def cmd_mux(self, depth, width, mux, type, target='all'):
        iname = self.name(depth, width, mux, type)
        
        wt = ""
        ct = ""
        if type == 'ram':
            wt = '-write_thru off'
            ct = '-compiler_type 2p'
        s = '{cmd} {type} -instname "{name}" -words {depth} -bits {width} -mux {mux} -pipeline off -write_mask off {wt} {ct} -check_instname off -mvt HS -flexible_banking 1 -libname sram_2p_uhde -corners ssgnp_0p72v_0p72v_125c'.format(
            cmd = self.generator_cmd[type],
            type='{type}', 
            name=iname,
            depth = int(depth),
            width = int(width),
            mux = int(mux),
            wt = wt,
            ct = ct
        )
        # ff_0p99v_0p99v_m40c,tt_0p90v_0p90v_25c,ffg_0p99v_0p99v_125c,ff_0p99v_0p99v_125c,
        c = ""
        if target == 'all':
            tar = [ self.targets[t] for t in self.targets ]
        else:
            tar = [self.targets[target]]
            
        for t in tar:
            c += s.format(type=t)
            if len(tar) > 1:
                c +=  ";"

        return c 

class tsmc_cln40g_sram(object):
    def __init__(self):
        self.mux_list  = {
            'ram': [16, 8, 4],
            'rf':  [4, 2,  1]
        }
        self.max_width_list  = {
            'ram': [36,  72, 144],
            'rf':  [72, 144, 288]
        }
        self.max_depth_list  = {
            'ram': [8192, 4096, 2048],
            'rf':  [1024,  512,  256]
        }
        self.min_depth_list  = {
            'ram': [256, 128, 64],
            'rf':  [32,   16,  8]
        }
        self.generator_cmd = {
            'ram': '/opt/ip/arm/tsmc/cln40g/sram_dp_hde_rvt_hvt_rvt/r5p0/bin/sram_dp_hde_rvt_hvt_rvt',
            'rf':  '/opt/ip/arm/tsmc/cln40g/rf_2p_hse_rvt_hvt_rvt/r9p1/bin/rf_2p_hse_rvt_hvt_rvt',
        }
        self.targets = {
            'lib':'synopsys',
            'txt':'ascii',
            'model':'verilog'
        }
        self.min_width = 4
        self.max_depth = self.max_depth_list['ram'][0]
        
    def is_rf(self, d, w):
        if d<8:
            print("Warning! Depths below 8 not supported, there will be unused addresses. d=%s, w=%s" % (d, w))
            return 1
        elif d<256:
            return 1
        else:
            return 0

    def depth_step(self, m, type):
        if type=='rf': 
            return 2*m
        else:
            return 4*m

    def max_width(self, mux, type):
        for n in range(len(self.mux_list[type])):
            if mux==self.mux_list[type][n]:
                return self.max_width_list[type][n]
        raise ValueError("A mux value of %s is not supported. supported values are %s" %( mux, self.mux_list))

    def min_depth(self, mux, type):
        for n in range(len(self.mux_list[type])):
            if mux==self.mux_list[type][n]:
                return self.min_depth_list[type][n]
        raise ValueError("A mux value of %s is not supported. supported values are %s" %( mux, self.mux_list))
        

    def conf(self, depth, width):
        c = {}
        # Decide RAM or RF
        if self.is_rf(depth, width):
            c['type'] = 'rf'
        else:
            c['type'] = 'ram'
        # Depth
        if depth > self.max_depth_list[c['type']][0]:
            #print "conf depth", depth, ">", self.max_depth_list[c['type']][0]
            c['rows'] = int(ceil(depth / (self.max_depth_list[c['type']][0]*1.0)))
        else:
            c['rows'] = 1
        c['md']   = int(ceil((depth*1.0) / c['rows'])) 
        c['mux']  = self.mux_d( c['md'], c['type'] ) 
        while c['md'] % self.depth_step(c['mux'], c['type']) != 0:
            c['md'] =  int(ceil((c['md']*1.0) / self.depth_step(c['mux'], c['type'])) * self.depth_step(c['mux'], c['type']))
            c['mux']  = self.mux_d( c['md'], c['type'] ) 

            # Width
        if width > self.max_width(c['mux'], c['type']): 
            c['cols'] = int(ceil(width / (self.max_width(c['mux'], c['type'])*1.0)))
        else:
            c['cols'] = 1
        c['mw']   = int(ceil((width*1.0) / c['cols'])) 
        if c['mw'] < self.min_width:
            c['mw'] = self.min_width
        md = self.min_depth(c['mux'], c['type'])
        if c['md'] < md:
            c['md'] = md
        return c
            
            
    def mux_d(self, depth, type):
        #print "mux_d", depth, type
        mi = 0
        for n in reversed(list(range(len(self.max_depth_list[type])))):
            if depth<=self.max_depth_list[type][n]:
                #print "mux_d   break at", n, "d", self.max_depth_list[type][n], " >= ", depth
                break
        #print "mux_d   after loop", n
        while depth < self.min_depth_list[type][n]:
            print("Depth", depth, "is less than the min", self.min_depth_list[type][n]) 
            if n==len(self.max_depth_list[type])-1:
                break
            else:
                n = n+1
        #print "mux_d   after while", n
        if depth <= self.max_depth_list[type][n]:
            if depth < self.min_depth_list[type][n]:
                print("Warning! Depth %s < %s and thus causes unused addresses" % (depth, self.min_depth_list[type][n]))
            return self.mux_list[type][n]
        else:
            raise ValueError("The depth=%s is out of range %s-%s"%(depth, self.min_depth_list[type][0], self.max_depth_list[type][-1]))

    def mux_val(self, depth, width, rf=0):
        if rf==0:
            if depth>4096:
                mux = 16
                if width > 36:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
            elif depth>2048:
                mux = 8
                if width > 72:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
            else:
                mux = 4
                if width>144:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
        else:
            if depth>512:
                mux = 4
                if width > 72:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
            elif depth>256:
                mux = 2
                if width > 144:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
            else:
                mux = 1
                if width > 288:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
        return mux
        
    def cmd(self, depth, width, type):
        return self.cmd_mux(depth, width, self.mux_d(int(depth), type), type)

    def name(self, depth, width, mux, type):
        iname = 'dp_d%s_w%s_m%s'%(depth, width, mux)
        return iname
        

    def cmd_mux(self, depth, width, mux, type, target='all'):
        iname = self.name(depth, width, mux, type)
        
        wt = ""
        if type == 'ram':
            wt = '-write_thru off'
        s = '{cmd} {type} -instname "{name}" -words {depth} -bits {width} -mux {mux} -pipeline off -write_mask off {wt} -name_case upper -check_instname off -corners ss_0p81v_0p81v_125c,ss_0p81v_0p81v_m40c'.format(
            cmd = self.generator_cmd[type],
            type='{type}', 
            name=iname,
            depth = int(depth),
            width = int(width),
            mux = int(mux),
            wt = wt
        )
        # ff_0p99v_0p99v_m40c,tt_0p90v_0p90v_25c,ffg_0p99v_0p99v_125c,ff_0p99v_0p99v_125c,
        c = ""
        if target == 'all':
            tar = [ self.targets[t] for t in self.targets ]
        else:
            tar = [self.targets[target]]
            
        for t in tar:
            c += s.format(type=t)
            if len(tar) > 1:
                c +=  ";"

        return c 

class smic_28hkmg_sram(object):
    def __init__(self):
        self.mux_list  = {
            'ram': [16, 8, 4],
            'rf':  [2, 1]
        }
        self.max_width_list  = {
            'ram': [40,  80, 160],
            'rf':  [160, 160]
        }
        self.max_depth_list  = {
            'ram': [4096, 2048, 1024],
            'rf':  [512, 256]
        }
        self.min_depth_list  = {
            'ram': [128, 64, 32],
            'rf':  [32, 8]
        }
        self.width_step_list = {
            'ram': [1, 1, 1],
            'rf':  [2, 2]
        }
        self.generator_cmd = {
            'ram': '/opt/ip/arm/smic/28hkmg/sram_dp_hsd_mvt/r1p0/bin/sram_dp_hsd_mvt',
            'rf':  '/opt/ip/arm/smic/28hkmg/rf_2p_hde_mvt/r4p0/bin/rf_2p_hde_mvt',
        }
        self.targets = {
            'lib':'liberty',
            'txt':'ascii',
            'model':'verilog'
        }
        self.min_width = 4
        self.max_depth = self.max_depth_list['ram'][0]
        
    def is_rf(self, d, w):
        if d<self.min_width:
            print("Warning! Depths below %s not supported, there will be unused addresses. d=%s, w=%s" % (self.min_width, d, w))
            return 1
        elif d<min(self.min_depth_list['ram']):
            return 1
        else:
            return 0

    def depth_step(self, m, type):
        if type=='rf': 
            return 2*m
        else:
            return 4*m

    def max_width(self, mux, type):
        for n in range(len(self.mux_list[type])):
            if mux==self.mux_list[type][n]:
                return self.max_width_list[type][n]
        raise ValueError("A mux value of %s is not supported. supported values are %s" %( mux, self.mux_list))

    def min_depth(self, mux, type):
        for n in range(len(self.mux_list[type])):
            if mux==self.mux_list[type][n]:
                return self.min_depth_list[type][n]
        raise ValueError("A mux value of %s is not supported. supported values are %s" %( mux, self.mux_list))
        

    def conf(self, depth, width):
        c = {}
        # Decide RAM or RF
        if self.is_rf(depth, width):
            c['type'] = 'rf'
        else:
            c['type'] = 'ram'
        # Depth
        if depth > self.max_depth_list[c['type']][0]:
            #print "conf depth", depth, ">", self.max_depth_list[c['type']][0]
            c['rows'] = int(ceil(depth / (self.max_depth_list[c['type']][0]*1.0)))
        else:
            c['rows'] = 1
        c['md']   = int(ceil((depth*1.0) / c['rows'])) 
        c['mux']  = self.mux_d( c['md'], c['type'] ) 
        while c['md'] % self.depth_step(c['mux'], c['type']) != 0:
            c['md'] =  int(ceil((c['md']*1.0) / self.depth_step(c['mux'], c['type'])) * self.depth_step(c['mux'], c['type']))
            c['mux']  = self.mux_d( c['md'], c['type'] ) 
        md = self.min_depth(c['mux'], c['type'])
        if c['md'] < md:
            c['md'] = md

        # Width
        if width > self.max_width(c['mux'], c['type']): 
            c['cols'] = int(ceil(width / (self.max_width(c['mux'], c['type'])*1.0)))
        else:
            c['cols'] = 1
        c['mw']   = int(ceil((width*1.0) / c['cols'])) 
        if c['mw'] % self.width_step_list[ c['type'] ][ self.mux_index(c) ] != 0:
            c['mw'] = ((c['mw'] / self.width_step_list[c['type']][ self.mux_index(c) ]) + 1) * (self.width_step_list[c['type']][ self.mux_index(c) ])
        if c['mw'] < self.min_width:
            c['mw'] = self.min_width
        return c
            
    def mux_index(self, c):
        #print "mux_index", c['type'], self.mux_list[c['type']]
        for n in range(len( self.mux_list[c['type']] )):
            if self.mux_list[c['type']][n] == c['mux']:
                #print "mux_index", c, "->", n
                return n
        raise ValueError()
            
    def mux_d(self, depth, type):
        #print "mux_d", depth, type
        mi = 0
        for n in reversed(list(range(len(self.max_depth_list[type])))):
            if depth<=self.max_depth_list[type][n]:
                #print "mux_d   break at", n, "d", self.max_depth_list[type][n], " >= ", depth
                break
        #print "mux_d   after loop", n
        while depth < self.min_depth_list[type][n]:
            print("Depth", depth, "is less than the min", self.min_depth_list[type][n]) 
            if n==len(self.max_depth_list[type])-1:
                break
            else:
                n = n+1
        #print "mux_d   after while", n
        if depth <= self.max_depth_list[type][n]:
            if depth < self.min_depth_list[type][n]:
                print("Warning! Depth %s < %s and thus causes unused addresses" % (depth, self.min_depth_list[type][n]))
            return self.mux_list[type][n]
        else:
            raise ValueError("The depth=%s is out of range %s-%s"%(depth, self.min_depth_list[type][0], self.max_depth_list[type][-1]))

    def mux_val(self, depth, width, rf=0):
        if rf==0:
            if depth>4096:
                mux = 16
                if width > 36:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
            elif depth>2048:
                mux = 8
                if width > 72:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
            else:
                mux = 4
                if width>144:
                    raise ValueError("The combination depth=%s and width=%s is out of range for sram"%(depth, width))
        else:
            if depth>512:
                mux = 4
                if width > 72:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
            elif depth>256:
                mux = 2
                if width > 144:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
            else:
                mux = 1
                if width > 288:
                    raise ValueError("The combination depth=%s and width=%s is out of range for rf"%(depth, width))
        return mux
        
    def cmd(self, depth, width, type):
        return self.cmd_mux(depth, width, self.mux_d(int(depth), type), type)

    def name(self, depth, width, mux, type):
        iname = 'dp_d%s_w%s_m%s'%(depth, width, mux)
        return iname
        

    def cmd_mux(self, depth, width, mux, type, target='all'):
        iname = self.name(depth, width, mux, type)
        
        wt = ""
        if type == 'ram':
            wt = '-write_thru off'
        s = '{cmd} {type} -instname "{name}" -words {depth} -bits {width} -mux {mux} -pipeline off -write_mask off {wt} -name_case upper -check_instname off -corners ss_0p81v_0p81v_125c,ss_0p81v_0p81v_m40c'.format(
            cmd = self.generator_cmd[type],
            type='{type}', 
            name=iname,
            depth = int(depth),
            width = int(width),
            mux = int(mux),
            wt = wt
        )
        # ff_0p99v_0p99v_m40c,tt_0p90v_0p90v_25c,ffg_0p99v_0p99v_125c,ff_0p99v_0p99v_125c,
        c = ""
        if target == 'all':
            tar = [ self.targets[t] for t in self.targets ]
        else:
            tar = [self.targets[target]]
            
        for t in tar:
            c += s.format(type=t)
            if len(tar) > 1:
                c +=  ";"

        return c 
    
ips = {
    'tsmc_cln40g':    tsmc_cln40g_sram(),
    'tsmc_cln16fpll': tsmc_cln16fpll_sram(),
    'smic_28hkmg':    smic_28hkmg_sram() }

if __name__ == "__main__":

    if len(sys.argv)<3:
        print("syntax: python memory_ip depth width [tech] [conf]")
        print("        Where tech is one of", list(ips.keys()))
        print("        The conf keyword will return a conf dictionary instead of a generator command string.")
        exit()
    if len(sys.argv)>=4:
        if sys.argv[3] in list(ips.keys()):
            mem = ips[sys.argv[3]]
        else:
            raise ValueError("Unknown library %s" % sys.argv[3]) 
    else:
        mem = tsmc_cln40g_sram()
            
    if len(sys.argv)==5:
        if sys.argv[4]=='conf':
            print(mem.conf(int(sys.argv[1]), int(sys.argv[2])))
            exit()
    print(mem.cmd(sys.argv[1], sys.argv[2], 'ram'))
