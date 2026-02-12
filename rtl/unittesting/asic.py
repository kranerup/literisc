from modules.common.Common import rundir, hwdir
import os 

class asic_mem:
    lock_flag = False
    memory_unique = []
    memory_inst = []
    memory_cmd = []

    memory_conf_inst = []
    
    def add_cmd(self, m):
        print("asic.add_cmd %s" %m)
        if not self.lock_flag:
            new = 1
            for u in self.memory_cmd:
                if (u == m):
                    new = 0
                    break
            if new:
                self.memory_cmd.append(m)

    def add_mem(self,m, conf=False):
        if not self.lock_flag:
            if conf:
                self.memory_conf_inst.append(m)
            else:
                self.memory_inst.append(m)
            new = 1
            md = dict(m)
            del md['name']
            for u in self.memory_unique:
                if (u == md):
                    new = 0
                    break
            if new:
                print("asic.py: Adding memory", md)
                self.memory_unique.append(md)

    def lock_mem(self):
        if not self.lock_flag:
            self.lock_flag = True
            uniq_inst = []
            for i in self.memory_inst:
                if not i in uniq_inst:
                    uniq_inst.append(i)
            self.to_file(uniq_inst, "memory_inst.txt")
            uniq_inst = []
            for i in self.memory_conf_inst:
                if not i in uniq_inst:
                    uniq_inst.append(i)
            self.to_file(uniq_inst, "memory_conf.txt")
            print("asic.py: Saving macro list", self.memory_unique) 
            self.to_file(self.memory_unique, "memory_unique.txt")
            self.cmd_to_file(self.memory_cmd, "memory")

    def cmd_to_file(self, cmdlist, file):
        lists = {'synopsys': [], 'liberty': [], 'verilog': [],'ascii': []}
        for t in list(lists.keys()):
            sf = os.path.join(rundir(), "%s_%s.sh"%(file, t))
            with open(sf, 'w') as F:
                for l in cmdlist:
                    s = l.split(";")
                    for c in s:
                        if t in c:
                            c = c.replace(";", "")
                            c = c.replace(r'/opt/ip/arm', "$ARMPATH")
                            F.write(c+"\n")
            if os.stat(sf).st_size == 0:
                print("No output for %s, removing" % (sf))
                os.remove(sf)
           
    def field_order(self):
        return [ 'type', 'width', 'depth', 'write_through', 'input_flops', 'output_flops', 'name' ]

    def ordered_existing(self, md ):
        order = self.field_order()
        ordered = []
        for f in order:
            if f in md:
                ordered.append(f)
        for k in list(md.keys()):
            assert k in order, "memory field {} is unknown".format(k)
        return ordered

    def to_file(self, memlist, file):
        with open(os.path.join(hwdir(), "hdl", file), 'w') as F:
            print("Writing memory list", file)
            types = []
            noname = 0
            for m in memlist:
                if not m['type'] in types:
                    types.append(m['type'])
            for t in types:
                F.write('\n')
                F.write('# ')
                for m in memlist:
                    if m['type']==t:
                        md = dict(m)
                field_lst = self.ordered_existing( md )
                field = ', '.join( field_lst )
                F.write( field )
                F.write(',\n')

                for m in memlist:
                    if m['type'] == t:
                        md = dict(m)
                        values = [ str(md[f]) for f in field_lst ]
                        val_str = ', '.join( values )
                        F.write( val_str )
                        F.write(',\n')

