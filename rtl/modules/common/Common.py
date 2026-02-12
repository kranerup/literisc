#!/usr/bin/python
# -*- coding: utf-8 -*-
from myhdl import *
import random
import sys, os
import re
import shutil
#import yaml


"""
File status: There is a lot of crap in the Common module. Most of it
is used somewhere, but there is no order and only some of it is
tested.

Since these blocks are used in lots of places, testing is particularly
important for these little blocks. Since they are so limited in
functionality it is often very easy to test them too.

It would be a good idea to check if there are unused blocks, sort them
in some reasonable order. Perhaps rename a few to make it more
intuitive. Make brief table of contents perhaps.
"""

class byteStream(object):
    """
    A class representing a stream of bytes. Can be initialized with either
    a lenght, a width, and a random seed; or with a width and a list of values.
    The class can be iterated over or emptied using the next() method.

    The nifty thing is that you don't create the actual data until it is necessary,
    thus saving memory. If the byte stream is used as in TLM transactions, no data
    will be created at all.

    The reason for having both "length" and "length_in_bytes" is that byteStream is 
    used with both versions. They mean the same.
    """
    def __init__(self, length=None, seed=None, data=None, width=8, setBytes={}, delBytes=[], addBytes={}, length_in_bytes=None, name="", debug=False, **kwargs): 
        if setBytes != {} and width%8 != 0:
            print("ERROR for byteStream %s! setBytes only supported with even byte widths." % name)
            assert False
        self.setBytes = setBytes
        self.delBytes = delBytes
        self.addBytes = addBytes
        self.addCurrent = None
        if self.addBytes != {}:
            self.addCurrent = list(range(len(self.addBytes)))
        self.addCnt     = 0
        if data != None:
            self.length = len(data)
        elif length:
            self.length = length
        else:
            self.length = length_in_bytes
        self.seed = seed
        self.data = data
        if self.data == None and self.seed == None:
            self.seed = random.random()
        self.width = width
        self.cnt = 0
        self.byteCnt = 0
        self.mask = 2**self.width - 1
        self.name = name
        self.debug = debug
        if self.debug:
            print("Instatiated", type(self).__name__, name, "length =", length, "seed =", seed, "width =", width, "setBytes =", setBytes, "delBytes =", delBytes, "addBytes =", addBytes) 
    def __iter__(self):
        return self
    def __next__(self):
        if self.cnt == self.length:
            raise StopIteration
        if self.data != None:
            self.cnt += 1
#            #print "Data!=None"
            return self.data[self.cnt-1]
        if self.cnt == 0:
            self.rand = random.Random(self.seed)
        self.cnt += 1
        byteData = 0
#        #print "Next", self.width, self.width//8
        bytesPerWord=self.width//8
        collected_bytes = 0
        for bc in reversed(list(range(bytesPerWord))):
            while collected_bytes<bytesPerWord:
                inc_byteCnt = False
                #print "cnt, keys:", self.byteCnt, self.addBytes.keys()
                if self.byteCnt in list(self.addBytes.keys()):
                    #print "Byte", self.byteCnt, "in keys", self.addBytes.keys() 
                    #print "  curr", self.addCurrent
                    #print "  cnt ", self.addCnt
                    if self.addCurrent != self.byteCnt:
                        self.addCurrent = self.byteCnt
                        self.addCnt     = 0
                    if self.addCnt < len(self.addBytes[self.byteCnt]):
                        randByte = self.addBytes[self.byteCnt][self.addCnt]
                        self.addCnt += 1
                    else:
                        randByte = self.rand.randrange(2**8)
                        inc_byteCnt = True
                else:
                    randByte = self.rand.randrange(2**8)
                    inc_byteCnt = True
                if not self.byteCnt in self.delBytes:
                    collected_bytes += 1
                    byteData = byteData >> 8
                    if self.byteCnt < len(self.setBytes) and inc_byteCnt:
                        byteData = byteData + (self.setBytes[self.byteCnt] << ((bytesPerWord-1)*8))
                    else:
                        byteData = byteData | (randByte << ((bytesPerWord-1)*8))
                if inc_byteCnt:
                    self.byteCnt += 1
        return byteData
    def __str__(self):
        out = "byteStream object "+self.name+" length=" + str(self.length) +  ", seed=" + str(self.seed) + ", width=" + str(self.width) +  " setBytes=" +  str(self.setBytes) + "\n  "
        return out + "\n"

    def __list__(self):
        word_list = []
        for data in self:
            word_list.append(data)
        byte_list = toByteList(word_list, self.width)
        return byte_list

def getFletcherHash(length=None, seed=None, lastHash=None, data=None, setBytes={}, width=8):
    """
    Caculate and return a fletcher hash. 

    If a lastHash is supplied (the two last c0 and c1 concatinated)
    then a fletcher hash can be calculated directly.

    If a seed and a lenght is supplied the fletcher hash needs to 
    be calculated by working thorugh the entire data stream. 

    If neither lastHash nor seed is given, then a valid fletcher hash 
    is calculated from a random seed.

    Also a list of bytes can be given as input
    """
    if not lastHash == None:        
        c0 = signmod(lastHash, width)
        c1 = signmod(lastHash >> width, width)

        r0 = signmod(-c0 - c1, width)
        c0 = signmod(c0 + r0, width)
        c1 = signmod(c1 + c0, width)
        
        r1 = signmod(-c0 - c1, width)
        c0 = signmod(c0 + r1, width)
        c1 = signmod(c1 + c0, width)
        assert [0, 0] == [c0, c1]
        return int(concat(intbv(r0)[width:], intbv(r1)[width:]))
    if data==None:
        if seed == None:
            seed = random.random()
        assert length != None
        stream = byteStream(length=length, seed=seed, setBytes=setBytes, width=width)
        l = []
        for i in stream:
            l.append(i)
        stream = toByteList(l, width)
    else:
        stream = toByteList(data, width)
    data_hash = intbv(0)[2*8:]
    for i in stream:
        value = intbv(i)[8:]
        data_hash = hashMethod(value, data_hash, method="Fletcher")
    return getFletcherHash(lastHash=data_hash)

def signalType(i):
    """ Return true if the input is a signal """
    return type(i).__name__ in ["_Signal", "_SliceSignal", "ConcatSignal"]
def signalOrIntType(i):
    """ Return true if the input is a signal or an int"""
    return type(i).__name__ in ["_Signal", "_SliceSignal", "ConcatSignal", "int"]
def listType(i):
    """ Return true if the input is a list or tuple """
    return type(i).__name__ in ["list", "tuple"]
def listOfSignalsType(i, or_less=False):
    """ Return true if the input is a list of signals """
    if or_less and (i==None or signalType(i)):
        return True
    if not listType(i):
        return False
    for s in i:
        if not signalType(s):
            return False
    return True

def toByteList(a, w):
    out = []
    for i in range(len(a)):
        word = a[i]
        for j in range(w//8):
            out.append(word & 255)
            word = word >> 8
    assert len(out)==len(a)*w//8
    return out

# Sum a nested array
def sumall(a):
    ret = 0
    if isinstance(a, int) or isinstance(a, float):
        return a
    else:
        return sum( [sumall(i) for i in a] )
    
def compoundWidth(a):
    """
    Sums the widths in a compound list of signals or ints
    """
    if listType(a):
        val = 0
        for i in a:
            val += compoundWidth(i)
        return val
    elif type(a).__name__ == "bool":
        #print "Warning! compoundWidth data type is not a Signal:", type(a).__name__
        return 1
    elif signalType(a):
        return len(a)
    elif a==None:
        return 0
    else:
        print("Warning! compoundWidth data type is not a Signal:", type(a).__name__)
        return a

def istrue(a, b):
    if a:
        if b in a:
            if a[b]:
                return True
    return False
    
def is_flopmem(width, depth, hwconf):
    flopmem = 0
    if hwconf.memory_flop_limit != None:
        if (width*depth < hwconf.memory_flop_limit):
            flopmem=1
    if hwconf.memory_flop_dlimit != None:
        if (depth < hwconf.memory_flop_dlimit):
            flopmem=1
    return flopmem
            
def nrOfSignals(a):
    """
    Return the number of signals in a compound list of signals
    """
    if signalType(a):
        return 1
    else:
        out = 0
        for i in a:
            out += nrOfSignals(i)
        return out
    
def flatSignalList(a):
    """
    Return a flattened verison of a compound list of signals
    """
    if signalType(a) or listOfSignalsType(a):
        return copySignal(a)
    else:
        out = []
        for i in a:
            out.append(Signal(intbv(0)[compoundWidth(i):]))
        return out

def get_q2prio(q2prio_in, q2prio, ss_id, hwconf, name):
    zPassq2p = []
    if q2prio_in==None:
        linear = []
        for port in range(len(hwconf.slice_ports[ss_id])):
            offs = hwconf.slice_queue_offset[ss_id][port]
            for q in range(hwconf.slice_port_queues[ss_id][port]):
                linear.append(q)
        for i in range(len(linear)):
            zPassq2p.append(pass_through(linear[i], q2prio[i], name=name+".zPassq2p%s"%i))
    else:
        for i in range(len(q2prio)):
            zPassq2p.append(pass_through(q2prio_in[i], q2prio[i], name=name+".zPassq2p%s"%i))
    return instances()
    
def get_q2prio_list(ss_id, hwconf):
    return [ Signal(intbv(0)[(hwconf.max_queues_per_port-1).bit_length():]) for i in range(hwconf.slice_total_queues[ss_id]) ]

def get_q2port(q2port_in, q2port, ss_id, hwconf, name):
    zPassq2port = []
    if q2port_in==None:
        q2port_list = []
        for port in range(len(hwconf.slice_ports[ss_id])):
            offs = hwconf.slice_queue_offset[ss_id][port]
            queues = hwconf.slice_port_queues[ss_id][port]
            for q in range(hwconf.max_queues_per_port):
                if q<queues:
                    q2port_list.append(port)
        for i in range(len(q2port)):
            zPassq2port.append(pass_through(q2port_list[i], q2port[i], name=name+".zPassq2port%s"%i))
    else:
        for i in range(len(q2port)):
            zPassq2port.append(pass_through(q2port_in[i], q2port[i], name=name+".zPassq2port%s"%i))
        
    return instances()
            
def get_q2port_list(ss_id, hwconf):
        return [ Signal(intbv(0)[(len(hwconf.slice_ports[ss_id])-1).bit_length():] ) for _ in range(hwconf.slice_total_queues[ss_id]) ]
    
def sort_chunk_group(hwconf, ss):
    """
    Analyze port_distance and return how many cell_builder/splitter are needed
    """
    schedule_group_ports = []    
    chunk_group_ports = [] # Excluding non-spps ports
    print("chunk_group_schedule", hwconf.chunk_group_schedule)
    if not isinstance(hwconf.chunk_group_schedule[ss][0], int):
        # More than one instance
        for visit_order in hwconf.chunk_group_schedule[ss]:
            port_list = list(dict.fromkeys(visit_order))
            schedule_group_ports.append(len(port_list))
            chunk_group_ports.append(0)
            for p in port_list:
                if not p in hwconf.internal_raw_ports:
                    chunk_group_ports[-1]+=1
    else:
        port_list = list(dict.fromkeys(hwconf.chunk_group_schedule[ss])) # remove repeated port
        schedule_group_ports.append(len(port_list))
        chunk_group_ports.append(0)
        for p in port_list:
            if not p in hwconf.internal_raw_ports:
                chunk_group_ports[-1]+=1
    print("schedule_group_ports", schedule_group_ports)
    
    # Total number of ports shall match
    if hwconf.sub_id_config:
        if len(hwconf.group_ports[ss]) != sum(schedule_group_ports):        
            print("ERROR! len(bridge_group_ports[%s]=%s)=%s does not match sum(schedule_group_ports=%s=%s" % (ss, hwconf.group_ports[ss], len(hwconf.group_ports[ss]), schedule_group_ports, sum(schedule_group_ports)))
            assert False
    else:
        if hwconf.nr_of_internal_raw_ports==0: 
            if sum(hwconf.group_ports[ss]) != sum(schedule_group_ports):
                print("ERROR! The schedule_group_ports %s do not match the group_ports %s for slice %s" % (schedule_group_ports, hwconf.group_ports[ss], ss))
                assert False
    # Total number of chunk mems shall not larger than the number of different chunk size
    if len(chunk_group_ports) > len(hwconf.chunk_size[ss]):
        print("ERROR! Cannot have %d chunk memories while there's only %d different chunk size"%(len(chunk_group_ports), len(hwconf.chunk_size[ss])))
        assert False
    
    return chunk_group_ports, schedule_group_ports
    
    
def blen(a):
    if signalType(a):
        return len(a)
    elif all_signals(a):
        return compoundWidth(a)
    elif type(a).__name__ == "int":
        return a.bit_length()
    elif a==None:
        return 0
    else:
        print("Unsupported type for blen, ", type(a).__name__)
        raise ValueError()

def all_signals(a):
    if signalType(a):
        return True
    if listType(a):
        for i in a:
            if not all_signals(i):
                return False
    else:
        return False        
    return True
    
def compoundSignal(a):
    """ 
    Creates a compound signal from a compound width list
    """
    if listType(a):
        out = []
        for i in a:
            out.append(compoundSignal(i))
        return out
    else:
        return Signal(intbv(0)[a:])

def compoundLen(a):
    """ 
    Creates a compound list of widths from a compound list of signals
    """
    if listType(a):
        out = []
        for i in a:
            out.append(compoundLen(i))
        return out
    else:
        if a==None:
            return 0
        else:
            return len(a)

def debugSignal(a, debug_level=None, debug_descr=None, flatten=True):
    """ Make a single flattened debug signal or a set of indexed debug signals a from a compound input"""
    if flatten or not isinstance(a, list):
        debug = Signal(modbv(0)[compoundWidth(a):0:0], debug_level=debug_level, debug_descr=debug_descr)
        zPassdebug = pass_through(a, debug, name="debugSignal_"+str(debug_descr).replace(" ", "_"))
    else:
        zDebug = []
        for i in range(len(a)):            
            zDebug.append(debugSignal(a[i], debug_level=debug_level, debug_descr=debug_descr+" %s"%i, flatten=False))
    return instances()
    
def copySignal(a, t=intbv, debug_level=None, debug_descr=None ):
    """
    Returns a new, structurally identical, signal. The type (modbv or intbv) has to be set explicitly though
    TODO: Make enums work, preferably by making the len() method work on enums
    """
    if signalType(a):
        if a.val.__class__.__name__ == 'EnumItem':
            print("ERROR! copySignal does not work for enums, fix it sometime!:", a.val) 
            assert False
        if t is intbv:
            return Signal(intbv(0, min=a.min, max=a.max), debug_level=debug_level, debug_descr=debug_descr)
        else:
            return Signal(modbv(0)[len(a):], debug_level=debug_level, debug_descr=debug_descr)
    elif listType(a):
        return [ copySignal(i, t, debug_level=debug_level, debug_descr=debug_descr) for i in a ]
    elif type(a).__name__ == 'bool':
        return Signal(t(0)[1:0], debug_level=debug_level, debug_descr=debug_descr)
    elif a==None:
        return None
    else:
        print("ERROR! Unsupported type for copySignal:", type(a).__name__)
        assert False


def pass_through(i, o, allow_mismatch=False, translate_off=0, name=""):
    if i is None or o is None:
        return []

    if isinstance(o, SignalType) and isinstance(i, (SignalType, int, bool)):
        if isinstance(i, (int, bool)):
            st = o._val.__class__
            i = Signal(st(int(i), max=o.max, min=o.min))

        @always_comb
        def assign():
            o.next = i

        return instances()

    return connect(o, i)


#@partition('PASS_THROUGH')
def pass_through_old(i, o, allow_mismatch=0, translate_off=0, name=''):

    assert name!='', "ERROR! Unnamed pass_throughs are forbidden"

    """
    Just pass the input to the output. Handy for connecting things

    When passing from a list/signal to a signal/list, the order of the signals
    are reversed. e.g. 
    pass_through([sigA,sigB],sigC) equals sigC.next = concat(sigB,sigA)
    """
    #print name, "IN",i, "OUT", o
    #sys.stdout.flush()

    if (type(i).__name__ == "_Signal") and (len(i) < compoundWidth(o)):
        #print "pass_through %s got input %s %s len %s (%s) and output %s %s len %s (%s) with allow_mismatch = %s" % (name, type(i).__name__, i, len(i), compoundLen(i), type(o).__name__, o, compoundWidth(o), compoundLen(o), allow_mismatch )
        assert allow_mismatch, "ERROR! pass_through %s got input %s %s len %s (%s) and output %s %s len %s (%s) with allow_mismatch = %s" % (name, type(i).__name__, i, len(i), compoundLen(i), type(o).__name__, o, compoundWidth(o), compoundLen(o), allow_mismatch )
        narr = Signal(intbv(int(i))[compoundWidth(o):])
        if signalType(o):
            zNo  = pass_signal(i, o, allow_mismatch=True, translate_off=translate_off, name=name+".zNarr")
        else:
            zNarr = pass_signal(i, narr, allow_mismatch=True, translate_off=translate_off, name=name+".zNarr")
            zOut = pass_through(narr, o, translate_off=translate_off, name=name+".zOut")
        return instances()
        
    # Connect a signal to a compound list of signals
    if translate_off==1:
        print("Consistency_check observation point", name)
    if type(i).__name__ in ['bool', 'int', 'NoneType']:
        leni = 1
    else:
        leni = len(i)
    if i == None:
        o = i
        return instances()
    if signalType(i):
        #if leni<compoundWidth(o):
        #    ii = copySignal(o)
        #    @always_comb
        #    def followit1():
        #        ii.next = i
        if not signalType(o):
            zSs2l = []
            zPs2l = []
            s2lwire = []
            listOfSignals = True
            for item in o:
                if not signalType(item):
                    listOfSignals = False
            if listOfSignals:
                def slice(i, offset, o):
                    oend = offset+len(o)
                    # I would very much like to avoid the shifting here, and use slicing
                    # But that causes "Signal elements should have the same bit width"-errors /Per
                    out = copySignal(o, t=modbv)
                    ow = len(out)
                    tw = max(len(i), len(o))
                    @always_comb
                    def shiftit():
                        tmp = intbv(0)[tw:]
                        tmp[:] = i>>offset
                        out.next = tmp[ow:]
                    if translate_off==1:
                        @always_comb
                        def sliceit1():
                            needless = intbv(0)[1:]
                            "synthesis translate_off"
                            "This is an observation point for consistency checks"
                            o.next = out #i[oend:offset]
                            "synthesis translate_on"
                    else:
                        @always_comb
                        def sliceit2():
                            o.next = out #i[oend:offset]
                    return instances()
                offs = 0
                for x in range(len(o)):
                    zSs2l.append(slice(i, offs, o[x]))
                    offs += len(o[x])
            else:
                for cnt in range(len(o)):
                    if compoundWidth(o[cnt]) == 0:
                        print("ERROR:"+name+" Zero width signal!", end=' ')
                        for i in o:
                            print("  ", leni, compoundWidth(i))
                        print() 
                    s2lwire.append(Signal(intbv(0)[compoundWidth(o[cnt]):]))
                for cnt in range(len(o)):
                    zPs2l.append(pass_through(s2lwire[cnt], o[cnt], allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".out%d"%cnt))
                zPs2li = pass_through(i, s2lwire, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".iwire")
            return instances()
            
    # Connect a compound list of signals to a signal
    if listType(i):
        if not listType(o):
            instl = []
            wire = []
            listOfSignals = True
            for item in i:
                if not signalType(item):
                    listOfSignals = False
            if listOfSignals:
                if leni==1:
                    instl.append(pass_through(i[0], o, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".ltolist1"))
                else:
                    instl.append(pass_through(ConcatSignal(*reversed(i)), o, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".ltolist2"))
            else:
                for cnt in range(leni):
                    print("connect cnt,i", cnt, i, i[cnt])
                    wire.append(Signal(intbv(0)[compoundWidth(i[cnt]):]))
                for cnt in range(leni):
                    instl.append(pass_through(i[cnt], wire[cnt], allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".in%d"%cnt))
                instl.append(pass_through(wire, o, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".owire"))
            return instances()
    # Connect a signal to a signal
    if signalOrIntType(i) or type(i).__name__ == 'bool':
        zPasssig = pass_signal(i, o, allow_mismatch=allow_mismatch, name=name+".signal")
    # Connect a list of signals to a list of signals
    elif listType(i):
        if compoundLen(i)==compoundLen(o) and listOfSignalsType(i): # The listOfSignals guard is because the tb_xchecker freaks out about list of lists of signals
            zPasslist = []
            for x in range(len(i)):
                zPasslist.append( pass_through(i[x], o[x], allow_mismatch=0, translate_off=translate_off, name=name+".iPasslist%s"%x))
        else:
            iflat = Signal(intbv(0)[compoundWidth(i):])
            oflat = Signal(intbv(0)[compoundWidth(o):])
            zif = pass_through(i, iflat, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".iflat")
            zm  = pass_through(iflat, oflat, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".flatflat")
            zof = pass_through(oflat, o, allow_mismatch=allow_mismatch, translate_off=translate_off, name=name+".oflat")
            #instb = pass_list(i,o, allow_mismatch,name)
    elif "receiverSignals" in dir(i) and "driverSignals" in dir(o): 
        instc = pass_interface(i, o)
    else:
        print("pass_through:"+name+" Unsupported type", type(i).__name__)
        exit()
    return instances()

# This is a dut for the pass_through testbench for ints
def pass_int_dut(i,o,ival,allow_mismatch, translate_off, name=""):
    print("pass_int_dut instanciated with ival =", ival)
    tmp = copySignal(o)
    zPass = pass_through(ival, tmp, allow_mismatch, translate_off, name=name+".zPass")
    @always_comb
    def printi1():
        o.next = i|tmp
    return instances()
# This is a dut for the pass_through testbench for static signals
def pass_static_dut(i,o,ival,allow_mismatch, translate_off, name=""):
    print("pass_static_dut instanciated with ival =", ival)
    tmp = Signal(intbv(ival)[len(o):])
    tmp2 = Signal(intbv(ival)[len(o):])
    zPass = pass_through(tmp, tmp2, allow_mismatch, translate_off, name=name+".zPass")
    @always_comb
    def printi2():
        o.next = i|tmp2
    return instances()
# This is a dut for the pass_through testbench for undriven 
def pass_undriven_dut(i,o,ival,allow_mismatch, translate_off, name=""):
    print("pass_static_dut instanciated with ival =", ival)
    tmp = Signal(intbv(ival)[len(o):])
    tmp2 = Signal(intbv(ival)[len(o):])
    zPass = pass_through(tmp, tmp2, allow_mismatch, translate_off, name=name+".zPass")
    @always_comb
    def printi3():
        o.next = i|tmp2
    return instances()

def all_ints(a, isint=True):
    if type(a).__name__ == "int" or type(a).__name__ == "bool" and isint:
        return True
    elif listType(a):
        for i in a:
            isint = isint and all_ints(i)
        return isint
    else:
        return False

def pass_signal(i,o,allow_mismatch=0,translate_off=0,name=""):
    #print name, "IN",i, "OUT", o
    #sys.stdout.flush()
    if translate_off==1:
        print("Consistency_check observation point", name)
    if type(o).__name__ == 'bool':
        leno = 1
    else:
        leno = len(o)
    if type(i).__name__ in ('bool'):
        leni = 1
    elif type(i).__name__ in ('int'):
        leni = leno
    else:
        leni = len(i)
    if leno==0:
        print("ERROR! Width zero for pass_through", name, "in len:%d, out len:%d o:" % (leni, len(o)), str(o))
        assert False
    if leni!=leno and allow_mismatch==0:
        print("ERROR! Width error for pass_through", name, "in len:", leni, "out len:", len(o))
        assert False
        
    if not (signalType(i) or type(i).__name__ == 'bool'): 
        tmp = Signal(intbv(i)[len(o):]) 
        @always_comb
        def rtl1():
            o.next = tmp
        return instances()
    if leni<leno:
        ii = copySignal(o)
        @always_comb
        def followit2():
            ii.next = i
    else:
        ii=i
    # The logic_zero is here so that an int input will not cause a "sensitivity list empty" error.

    om = o.max-1
    if translate_off==1:
        @always_comb
        def rtl2():
            mask = intbv(-1)[leno:]
            "synthesis translate_off"
            if (ii) & mask > om:
                print(name, "ERROR! output value larger than output max value: %d"%om)
                assert False
            "This is an observation point for consistency checks"
            o.next = ii[leno:]
            "synthesis translate_on"
    else:
        if leni == leno:
            @always_comb
            def rtl3():
                #"synthesis translate_off"
                #if ii>om:
                #    print name,"ERROR! output value larger than output max value: %d"%om
                #    assert False
                #"synthesis translate_on"
                o.next = ii
        else:
            @always_comb
            def rtl4():
                o.next = ii[leno:]
            
    return instances()

def pass_interface(i, o):
    input_list  = i.receiverSignals()
    output_list = o.driverSignals()
    inst = pass_list(input_list, output_list)
    return inst


def value(i):
    """
    Used for making different data types comparable in the testbench
    """
    if signalType(i):
        if i.val.__class__.__name__ == 'EnumItem':
            return int(i._val, 2)
        return int(i)
    if listType(i):
        v=0
        c=0
        for x in i:
            v += value(x) << c
            c+=compoundLen(x)
        return v
    if type(i).__name__ in ['intbv']:
        return int(i)
    print("Warning! Unsupported type for Common.value:", type(i).__name__)
    return i


def _flatlist(s, lst):
    if isinstance(s, list):
        for x in s:
            _flatlist(x, lst)
    else:
        lst.append(s)


def flatten(s):
    """
    Flatten an arbitrarily deep list of signals to a signal
    """
    if isinstance(s, SignalType):
        return s
    else:
        lst = []
        _flatlist(s, lst)
        return ConcatSignal(*reversed(lst))


def flatValue(i):
    """
    Used for making list types and flattened lists comparable in the testbench
    """
    if listType(i):
        return int(flatten(i))
    else:
        return int(i)

    print("ERROR! Unsupported flatValue type:", type(i).__name__)

# Flatten, above, flattens to signals, this flattens to a list
def flatlist(items):
    if not listType(items):
        return [items]
    else:
        ret = []
        for i in items:
            if listType(i):
                ret.extend(i)
            else:
                ret.append(i)
        return ret
            
def randSignal(a):
    if listType(a):
       out = [] 
       for i in a:
           out.append(randSignal(i))
       return out
    elif signalType(a):
        return random.randrange(a.min, a.max)
    else:
        print("ERROR unsupported randSignal type". type(a).__name__)
        assert False

def nr_of_members(a):
    out = 0
    if listType(a):
        for i in a:
            out = out + nr_of_members(i)
    else:
        return 1
    return out

def startInMatrix(m,index,offset=0):
    start = offset
    for i in range(index[-1]):
        start += compoundWidth(m[i])
    if len(index) == 1:
        return start
    else:
        return startInMatrix(m[index[-1]], index[-2:0], start)

def slice2matrix(flat, los, name=''):
    """
    Takes a wide signal (flat), and a list of narrower signals (los) as input. 
    
    The wide signal is sliced onto the narrow signals.
    The sum of the widths of the narrow signals have to match the width of the wide.
    """

    fl = len(flat)
    al = compoundWidth(los)
    if fl != al:
        print("ERROR! sliceSignal", name, "width mismatch flat", fl, "list", al) 
        assert False
    slice_inst = []
    slice_wire = []
    cnt = 0
    #print "s2m", name, fl, los
    if listType(los) and len(los) == 1:
        #print "  len1"
        slice_inst.append(pass_through(flat, los[0], name=name+".len1"))
    elif listOfSignalsType(los):
        #print "  los"
        slice_inst.append(pass_through(flat, los, name=name+".slice"))
    else:
        #print "  comp"
        for i in range(len(los)):
            cnt_last = cnt
            cnt += compoundWidth(los[i])
            slice_wire.append(flat(cnt, cnt_last))
            slice_inst.append(slice2matrix(slice_wire[i], los[i], name=name+'.los%d'%i))
    return instances()


def sliceSignal(flat, los, name=''):
    """
    Takes a wide signal (flat), and a list of narrower signals (los) as input. 
    
    The wide signal is sliced onto the narrow signals.
    The sum of the widths of the narrow signals have to match the width of the wide.
    """

    fl = len(flat)
    al = sum([len(a) for a in los])
    if fl != al:
        print("ERROR! sliceSignal", name, "width mismatch flat", fl, "list", al, [len(a) for a in los])
        assert False
    zSlice = []
    slice_wire = []
    cnt = 0
    if len(los) == 1:
        slice_inst.append(pass_through(flat, los[0], name=name+".len1"))
    else:
        lower = 0
        for i in range(len(los)):
            upper = lower + len(los[i])
            zSlice.append(signalSlice(flat, upper, lower, los[i]))
            lower = upper
    return instances()

def sliceSignalArg(flat, *arg):
    """
    Takes a wide signal (flat), and a number of narrower signals (*arg) as input. 
    
    The wide signal is sliced onto the narrow signals.
    The sum of the widths of the narrow signals have to match the width of the wide.
    """

    fl = len(flat)
    al = sum([len(a) for a in arg])
    if fl != al:
        print("ERROR! sliceSignal width mismatch flat", fl, "list", al) 
        assert False
    slice_inst = []
    slice_wire = []
    cnt = 0
    if len(arg) == 1:
        slice_inst.append(pass_through(flat, arg[0], name="ssArg1"))
    else:
        for i in range(len(arg)):
            cnt_last = cnt
            cnt += len(arg[i])
            slice_wire.append(flat(cnt, cnt_last))
            slice_inst.append(pass_through(slice_wire[i], arg[i], name='ssArg2'))
    return instances()

def signalSlice(a, upper, lower, b, name=""):
    """
    The slice signals are broken in MyHDL, so this is a substitute.
    b.next = a[offset+len(b):offset]
    """
    @always_comb
    def sliceit3():
        b.next = a[upper:lower]
    return instances()
    
def transpose(i, o, name=""):
    ''' Transpose (AxB -> BxA) a matrix or a list of signals 
    '''
    if listOfSignalsType(i) and listOfSignalsType(o): 
        d = len(i)
        w = len(o)
        assert w==len(i[0])
        assert d==len(o[0])

        @always_comb
        def tras():
            for x in range(w):
                temp = intbv(0)[d:]
                for y in range(d):
                    temp[y] = i[y][x]
                o[x].next = temp
    else:
#        print "ERROR! For some reason transposing matrices does not work for synthesis. Debug it sometimes! /P"
#        assert False
        assert len(o) == len(i[0]), "%s ERROR! len(o) %s != len(i[0]) %s" % (name, len(o), len(i[0]))
        assert len(o[0]) == len(i), "%s ERROR! len(o[0]) %s != len(i) %s" % (name, len(o[0]), len(i))
        zTsp = []
        #print "transpose %s o %s" % (name, o)
        #print "transpose %s i %s" % (name, i)
        iflat = Signal(intbv(0)[compoundWidth(i):])
        iFs = pass_through(i, iflat, name=name+".iFs")
        
        for x in range(len(o)):
            for y in range(len(o[x])):
                #print "trans", y,x, "->", x, y
                zTsp.append(pass_through(i[y][x], o[x][y], name=name+".zTsp_%d_%d"%(x, y)))
    return instances()


# -------------------------------------------------------------------------------
def flop(i, o, clk, rstn, reset_value=0, name=""):
    if i == None:
        o = i
    else:
        inst = flop_e(i, o, 1, clk, rstn, reset_value, name)
    return instances()

def flop_e(i, o, e, clk, rstn, reset_value=0, name=""):
    """
    Flip-flop with enable

    TODO: Use pass_through to convert to and from compound data types (in fact do that everywhere)
    """

    zpass = []
    if compoundLen(i)!=compoundLen(o):
        i_flat = Signal(intbv(0)[compoundWidth(i):])
        zpass.append(pass_through(i, i_flat, name=name+".passi"))
        o_flat = Signal(intbv(0)[compoundWidth(o):])
        zpass.append(pass_through(o_flat, o, name=name+".passo"))
        inst = flop_e_signal(i_flat, o_flat, e, clk, rstn, reset_value, name=name+".flatflop")
    elif signalOrIntType(i):
        inst = flop_e_signal(i, o, e, clk, rstn, reset_value, name+".sigflop")
    elif listType(i):
        inst = flop_e_list(i, o, e, clk, rstn, reset_value, name+".listflop")
    else:
        print("ERROR:"+name, "Unsupported type", type(i).__name__)
        exit()
    return instances()

def flop_list(i,o,e,clk,rstn, reset_value=0, name=""):
    iflop = flop_e_list(i, o, 1, clk, rstn, reset_value, name)
    return instances()

def flop_e_list(i,o,e,clk,rstn, reset_value=0, name=""):
    input_list = list(i)
    output_list = list(o)
    f = []
    for x in range(len(input_list)):
        if listType(e):
            f.append(flop_e_signal(input_list[x], output_list[x], e[x], clk, rstn, reset_value, name=name+".l%d"%x))
        else:
            f.append(flop_e_signal(input_list[x], output_list[x], e, clk, rstn, reset_value, name=name+".s%d"%x))
    return instances()

def flop_signal(d, q, clk, rstn, reset_value=0, name=""):
    iflop = flop_e_signal(d, q, 1, clk, rstn, reset_value, name)
    return instances()

def flop_e_signal(d, q, e, clk, rstn, reset_value=0, name=""):
    if not signalType(q):
        print(name, "ERROR! flop_e_signal takes only signal input. q is %s" % type(q).__name__)
        assert False
    if not signalType(d):
        print(name, "ERROR! flop_e_signal takes only signal input. d is %s" % type(d).__name__)
        assert False
    try:
        reset_data = int(reset_value)
    except:
        print("ERROR!", name, "reset_value", reset_value, "cannot be converted to int")
        assert False
    qm = q.max
    qmin = q.min
    if d.max>qm:
        check_max=1
    else:
        check_max=0
    if d.min<qmin:
        check_min=1
    else:
        check_min=0
    if type(e).__name__=='int' and e==1:
        @always(clk.posedge, rstn.negedge)
        def flop_rtl():
            if rstn==0:
                q.next = int(reset_value)
            else:
                if check_max==1:
                    "synthesis translate_off"
                    if d>=qm:
                        print("WARNING! %s value overflow for %s, must be less than %s" % (d, name, qm))
                        #assert False
                    "synthesis translate_on"
                if check_min==1:
                    "synthesis translate_off"
                    if d<qmin:
                        print("WARNING! %s value underflow for %s, must be %s or more" % (d, name, qmin))
                        #assert False
                    "synthesis translate_on"
                q.next = d
    else:
        @always(clk.posedge, rstn.negedge)
        def flop_e_rtl():
            if rstn==0:
                q.next = int(reset_value)
            else:
                if e==1:
                    if check_max==1:
                        "synthesis translate_off"
                        if d>=qm:
                            print("WARNING! %s value overflow for %s, must be less than %s" % (d, name, qm))
                            #assert False
                        "synthesis translate_on"
                    if check_min==1:
                        "synthesis translate_off"
                        if d<qmin:
                            print("WARNING! %s value underflow for %s, must be %s or more" % (d, name, qmin))
                            #assert False
                        "synthesis translate_on"
                    q.next = d
                else:
                    q.next = q
    return instances()

# -------------------------------------------------------------------------------
def multiflop(i, o, clk, rstn, depth=1, reset_value=0, name=""):
    iMf = multiflop_e(i, o, 1, clk, rstn, depth, reset_value, name=name+".Mf")
    return instances()
    
def multiflop_e(i, o, e, clk, rstn, depth=1, reset_value=0, name=""):
    # Check if the input has signal in it.
    hasSignal = False
    hasNoneType  = False
    if i==None:
        return instances()
    elif listType(i):
        str_i = str(i)
        if 'Signal' in str_i:
            hasSignal = True
        if 'None' in str_i:
            hasNoneType = True
    if hasNoneType:
        if hasSignal:
            print("ERROR! Input has None type with signals in", name, i)
            assert False
        else:
             o = i
             return instances()

    zMf = []
    iflat = Signal(intbv(0)[compoundWidth(i):])
    if not signalType(o):
        if compoundLen(i)!=compoundLen(o) or reset_value!=0:
            oflat = Signal(intbv(0)[compoundWidth(o):])
            zMf.append(pass_through(i, iflat, name=name+".zPassi"))
            zMf.append(pass_through(oflat, o, name=name+".zPasso"))
            zFlatpipe = multiflop_e(iflat, oflat, e, clk, rstn, depth=depth, reset_value=reset_value, name=name+".zFlatpipe")
            return instances()
        else:
            for x in range(len(i)):
                zMf.append(multiflop_e(i[x], o[x], e, clk, rstn, depth=depth, reset_value=0, name=name+".zMf%s"%x))
            return instances()
                
    if depth==0:
        zpass = pass_through(i, o, name=name+".pass")
    elif depth==1:
        zflop = flop_e(i, o, e, clk, rstn, reset_value=reset_value, name=name+".flop")
    else:
        zMf.append(pass_through(i, iflat, name=name+".zPassi"))
        stage = [ copySignal(iflat) for _ in range(depth) ]
        assert depth > 1
        @always(clk.posedge, rstn.negedge)
        def flop_stage1():
            if rstn==0:
                for x in range(depth):
                    stage[x].next = reset_value
            else:
                if e==1:
                    stage[0].next = iflat
                    if depth > 1:
                        for x in range(0, depth-1):
                            stage[x+1].next = stage[x]
        zPass = pass_through(stage[depth-1], o, name=name+".zPass")
    return instances()

# -------------------------------------------------------------------------------
def sync_flop_signal(i, o, clk, rstn, depth=2, name="" ):

    o.driven = "wire"
    width = len(i)
    __verilog__ = \
'''
verilog_sync_flops #(.depth({depth}), .width({width})) {name}_sync_ff(
  .inp(%(i)s),
  .outp(%(o)s),
  .clk(%(clk)s),
  .rstn(%(rstn)s ));
'''.format( depth=depth, width=width, name=name.replace(".", "_") )

    # myhdlsim version:
    ff_async = multiflop( i, o, clk, rstn, depth=depth, reset_value=0, name=name+".sync" )

    return instances()

def sync_flop_list(i,o,clk,rstn, input_clk=None, depth=2,  name=""):
    input_list = list(i)
    output_list = list(o)
    f = []
    for x in range(len(input_list)):
        f.append(sync_flop_signal( input_list[x], output_list[x], clk, rstn, 
                                   depth=depth, 
                                   name=name+".s%d"%x))
    return instances()

# If the input is not directly from a flop then there needs to be a
# extra flop, clocked on the sending clock, before the synchronization flops.
# This will be added if input_clk is set.
def sync_flop( i, o, clk, rstn, input_clk=None,input_rstn=None, depth=2, itype='always_ff', name="" ):
    #print 'instantiating sync_flop', depth, itype, name

    if itype == 'always_ff':
        the_flops = multiflop( i, o, clk, rstn, depth=depth, reset_value=0, name=name+".sync" )
        return instances()

    if i==None:
        #print 'sync_flop: None'
        return instances()
    if depth==0:
        #print 'sync_flop: depth=0'
        zpass = pass_through(i, o, name=name+".pass")
        return instances()

    zpass = []
    if compoundLen(i)!=compoundLen(o):
        #print 'sync_flop: i or o needs flattening'
        i_flat = Signal(intbv(0)[compoundWidth(i):])
        zpass.append(pass_through(i, i_flat, name=name+".passi"))
        o_flat = Signal(intbv(0)[compoundWidth(o):])
        zpass.append(pass_through(o_flat, o, name=name+".passo"))

        if input_clk is None:
            inst = sync_flop_signal( i_flat, o_flat, clk, rstn,
                                     depth=depth,
                                     name=name+".flatflop")
        else:
            presync = copySignal( i_flat )
            ff_sync = multiflop( i_flat, presync, input_clk, input_rstn, depth=1, reset_value=0, name=name+".sync" )
            inst = sync_flop_signal( presync, o_flat, clk, rstn,
                                     depth=depth,
                                     name=name+".flatflop")

    elif signalOrIntType(i):
        #print 'sync_flop: i is single Signal'
        if input_clk is None:
            inst = sync_flop_signal( i, o, clk, rstn,
                                     depth=depth,
                                     name=name+".sigflop")
        else:
            presync = copySignal( i )
            ff_sync = multiflop( i, presync, input_clk, input_rstn, depth=1, reset_value=0, name=name+".sync" )
            inst = sync_flop_signal( presync, o, clk, rstn,
                                     depth=depth,
                                     name=name+".sigflop")
    elif listType(i):
        #print 'sync_flop: i is list'
        if input_clk is None:
            inst = sync_flop_list( i, o, clk, rstn,
                                   depth=depth,
                                   name=name+".listflop")
        else:
            presync = copySignal( i )
            ff_sync = multiflop( i, presync, input_clk, input_rstn, depth=1, reset_value=0, name=name+".sync" )
            inst = sync_flop_list( presync, o, clk, rstn,
                                   depth=depth,
                                   name=name+".listflop")
    else:
        print("ERROR:"+name, "Unsupported type", type(i).__name__)
        exit()
    return instances()
 
# -------------------------------------------------------------------------------
def pipeline(idata, stage, clk, rstn, start=0,
             reset_value=0, drive_first_stage=True, clear=None, name=""):
    """
    Takes a signal---idata---and a list of signals---stage
    stage[x] outputs idata delayed by x clock cycles.
    """
    
    # print "Instanciated pipeline %s: idata %s, stage %s, start %d" % (name,
    # compoundLen(idata), compoundLen(stage), start)

    depth = len(stage)
    width = len(idata)
    assert start < depth
    if clear is None:
        clear = 0 # Hdl block conversion to Verilog can't handle the NoneType

    if width==1 and signalType(stage):
        print("Instanciated %s with width=%s, depth=%s, start=%s" %(name, width, depth, start))
        if depth==1:
            assert start==0
            @always_comb
            def pdata1():
                stage.next = idata
            return instances()
        elif start==depth-1:
            @always_comb
            def pdata2():
                stage.next = idata<<start
            return instances()
            
        pipe = Signal(intbv(0)[depth-start-1:])
        pl = len(pipe)
        if start>0:
            zero = Signal(intbv(0)[start:])
            @always_comb
            def conc1():
                stage.next = concat(pipe, idata, zero)
        else:
            @always_comb
            def conc2():
                stage.next = concat(pipe, idata)
            
        @always(clk.posedge, rstn.negedge)
        def flop_stage2():
            if rstn==0:
                for i in range(pl):
                    pipe.next[i] = reset_value
            else:
                for i in range(pl-1):
                    pipe.next[i+1] = pipe[i]
                pipe.next[0] = idata
                if clear==1:
                    for i in range(pl):
                        pipe.next[i] = reset_value
    
        return instances()
    if listType(idata):
        iw = compoundWidth(idata)
        idata_flat = Signal(intbv(0)[iw:])
        passiflat = pass_through(idata, idata_flat, name=name+".passiflat")
        odata_flat = [ Signal(intbv(0)[iw:]) for _ in range(depth) ]
        passoflat = pass_through(odata_flat, stage, name=name+".passoflat")
        iPipe = pipeline(idata_flat, odata_flat, clk=clk, rstn=rstn, start=start,
                         reset_value=reset_value, drive_first_stage=drive_first_stage, clear=clear, name=name+".iPipe")
        return instances()
    
    if depth==1:
        @always_comb
        def pdata3():
            stage[0].next = idata
    elif start==depth-1:
        @always_comb
        def pdata4():
            stage[start].next = idata
            for i in range(start):
                stage[i].next = 0
        return instances()
    else:
        iw = len(idata)
        pipe = [ Signal(intbv(0)[iw:]) for _ in range(depth-start-1) ]
        pl = len(pipe)
        if drive_first_stage:
            @always_comb
            def conc():
                for i in range(start):
                    stage[i].next = 0
                stage[start].next = idata    
                for i in range(pl):
                    stage[start+i+1].next = pipe[i]
        else:
            @always_comb
            def concNoFirstStage():
                for i in range(start+1):
                    stage[i].next = 0
                for i in range(pl):
                    stage[start+i+1].next = pipe[i]
            
        @always(clk.posedge, rstn.negedge)
        def flop_stage3():
            if rstn==0:
                for i in range(pl):
                    pipe[i].next = reset_value
            else:
                for i in range(pl-1):
                    pipe[i+1].next = pipe[i]
                pipe[0].next = idata
                if clear==1:
                    for i in range(pl):
                        pipe[i].next = reset_value
    return instances()

def pipeline_e(idata, stage, enable, clk, rstn, name, reset_value=0, start=0, clear=0):
    depth = len(stage)
    splus = start+1
    
    pipe = Signal(intbv(0)[depth-start-1:])
    pl = len(pipe)
    @always_comb
    def conc():
        for i in range(start):
            stage[i].next = 0
        stage[start].next = idata    
        for i in range(pl):
            stage[start+i+1].next = pipe[i]
            
    @always(clk.posedge, rstn.negedge)
    def flop_stage4():
        if rstn==0:
            for i in range(pl):
                pipe[i].next = reset_value
        else:
            if enable==1:
                pipe[0].next = idata
                for i in range(pl-1):
                    pipe[i+1].next = pipe[i]
            if clear == 1:
                for i in range(pl):
                    pipe[i].next = reset_value
    return instances()

# -------------------------------------------------------------------------------
def fifo_parallel_read(idata, odata, push, pop, stage, full, empty, level, clk, rstn, name=""):
    depth = len(stages)
    
    @always(clk.posedge, rstn.negedge)
    def portpipe():
        if rstn == 0:
            level.next = 0
            full.next = 0
            empty.next = 1
            for i in range(depth):
                stage[i].next = 0
        else:
            if pop == 1:
                level.next = level-1
                for i in range(depth):
                    if i<level:
                        stage[i].next = stage[i+1]
                if level==depth:
                    full.next = 0
                if level==1:
                    empty.next = 1
            if push == 1:
                level.next = level+1
                stage[level].next = idata
                if level==depth-1:
                    full.next = 1
                if level==0:
                    empty.next = 0
            if push == 1 and pop == 1:
                level.next = level
                empty.next = empty
                full.next = full
    odata = stage[0]
    return instances()
                
def zext(i, target_width):
    """
    Zero-extend the input to the width of target
    """
    assert len(i)<=target_width
    out = intbv(0)[target_width:]    
    out[len(i):] = i
    return out

def clamp(i, floor, ceil):
    """
    Clamp the input to the range
    """
    out = intbv(0, min=floor, max=ceil)    
    if i >= ceil:
        out[:] = ceil-1
    elif i < floor:
        out[:] = floor
    else:
        out[:] = i
    return out

def signmod(value, width):
    """
    Return the "value" sliced to "width" as a signed int.
    """
    return intbv(value)[width:].signed()

def modInc(a):
    """
    A wrapping up counter
    """
    out = intbv(0)[len(a):]
    if a+1 < a.max:
        out[:] =  (a + 1) 
    else:
        out[:] = a.min 
    return out[len(a):]

def modDec(a):
    """
    A wrapping down counter
    """
    out = intbv(0)[len(a):]
    if a==a.min:
        return a.max-1
    else:
        return a - 1

def hashMethod(value, last, method="XOR"):
    if method == "XOR":
        return value ^ last
    elif method == "Fletcher":
        width = len(value)
        l0 = intbv(last)[width:0].signed()
        l1 = intbv(last>>width)[width:0].signed()
        c0 = (l0 + value.signed())
        bvc0 = intbv(c0)[width:]
        c1 = (l1 + bvc0)
        bvc1 = intbv(c1)[width:]
        return int(concat(bvc0, bvc1))
    else:
        print("Method not implemented", method)
        exit()

def signalString(w):
    return "Signal(intbv(0)["+str(w)+":])"

def cleanString(s):
    return str(s).replace(" ", "").replace(",", "_").replace("[", "").replace("]", "")

def range_string(range_list):
    s = ""
    for i in range(len( range_list )):
        l = range_list[i]
        if i > 0:
            if i < len( range_list )-1:
                s += ", "
            else:
                s += " and "
        if len(l) == 1:
            s += "%s" % l[0]
        elif len(l) == 2:
            if len(range_list)==1:
                s += "%s and %s" % (l[0], l[1])
            else:
                s += "%s, %s" % (l[0], l[1])
        else:
            s += "%s-%s" % (l[0], l[-1])
    return s
        
def splicePacketInterface(idata, ivalid_bytes, ifirst, ilast, source_id, odata, ovalid_bytes, ofirst, olast, clk, rstn):
    """
    Takes a compound packet input interface and splices it to multiple simple packet interfaces (of the same width).
    """

    @always(clk.posedge, rstn.negedge)
    def spliceIt():
        if rstn==0:
            for i in range(len(odata)):
                odata[i].next        = 0
                ovalid_bytes[i].next = 0
                ofirst[i].next       = 0
                olast[i].next        = 0
        else:
            for i in range(len(odata)):
                if source_id == i:
                    odata[i].next        = idata
                    ovalid_bytes[i].next = ivalid_bytes
                    ofirst[i].next       = ifirst
                    olast[i].next        = ilast
                else:
                    ovalid_bytes[i].next = 0
                    odata[i].next        = 0
                    ofirst[i].next       = 0
                    olast[i].next        = 0
    return instances()

def bits(a, start, end):
    return (a >> start) & (2**(end-start)-1)

def mux_n(a,o,sel,name=""):
    inst = []

    if listType(sel):
        for i in range(len(sel)):
            inst.append( mux_n(a[i], o[i], sel[i], name=name+".%d"%i) )
        return instances()

    aflat = []
    for i in range(len(a)):
        aflat.append(Signal(intbv(0)[compoundWidth(a[i]):]))
        inst.append(pass_through(a[i], aflat[i], name=name+".flata%d"%i))

    oflat = o
    if listType(o):
        oflat = Signal(intbv(0)[compoundWidth(o):])
        inst.append(pass_through(oflat, o, name=name+".flato"))
    inst.append(mux_n_signal(aflat, oflat, sel))
    return instances()

def mux_n_signal(a, o, sel, name=""):
    @always_comb
    def muxn():
        o.next = a[sel]
    return instances()

def mux2(a, b, o, sel, name=""):
    inst = []

    aflat = a
    if listType(a):
        aflat = Signal(intbv(0)[compoundWidth(a):])
        inst.append(pass_through(a, aflat, name=name+".flata"))

    bflat = b
    if listType(b):
        bflat = Signal(intbv(0)[compoundWidth(b):])
        inst.append(pass_through(b, bflat, name=name+".flatb"))

    oflat = o
    if listType(o):
        oflat = Signal(intbv(0)[compoundWidth(o):])
        inst.append(pass_through(oflat, o, name=name+".flato"))

    inst.append(mux2_signal(aflat, bflat, oflat, sel, name=name+".mux2"))
    return instances()

def mux2_signal(a, b, o, sel, name=""):
    error = False
    af = a
    bf = b
    if signalType(a):
        if a.max>o.max:
            error=True
            print("ERROR!", name, " a.max", a.max, "o", o.max)
        if len(a)!=len(o):
            error=True
            print("ERROR!", name, " len(a)", len(a), " != len(o)", len(o))
    else:
        af = Signal(intbv(a)[len(o):])
        if a>=o.max:
            error=True
            print("ERROR!", name, " a", a, "o", o.max)
    if signalType(b):
        if b.max>o.max:
            error=True
            print("ERROR!", name, " b.max", b.max, "o", o.max)
        if len(b)!=len(o):
            error=True
            print("ERROR!", name, " len(b)", len(b), " != len(o)", len(o))
    else:
        bf = Signal(intbv(b)[len(o):])
        if b>=o.max:
            error=True
            print("ERROR", name, " b", b, "o", o.max)
    if error:
        print("ERROR!", name, "Width mismatch")
        assert False
        
    @always_comb
    def filtC():
        if sel==0:
            o.next = af
        else:
            o.next = bf
    return instances()

def mux_list(a, b, o, sel, name=""):
    """ 
    a, b, o, sel are all lists. The output o[n] is eigher a[n] or b[n] sepending on the state of sel[n]
    """
    inst =  []
    for i in range(len(a)):
        inst.append(mux2(a[i], b[i], o[i], sel[i], name=name+".%d"%i))
    return instances()
    
def add(a, b, s):
    @always_comb
    def iadd():
        s.next = a + b
    return instances()

def OR(a,b,s, name=''):
    if signalType(a):
        if b==None:
            w = len(a)
            @always_comb
            def ior1():
                out = 0
                for i in range(w):
                    if a[i] == 1:
                        out = 1
                s.next = out
        else:
            @always_comb
            def ior2():
                s.next = a | b
    else:
        ior = []
        out = [ Signal(intbv(0)[1:]) for _ in range(len(a))  ]
        for i in range(len(a)):
            if b==None:
                ior.append(OR(a[i], None, out[i], name=name+".or%d"%i))
            else:
                ior.append(OR(a[i], b[i], out[i], name=name+".or%d"%i))
        if b==None:
            out_sig = Signal(intbv(0)[len(a):])
            ior.append(pass_through(out, out_sig, name=name+".flat"))
            ior.append(OR(out_sig, None, s, name=name+".conc"))
        else:
            ior.append(pass_through(out, s, name=name+".pass"))
    return instances()

def list_OR(a,out, name=''):
    w = len(out)
    d = len(a)

    max_d = 100
    if d > max_d:
        left_d  = d//2
        right_d = d - left_d

        right_list = [copySignal(out)  for _ in range(right_d)]
        left_list =  [copySignal(out)  for _ in range(left_d)]

        b = right_list + left_list

        zPs = []
        for i in range(d):
            
            zPs.append(pass_through(a[i], b[i], name=name+'.psList%d'%i))
                   
        right_out = copySignal(out)
        left_out = copySignal(out)
        
        iRight = list_OR(right_list, right_out, name=name+'.right')
        iLeft  = list_OR(left_list, left_out, name=name+'.left')
        
        iEnd   = list_OR([left_out, right_out], out, name=name+'.end')
        
    else:
        @always_comb
        def orout():
            temp = intbv(0)[w:0]
            for i in range(d):
                temp[:] = temp | a[i]
            out.next = temp
    return instances()
        
def AND(a,b,s, name=''):
    if signalType(a):
        if b==None:
            w = len(a)
            @always_comb
            def iand1():
                out = 1
                for i in range(w):
                    if a[i] == 0:
                        out = 0
                s.next = out
        else:
            @always_comb
            def iand2():
                s.next = a & b
    else:
        iand = []
        out = [ Signal(intbv(0)[1:]) for _ in range(len(a))  ]
        for i in range(len(a)):
            if b==None:
                iand.append(AND(a[i], None, out[i], name=name+".or%d"%i))
            else:
                iand.append(AND(a[i], b[i], out[i], name=name+".or%d"%i))
        if b==None:
            out_sig = Signal(intbv(0)[len(a):])
            iand.append(pass_through(out, out_sig, name=name+".flat"))
            iand.append(AND(out_sig, None, s, name=name+".conc"))
        else:
            iand.append(pass_through(out, s, name=name+".pass"))
    return instances()

def count_ones(a, w=32):
    cnt = intbv(0)[w:]
    for i in range(len(a)):
        if a[i]==1:
            cnt[:] = cnt + 1
    return cnt
    
def highest_one(a, w=32):
    cnt = intbv(0)[w:]
    for i in range(len(a)):
        if a[i]==1:
            cnt[:] = i
    return cnt
    
def select_strict(mask, selected, valid, name=""):
    # Strict priority scheduler
    print("Instanciated select_strict %s, with mask %s, selected %s and valid %s" % (name, str(compoundLen(mask)), str(compoundLen(selected)), str(compoundLen(valid) )))
    sw = len(selected)
    nr = len(mask)
    @always_comb
    def sps():
        found = False
        out = intbv(0, min=0, max=nr)
        for i in range(nr):
            if mask[i] == 1 and found==False:
                out[:] = i
                found = True
        selected.next = out[sw:]
    @always_comb
    def spv():
        vtemp=intbv(0)[1:]
        for i in range(nr):
            vtemp[:] = vtemp | mask[i]
        valid.next = vtemp

            # TODO: Remove the above and uncomment the below when the extra_renables are tied properly in PPP
#    if not signalType(mask):
#        mask_s = Signal(intbv(0)[compoundWidth(mask):])
#        ipassm = pass_through(mask, mask_s, name=name+".ipassm")
#        selected_oh = copySignal(mask_s)
#        ioh = select_rr_onehot(mask_s, 1, selected_oh, name=name+".ioh")
#    else:
#        selected_oh = copySignal(mask)
#        ioh = select_rr_onehot(mask, 1, selected_oh, name=name+".ioh")
#    inr = onehot_to_nr(selected_oh, selected, name=name+".inr")
#    ior = OR(selected_oh, None, valid, name=name+".ior")
    return instances()

def select_rr(mask, last, selected, valid, name=""):
    w = len(mask)
    bit_mask = Signal(intbv(0)[w:0])
    zpass = pass_through(mask, bit_mask, name=name+".lin")

    assert len(selected) == len(last), "%s ERROR! len(sel)=%d != len(last)=%d" % (name, len(selected), len(last))
#    print "RR", name, "mask %d, last %d, selected %d" % (len(mask), last.max, selected.max)
    start = Signal(intbv(0)[w:0])
    @always_comb
    def setstart2():
        last_plus_one = (last + 1) % w
        start.next = 0
        start.next[last_plus_one] = 1
    bit_sel = Signal(intbv(0)[w:0])
    ba = select_rr_onehot(bit_mask, start, bit_sel)
    nr = copySignal(selected) 
    tobin = onehot_to_nr(bit_sel, nr, name=name+".tobin")
    @always_comb
    def setout():
        if (bit_mask & bit_sel) != 0:
            selected.next = nr
            valid.next = 1
        else:
            selected.next = 0
            valid.next = 0
    return instances()

def select_rr_onehot(mask, start, selected, name=""):
    if listType(mask):
        mask_flat = Signal(modbv(0)[len(mask):])
        zpm = pass_through(mask, mask_flat, name=name+".pm")
    else:
        mask_flat = mask
    if listType(start):
        start_flat = Signal(modbv(0)[len(start):])
        zpl = pass_through(start, start_flat, name=name+".pst")
    else:
        start_flat = start
    if listType(selected):
        selected_flat = Signal(modbv(0)[len(selected):])
        zps = pass_through(selected_flat, selected, name=name+".ps")
    else:
        selected_flat = selected

    w = len(mask_flat)
    wx2 = 2*w
    @always_comb
    def arb():
        mskx2 = intbv(0)[wx2:0]
        selx2 = intbv(0)[wx2:0]
        mskx2[:] = (mask_flat << w) + mask_flat 
        starts = intbv(0)[w:0]
        starts[:] = start_flat
        selx2[:] = mskx2 & ~(mskx2 - starts)
        selected_flat.next = selx2[w:0] | selx2[wx2:w]
    return instances()

def select_rr_onehot_last(mask, last, selected, name=""):
    if listType(mask):
        mask_flat = Signal(modbv(0)[len(mask):])
        zpm = pass_through(mask, mask_flat, name=name+".pm")
    else:
        mask_flat = mask
    if listType(last):
        last_flat = Signal(modbv(0)[len(last):])
        zpl = pass_through(last, last_flat, name=name+".pl")
    else:
        last_flat = last
    if listType(selected):
        selected_flat = Signal(modbv(0)[len(selected):])
        zps = pass_through(selected_flat, selected, name=name+".ps")
    else:
         selected_flat = selected

    w = len(last_flat)
    start  = Signal(intbv(0)[w:0])
    @always_comb
    def rotate():
        temp = intbv(0)[w:0]
        temp[:] = 0
        for i in range(1, w):
            temp[i] = last_flat[i-1]
        temp[0] = last_flat[w-1]
        start.next = temp
    isch = select_rr_onehot(mask_flat, start, selected_flat)            
    return instances()

def fsb(mask):
    return mask & ~(mask-1)

def first_set_bit(mask, first):
    @always_comb
    def frst():
        first.next = mask & ~(mask-1)
    return instances()

def onehot_to_nr(onehot, nr, name=""):
    wo = len(onehot)
    wn = len(nr)
    nm = nr.max
#    print name, "hot w", wo, "nr max", nr.max
    if nr.max < len(onehot):
        print(name, "ERROR width mismatch", nr.max, "<=", len(onehot))
        assert False
    @always_comb
    def tobin():
        local_var = intbv(0)[wn:]
        for i in range(wn):
            temp = intbv(0)[wo:0]
            for j in range(wo):
                bit_var  = intbv(0)[wn:0]
                bit_var[:] = j
                temp[j] = int(bit_var[i])
            if (temp & onehot) == 0:
                local_var[i] = 0
            else:
                local_var[i] = 1
        nr.next = local_var

    return instances()

class progressbar():
    def __init__(self, max=100, unit="%", unit2="", unit3="", tcconf=None):
        self.max = max
        self.unit = unit
        self.unit2 = unit2
        self.unit3 = unit3
        self.tcconf = tcconf
        self.silent = 0
        if self.tcconf:
            if "progressbar" in tcconf:
                if tcconf['progressbar'] == 0:
                    self.silent = 1
        if not self.silent:
            print("Set up progressbar with max="+str(self.max)+" and unit="+str(self.unit))
    def update(self, a, b="", c=""):
        if not self.silent:
            sys.stdout.write('\r')
            sys.stdout.write("[%-20s] %d%s %s%s %s%s\r" % ('='*int(20.0*a/self.max), a, self.unit, str(b), self.unit2, str(c), self.unit3))
            sys.stdout.flush()
    def finish(self):
        if not self.silent:
            sys.stdout.write('\r')
            sys.stdout.flush()

def mcSamePkt(pkt, mask, nr_of_ports):
    out = []
    for port in range(nr_of_ports):
        if (mask & (1<<port)) != 0:
            out.append(pkt)
        else:
            out.append(None)
    return out

def reListPkt(pkt, dp, queue):
    out = []
    for i in range(len(pkt)):
        dic = {}
        dic['pkt'] = pkt[i]
        dic['dstPort'] = dp[i]
        dic['dstQueue'] = queue[i]
        out.append(dic)
    return out

def minmax_signal(i,o,order='min',name=""):
    """
    Get the minimum or maximum signal from a list
    """
    if not listType(i):
        print('Input should be a list of signals', name)
        assert False
    nr = len(i)
    depth = o.max
    if order == 'min':
        @always_comb
        def minsignal():
            tmp = intbv(0, min=0, max=depth)
            tmp[:] = i[0]
            for i in range(nr-1):
                if i[i+1] < tmp:
                    tmp[:] = i[i+1]
            o.next = tmp
    elif order == 'max':
        @always_comb
        def maxsignal():
            tmp = intbv(0, min=0, max=depth)
            tmp[:] = i[0]
            for j in range(nr-1):
                if i[j+1] > tmp:
                    tmp[:] = i[j+1]
            o.next = tmp
    return instances()

def sort(num, ordered, name=""):
    # Order the values on num from low to high
    nr = len(num)
    nr_w = (nr-1).bit_length()
    # Order the interfaces, emptiest first.
    rank_sig = [ Signal(modbv(0)[nr_w:]) for _ in range(nr) ]
    @always_comb
    def setrank():
        rank = [ modbv(0)[nr_w:] for _ in range(nr) ]
        for i in range(nr):
            rank[i][:] = 0
            for j in range(nr):
                if i!=j and num[i]<num[j]:
                    rank[i][:] = rank[i] + 1
            rank_sig[i].next = rank[i]

    @always_comb
    def setsort():
        order = [ modbv(0)[nr_w:] for _ in range(nr) ]
        cnt = modbv(0)[nr_w:]
        flag = modbv(0)[nr:]
        flag[:] = 0
        cnt[:] = 0
        for i in range(nr):
            order[i][:] = 0
        for j in range(nr):
            for i in range(nr):
                if flag[i]==0 and rank_sig[i]==j:
                    order[cnt][:] = i
                    flag[i] = 1
                    cnt[:] = cnt + 1
        for i in range(nr):
            ordered[i].next = order[i]
    return instances()

def max_jumbo_bytes(hwconf, i2e_bytes):
    if hwconf.max_jumbo_bytes=="default":
        length_mem_max_bytes = (1<<hwconf.pkt_length_w)-1-i2e_bytes
        pb_max_bytes = (min(hwconf.pb_depth)*hwconf.cell_size//8)-i2e_bytes
        if pb_max_bytes < length_mem_max_bytes:
            print("Common: max_jumbo_bytes is limited to %s by the pb_depth %s" % (pb_max_bytes, hwconf.pb_depth))
            return pb_max_bytes
        else:
            print("Common: max_jumbo_bytes is limited to %s by the width of the length mem %s" % (length_mem_max_bytes, hwconf.pkt_length_w))
            return length_mem_max_bytes
    return hwconf.max_jumbo_bytes

def yamlLoadHw(f):
    return yamlLoad(f, hwdir())

def yamlExportHw(v, f):
    yamlExport(v, f, hwdir())

def yamlLoadRun(f):
    return yamlLoad(f, rundir())

def yamlExportRun(v, f):
    yamlExport(v, f, rundir())


_yaml_cache = {}


def yamlLoad(f, basedir):
    pf = os.path.abspath(os.path.join(basedir, f))
    print(f"yamlLoad {pf}", flush=1)
    t = os.stat(pf).st_mtime
    if e := _yaml_cache.get(pf):
        ts, c = e
        if t == ts:
            return c
    with open(pf, 'r') as fh:
        print(f"reading yaml {pf}", flush=1)
        c = yaml.full_load(fh.read())
        _yaml_cache[pf] = t, c
    return c


def yamlExport(v, f, basedir):
    pf = os.path.realpath(os.path.join(basedir, f))
    try:
        with open(pf, 'w') as fh:
            fh.write(yaml.dump(v, default_flow_style=False))
        t = os.stat(pf).st_mtime
        _yaml_cache[pf] = t, v
    except:
        print(f"yamlExport failed to export to: {pf}")
        raise


def loadTcConf(old_tcconf={},silent=False):
    from modules.common.Common import hwdir, reldir, rundir, rootdir, yamlLoadRun
    tcconf_file = os.path.join(rundir(), "tcconf.yml")
    if os.path.isfile(tcconf_file):
        if not silent: print("Loading tcconf from", tcconf_file)
        tcconf = yamlLoadRun(tcconf_file)
        #print "tcconf:"
        #import pprint
        #pprint.pprint(tcconf)
        if old_tcconf != {}:
            tcconf = mergeDict(old_tcconf, tcconf, overwrite=True)

        if tcconf['questaSim'] or tcconf['questaSimGui'] or tcconf['alteraSynt'] or tcconf['xilinxSynt'] or tcconf['xilinxElab'] or tcconf['xilinxSim'] or tcconf['xilinxSimGui'] or tcconf['xilinxFiles'] or tcconf['vivadoImplement'] or tcconf['cosim']:
            if not silent: print("loadTcConf: Setting generate_rtl because questaSim or questaSimGui or alteraSynt or xilinxSynt or xilinxElab or xilinxSim or xilinxSimGui or xilinxFiles or vivadoImplement or cosim is set")
            tcconf['generate_rtl'] = 1
        if tcconf['xilinxElab'] or tcconf['xilinxSim'] or tcconf['xilinxSimGui'] or tcconf['xilinxFiles']:
            if not silent: print("Setting xilinxSynt because xilinxElab or xilinxSim or xilinxSimGui or xilinxFiles is set")
            tcconf['xilinxSynt'] = 1
        if tcconf['refmod'] != '':
            tcconf['dfgsim'] = 1
    else:
        print("No tcconf file found:", os.path.realpath(tcconf_file))
        tcconf={}

    return tcconf

def mergeDict(a, b, path=None, overwrite=False):
    "merges b into a"
    if path is None: path = []
    for key in b:
        if key in a:
            if isinstance(a[key], dict) and isinstance(b[key], dict):
                mergeDict(a[key], b[key], path + [str(key)], overwrite=overwrite)
            elif a[key] == b[key]:
                pass # same leaf value
            elif overwrite:
                print("mergeDict note:", key, "=", a[key], "overwritten with", b[key])
                a[key] = b[key]
            else:
                raise Exception('Conflict at %s %s != %s' % ('.'.join(path + [str(key)]), str(a[key]), str(b[key]) ))
        else:
            a[key] = b[key]
    return a

def rootdir():
    return os.environ.get("FLEXSWITCH_PATH")

def reldir():
    return "../../"

def rundir():
    return os.path.abspath(os.getcwd())

def hwdir():
    return os.path.abspath(os.path.join(rundir(), reldir()))


from struct import *

# Translate Scapy packet to a byte list/dictionary
def tcPkt(pkt):
    s = str(pkt)
    iv = unpack('%0dB'%len(s), s)
    tc_bytes_int = {}
    tc_bytes_hex = []
    for i in range(len(iv)):
        tc_bytes_int[i] = (iv[i])
        tc_bytes_hex.append(hex(iv[i]))
    return {'int':tc_bytes_int,'hex':tc_bytes_hex}

def tcPktInt(pkt):
    s = bytes(pkt)
    iv = unpack('%0dB'%len(s), s)
    return bytearray(iv)

def printHex(a):
    if listType(a):
        return [ printHex(i) for i in a ]
    else:
        return hex(a)

def macGen(mac):
    mac_oct = []
    for i in range(0, 48, 8):
        mac_oct.append("%02x"%(mac>>i&0xff)) # At least two digits
    mac_str = ":".join(mac_oct)
    return mac_str
    
def callCmd(self, cmd_list, **kwarg):
    for s in cmd_list:
        print(s, end=' ')
    print() 
    if subprocess.call(cmd_list, **kwarg) != 0: raise RuntimeError

def set_wire(o):
    if signalType(o):
        o.driven = 'wire'
    else:
        for i in o:
            set_wire(i)

def getVerilogFiles(dir=None, exclude_list=[], exclude_tb=True):
    from glob import glob
    if dir==None:
        dir = os.path.join(hwdir(), 'hdl')
    vfiles = glob(dir+'/*.v')
    vfiles += glob(dir+'/*.sv')
    outfiles = []
    for vf in vfiles:
        if os.path.basename(vf)[:3] != "tb_" or not exclude_tb:
            base_exclude = [ os.path.basename(x) for x in exclude_list ]
            if base_exclude == exclude_list:
                # It the exclude list is without path, compare just the basename
                if os.path.basename(vf) not in exclude_list:
                    outfiles.append(vf)
            else:
                if vf not in exclude_list:
                    outfiles.append(vf)
    return outfiles

def copyVerilogFiles(dir, exclude_name=""):
    print("copyVerilogFile", dir, "exclude", exclude_name) 
    if listType(exclude_name):
        exclude_list = exclude_name
    else:
        exclude_list=[exclude_name]
    vfiles = getVerilogFiles(exclude_list=exclude_list)
    from shutil import copy2 
    for vf in vfiles:
        copy2(vf, dir)
        print("  Copying %s to %s"% (vf, dir))

def pipe_request(request_address, request_data,request_id,request_type,request_re,request_we,
                 request_address_d, request_data_d,request_id_d,request_type_d,request_re_d,request_we_d,
                 clk, rstn, depth=1, name=""):
    zp = []
    zp.append(multiflop(request_address, request_address_d, clk, rstn, depth=depth, name=name+".a"))
    zp.append(multiflop(request_data,    request_data_d,    clk, rstn, depth=depth, name=name+".d"))
    zp.append(multiflop(request_id,      request_id_d,      clk, rstn, depth=depth, name=name+".i"))
    zp.append(multiflop(request_type,    request_type_d,    clk, rstn, depth=depth, name=name+".t"))
    zp.append(multiflop(request_re,      request_re_d,      clk, rstn, depth=depth, name=name+".r"))
    zp.append(multiflop(request_we,      request_we_d,      clk, rstn, depth=depth, name=name+".w"))
    return instances()

def pipe_reply(reply_data,   reply_id,   reply_status,
               reply_data_d, reply_id_d, reply_status_d,
               clk, rstn, depth=1, name=""):
    zp = []
    zp.append(multiflop(reply_data,   reply_data_d,   clk, rstn, depth=depth, name=name+".rd"))
    zp.append(multiflop(reply_id,     reply_id_d,     clk, rstn, depth=depth, name=name+".ri"))
    zp.append(multiflop(reply_status, reply_status_d, clk, rstn, depth=depth, name=name+".rs"))
    return instances()

def bridge(adata, bdata, enable, clka, clkb, rstna, rstnb, name, transparent=False):
    w=len(adata)

    if transparent:
        print("Bridge %s configured as transparent." % name)
        iPass = pass_through(adata, bdata, name=name+".iPass")
        return instances()
    
    write = Signal(intbv(0)[1:])
    read = Signal(intbv(0)[1:])
    write_d = Signal(intbv(0)[1:])
    read_d = Signal(intbv(0)[1:])
    adata_d = Signal(intbv(0)[w:])
    adata_d0 = Signal(intbv(0)[w:])
    iDelayw = multiflop(write, write_d, clkb, rstnb, depth=2)
    iDelayr = multiflop(read,  read_d,  clka, rstna, depth=2)
    iDelayd = multiflop(adata_d0, adata_d,  clkb, rstnb, depth=2)
    @always(clka.posedge, rstna.negedge)
    def iproc():
        if rstna==0:
            write.next = 0
            adata_d0.next = 0
        else:
            if write==0 and enable==1:
                write.next = 1
                adata_d0.next = adata
            if write==1 and read_d==1:
                write.next = 0
                
    @always(clkb.posedge, rstnb.negedge)
    def oproc():
        if rstnb==0:
            read.next = 0
            bdata.next = 0
        else:
            bdata.next = 0
            if read==0 and write_d==1:
                bdata.next = adata_d
                read.next = 1
            if read==1 and write_d==0:
                read.next = 0
    return instances()

def request_bridge(request_address,   request_data,   request_id,   request_type,   request_re,   request_we,
                   request_address_d, request_data_d, request_id_d, request_type_d, request_re_d, request_we_d,
                   clki, clko, rstni, rstno, transparent=False, name=""):
    idata = [request_address, request_data, request_id, request_type, request_re, request_we]
    w = compoundWidth(idata)
    iflat = Signal(intbv(0)[w:])
    zPassi = pass_through(idata, iflat, name=name+".zPassi")
    oflat = Signal(intbv(0)[w:])
    zPasso = pass_through(oflat, [request_address_d, request_data_d, request_id_d, request_type_d, request_re_d, request_we_d], name=name+".zPasso")
    enable = Signal(intbv(0)[1:])
    @always_comb
    def seten1():
        if request_re==1 or request_we==1:
            enable.next = 1
        else:
            enable.next = 0
    iBridge = bridge(iflat, oflat, enable, clki, clko, rstni, rstno, transparent=transparent, name=name+".iBridge")
    return instances()

def reply_bridge(reply_data,   reply_id,   reply_status,
                 reply_data_d, reply_id_d, reply_status_d,
                 clki, clko, rstni, rstno, transparent=False, name=""):
    idata = [reply_data,   reply_id,   reply_status]
    w = compoundWidth(idata)
    iflat = Signal(intbv(0)[w:])
    zPassi = pass_through(idata, iflat, name=name+".zPassi")
    oflat = Signal(intbv(0)[w:])
    zPasso = pass_through(oflat, [reply_data_d, reply_id_d, reply_status_d], name=name+".zPasso")
    enable = Signal(intbv(0)[1:])
    @always_comb
    def seten2():
        if reply_status!=0:
            enable.next = 1
        else:
            enable.next = 0
    iBridge = bridge(iflat, oflat, enable, clki, clko, rstni, rstno, transparent=transparent, name=name+".iBridge")
    return instances()

def num2sig(number, signal=None, mi=None, ma=None, name=""):
    """
    if signal = None: returns a signal that can hold the value(s) in number
    if signal is a Signal: Drives that signal with the value(s) of number 
    """
    
    if signal==None:
        ret = []
        if listType(number):
            if mi == None:
                mi = min(number)
            if ma == None:
                ma = max(number)
            for i in range(len(number)):
                ret.append(num2sig(number[i], mi=mi, ma=ma))
        else:
            if mi == None:
                mi = number
            if ma == None:
                ma = number
            return Signal(intbv(number, min=mi, max=ma+1))
        return ret
        
    zleaf = []

    def assign(number, signal):
        logic_zero = Signal(intbv(0)[1:0])

        @always_comb
        def iassign():
            signal.next = logic_zero + number
        return instances()

    zinst = []
    if listType(number):
        for i in range(len(number)):
            if listType(number[i]):
                zinst.append(num2sig(number[i], signal[i], name=name+".%s"%i))
            else:
                zleaf.append(assign(number[i], signal[i]))
    else:
        zleaf.append(assign(number, signal))
    return instances()

def port_clk(mac_clk, port, hwconf):
    """
    When there are several port groups this function maps the port group clocks the the ports
    """
    if hwconf.group_set==1 and not hwconf.xilinx_mac_mode:
        scnt = 0
        gcnt = 0
        pcnt = 0
        ccnt = 0
        sumcnt = 0
        for _ in range(port+1):
            #print "before p %s, s %s, g %s, c%s" % (pcnt, scnt, gcnt, ccnt)
            if pcnt >= hwconf.group_ports[scnt][gcnt]:
                if gcnt >= len(hwconf.group_ports[scnt])-1:
                    scnt += 1
                    gcnt = 0
                else:
                    gcnt += 1
                #sumcnt += hwconf.group_ports[scnt][gcnt]:
                pcnt = 0
                ccnt += 1 
            pcnt += 1
            #print "after p %s, s %s, g %s, c%s" % (pcnt, scnt, gcnt, ccnt)

            
        #print "group_ports %s" % (hwconf.group_ports)
        print("port_clk returning clock number %s for port %s. slice %s, group %s" %(ccnt, port, scnt, gcnt))
        #print "calcuated %s" % (len(hwconf.group_ports[0])*hwconf.port2slice[port] + hwconf.port2group[port])
        #print "port2slice %s, port2group %s" % (hwconf.port2slice[port], hwconf.port2group[port])
        #print "len(mac_clk) %s" % (len(mac_clk))
        #sys.stdout.flush()
        return mac_clk[ccnt]
    else:
        return mac_clk

def find_flank(before, after, clk_slow, clk_fast, rstn, divisor, cnt=None, name=""):
    #
    #flank is high for one cycle of the fast clock, before the positive edge on the slow clock
    #
    div_w = (divisor).bit_length()
    print("Instanciated find_flank %s with divisor=%s, and cnt_w=%s" % (name, divisor, 0 if cnt==None else len(cnt)))
    assert divisor > 1, "ERROR! find_flank %s instanciated with divisor=%s < 2" % (name, divisor)
    
    cnt_reg = Signal(intbv(0)[div_w:])
    toggle1 = Signal(modbv(0)[1:0])
    toggle2 = Signal(modbv(0)[1:0])
    after_comb   = Signal(modbv(0)[1:0])
    start   = Signal(modbv(0)[1:0])
    @always(clk_slow.posedge, rstn.negedge)
    def toggle():
        if rstn==0:
            toggle1.next = 0
        else:
            toggle1.next = not toggle1

    @always(clk_fast.posedge, rstn.negedge)
    def serialize():
        if rstn==0:
            toggle2.next = 0
        else:
            toggle2.next = toggle1
            
    # When after_comb is high we are in the first cycle after the positive edge of clk
    @always_comb
    def findedge():
        after_comb.next = toggle1^toggle2

    flank_nr = 1
    @always(clk_fast.posedge, rstn.negedge)
    def count():
        if rstn==0:
            cnt_reg.next = 0
            start.next = 0
            before.next = 0
            after.next = 0
        else:
            before.next = 0                
            after.next = 0                            
            if start==1 and cnt_reg==divisor-2:
                before.next = 1                
            if start==1 and cnt_reg==divisor-1:
                after.next = 1                
            if after_comb==1:
                cnt_reg.next = flank_nr
                start.next = 1
            elif cnt_reg<divisor-1:
                cnt_reg.next = cnt_reg + 1
            elif start==1:
                cnt_reg.next = 0
                
    if cnt!=None:
        @always_comb
        def drivecnt():
            if start==1:
                cnt.next = cnt_reg
            else:
                cnt.next = 0
                
                
    return instances()

def clock_divider(master_clk, divisor, clk, rstn, name):
    print("TB: Creating clock with divisor %s" % divisor)
    cnt = Signal(intbv(0, min=0, max=divisor))

    mask = Signal(intbv(0)[1:])
    if divisor==1:
        logic_high = 1
        zPassmask = pass_through(logic_high, mask, name=name+".zPassmask")
    else:
        
        @instance
        def resetmclk():
            cnt.next = divisor-1
            yield delay(1)

        @always(master_clk.posedge) # PALINT no_rstn
        def createclkcnt1():
            if cnt==divisor-1:
                cnt.next = 0
            else:
                cnt.next = cnt + 1

        @always(master_clk.negedge) # PALINT no_rstn
        def createclkmask():
            if cnt==0:
                mask.next = 1
            else:
                mask.next = 0
                
    @always_comb
    def gatemclk():
        clk.next = master_clk & mask

    return instances()

# This was a try to make the sychronous clocks work in myhdlsim
def clock_divider_behavioral(master_clk, divisor, clk, rstn, name):
    print("TB: Creating clocks with divisors %s" % divisor)
    nr_of_clocks = len(clk)
    mask = Signal(intbv(0)[nr_of_clocks:])
    cnt = [ Signal(intbv(0, min=0, max=max(divisor))) for _ in range(nr_of_clocks) ]

    assert min(divisor) > 1, "ERROR! The behavioral clock generator needs all derived clocks to be slower than the master_clock"
    
    div = tuple(divisor)
    if max(divisor)==1:
        logic_high = 1
        zPassmask = []
        for i in range(nr_of_clocks):
            zPassmask = pass_through(master_clk, clk[i], name=name+".zPassmask%s"%i)
    else:
        @instance
        def setstart1():
            start.next = 1
            yield delay(1)
            
        start = Signal(intbv(0)[1:])
        @always(master_clk.posedge, rstn.negedge)
        def createclkcnt2():
            if rstn == 0 and start==1:
                start.next = 0
            for i in range(nr_of_clocks):
                if start==1:
                    cnt[i].next = 0
                else:
                    d = div[i]
                    if cnt[i]==d-1:
                        cnt[i].next = 0
                    else:
                        cnt[i].next = cnt[i] + 1

        @always(master_clk.posedge) # PALINT no_rstn
        def createclkmask2():
            for i in reversed(list(range(nr_of_clocks))):
                if cnt[i]==0:
                    clk[i].next = 1
                else:
                    clk[i].next = 0
                
    return instances()
    
def num2string(nr):
    """
    For numbers in the documentation. Words up to twelve, then numbers.
    """
    if nr < 13:
        from num2words import num2words
        return num2words(nr)
    else:
        return str(nr)


def assign_const(dout, value=0, bit_assign=0,name=""):
    """
    bit_assign=1:  Set 0/1 to each bit of dout
    bit_assign=0:  Set value to dout
    """
    width = len(dout)

    if value == 0:
        tied = Signal(intbv(0)[width:0])

    else:
        if bit_assign==1 and value==1:
            tied = Signal(intbv(2**width-1))[width:0]
        else:
            tied = Signal(intbv(value)[width:0])

    @always_comb
    def logicAssign():
        dout.next = tied
    return instances()


#-------------------------------------------------------------- print_test_summary
def print_test_summary(ts):
    lines = ts.split('\n')[1:]
    for l in lines:
        print(re.sub(r'^\s*#', '#', l))
    print()


def name2slice(name):
    try:
        s = re.search(r'switch\.\w*([0-9]+)', name).group(1)
    except:
        return None
    return int(s)

def slice_stable_Random(hwconf, name):
    if len(hwconf.slice_random) == 0:
        for i in range(hwconf.nr_of_pb+1):
            hwconf.slice_random.append(random.Random(hwconf.slice_random_seed))

    s = name2slice(name)
    if s != None:
        return hwconf.slice_random[name2slice(name)]
    else:
        return hwconf.slice_random[hwconf.nr_of_pb]
def slice_stable_randrange(hwconf, name):
    return slice_stable_Random(hwconf, name).randrange
def slice_stable_randint(hwconf, name):
    return slice_stable_Random(hwconf, name).randint

def armpkt(pkt):
    npkt = pkt.copy()
    if not 'length_in_bytes' in pkt:
        npkt['length_in_bytes']=pkt['length']
    if not 'delBytes' in pkt:
        npkt['delBytes']={}
    if not 'setBytes' in pkt:
        npkt['setBytes']={}
    if not 'addBytes' in pkt:
        npkt['addBytes']={}
    if not 'seed' in pkt:
        npkt['seed']=0
    return npkt


def pktprint(pkt, pkt2=None, desc="pkt1 ; pkt2", highlight_diff=True, only_diff=False):
    chunk = 8
    first = 1
    if pkt is None:
        return

    try:
        pkt1_data = pkt['data']
    except:
        pkt1_data = list(byteStream(**pkt))
    #print "len1", len(pkt1_data)
    max_data = len(pkt1_data)
    if pkt2:
        try:
            pkt2_data = pkt2['data']
        except:
            pkt2_data = list(byteStream(**pkt2))
        max_data = max(max_data, len(pkt2_data))
        #print "len2", len(pkt2_data)
    if pkt2 is None:
        print("{", end=' ')
    else:
        print()
        print(desc)
    c = 0
    tmp = 0
    tmp2 = 0
    if pkt2 is None:
        print("data: [", end=' ')
    f2=1
    for i in range(max_data):
        if i%chunk == 0:
            if len(pkt1_data)>i:
               tmp = pkt1_data[i]
            if pkt2 is not None:
                if len(pkt2_data)>i:
                    tmp2 = pkt2_data[i]

        else:
            if len(pkt1_data)>i:
               tmp = (pkt1_data[i]<<((i%chunk)*8)) | tmp
            if pkt2 is not None:
                if len(pkt2_data)>i:
                    tmp2 = (pkt2_data[i]<<((i%chunk)*8)) | tmp2
        if i == max_data-1 or i%chunk == chunk-1:
            if pkt2 is None:
                if not f2:
                    print(", ", end=' ')
                else:
                    f2=0
                print('{}:{:016x}'.format((i//chunk)*chunk, tmp), end=' ')
            else:
                if not only_diff or tmp!=tmp2:
                    if len(pkt1_data)>((i//chunk)*8):
                        tmp1_str = '{:016x}'.format(tmp)
                    else:
                        tmp1_str = "----------------"
                    if len(pkt2_data)>((i//chunk)*8):
                        tmp2_str = '{:016x}'.format(tmp2)
                    else:
                        tmp2_str = "----------------"
                    if highlight_diff:
                        tmp1_format = ""
                        tmp2_format = ""
                        for x in range(len(tmp2_str)):
                            try:
                                eq = tmp1_str[x] == tmp2_str[x]
                            except:
                                eq = False
                            if eq:
                                tmp2_format += tmp2_str[x]
                                tmp1_format += tmp1_str[x]
                            else:
                                tmp2_format += "\033[;7m"+tmp2_str[x]+"\033[0;0m"
                                tmp1_format += "\033[;7m"+tmp1_str[x]+"\033[0;0m"
                        tmp1_str = tmp1_format
                        tmp2_str = tmp2_format
                    print('data{}: {} ; {}'.format((i//chunk)*chunk, tmp1_str, tmp2_str))

    if pkt2 is None:
        print("]", end=' ')

    for k in pkt:
        if pkt2 is not None:
            print()
        else:
            if not first:
                print(", ", end=' ')
            else:
                first = 0
        if not k in ('data', 'setBytes'):
            print("%s: %s" % (k, pkt[k]), end=' ')
            if pkt2 is not None:
                if k in pkt2:
                    print("; %s" % pkt2[k], end=' ')
                else:
                    print("; None", end=' ')
    if pkt2 is None:
        print("}")
    print(flush=True)
