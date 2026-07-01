#!/usr/bin/env python3
#
# (C) Packet Architects AB.
#
# ---------------- OBSERVE -------------------
#
# You need to "source tools/tool-config.sh"
# before running this script.
#
# --------------------------------------------
#
# Read in a yml file and create a C-API
#

from tools.ptf.ptf import *
setup_flexsw()
import sys
import yaml
import math
import argparse
import os
import importlib
import shutil
import re
from scapy.all import *
from string import Template
import string
import api.genBackend.emptyMemBackend_16
import api.genBackend.emptyMemBackend_32
import api.genBackend.emptyMemBackend_64
import api.genBackend.nb_32
import api.genBackend.nb_64
import api.genBackend.apb_32
import api.genBackend.apb_64
import pprint
from textwrap import dedent, indent
from collections import defaultdict

dbg=True
debug=False
debug_bit_packing=False

# All registers are transfeered to this structure
# Needs to be done to know how many slices there are
# and the addresses for each slice.
gRegs = {}
globalPorts = 0
globalQueues = 0

READ = 1
WRITE = 0
#
# Functions
#

# ----------------------------------------------------------- calcSize
def calcSize(start,end):
    return end-start+1

# ----------------------------------------------------------- createComment
def createComment(txt,size):
    txt_list = txt.split()
    cmt = "  // "
    cnt = 0
    for word in txt_list:
        if word != "\n" and word != " " and word !="\t":
            cnt += len(word)+1
            if cnt>size:
                cnt = len(word)+1
                cmt += "\n  // "
            cmt += " "+word

    if len(txt_list)!=0:
        cmt += "\n"
    else:
        cmt = ""

    return cmt

# ----------------------------------------------------------- findHighest
def findHighest(fields):
    highest = 0
    size = 0
    #print "start:"
    for field in fields:
        tmpSize = calcSize(field['Start bit'],field['End bit'])
        size += tmpSize
        #print "field:"+field['Name']+ " size:"+str(size)
    return size-1

# ----------------------------------------------------------- getField
def getField(fields,currBit,startOrEnd):
    #print "getField:"+str(currBit)+ " SoE:"+str(startOrEnd)

    for field in fields:
        if startOrEnd:
            if field['Start bit']==currBit:
                return field
        else:
            if field['End bit']==currBit:
                return field

    #print "Return None: getField:"+str(currBit)+" StartOrEnd:"+str(startOrEnd) + " Fields:"+str(fields)
    return None

def max_field_name( fields ):
    maxw = max([ len( convertToLegal( fld['Name'] ) ) for fld in fields ])
    return maxw

# ----------------------------------------------------------- genStruct
def genStruct(name,fields,backend,c_code,definedUintSize,use_bitfields,field_widths):

    c = '\n// ------- Struct declaration for register: {0} --------- \n'.format(name)
    c += "typedef struct {\n"
    py =  "class "+ name + "(ctypes.Structure):\n"
    py += "    _slots_ = ()\n"
    py += "    _fields_ =[\n"
    sname = name

    max_name_width = max_field_name( fields )
    # This is hardcoded since all frontend structs will be based on the longest
    # possible uint.
    wordsize = definedUintSize;

    if wordsize == 64:
        pyWordSize = "ctypes.c_uint64"
    if wordsize == 32:
        pyWordSize = "ctypes.c_uint32"

    if wordsize != 32 and wordsize != 64:
        print("ERROR: a undefined frontend word size for the API has been requested. Not 32 or 64 rathern = "+str(definedUintSize)+" \n")
        exit(-1)

    currBit= findHighest(fields)
    startOrEnd = False

    field = getField(fields,currBit,startOrEnd)

    if field==None:
        print("Struct Code: currBit:"+str(currBit)+" fields:"+str(fields))
        assert False, "ERROR: Something is wrong!"

    while field!= None:
        name = convertToLegal(field['Name'])
        c += '\n'
        c += '  // field: {0}\n'.format(name)
        c += createComment(field['Description'],77)
        size = calcSize(field['Start bit'],field['End bit'])

        # Collect the widths for later use. Avoids doing the same calculation
        # in multiple places.
        field_widths[name] = size

        currBit -= size

        if size<=wordsize:
            bitw    = ":{0:3d}".format(size) if use_bitfields else ""
            py_bitw = ",{0}".format(size) if use_bitfields else ""
            c += "  uint{0}_t {1:{nwidth}s}{2};".format( wordsize, name, bitw, nwidth=max_name_width )
            py+= "              ( \"{0}\",{1}{2} ),\n".format(name,pyWordSize,py_bitw)
            if size>1:
                c+= " // bit {0} to {1}\n".format(field['Start bit'],field['End bit'])
            else:
                c+= " // bit {0}\n".format(field['Start bit'])
        else:
            sizeleft = size
            if (size % 8)  ==0:
                charsize = size // 8
            else:
                charsize = (size+7) // 8

            c+= "  uint8_t "+name+"["+str(charsize)+"]"
            c+= "; // bit "+str(field['Start bit'])+" to "+str(field['End bit'])+"\n"
            py+= "              ( \""+name+"\",ctypes.c_uint8 * "+str(charsize)+" ),\n"

        field = getField(fields,currBit,startOrEnd)
    c += "} t_"+sname+";\n"
    py = py[0:-2]
    py+= "    ]\n"
    if c_code:
        return c
    else:
        return py

# ----------------------------------------------------------- genReadUint8FieldCode
def genReadUint8FieldCode(fieldName,currId,size,entryBit,uintsize,backend):
    fldBytes = (size+7) // 8
    currEntryBit = entryBit
    currFieldSize = size
    tmp8 = False;
    c = "  // ----------------- genUint8FieldCode --------------------\n"

    start = 0
    end = fldBytes
    step = 1

    for i in range(start,end,step):
        bitDecr = 8
        #if currFieldSize %8 != 0:
        #    bitDecr = currFieldSize % 8
        if debug_bit_packing:
            c += "  // Byte:"+str(i)+"\n"
            c += "  // Bit Decrement:"+str(bitDecr)+"\n"
            c += "  // Curr Field Size:"+str(currFieldSize)+"\n"
            c += "  // Curr Entry Bit:"+str(currEntryBit)+"\n"

        if currFieldSize<bitDecr:
            bitDecr = currFieldSize

        currFieldSize -= bitDecr


        prevCurrId = currEntryBit // uintsize

        currEntryBit += bitDecr

        nextCurrId = currEntryBit // uintsize

        if debug_bit_packing:
            c += "  // Prev Curr Entry Id:"+str(prevCurrId)+"\n"
            c += "  // Next Curr Entry Id:"+str(nextCurrId)+"\n"
        if prevCurrId != nextCurrId:
            currId +=1

            beforeBits = bitDecr - (currEntryBit-bitDecr) % bitDecr
            afterBits = (currEntryBit) % bitDecr

            if debug_bit_packing:
                c += "  // beforeBits: "+str(beforeBits)+"\n"
                c += "  // afterBits: "+str(afterBits)+"\n"

                c += "  // Bits on currId:"+str(currId-1)+" bits:"+str(beforeBits)+"\n"
                c += "  // Bits on currId:"+str(currId)+" bits:"+str(afterBits)+"\n"
            beforeBitsPwr2 = str(hex((2**beforeBits)-1)).rstrip("L")
            afterBitsPwr2 = str(hex((2**afterBits)-1)).rstrip("L")
            if debug_bit_packing:
                c += "  // New currId: "+str(currId)+"\n"
            lc_tmp8 = False

            if beforeBits > 0:

                # Read
                if debug_bit_packing: c += "  // Before: \n"
                c += "  data->"+fieldName+"["+str(i)+"] = entry"+str(currId-1)+" & "+beforeBitsPwr2+";\n"
                c += "  entry"+str(currId-1)+" = entry"+str(currId-1)+" >> "+str(beforeBits)+";\n"
            else:
                if debug_bit_packing: c += "  // Before: is Zero no code!\n"
            if afterBits > 0:
                # Read
                if debug_bit_packing: c += "  // After: \n"
                tmp8 = True;
                c += "  tmp8 = entry"+str(currId)+" & "+afterBitsPwr2+";\n"
                c += "  data->"+fieldName+"["+str(i)+"] |= tmp8 <<"+str(beforeBits)+";\n"
                c += "  entry"+str(currId)+" = entry"+str(currId)+" >> "+str(afterBits)+";\n"
            else:
                if debug_bit_packing: c += "  // After: is Zero no code!\n"
        else:
            if debug_bit_packing: c += "  // currId: "+str(currId)+"\n"
            bitDecrPwr2 = str(hex((2**bitDecr)-1)).rstrip("L")
            c += "  data->"+fieldName+"["+str(i)+"] = entry"+str(currId)+" & "+bitDecrPwr2+";\n"
            c += "  entry"+str(currId)+" = entry"+str(currId)+" >> "+str(bitDecr)+";\n"
    c+= "   // ----------------------- end ------------------------ \n\n"

    return [currId,c,tmp8]

# ----------------------------------------------------------- genWriteUint8FieldCode
def genWriteUint8FieldCode(fieldName,currId,size,entryBit,shift,uintsize,backend):
    fldBytes = (size+7) // 8
    currFieldSize = size
    currEntryBit = entryBit
    tmp8 = False;
    c =  "  // ----------------- genWriteUint8FieldCode --------------------\n"

    start = 0
    end = fldBytes
    step = 1

    for i in range(start,end,step):
        bitDecr = 8
        if debug_bit_packing:
            c += "  // Byte:"+str(i)+"\n"
            c += "  // Bit Decrement:"+str(bitDecr)+"\n"
            c += "  // Curr Field Size:"+str(currFieldSize)+"\n"
            c += "  // Curr Entry Bit:"+str(currEntryBit)+"\n"

        if currFieldSize<bitDecr:
            bitDecr = currFieldSize

        currFieldSize -= bitDecr

        prevCurrId = currEntryBit//uintsize

        currEntryBit += bitDecr

        nextCurrId = currEntryBit//uintsize

        if debug_bit_packing:
            c += "  // Prev Curr Entry Id:"+str(prevCurrId)+"\n"
            c += "  // Next Curr Entry Id:"+str(nextCurrId)+"\n"
        if prevCurrId != nextCurrId:
            currId +=1

            beforeBits = bitDecr - (currEntryBit-bitDecr) % bitDecr
            afterBits = (currEntryBit) % bitDecr
            if debug_bit_packing:
                c += "  // beforeBits: "+str(beforeBits)+"\n"
                c += "  // afterBits: "+str(afterBits)+"\n"
                c += "  // Bits on currId:"+str(currId-1)+" bits:"+str(beforeBits)+"\n"
                c += "  // Bits on currId:"+str(currId)+" bits:"+str(afterBits)+"\n"
            beforeBitsPwr2 = str(hex((2**beforeBits)-1)).rstrip("L")
            afterBitsPwr2 = str(hex((2**afterBits)-1)).rstrip("L")
            if debug_bit_packing:
                c += "  // New currId: "+str(currId)+"\n"
            lc_tmp8 = False

            if beforeBits > 0:
                # Write
                if debug_bit_packing: c += "  // Before: \n"
                c += "  tmp = data->"+fieldName+"["+str(i)+"] & "+beforeBitsPwr2+";\n"
                c += "  tmp = tmp << "+str(shift)+";\n"
                c += "  entry"+str(currId-1)+" |= tmp;\n"
                c += "  tmp = 0;\n"
            else:
                if debug_bit_packing: c += "  // Before: is Zero no code!\n"
            if afterBits > 0:
                # Write
                if debug_bit_packing: c += "  // After: \n"
                c += "  tmp = data->"+fieldName+"["+str(i)+"];\n"
                c += "  tmp = tmp >> "+str(uintsize-shift)+";\n"
                c += "  entry"+str(currId)+" |= tmp;\n"
                c += "  tmp = 0;\n"
            else:
                if debug_bit_packing: c += "  // After: is Zero no code!\n"
        else:
            if debug_bit_packing: c += "  // currId: "+str(currId)+"\n"
            bitDecrPwr2 = str(hex((2**bitDecr)-1)).rstrip("L")
            c += "  tmp = data->"+fieldName+"["+str(i)+"] & "+bitDecrPwr2+";\n"
            c += "  tmp = tmp << "+str(shift)+";\n"
            c += "  entry"+str(currId)+" |= tmp;\n"
            c += "  tmp = 0;\n"

        shift += bitDecr
        if shift>=uintsize:
            shift -= uintsize

    c+= "   // ----------------------- end ------------------------ \n\n"

    return [currId,c,tmp8]


# ----------------------------------------------------------- genReadFieldCode
def genReadFieldCode(fieldName,currId,size,uintSize):
    entryName = "entry";
    c = ''
    if debug_bit_packing:
        c += "  // fieldName:"+fieldName+"\n"
        c += "  // currId:"+str(currId)+"\n"
        c += "  // size:"+str(size)+"\n"
    c += "  data->"+fieldName+" = "+entryName+str(currId)+" & "
    c += str(hex(2**size-1).rstrip("L"))+";\n"
    if size!=uintSize:
        c += '  ' + entryName+str(currId)+" = "+entryName+str(currId)+" >> "+str(size)+";\n"

    return c;

# ----------------------------------------------------------- genWriteFieldCode
def genWriteFieldCode(fieldName,currId,shift,uintSize):
    c = ''
    if debug_bit_packing: c += "  //genWriteFieldCode\n"

    if shift!=0 and shift!=uintSize:
        c +=  "  tmp = data->"+fieldName+";\n"
        c +=  "  tmp = tmp << "+str(shift)+";\n"
    else:
        c +=  "  tmp = data->"+fieldName+";\n"
    c +=  "  entry"+str(currId)+" |= tmp;\n"
    c += "  tmp = 0;\n"

    return c;

# ----------------------------------------------------------- genEdgeReadFieldCode
def genEdgeReadFieldCode(fieldName,currId,size,left,uintsize):
    if debug_bit_packing:
        c  = "  // Edge fieldName:"+fieldName+"\n"
        c += "  // currId:"+str(currId)+"\n"
        c += "  // size:"+str(size)+"\n"
        c += "  // left:"+str(left)+"\n"
    else:
        c = ''
    c += "  data->"+fieldName+" = entry"+str(currId)+" & "+str(hex(2**left-1).rstrip("L"))+";\n"
    if (size-left)>0:
        c += "  data->"+fieldName+" |= (entry"+str(currId+1)+" & "
        c += str(hex((2**(size-left)-1))).rstrip("L")+") <<"+str(left)+";\n"

        if (size-left)!=uintsize:
            c += "  entry"+str(currId+1)+" = entry"+str(currId+1)+" >> "+str(size-left)+";\n"
    return c;

# ----------------------------------------------------------- genEdgeWriteFieldCode
def genEdgeWriteFieldCode(fieldName,currId,size,left,uintsize):
    if debug_bit_packing:
        c  = "  // Edge fieldName:"+fieldName+"\n"
        c += "  // currId:"+str(currId)+"\n"
        c += "  // size:"+str(size)+"\n"
        c += "  // left:"+str(left)+"\n"
        c += "  // Releft:"+str(size-left)+"\n"
    else:
        c = ''
    c += "  tmp = data->"+fieldName+";\n"
    c += "  tmp = tmp <<"+str(uintsize-left)+";\n"
    c += "  entry"+str(currId)+" |= tmp;\n"
    c += "  tmp = data->"+fieldName+";\n"
    if left != uintsize and size-left!=0:
        c += "  tmp = tmp >>"+str(left)+";\n"
        c += "  entry"+str(currId+1)+" |= tmp;\n"
    c += "  tmp = 0;\n"
    return c;


# ----------------------------------------------------------- genFieldReadCode
def genFieldReadCode(fields,width,backend,uintSize,definedUintSize):
    c = ''
    if debug_bit_packing: c += "  // Width is:"+str(width)+"\n"
    bit = 0
    currId = 0
    entries = (width+definedUintSize-1) // definedUintSize
    entries_rd = (width+uintSize-1) // uintSize
    tmp8 = False
    if debug_bit_packing: c += "  // Entries: "+str(entries)+"\n"
    if debug_bit_packing: c += "  // Entries read: "+str(entries_rd)+"\n"

    for i in range(entries):
        c += "  uint"+str(definedUintSize)+"_t entry"+str(i)+"=0;\n"

    if uintSize != definedUintSize:
        uintSizePerDefined = definedUintSize / uintSize
        for i in range(entries_rd):
            c+= "  uint"+str(uintSize)+"_t entry_rd"+str(i)+"=0;\n"
            c+= "  uint"+str(definedUintSize)+"_t entry_rd"+str(i)+str(definedUintSize)+"=0;\n"

    if entries_rd>1:
        for i in range(entries_rd):
            if i==0:
                readout=0 # first read access must be Direct
            else:
                readout=1 # following reads should be Accumulator
            if uintSize != definedUintSize:
                c += "  "+backend.c_read("address+"+str(i),readout,"entry_rd"+str(i))
                c += "  entry_rd"+str(i)+str(definedUintSize)+"= entry_rd"+str(i)+";\n"
            else:
                c += "  "+backend.c_read("address+"+str(i),readout,"entry"+str(i))
    else:
        if uintSize != definedUintSize:
            c += "  "+backend.c_read("address",0,"entry_rd"+str(i))
        else:
            c += "  "+backend.c_read("address",0,"entry"+str(i))

    if definedUintSize > uintSize and definedUintSize!=uintSize:
        shift = 0
        realEntry = 0
        for i in range(entries_rd):
            c+= "  entry_rd"+str(i)+str(definedUintSize)+" = entry_rd"+str(i)+";\n"
            c+= "  entry"+str(realEntry)+" |= entry_rd"+str(i)+str(definedUintSize)+"<<"+str(shift)+";\n"
            shift = shift + uintSize
            if shift>=definedUintSize:
                shift = 0
                realEntry = realEntry +1

    currBit = 0
    startOrEnd = True

    field = getField(fields,currBit,startOrEnd)

    if field==None:
        print("Read Code: currBit:"+str(currBit)+" fields:"+str(fields))
        assert False, "ERROR: Something is wrong!"

    entryBit = 0
    while field != None:
        size = calcSize(field['Start bit'],field['End bit'])
        realName = convertToLegal(field['Name'])

        currBitBefore = currBit

        currBit += size
        times = currBitBefore // definedUintSize
        left = definedUintSize*(times+1)-currBitBefore

        #print "CurrBit:"+str(currBit)
        if size>definedUintSize:
            # A field is actually larger then the wordsize!
            [currId,c_code,tmp8_new] = genReadUint8FieldCode(realName,currId,size,entryBit,definedUintSize,backend)
            if tmp8_new:
                tmp8 = True

            c += c_code
        else:
            if (currBit//definedUintSize) == (currBitBefore//definedUintSize):
                c += genReadFieldCode(realName,currId,size,definedUintSize)
            else:
                c += genEdgeReadFieldCode(realName,currId,size,left,definedUintSize)
            # Handle fields which end on a edge
            if (currBit // definedUintSize) != (currBitBefore//definedUintSize):
                currId = currId +1
                if debug_bit_packing:  c+= "  // Increate currId \n"

        if debug_bit_packing: c += "  // bit: "+str(currBit)+" left:"+str(left)+" CurrId:"+str(currId)+"\n"
        entryBit += size
        field = getField(fields,currBit,startOrEnd)

    if tmp8:
        tmp8_code = "  uint8_t tmp8;\n"
    else:
        tmp8_code = ""

    return tmp8_code+c

# ----------------------------------------------------------- genFieldCheckCode
def genFieldCheckCode(fields,definedUintSize):
    c = ""
    for field in fields:
        fname = convertToLegal(field['Name'])
        width = calcSize(field['Start bit'],field['End bit'])
        if width < definedUintSize:
            c += f"  PA_ERROR_FIELD_CHECK( data->{fname}, {width} )\n"
    return c 

# ----------------------------------------------------------- genFieldWriteCode
def genFieldWriteCode(fields,width,backend,uintSize,definedUintSize):
    c = "  uint"+str(definedUintSize)+"_t tmp = 0;\n"
    tmp8 = False

    bit = 0
    currId = 0
    entries = (width+definedUintSize-1) // definedUintSize
    entries_wr = (width+uintSize-1) // uintSize
    for i in range(entries):
        c += "  uint"+str(definedUintSize)+"_t entry"+str(i)+"=0;\n"

    if uintSize != definedUintSize:
        for i in range(entries_wr):
            c+= "  uint"+str(uintSize)+"_t entry_wr"+str(i)+"=0;\n"
            c+= "  uint"+str(definedUintSize)+"_t entry_wr"+str(i)+str(definedUintSize)+"=0;\n"

    currBit = 0
    startOrEnd = True

    field = getField(fields,currBit,startOrEnd)

    if field==None:
        print("Write Code: currBit:"+str(currBit)+" fields:"+str(fields))
        assert False, "ERROR: Something is wrong!"

    entryBit = 0
    shift = 0
    while field != None:
        size = calcSize(field['Start bit'],field['End bit'])
        realName = convertToLegal(field['Name'])

        currBitBefore = currBit

        currBit += size
        times = currBitBefore // definedUintSize
        left = definedUintSize*(times+1)-currBitBefore
        if debug_bit_packing: c += "  // bit: "+str(currBit)+" left:"+str(left)+" CurrId:"+str(currId)+"\n"

        #print "CurrBit:"+str(currBit)
        if size>definedUintSize:
            # A field is actually larger then the wordsize!
            [currId,c_code,tmp8_new] = genWriteUint8FieldCode(realName,currId,size,entryBit,shift,definedUintSize,backend)
            if tmp8_new:
                tmp8 = True
            c += c_code
        else:
            if (currBit // definedUintSize) == (currBitBefore // definedUintSize):
                c += genWriteFieldCode(realName,currId,shift,definedUintSize)
            else:
                c += genEdgeWriteFieldCode(realName,currId,size,left,definedUintSize)
            # Handle fields which end on a edge
            if (currBit // definedUintSize) != (currBitBefore//definedUintSize):
                currId = currId +1
                if debug_bit_packing: c+= "  // Increate currId \n"

        entryBit += size
        field = getField(fields,currBit,startOrEnd)

        shift +=size
        while(shift>=definedUintSize):
            shift -=definedUintSize

    if uintSize == definedUintSize:
        if entries>1:
            entry_ptr = (entries-1)
            for i in range(entries):
                if i==(entries-1):
                    writeout=0
                else:
                    writeout=1
                c += "  "+backend.c_write("address+"+str(i),writeout,"entry"+str(i))
        else:
            c += "  "+backend.c_write("address+"+str(i),0,"entry"+str(i))
    else:
        shift = 0
        realEntry = 0
        for i in range(entries_wr):
            if shift==0:
                c += "  entry_wr"+str(i)+str(definedUintSize)+" = entry"+str(realEntry)+";\n"
            else:
                c += "  entry_wr"+str(i)+str(definedUintSize)+" = entry"+str(realEntry)+">>"+str(shift)+";\n"
            c += "  entry_wr"+str(i)+" = (uint"+str(uintSize)+"_t) entry_wr"+str(i)+str(definedUintSize)+";\n"
            shift = shift + uintSize
            if shift>=definedUintSize:
                shift = 0
                realEntry = realEntry +1
        if entries_wr>1:
            entry_ptr = (entries_wr-1)
            for i in range(entries_wr):
                if i==(entries_wr-1):
                    writeout=0
                else:
                    writeout=1
                c += "  "+backend.c_write("address+"+str(i),writeout,"entry_wr"+str(i))
        else:
            c += "  "+backend.c_write("address+"+str(i),0,"entry_wr"+str(i))

    if tmp8:
        tmp8_code = "uint8_t tmp8;\n"
    else:
        tmp8_code = ""

    return tmp8_code + c


# ----------------------------------------------------------- genAddrCode
def genAddrCode(name,hasSlice,hasPort,hasSet,address,entries,sizes,definedUintSize):
    c = ''
    if hasPort or hasSlice or hasSet:
        c +="  int hit = 0;\n"
    c +="  uint"+str(definedUintSize)+"_t address;\n"

    for addr_entry in address:
        [addr,sliceId,portId,setId] = addr_entry
        prev = False
        cmp = ""

        if hasPort:
            cmp = " port=="+str(portId)
            prev = True

        if hasSet:
            if prev:
                cmp = cmp + " && set=="+str(setId)
            else:
                cmp = " set=="+str(setId)
            prev = True

        if hasSlice:
            if prev:
                cmp = cmp + "&& slice=="+str(sliceId)
            else:
                cmp = " slice=="+str(sliceId)
            prev = True

        #const_name = addrConstName(name, addr_entry, hasSlice, hasPort, hasSet)
        # These regs are created independently and may not have consecutive
        # addresses. To be able to access them with an index we need to
        # map index to address.
        if hasPort or hasSet or hasSlice:
            #c += f"  if ({cmp}) {{ address = "+str(addr)+"; hit = 1;}}\n"
            c += f"  if ({cmp}) {{ address = {addr}; hit = 1; }}\n"

    if not hasSlice and not hasPort and not hasSet:
        #const_name = addrConstName(name, address[0], hasSlice, hasPort, hasSet)
        c += f"  address = {address[0][0]};\n"

        #if isinstance(entries,list):
        #    for slice in range(len(entries)):
        #        c += f"  PA_ERROR_SLICE_IDX_CHECK( slice, idx, {slice}, {entries[slice]} )\n"
        #elif entries > 0:
        #    c += f"  PA_ERROR_IDX_CHECK( idx, {entries} )\n"

    if isinstance(entries,list) or entries > 0:
        c += f"  address += idx*{sizes};\n"

        #if hasPort or hasSlice or hasSet:
        #    c += "  PA_ERROR_CHECK\n"

    return c

# ----------------------------------------------------------- genCReadRegFunction
def genCReadRegFunction(name,fields,address,
                        parts,width,backend,hasSlice,hasPort,hasSet,definedUintSize):
    #c = "// "+name+" read single register function.\n\n"
    #h = "// "+name+" read single register function.\n\n"
    c = ""
    h = ""
    wordsize = backend.getWordSize();
    uint = "uint"+str(definedUintSize)+"_t"
    sliceLine = ""
    portLine = ""
    setLine = ""
    devPtr = ""
    if backend.isMemory():
        devPtr = uint+" *device_ptr,"
    if hasSlice:
        sliceLine =  uint+" slice,"
    if hasPort:
        portLine =  uint+" port,"
    if hasSet:
        setLine =  uint+" set,"

    if hasSlice > 0:
        fname = f"{name}_slice"
    else:
        fname = name
    
    #h += f"// regdata: {name} {gRegs[name]['Type']} slice:{hasSlice} {gRegs[name]['Statistics']}\n"

    params = devPtr+sliceLine+portLine+setLine
    c += f"PA_RETURN_TYPE rd_{fname}( {params} t_{name}* data ) {{\n"
    h += f"extern PA_RETURN_TYPE rd_{fname}( {params} t_{name}* data );\n" # }}

    c += genAddrCode(name,hasSlice,hasPort,hasSet,address,0,1,definedUintSize)

    c += genFieldReadCode(fields,width,backend,wordsize,definedUintSize)
    c += genPrintCall(name,hasSlice,hasPort,hasSet,hasIndex=False,rw='read')

    c += "  PA_RETURN_OK\n"
    c += "}\n\n"

    if hasSlice > 0: # hasSlice is actually number of slices
        # this reads from first slice
        params = devPtr+portLine+setLine
        call_params = (( "device_ptr, " if backend.isMemory() else "" ) +
                       ( "0, ") +
                       ( "port, " if hasPort else "" ) +
                       ( "set, " if hasSet else "" ))

        h += f"extern PA_RETURN_TYPE rd_{name}( {params} t_{name}* data );\n"
        c += dedent(f"""
            PA_RETURN_TYPE rd_{name}( {params} t_{name}* data ) {{
              rd_{name}_slice( {call_params} data );
              PA_RETURN_OK
            }}

            """)

    return (h,c)

def genPrintCall(name,hasSlice,hasPort,hasSet,hasIndex,rw):
    c = '#ifdef FLEXSW_DEBUG\n'
    c += '  if (flexsw_print_enable) {'
    c += f'    printf("{rw} {name}: ");\n'
    if hasSlice:
        c += f'    printf("|%0ld",slice);\n'
    if hasPort:
        c += f'    printf("|%0ld",port);\n'
    if hasSet:
        c += f'    printf("|%0ld",set);\n'
    if hasIndex:
        c += f'    printf("|%0ld",idx);\n'
    c += f'    printf("\\n");\n'
    c += f'    print_t_{name}( data );\n'
    c += '  }\n'
    c += '#endif\n\n'
    return c

def genPrintStruct(name, fields, definedUintSize, use_bitfields ):

    h = '#ifdef FLEXSW_DEBUG\n'
    h += f"extern void print_t_{name}( t_{name}* s );\n"
    h += '#endif\n'

    c = '#ifdef FLEXSW_DEBUG\n'
    c += f"void print_t_{name}( t_{name}* s ) {{\n" # }}
    max_width = max( [ len( convertToLegal(field['Name']) ) for field in fields ] )

    if max( [ calcSize(field['Start bit'],field['End bit']) for field in fields ] ) > definedUintSize:
      max_width += 4 # we have byte vectors so need room fox index

    for field in fields:
        fname = convertToLegal(field['Name'])
        bits = calcSize(field['Start bit'],field['End bit'])
        if bits > definedUintSize:
            nr_bytes = (bits + 7) // 8
            c += f'  for (int i=0; i<{nr_bytes}; i++)\n'
            c += f'    printf(" {fname:{max_width}s}[%2d] | 0x%02x\\n", i, s->{fname}[i]);\n'
        else:
            if bits <= 3:
                hspec = 'd' if use_bitfields else 'ld'
                c += f'  printf(" {fname:{max_width}s} | %1{hspec}\\n", s->{fname});\n'
            else:
                nibbles = (bits + 3) // 4
                hspec = 'lx' if bits > 32 or not use_bitfields else 'x'
                cast = '(uint64_t)' if bits > 32 and use_bitfields else ''
                c += f'  printf(" {fname:{max_width}s} | 0x%0{nibbles}{hspec}\\n", {cast}s->{fname});\n'
    c += "}\n"
    c += '#endif\n\n'
    return (h,c)

# ----------------------------------------------------------- genCWriteRegFunction
def genCWriteRegFunction(name,fields,address,
                         parts,width,backend,hasSlice,hasPort,hasSet,definedUintSize):
    #c = "// "+name+" write single register function.\n\n"
    #h = "// "+name+" write single register function.\n\n"
    c = ""
    h = ""
    uint = "uint"+str(definedUintSize)+"_t"
    wordsize = backend.getWordSize();
    sliceLine = ""
    portLine = ""
    setLine = ""
    devPtr = ""
    if backend.isMemory():
        devPtr = uint+" *device_ptr,"
    if hasSlice:
        sliceLine =  uint+" slice,"
    if hasPort:
        portLine =  uint+" port,"
    if hasSet:
        setLine =  uint+" set,"

    if hasSlice:
        fname = f"{name}_slice"
    else:
        fname = name

    params = devPtr+sliceLine+portLine+setLine
    c += f"PA_RETURN_TYPE wr_{fname}( {params} t_{name}* data ) {{\n"
    h += f"extern PA_RETURN_TYPE wr_{fname}( {params} t_{name}* data );\n"

    c += genPrintCall(name,hasSlice,hasPort,hasSet,hasIndex=False,rw='write')

    c += genAddrCode(name,hasSlice,hasPort,hasSet,address,0,1,definedUintSize)

    c += genFieldCheckCode(fields,definedUintSize)

    c += genFieldWriteCode(fields,width,backend,wordsize,definedUintSize)

    c += "  PA_RETURN_OK\n"
    c += "}\n\n"

    if hasSlice: # hasSlice is actually number of slices
        # this writes to all slices
        params = devPtr+portLine+setLine
        call_params = (( "device_ptr, " if backend.isMemory() else "" ) +
                       ( "slice, ") +
                       ( "port, " if hasPort else "" ) +
                       ( "set, " if hasSet else "" ))

        h += f"extern PA_RETURN_TYPE wr_{name}( {params} t_{name}* data );\n"
        c += dedent(f"""
            PA_RETURN_TYPE wr_{name}( {params} t_{name}* data ) {{
              for (int slice = 0; slice < {hasSlice} ; slice++ ) {{
                wr_{name}_slice( {call_params} data );
              }}
            PA_RETURN_OK
            }}

            """)

    return (h,c)

#
# Table  Access
#
# ----------------------------------------------------------- genCReadTabFunction
def genCReadTabFunction(name,fields,address,
                             parts,width,entries,backend,hasSlice,hasPort,hasSet,definedUintSize):
    c = ""
    h = ""
    wordsize = backend.getWordSize();
    sliceLine = ""
    portLine = ""
    setLine = ""
    uint = "uint"+str(definedUintSize)+"_t"
    devPtr = ""
    if backend.isMemory():
        devPtr = uint+" *device_ptr,"
    if hasSlice:
        sliceLine =  uint+" slice,"
    if hasPort:
        portLine =  uint+" port,"
    if hasSet:
        setLine =  uint+" set,"

    if hasSlice > 0:
        fname = f"{name}_slice"
    else:
        fname = name

    #h += f"// regdata: {name} {gRegs[name]['Type']} slice:{hasSlice} {gRegs[name]['Statistics']}\n"

    params = devPtr+sliceLine+portLine+setLine+" "+uint+" idx,"
    c += f"PA_RETURN_TYPE rd_{fname}( {params} t_{name}* data ) {{\n" # }}
    h += f"extern PA_RETURN_TYPE rd_{fname}( {params} t_{name}* data );\n"

    c += genAddrCode(name,hasSlice,hasPort,hasSet,address,entries,parts,definedUintSize)

    c += genFieldReadCode(fields,width,backend,wordsize,definedUintSize)
    c += genPrintCall(name,hasSlice,hasPort,hasSet,hasIndex=True,rw='read')

    c += "  PA_RETURN_OK\n"
    c += "}\n\n"


    if hasSlice > 0: # hasSlice is actually number of slices
        # this reads from first slice
        params = devPtr+portLine+setLine+" "+uint+" idx,"
        call_params = (( "device_ptr, " if backend.isMemory() else "" ) +
                       ( "0, ") +
                       ( "port, " if hasPort else "" ) +
                       ( "set, " if hasSet else "" ) +
                       ( "idx, " ))

        h += f"extern PA_RETURN_TYPE rd_{name}( {params} t_{name}* data );\n"
        c += dedent(f"""
            PA_RETURN_TYPE rd_{name}( {params} t_{name}* data ) {{
              rd_{name}_slice( {call_params} data );
              PA_RETURN_OK
            }}

            """)

    return (h,c)


# ----------------------------------------------------------- genCWriteTabFunction
def genCWriteTabFunction(name,fields,address,
                         parts,width,entries,backend,hasSlice,hasPort,hasSet,definedUintSize):
    c = ""
    h = ""
    wordsize = backend.getWordSize();
    uint = "uint"+str(definedUintSize)+"_t"
    sliceLine = ""
    portLine = ""
    setLine = ""
    devPtr = ""
    if backend.isMemory():
        devPtr = uint+" *device_ptr,"

    if hasSlice > 0:
        sliceLine =  uint+" slice,"
    if hasPort:
        portLine =  uint+" port,"
    if hasSet:
        setLine =  uint+" set,"

    if hasSlice > 0:
        fname = f"{name}_slice"
    else:
        fname = name

    params = devPtr+sliceLine+portLine+setLine+" "+uint+" idx,"
    c += f"PA_RETURN_TYPE wr_{fname}( {params} t_{name}* data ) {{\n"
    h += f"extern PA_RETURN_TYPE wr_{fname}( {params} t_{name}* data );\n"

    c += genPrintCall(name,hasSlice,hasPort,hasSet,hasIndex=True,rw='write')

    c += genAddrCode(name,hasSlice,hasPort,hasSet,address,entries,parts,definedUintSize)

    c += genFieldWriteCode(fields,width,backend,wordsize,definedUintSize)

    c += "  PA_RETURN_OK\n"

    c += "}\n\n"

    if hasSlice > 0: # hasSlice is actually number of slices
        # this writes to all slices
        params = devPtr+portLine+setLine+" "+uint+" idx,"
        call_params = (( "device_ptr, " if backend.isMemory() else "" ) +
                       ( "slice, ") +
                       ( "port, " if hasPort else "" ) +
                       ( "set, " if hasSet else "" ) +
                       ( "idx, " ))

        h += f"extern PA_RETURN_TYPE wr_{name}( {params} t_{name}* data );\n"
        c += dedent(f"""
            PA_RETURN_TYPE wr_{name}( {params} t_{name}* data ) {{
              for (int slice = 0; slice < {hasSlice} ; slice++ ) {{
                wr_{name}_slice( {call_params} data );
              }}
              PA_RETURN_OK
            }}

            """)
    return (h,c)

# ----------------------------------------------------------- 
def generic_conf_acl_name( reg ):
    no_num = reg.replace('IngressConfigurableACL','')
    no_num = no_num.lstrip(string.digits) 
    return no_num

# ----------------------------------------------------------- 
def get_conf_acl_engine( reg ):
  m = re.match(r'^IngressConfigurableACL([0-9]+)',reg)
  return int(m.group(1))

# ----------------------------------------------------------- collect_conf_acl_fields
def collect_conf_acl_fields(reg, reg_entry, nr_acl_engines, conf_acl_fields, conf_acl_table, conf_acl_read_only, conf_acl_engine_has_table ):
    engine = get_conf_acl_engine( reg )
    generic_name = generic_conf_acl_name(reg)
    conf_acl_engine_has_table[ engine ].add( reg )
    print(f"collect engine {engine} table {reg}")
    fnw = [ (f['Name'],f['Width']) for f in reg_entry['Fields']]
    for fn,fw in fnw:
        if len(conf_acl_fields[generic_name][fn]) == 0:
            conf_acl_fields[generic_name][fn] = [0] * nr_acl_engines
        conf_acl_fields[generic_name][fn][engine] = fw
    print(f"generic:{generic_name}", generic_name in conf_acl_fields)
    conf_acl_table[generic_name] = not reg_entry['Single']
    conf_acl_read_only[generic_name] = 'w' not in reg_entry['Type']

# -----------------------------------------------------------
def gen_conf_common_struct( definedUintSize, conf_acl_fields ):
    struct_decls = ""
    for gname, fields in conf_acl_fields.items():
        field_decl = []
        type_name = f"t_IngressConfigurableACL{gname}Generic"
        for fname, widths in fields.items():
            #assert (( min(widths) > definedUintSize and max(widths) > definedUintSize ) or
            #        ( min(widths) <= definedUintSize and max(widths) <= definedUintSize ))
            max_w = max(widths)
            name_decl = convertToLegal(fname)
            if max_w > definedUintSize:
                nr_bytes = (max_w+7)//8
                type_decl = f"  uint8_t  {name_decl}[{nr_bytes}];"
            else:
                type_decl = f"  uint{definedUintSize}_t {name_decl};"
            field_decl.append( type_decl );
        decl = f"\n// ------ generic struct for IngressConfigurableACL {gname} ----------------\n"
        decl += "typedef struct {\n"
        decl += "\n".join( field_decl )
        decl += f"\n}} {type_name};\n\n"
        struct_decls += decl 
    return struct_decls 

# -----------------------------------------------------------
def gen_conf_common_rdwr_func( definedUintSize, nr_slices, nr_engines, backend,
                              conf_acl_fields, conf_acl_table, conf_acl_read_only, conf_acl_engine_has_table ):

    c = ""
    h = ""
    uint = "uint"+str(definedUintSize)+"_t"

    if False: # there should not be a need pass slice to rd/wr functions any more
        slice_param = " uint32_t slice," if nr_slices > 1 else ""
        pass_slice = " slice," if nr_slices > 1 else ""
    else:
        slice_param = ""
        pass_slice = ""
    dev_param = uint+"* device_ptr," if backend.isMemory() else ""
    pass_dev = " device_ptr," if backend.isMemory() else ""
 
    for gname, fields in conf_acl_fields.items():
        type_name = f"t_IngressConfigurableACL{gname}Generic"
        func_name = f"IngressConfigurableACL{gname}Generic"
        table_index_param = " uint32_t index," if conf_acl_table[gname] else ""
        pass_index = " index," if conf_acl_table[gname] else ""
        read_only = conf_acl_read_only[gname]
        if not read_only:
            h += f"extern PA_RETURN_TYPE wr_{func_name}( {dev_param}{slice_param} uint32_t engine, {table_index_param}{type_name}* data );\n"
        h += f"extern PA_RETURN_TYPE rd_{func_name}( {dev_param}{slice_param} uint32_t engine, {table_index_param}{type_name}* data );\n"

        # The struct pack function
        for eidx in range(nr_engines):
            if f"IngressConfigurableACL{eidx}{gname}" in conf_acl_engine_has_table[eidx]:
                c += f"void pack_struct_{gname}{eidx}( t_IngressConfigurableACL{eidx}{gname}* to, {type_name}* from ) {{\n"
                for fname, widths in fields.items():
                    legal_fname = convertToLegal( fname )
                    if widths[eidx] > 0:
                        if widths[eidx]  > definedUintSize:
                            nr_bytes = (widths[eidx] + 7)//8
                            c += f"  memcpy( to->{legal_fname}, from->{legal_fname}, {nr_bytes} );\n"
                        else:
                            c += f"  to->{legal_fname} = from->{legal_fname};\n"
                    else:
                        c += f"  assert(from->{legal_fname}==0);\n"
                c += "}\n"

        if not read_only:
            # The generic write function
            c += f"extern PA_RETURN_TYPE wr_{func_name}( {dev_param}{slice_param} uint32_t engine, {table_index_param}{type_name}* data ) {{\n"
            c += f"  PA_ERROR_IDX_CHECK(engine,{nr_engines})\n"
            c += "  switch (engine) {"
            need_default = False
            for eidx in range(nr_engines):
                if f"IngressConfigurableACL{eidx}{gname}" in conf_acl_engine_has_table[eidx]:
                    c += indent(
                            dedent(f"""
                      case {eidx}: {{
                        t_IngressConfigurableACL{eidx}{gname} packed;
                        pack_struct_{gname}{eidx}( &packed, data );
                        wr_IngressConfigurableACL{eidx}{gname}({pass_dev}{pass_slice}{pass_index} &packed );
                        break;
                      }}"""),"    ")
                else:
                    need_default = True
            if need_default:
                c += indent(
                        dedent(f"""
                  default: {{
                    PA_ERROR;
                  }}"""),"    ")
            c += "\n"
            c += "  }\n"
            c += "}\n"

        # The struct unpack function
        for eidx in range(nr_engines):
            if f"IngressConfigurableACL{eidx}{gname}" in conf_acl_engine_has_table[eidx]:
                c += f"void unpack_struct_{gname}{eidx}( {type_name}* to, t_IngressConfigurableACL{eidx}{gname}* from) {{\n"
                for fname, widths in fields.items():
                    legal_fname = convertToLegal( fname )
                    if widths[eidx] > 0:
                        if widths[eidx]  > definedUintSize:
                            nr_bytes = (widths[eidx] + 7)//8
                            c += f"  memcpy( to->{legal_fname}, from->{legal_fname}, {nr_bytes} );\n"
                        else:
                            c += f"  to->{legal_fname} = from->{legal_fname};\n"
                    else:
                        if widths[eidx]  > definedUintSize:
                            c += f"  memset( to->{legal_fname}, 0, {nr_bytes} );\n"
                        else:
                            c += f"  to->{legal_fname} = 0;\n"
                c += "}\n"

        # The generic read function
        c += f"extern PA_RETURN_TYPE rd_{func_name}( {dev_param}{slice_param} uint32_t engine, {table_index_param}{type_name}* data ) {{\n"
        c += f"  PA_ERROR_IDX_CHECK(engine,{nr_engines})\n"
        c += "  switch (engine) {"
        need_default = False
        for eidx in range(nr_engines):
            if f"IngressConfigurableACL{eidx}{gname}" in conf_acl_engine_has_table[eidx]:
                c += indent(
                        dedent(f"""
                  case {eidx}: {{
                    t_IngressConfigurableACL{eidx}{gname} packed;
                    rd_IngressConfigurableACL{eidx}{gname}({pass_dev}{pass_slice}{pass_index} &packed );
                    unpack_struct_{gname}{eidx}( data, &packed );
                    break;
                  }}"""),"    ")
            else:
                need_default = True
        if need_default:
            c += indent(
                    dedent(f"""
              default: {{
                PA_ERROR;
              }}"""),"    ")
        c += "\n"
        c += "  }\n"
        c += "}\n"


    return c,h


# -----------------------------------------------------------
prelookup_to_enum = {
        'SPORT':     'prelookup_field_sport',
        'VLAN':      'prelookup_field_vlan',
        'L2':        'prelookup_field_l2',
        'L2_MACSEC': 'prelookup_field_l2_macsec',
        'PPPOE':     'prelookup_field_pppoe',
        'L3':        'prelookup_field_l3',
        'GRE':       'prelookup_field_gre',
        'INNER_L3':  'prelookup_field_inner_l3',
        'L4':        'prelookup_field_l4',
}
# -----------------------------------------------------------
def create_prelookup_field_db( hwconf_yml ):
    nr_prelookup_bits = hwconf_yml['defines']['c_SRCPORT_PRE_LOOKUP_BITS']
    nr_engines = len(hwconf_yml['defines']['conf_CONF_INGRESS_nr_fields'])
    widths = {
        'SPORT':     nr_prelookup_bits,
        'VLAN':      2,
        'L2':        1,
        'L2_MACSEC': 1,
        'PPPOE':     1,
        'L3':        2,
        'GRE':       1,
        'INNER_L3':  2,
        'L4':        3,
    }
    db = [ None ] * nr_engines
    for engine in hwconf_yml['defines']['conf_CONF_INGRESS_pre_lookup_fields'].keys():
        bit = 0
        db[engine] = []
        for field in hwconf_yml['defines']['conf_CONF_INGRESS_pre_lookup_fields'][engine]:
            db[engine].append( ( prelookup_to_enum[field], bit, widths[field] ))
            bit += widths[field]
    return widths, db

# -----------------------------------------------------------
def conf_acl_has_prelookup( defines ):
    has_prelookup = 0
    if 'conf_CONF_INGRESS_pre_lookup' in defines:
        has_prelookup = sum(x[0] for x in defines['conf_CONF_INGRESS_pre_lookup'].values())
    print(f"{has_prelookup=}")
    return has_prelookup > 0


# -----------------------------------------------------------
def gen_conf_acl_defs( hwconf_yml ):
    c_decl = ""
    h_decl = ""
    if '_CONF_INGRESS_ACL' in hwconf_yml['defines']:
        nr_decoded_acl_fields = hwconf_yml['defines']['conf_CONF_INGRESS_nr_fields']
        max_decoded_fields = max( [ v[0] for k,v in nr_decoded_acl_fields.items() ] )
        nr_engines = len( nr_decoded_acl_fields )
        max_fields = max( [ v[0] for k,v in hwconf_yml['defines']['conf_CONF_INGRESS_rules_per_entry'].items() ] )
        nr_rules = [ v[0] for k,v in hwconf_yml['defines']['conf_CONF_INGRESS_rules'].items() ]
        max_rules = max( nr_rules )

        acl_fields  = hwconf_yml['defines']['conf_CONF_INGRESS_Acl_fields']
        acl_actions = hwconf_yml['defines']['conf_CONF_INGRESS_Acl_actions']

        max_search_bits = max( [ v[0] for k,v in hwconf_yml['defines']['conf_CONF_INGRESS_search_bits'].items() ] )
        max_search_bytes = ( max_search_bits + 7 ) // 8

        if conf_acl_has_prelookup( hwconf_yml['defines']):
            max_prelookup = len(prelookup_to_enum) - 1
            nr_prelookup_bits = hwconf_yml['defines']['c_SRCPORT_PRE_LOOKUP_BITS']

            prelookup_field_w, prelookup_field_db = create_prelookup_field_db( hwconf_yml )

        max_counters = hwconf_yml['defines'].get('c_CONF_INGRESS_ACL_STAT_CNT',0)

        h_decl = f"""
// all possible fields
typedef enum {{
  field_none                       = 0,
  field_Rule_Pointer               = 1,
  field_GRE_Key                    = 2,
  field_PPPoE_Session              = 3,
  field_Source_Port                = 4,
  field_L3_Type                    = 5,
  field_L4_Type                    = 6,
  field_Ethernet_Type              = 7,
  field_L4_Protocol                = 8,
  field_Destination_Portmask       = 9,
  field_Multicast_Packet           = 10,
  field_L2_Multicast_Pointer       = 11,
  field_VID                        = 12,
  field_GID                        = 13,
  field_AVTP_Data                  = 14,
  field_IPv6_Flow_Label            = 15,
  field_IGMP_Group_Address         = 16,
  field_IGMP_Type                  = 17,
  field_ICMP_Code                  = 18,
  field_ICMP_Type                  = 19,
  field_MLD_Address                = 20,
  field_L4_Destination_Port        = 21,
  field_L4_Source_Port             = 22,
  field_MACsec_Control_Byte        = 23,
  field_MACsec_Key                 = 24,
  field_IPSEC_SEQ                  = 25,
  field_IPSEC_SPI                  = 26,
  field_TTL                        = 27,
  field_TOS                        = 28,
  field_Inner_MPLS                 = 29,
  field_Outer_MPLS                 = 30,
  field_Inner_IPv6_DA              = 31,
  field_Inner_IPv6_SA              = 32,
  field_Inner_IPv4_DA              = 33,
  field_Inner_IPv4_SA              = 34,
  field_Inner_IP_Type              = 35,
  field_IPv6_DA                    = 36,
  field_IPv6_SA                    = 37,
  field_IPv4_DA                    = 38,
  field_IPv4_SA                    = 39,
  field_Inner_DEI                  = 40,
  field_Inner_PCP                  = 41,
  field_Inner_VID                  = 42,
  field_Outer_DEI                  = 43,
  field_Outer_PCP                  = 44,
  field_Inner_VLAN_Tag_Type        = 45,
  field_Outer_VLAN_Tag_Type        = 46,
  field_Has_VLANs                  = 47,
  field_Outer_VID                  = 48,
  field_MAC_SA                     = 49,
  field_MAC_DA                     = 50,
  field_TCP_Flags                  = 51,
  field_IPv6_Options               = 52,
  field_IPv4_Options               = 53,
  field_L2_Packet_Processing_Flags = 54,
  field_L2_Packet_Flags            = 55,
}} acl_field_name_t;

typedef struct {{
  uint8_t          id;
  acl_field_name_t name;
  uint8_t          size;
}} acl_field_t;

// number of fields that can be simultaneously selected in a rule, maximum of all engines:
#define c_max_nr_acl_fields ({max_fields})

// number of decoded fields, maximum of all engines:
#define c_max_decoded_fields ({max_decoded_fields})

// number of search bits that all selected fields must fit within, maximum of all engines
#define c_max_search_bits ({max_search_bits})
#define c_max_search_bytes ({max_search_bytes})

#define c_nr_acl_engines ({nr_engines})
// number of rules per engine, maximum of all engines
#define c_max_nr_rules ({max_rules})
#define c_max_acl_counters ({max_counters})

extern const acl_field_t conf_acl_fields[c_nr_acl_engines][c_max_decoded_fields];

// all possible acl actions
typedef enum {{
  action_none            = 0,
  action_ipsec           = 1,
  action_macsec          = 2,
  action_mac_op          = 3,
  action_force_wred      = 4,
  action_force_gid       = 5,
  action_force_route     = 6,
  action_force_vrf       = 7,
  action_send2cpu        = 8,
  action_meta_data       = 9,
  action_dec_ttl         = 10,
  action_drop            = 11,
  action_send2port       = 12,
  action_im              = 13,
  action_no_learning     = 14,
  action_om              = 15,
  action_stream_id       = 16,
  action_update_stat_cnt = 17,
  action_update_tos_exp  = 18,
  action_update_vlan     = 19,
  action_update_ip       = 20,
  action_update_l4       = 21,
  action_nat             = 22,
  action_ptp             = 23,
  action_tunnel_entry    = 24,
  action_tunnel_exit     = 25,
  action_crypto          = 26,
  action_force_color     = 27,
  action_force_mmp       = 28,
  action_force_queue     = 29,
  action_force_vid       = 30,
  action_nr_of           = 31, // always last enum item, just end indicator, not an action
}} acl_action_t;

extern const int acl_search_bits[ c_nr_acl_engines ];
extern const int acl_entries_small[ c_nr_acl_engines ];
extern const int acl_entries_large[ c_nr_acl_engines ];
extern const int acl_entries_tcam[ c_nr_acl_engines ];
extern const int acl_buckets[ c_nr_acl_engines ];
extern const int acl_rules[ c_nr_acl_engines ];
extern const int acl_counters[ c_nr_acl_engines ];
"""

        if conf_acl_has_prelookup( hwconf_yml['defines']):
            h_decl += f"""
// all possible prelookup fields
typedef enum {{
  prelookup_field_none,
  prelookup_field_sport,
  prelookup_field_vlan,
  prelookup_field_l2,
  prelookup_field_l2_macsec,
  prelookup_field_pppoe,
  prelookup_field_l3,
  prelookup_field_gre,
  prelookup_field_inner_l3,
  prelookup_field_l4,
}} acl_prelookup_field_name_t;

typedef struct {{
  acl_prelookup_field_name_t type;
  uint32_t                   width;
  uint8_t                    bit_pos; // pos of lowest bit
}} acl_prelookup_field_t;

#define c_max_prelookup_items ({max_prelookup})

// width of the Source Port Tables prelookup fields
#define c_nr_prelookup_bits ({nr_prelookup_bits})

extern const acl_prelookup_field_name_t conf_acl_prelookup_fields[c_nr_acl_engines][c_max_prelookup_items];
extern const acl_prelookup_field_t acl_prelookup_field_db[c_nr_acl_engines][c_max_prelookup_items];

        """

            c_decl = "" 
            # --------------- ACL prelookup field database -----------------------------

            c_decl += dedent(f"""
                // 
                const acl_prelookup_field_name_t conf_acl_prelookup_fields[c_nr_acl_engines][c_max_prelookup_items] = {{
                """)
            for engine, fields in hwconf_yml['defines']['conf_CONF_INGRESS_pre_lookup_fields'].items():
                c_decl += f"  [{engine}] = {{\n"
                count = 0
                for field in fields:
                    fname = prelookup_to_enum[ field ]
                    c_decl += f"    {fname},\n"
                    count += 1
                for c in range( count, max_prelookup ):
                    c_decl +=  "    prelookup_field_none,\n"
                c_decl += "  },\n"
            c_decl += "};\n"


            c_decl += dedent(f"""
                // 
                const acl_prelookup_field_t acl_prelookup_field_db[c_nr_acl_engines][c_max_prelookup_items] = {{
                """)
            for engine in range(nr_engines):
                c_decl += f"  [{engine}] = {{\n"
                count = 0
                for field in prelookup_field_db[engine]:
                    fname, fstart, fwidth = field
                    c_decl += f"    {{ {fname:25s}, {fwidth}, {fstart} }},\n"
                    count += 1
                for c in range( count, max_prelookup ):
                    c_decl +=  "    { prelookup_field_none     , 0, 0 },\n"
                c_decl += "  },\n"
            c_decl += "};\n"


        # --------------- ACL field database -----------------------------
        c_decl += dedent(f"""
            // actual available decoded fields for each acl engine
            const acl_field_t conf_acl_fields[c_nr_acl_engines][c_max_decoded_fields] = {{
            """)
        for engine, fields in acl_fields.items():
            c_decl += f"  [{engine}] = {{\n"
            for field in fields:
                fname = 'field_' + field['name'].replace(' ','_')
                c_decl += f"    {{ {field['id']}, {fname}, {field['size']} }},\n"
            c_decl += "  },\n"
        c_decl += "};\n"

        nr_fields_lst = ",".join( [ str(v[0]) for k,v in hwconf_yml['defines']['conf_CONF_INGRESS_rules_per_entry'].items() ] )

        c_decl += f"const int nr_acl_fields[c_nr_acl_engines] = {{ {nr_fields_lst} }};\n"
        h_decl +=  "// number of rule fields that can be used for each acl engine\n"
        h_decl += f"extern const int nr_acl_fields[c_nr_acl_engines];\n"

        # -------------- ACL action database ------------------------------
        c_decl += dedent(f"""
            // actual available actions for each acl engine
            const acl_action_t conf_acl_actions[c_nr_acl_engines][action_nr_of] = {{
            """)

        for engine, actions in acl_actions.items():
            c_decl += f"  [{engine}] = {{\n"
            for action in actions:
                action = action.replace("_ACL_ACTION_","")
                action = action.replace(" ","_")
                action = action.lower()
                aname = f'action_{action}'
                c_decl += f'    {aname},\n'
            c_decl += "    action_none\n"
            c_decl += "  },\n"
        c_decl += "};\n"

        # ------------ ACL size database ----------------------------------
        c_decl += "const int acl_search_bits[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            nr = hwconf_yml['defines']['conf_CONF_INGRESS_search_bits'][engine][0]
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        c_decl += "const int acl_entries_small[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            nr = hwconf_yml['defines']['conf_CONF_INGRESS_small_table'][engine][0]
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        c_decl += "const int acl_entries_large[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            nr = hwconf_yml['defines']['conf_CONF_INGRESS_large_table'][engine][0]
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        c_decl += "const int acl_entries_tcam[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            nr = hwconf_yml['defines']['conf_CONF_INGRESS_tcam_entries'][engine][0]
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        c_decl += "const int acl_buckets[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            nr = hwconf_yml['defines']['conf_CONF_INGRESS_par_buckets'][engine][0]
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        c_decl += "const int acl_rules[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            nr = nr_rules[engine]
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        c_decl += "const int acl_counters[ c_nr_acl_engines ] = {\n"
        for engine in range( nr_engines ): 
            if max_counters > 0:
                nr = hwconf_yml['defines']['c_CONF_INGRESS_ACL_STAT_CNT']
            else:
                nr = 0
            c_decl += f"  {nr},\n"
        c_decl += "};\n"

        return c_decl, h_decl
    else:
        return "",""
# ----------------------------------------------------------- okChar
CHECK_RE = re.compile('[a-z0-9_]+$')
def okChar(mystring):
    return CHECK_RE.match(mystring)

# ----------------------------------------------------------- convertToLegal
def convertToLegal(word,lowercap=False):
    #print "incoming word:"+word
    # Remove all non-standard characters
    word = re.sub('[^0-9a-zA-Z]+','',word)
    # Make all small characters
    if lowercap:
        word = word.lower()
    #print "outgoing word:"+word
    return word

# ----------------------------------------------------------- checkNameNumber
def checkNameNumber(strLst,searchName):
    hasLst = False
    lstRmv = []
    lstId = 0
    CHECK_NUM = re.compile('[0-9]+')
    if len(strLst)>2:
        for i in range(len(strLst)):
            if strLst[i]==searchName:
                if len(strLst)>(i+1):
                    if CHECK_NUM.match(strLst[i+1]):
                        hasLst=True
                        lstId = strLst[i+1]
                        lstRmv.append(i)
                        lstRmv.append(i+1)

    return [hasLst,lstRmv,lstId]

# ----------------------------------------------------------- createName
def createName(txt,lower,underscore):
    sliceReg = txt.split()
    regName  = ""
    hasSlice = False
    sliceId  = 0
    hasPort  = False
    portId   = 0
    hasSet   = False
    setId    = 0

    remove = []

    (hasPort,lstRmv,portId) = checkNameNumber(sliceReg,"Port")
    remove += lstRmv

    (hasSlice,lstRmv,sliceId) = checkNameNumber(sliceReg,"Slice")
    remove += lstRmv

    (hasSet,lstRmv,setId) = checkNameNumber(sliceReg,"Set")
    remove += lstRmv

    (hasSet2,lstRmv,setId2) = checkNameNumber(sliceReg,"Reason")
    remove += lstRmv

    hasSet = hasSet or hasSet2
    if hasSet2:
        setId = setId2

    #print "REMOVE:"+str(remove)

    for i in range(len(sliceReg)):
        part = convertToLegal(sliceReg[i],lower)
        if i not in remove:
            if regName!="":
                if underscore:
                    regName = regName +"_"+ part
                else:
                    regName = regName + part
            else:
                regName = part

    # The slice registers that are per port use the physical port number, not
    # the port number within the slice. Therefore they should not be regarded
    # as belonging to a slice. That would create problems in the API when
    # indexing by both slice number and port number.
    if hasSlice and hasPort:
        hasSlice = False

    #print "name:"+regName+"slice: "+str(hasSlice)+str(sliceId)+" port:"+str(hasPort)+str(portId)+" set:"+str(hasSet)+str(setId)
    return (regName,hasSlice,sliceId,hasPort,portId,hasSet,setId)

# ----------------------------------------------------------- removeReservedNames
def removeReservedNames(fields):
    newFldList = []

    # Reserved names in c
    rsvdNames = ['int','short','void','bool']

    for field in fields:
        hit = False
        for rsvd in rsvdNames:
            if field['Name']==rsvd:
                print("Replaced field name:"+field['Name']+" with "+ field['Name']+"Rsvd")
                field['Name'] = field['Name']+"Rsvd"
        newFldList.append(field)
    return newFldList

# ----------------------------------------------------------- getPorts
def getPorts(name,sliceId):
    sliceId = int(sliceId)
    addrLst = gRegs[name]['Address']
    retLst = []
    for addr in addrLst:
        sliceNr = int(addr[1])
        portId = int(addr[2])
        if sliceId==sliceNr:
            retLst.append(portId)

    return sorted(retLst)

# Since the slices are added in unknown order we need to extend
# the list incrementally.
def setEntries( reg, sliceId, entries ):
    reg_entry = gRegs[reg]['Entries']

    try:
        reg_entry[sliceId] = entries
    except IndexError:
        reg_entry += [''] * (sliceId+1 - len(reg_entry))
    except TypeError:
        print(reg,gRegs[reg]['Entries'],sliceId)
        assert False
    reg_entry[sliceId] = entries
    gRegs[reg]['Entries'] = reg_entry

# ----------------------------------------------------------- createReg

def createReg(name, fields, address, parts, width, entries,
              sliceId, rtype, statistics, single, hasSlice, hasPort,
              portId, hasSet, setId,backend):
    global globalPorts

    if hasSlice:
        sliceId = int(sliceId)

    if name in gRegs:
        #if dbg:
        #    print "-------------------"
        #    print "Adding address to:"+name
        #    print gRegs[name]
        gRegs[name]['Address'].append([address,int(sliceId),int(portId),int(setId)])
        #print name+":"+str(gRegs[name]['Address'])

        if hasPort:
            portId = int(portId)
            if gRegs[name]['maxPort']<portId:
                gRegs[name]['maxPort']=portId
                if portId>globalPorts:
                    globalPorts=portId

        if hasSlice:
            if gRegs[name]['maxSlice'] < sliceId:
                gRegs[name]['maxSlice']=sliceId
            # there can be a minium that is non-zero since some regs can exist
            # only in a subset of slices (e.g. in a single slice only).
            if sliceId < gRegs[name]['minSlice']:
                gRegs[name]['minSlice']=sliceId
            setEntries( name, sliceId, entries )

        if hasSet:
            setId = int(setId)
            if gRegs[name]['maxSet']<setId:
                gRegs[name]['maxSet']=setId
    else:
        gRegs[name] = {}
        gRegs[name]['hasSet'] = hasSet
        gRegs[name]['hasPort'] = hasPort
        gRegs[name]['hasSlice'] = hasSlice
        gRegs[name]['Address'] = [ [address,sliceId,portId,setId] ]
        gRegs[name]['Fields'] = removeReservedNames(fields)
        gRegs[name]['Parts'] = parts
        gRegs[name]['Width'] = width
        if hasSlice:
            gRegs[name]['Entries'] = []
            setEntries( name, sliceId, entries )
        else:
            gRegs[name]['Entries'] = entries
        gRegs[name]['Type'] = rtype
        gRegs[name]['Single'] = single
        gRegs[name]['maxPort'] = int(portId)
        gRegs[name]['maxSlice'] = int(sliceId)
        gRegs[name]['minSlice'] = int(sliceId)
        gRegs[name]['maxSet'] = int(setId)
        gRegs[name]['Statistics'] = statistics



def mergeSliceEntries():
    for reg in list(gRegs.keys()):
        if type(gRegs[reg]['Entries']) == list:
            entr = gRegs[reg]['Entries']
            # are all entries the same?
            if entr.count(entr[0]) == len(entr):
                # make it an integer, not a list
                gRegs[reg]['Entries'] = entr[0]
            else:
                print('slices are different',reg,entr)


def removeSingleSliceIndexes():
    """ Some registers exists in a single slice although the name of
        the register ends with a Slice N. In the API they should be
        treated as if they are not a slice register. """
    for reg in list(gRegs.keys()):
        # a register exists in only one slice if slice min and max are the same
        if ( gRegs[reg]['hasSlice'] and
             ( gRegs[reg]['maxSlice'] - gRegs[reg]['minSlice'] == 0 ) ):
            gRegs[reg]['hasSlice'] = False
            try:
                one_slice_entries = gRegs[reg]['Entries'][gRegs[reg]['minSlice']]
                gRegs[reg]['Entries'] = one_slice_entries
            except:
                pass

# ------------------------------------------------------------ addrConstName
def addrConstName(name, addr_entry, hasSlice, hasPort, hasSet):
    _, sliceId, portId, setId = addr_entry
    suffix = ""
    if hasPort:
        suffix += f"_P{portId}"
    if hasSet:
        suffix += f"_SET{setId}"
    if hasSlice:
        suffix += f"_S{sliceId}"
    return f"{name}_ADDR{suffix}"

# ------------------------------------------------------------ genAddrDefines
def genAddrDefines(name, address, hasSlice, hasPort, hasSet):
    c = ""
    for addr_entry in address:
        addr_val = addr_entry[0]
        const_name = addrConstName(name, addr_entry, hasSlice, hasPort, hasSet)
        c += f"#define {const_name} ({hex(addr_val)})\n"
    return c

# ------------------------------------------------------------ genCDefines
def genCDefines( reg, field_db ):
    c_code = '\n//---- constants for register {name} -----\n'.format(name=reg)

    if gRegs[reg]['hasSlice']:
        c_code += '#define {name}_nr_slices ({nr})\n'.format(name=reg, nr=gRegs[reg]['maxSlice']+1)
    if gRegs[reg]['hasPort']:
        c_code += '#define {name}_nr_ports ({nr})\n'.format(name=reg, nr=gRegs[reg]['maxPort']+1)
    if gRegs[reg]['hasSet']:
        c_code += '#define {name}_nr_sets ({nr})\n'.format(name=reg, nr=gRegs[reg]['maxSet'])
    if not gRegs[reg]['Single']:
        c_code += '#define {name}_nr_entries ({nr})\n'.format(name=reg, nr=gRegs[reg]['Entries'])

    c_code += '// bit width of each register field:\n'
    for f in field_db[reg]:
        c_code += '#define {name}_{field}_width ({bits})\n'.format(
            name=reg, field=f, bits=field_db[reg][f])

    return c_code

# ------------------------------------------------------------ genDefinesFieldWidth
def genDefinesFieldWidth(fields,fieldName):
    c = ""
    for field in fields:
        if field['Name'] == fieldName:
            fieldSize = calcSize(field['Start bit'],field['End bit'])
            c += "#define "+fieldName+"_WIDTH "+str(fieldSize)+" \n"
    return c

def getCoreVersion( gRegs ):
    cv = gRegs['CoreVersion']
    fld = cv['Fields']
    value = fld[0]['Default value']
    addr = cv['Address']
    addr = addr[0][0]
    return addr,value

# ------------------------------------------------------------ 
def genParamDefines( params ):
    defines = "// --- design parameters -------------\n"
    for pname, val in params.items():
        if isinstance(val,int):
            defines += f"#define {pname} ({val})\n"
    defines += "\n"
    return defines

# ------------------------------------------------------------ createCapi
def genCapi(gRegs,backend,definedUintSize,use_bitfields,field_db,hwconf_yml):

    h_code = dedent("""\
    // (C) Packet Architects AB
    // Generated code for flexswitch-interface
    #ifndef FLEXSWITCH_H
    #define FLEXSWITCH_H
    #ifdef IN_KERNEL
    #include <linux/types.h>
    #else
    #include <stdint.h>
    #endif
    #include <string.h> // only needed for memcpy/memset
    #ifdef FLEXSW_ERROR_ASSERTS
    #include <assert.h>
    #endif
    #ifdef FLEXSW_DEBUG
    #include <stdio.h>
    #include <stdbool.h>
    #endif
    #include <stdio.h>

    static const int FLEXSW_ERR = -1;
    static const int FLEXSW_OK = 0;

    #ifdef FLEXSW_DEBUG
    extern bool flexsw_print_enable;
    #endif

    // Select how to handle error detection:
    // 1. all functions returns status, ok or error.
    // 2. functions do not return error status but will
    //    assert when error is detected.
    // 3. There is no error check and no assertions.
    // Note that the only error check is that slice and port parameters are
    // valid.
    // The recommendation is to use FLEXSW_ERROR_ASSERTS during development
    // and then when compiling for mission mode, turn off error checks.
    #ifdef FLEXSW_ERROR_RETURNS
      #define PA_ERROR return FLEXSW_ERR
      #define PA_ERROR_CHECK if (hit==0) return FLEXSW_ERR;
      #define PA_ERROR_FIELD_CHECK(f,w) if ( (f & ((((uint64_t)1)<<w)-1)) != f ) return FLEXSW_ERR;
      #define PA_ERROR_SLICE_IDX_CHECK(slicev,idx,slice,nr) if (slicev==(slice) && idx >= (nr)) return FLEXSW_ERR;
      #define PA_ERROR_IDX_CHECK(idx,nr) if ( idx >= nr ) return FLEXSW_ERR;
      #define PA_RETURN_TYPE int
      #define PA_RETURN_OK return FLEXSW_OK;
    #elif defined(FLEXSW_ERROR_ASSERTS)
      #define PA_ERROR assert(0)
      #define PA_ERROR_CHECK assert(hit==1);
      #define PA_ERROR_FIELD_CHECK(f,w) assert( (f & ((((uint64_t)1)<<w)-1)) == f );
      #define PA_ERROR_SLICE_IDX_CHECK(slicev,idx,slice,nr) if (slicev==(slice)) assert( idx < (nr) );
      #define PA_ERROR_IDX_CHECK(idx,nr) assert( idx < nr );
      #define PA_RETURN_TYPE void
      #define PA_RETURN_OK
    #else
      #define PA_ERROR
      #define PA_ERROR_CHECK
      #define PA_ERROR_FIELD_CHECK(f,w)
      #define PA_ERROR_SLICE_IDX_CHECK(slicev,idx,slice,nr)
      #define PA_ERROR_IDX_CHECK(idx,nr)
      #define PA_RETURN_TYPE void
      #define PA_RETURN_OK
    #endif

    """)

    c_code = dedent("""\
    // (C) Packet Architects AB
    // Generated code for flexswitch-interface
    #ifndef FLEXSWITCH_C
    #define FLEXSWITCH_C

    #include "flexswitch.h"
    #include "device_read_write.h"
    """)

    h_structs = "\n//\n// Structs for register / tables\n//\n\n"
    h_param_defines = ""
    h_reg_defines = ""

    c_api_code = ""
    c_api_code += "#ifdef FLEXSW_DEBUG\n"
    c_api_code += "bool flexsw_print_enable = false;\n"
    c_api_code += "#endif\n"

    c_api_code += backend.initCode(backend.getWordSize())

    c_api_code += "\n//\n// API Code for register / tables\n//\n\n"

    h_func = "\n//\n// API Functions for register / tables\n//\n\n"
    h_func += backend.initHcode();

    conf_acl_fields = defaultdict(lambda: defaultdict(list))
    conf_acl_table = dict()
    conf_acl_read_only = dict()

    nr_acl_engines = -1
    for reg in gRegs:
        if re.match(r'^IngressConfigurableACL[0-9]',reg):
            engine = get_conf_acl_engine( reg )
            nr_acl_engines = max(engine,nr_acl_engines)
    nr_acl_engines += 1 # index to count

    conf_acl_engine_has_table = [ set() for _ in range(nr_acl_engines) ]

    for reg in gRegs:
        field_widths = {}
        h_structs += genStruct(reg,
                               fields=gRegs[reg]['Fields'],
                               backend=backend,
                               c_code=True,
                               definedUintSize=definedUintSize,
                               use_bitfields=use_bitfields,
                               field_widths=field_widths)
        field_db[reg] = field_widths


        if gRegs[reg]['hasSlice']:
            nr_slices = gRegs[reg]['maxSlice']+1
        else:
            nr_slices = 0

        h_reg_defines += genAddrDefines(reg, gRegs[reg]['Address'], nr_slices, gRegs[reg]['hasPort'], gRegs[reg]['hasSet'])

        h_func += "\n// {0} access functions.\n".format( reg )
        if gRegs[reg]['Single']:
            if 'r' in gRegs[reg]['Type']:
                (h_tmp,c_tmp) = genCReadRegFunction(reg,
                                                    fields=gRegs[reg]['Fields'],
                                                    address=gRegs[reg]['Address'],
                                                    parts=gRegs[reg]['Parts'],
                                                    width=gRegs[reg]['Width'],
                                                    backend=backend,
                                                    hasSlice=nr_slices,
                                                    hasPort=gRegs[reg]['hasPort'],
                                                    hasSet=gRegs[reg]['hasSet'],
                                                    definedUintSize=definedUintSize)
                h_func += h_tmp
                c_api_code += c_tmp
            if 'w' in gRegs[reg]['Type']:
                (h_tmp,c_tmp) = genCWriteRegFunction(reg,
                                                     fields=gRegs[reg]['Fields'],
                                                     address=gRegs[reg]['Address'],
                                                     parts=gRegs[reg]['Parts'],
                                                     width=gRegs[reg]['Width'],
                                                     backend=backend,
                                                     hasSlice=nr_slices,
                                                     hasPort=gRegs[reg]['hasPort'],
                                                     hasSet=gRegs[reg]['hasSet'],
                                                     definedUintSize=definedUintSize)
                h_func += h_tmp
                c_api_code += c_tmp
        else:
            h_reg_defines += genCDefines( reg, field_db )

            if 'r' in gRegs[reg]['Type']:
                (h_tmp,c_tmp) = genCReadTabFunction(reg,
                                                    fields=gRegs[reg]['Fields'],
                                                    address=gRegs[reg]['Address'],
                                                    parts=gRegs[reg]['Parts'],
                                                    width=gRegs[reg]['Width'],
                                                    backend=backend,
                                                    entries=gRegs[reg]['Entries'],
                                                    hasSlice=nr_slices,
                                                    hasPort=gRegs[reg]['hasPort'],
                                                    hasSet=gRegs[reg]['hasSet'],
                                                    definedUintSize=definedUintSize)
                h_func += h_tmp
                c_api_code += c_tmp


            if 'w' in gRegs[reg]['Type']:
                (h_tmp,c_tmp) = genCWriteTabFunction(reg,
                                                     fields=gRegs[reg]['Fields'],
                                                     address=gRegs[reg]['Address'],
                                                     parts=gRegs[reg]['Parts'],
                                                     width=gRegs[reg]['Width'],
                                                     backend=backend,
                                                     entries=gRegs[reg]['Entries'],
                                                     hasSlice=nr_slices,
                                                     hasPort=gRegs[reg]['hasPort'],
                                                     hasSet=gRegs[reg]['hasSet'],
                                                     definedUintSize=definedUintSize)
                h_func += h_tmp
                c_api_code += c_tmp

        hprt,cprt = genPrintStruct( name=reg, fields=gRegs[reg]['Fields'], definedUintSize=definedUintSize, use_bitfields=use_bitfields )
        h_func += hprt
        c_api_code += cprt

        if re.match(r'^IngressConfigurableACL[0-9]',reg):
            collect_conf_acl_fields(reg,gRegs[reg], nr_acl_engines, conf_acl_fields, conf_acl_table, conf_acl_read_only, conf_acl_engine_has_table )


    h_param_defines = genParamDefines( params )

    h_structs += gen_conf_common_struct( definedUintSize, conf_acl_fields )

    c_tmp,h_tmp = gen_conf_common_rdwr_func( definedUintSize, nr_slices, nr_acl_engines, backend, conf_acl_fields, conf_acl_table, conf_acl_read_only, conf_acl_engine_has_table )
    h_func += h_tmp
    c_api_code += c_tmp

    c_tmp,h_tmp = gen_conf_acl_defs( hwconf_yml )
    h_func += h_tmp
    c_api_code += c_tmp

    hc_end_code = "#endif\n\n"
    hc_end_code +=" // EOF\n"
    return (h_code+h_param_defines+h_reg_defines+h_structs+h_func+hc_end_code,c_code+c_api_code+hc_end_code)

# ------------------------------------------------------------ genFieldCapi
def genFieldCapi(gRegs, backend, definedUintSize):
    wordsize = backend.getWordSize()
    uint     = f"uint{definedUintSize}_t"
    uint_hw  = f"uint{wordsize}_t"

    c_code = dedent("""\
    // (C) Packet Architects AB
    // Generated field-level C API - per-field accessors (no structs)
    #ifndef FLEXSWITCH_FIELDS_H
    #define FLEXSWITCH_FIELDS_H

    #define uint8_t unsigned char
    #define uint32_t int
    #define uint64_t long long

    #define PA_RETURN_TYPE void
    #define PA_RETURN_OK

    #define CONF_LOW 65536

    uint32_t readFromDevice(uint32_t address, uint32_t mode) {
      return *(uint32_t *)((uint8_t *)CONF_LOW + address);
    }

    void writeToDevice(uint32_t address, uint32_t data, uint32_t mode) {
      *(uint32_t *)((uint8_t *)CONF_LOW + address) = data;
    }

    """)

    for reg in gRegs:
        fields    = gRegs[reg]['Fields']
        width     = gRegs[reg]['Width']
        is_single = gRegs[reg]['Single']
        has_port  = gRegs[reg]['hasPort']
        has_set   = gRegs[reg]['hasSet']
        has_slice = gRegs[reg]['hasSlice']
        reg_type  = gRegs[reg]['Type']
        address   = gRegs[reg]['Address']
        entries   = gRegs[reg]['Entries']
        parts     = gRegs[reg]['Parts']
        nr_slices = gRegs[reg]['maxSlice'] + 1 if has_slice else 0

        total_hw_words = (width + wordsize - 1) // wordsize
        entries_param  = 0 if is_single else entries

        c_code += f"\n// --- field accessors for: {reg} ---\n"

        for use_slice in ([False] + ([True] if nr_slices > 0 else [])):

            param_parts = []
            call_args   = []

            if use_slice:
                param_parts.append(f"{uint} slice")
                call_args.append("slice")
            if has_port:
                param_parts.append(f"{uint} port")
                call_args.append("port")
            if has_set:
                param_parts.append(f"{uint} set")
                call_args.append("set")
            if not is_single:
                param_parts.append(f"{uint} idx")
                call_args.append("idx")

            fn_infix  = "_slice" if use_slice else ""
            addr_code = genAddrCode(reg, use_slice, has_port, has_set,
                                    address, entries_param, parts, definedUintSize)

            for field in fields:
                fname     = convertToLegal(field['Name'])
                start_bit = field['Start bit']
                fsize     = calcSize(field['Start bit'], field['End bit'])

                # Fields wider than 64 bits use uint8_t* byte-array interface
                # with bit-level scatter/gather loops.
                # Fields 33-64 bits use uint64_t.
                # Fields <= 32 bits use uint32_t.
                is_bytearray = fsize > 64
                field_uint   = "uint64_t" if fsize > definedUintSize else uint

                w_start   = start_bit // wordsize
                w_end     = (start_bit + fsize - 1) // wordsize
                bit_off   = start_bit % wordsize
                lo_bits   = wordsize - bit_off
                last_bits = (start_bit + fsize - 1) % wordsize + 1
                nr_hw_words = w_end - w_start + 1
                nr_bytes  = (fsize + 7) // 8

                def addr_expr(w):
                    return f"address+{w}" if w else "address"

                # ======== BYTE-ARRAY PATH (fsize > 64) ========
                if is_bytearray:
                    # -------- READ --------
                    if 'r' in reg_type:
                        all_params = param_parts + ["uint8_t *out"]
                        params_str = ", ".join(all_params)
                        c_code += f"PA_RETURN_TYPE rd_{reg}{fn_infix}_{fname}( {params_str} ) {{\n"
                        c_code += addr_code
                        c_code += f"  int i, j;\n"

                        if bit_off == 0:
                            c_code += f"  {uint_hw} merged;\n"
                            c_code += f"  for (i = 0; i < {nr_hw_words}; i++) {{\n"
                            c_code += f"    merged = readFromDevice({addr_expr(w_start)}+i,0);\n"
                            inner_bound = f" && i*4+j < {nr_bytes}" if nr_bytes % 4 != 0 else ""
                            c_code += f"    for (j = 0; j < 4{inner_bound}; j++)\n"
                            c_code += f"      out[i*4+j] = (uint8_t)(merged >> (j*8));\n"
                            c_code += f"  }}\n"
                        else:
                            hi_shift = wordsize - bit_off
                            c_code += f"  {uint_hw} prev = readFromDevice({addr_expr(w_start)},0);\n"
                            c_code += f"  for (i = 0; i < {nr_hw_words - 1}; i++) {{\n"
                            c_code += f"    {uint_hw} curr = readFromDevice({addr_expr(w_start+1)}+i,0);\n"
                            c_code += f"    {uint_hw} merged = (prev >> {bit_off}) | (curr << {hi_shift});\n"
                            c_code += f"    prev = curr;\n"
                            inner_bound = f" && i*4+j < {nr_bytes}" if nr_bytes % 4 != 0 else ""
                            c_code += f"    for (j = 0; j < 4{inner_bound}; j++)\n"
                            c_code += f"      out[i*4+j] = (uint8_t)(merged >> (j*8));\n"
                            c_code += f"  }}\n"

                        c_code += "  PA_RETURN_OK\n"
                        c_code += "}\n\n"

                    # -------- WRITE --------
                    if 'w' in reg_type:
                        all_params = param_parts + [f"const uint8_t *{fname}"]
                        params_str = ", ".join(all_params)
                        c_code += f"PA_RETURN_TYPE wr_{reg}{fn_infix}_{fname}( {params_str} ) {{\n"
                        c_code += addr_code
                        c_code += f"  int i, j;\n"
                        c_code += f"  {uint_hw} v;\n"

                        if bit_off == 0:
                            if 'r' in reg_type:
                                c_code += f"  {uint_hw} w0 = readFromDevice({addr_expr(w_start)},0) & 0x0;\n"
                                hi_mask = hex(~((1 << last_bits) - 1) & ((1 << wordsize) - 1))
                                c_code += f"  {uint_hw} wlast = readFromDevice({addr_expr(w_end)},0) & {hi_mask};\n"
                            else:
                                c_code += f"  {uint_hw} w0 = 0;\n"
                                c_code += f"  {uint_hw} wlast = 0;\n"
                            c_code += f"  {uint_hw} wprev = w0;\n"
                            c_code += f"  for (i = 0; i < {nr_hw_words}; i++) {{\n"
                            inner_bound = f" && i*4+j < {nr_bytes}" if nr_bytes % 4 != 0 else ""
                            c_code += f"    {uint_hw} wcurr = (i == {nr_hw_words-1}) ? wlast : 0;\n"
                            c_code += f"    for (j = 0; j < 4{inner_bound}; j++) {{\n"
                            c_code += f"      v = {fname}[i*4+j];\n"
                            c_code += f"      wcurr |= v << (j*8);\n"
                            c_code += f"    }}\n"
                            c_code += f"    writeToDevice({addr_expr(w_start)}+i, wcurr, 0);\n"
                            c_code += f"  }}\n"
                        else:
                            hi_shift = wordsize - bit_off
                            if 'r' in reg_type:
                                lo_mask = hex((1 << bit_off) - 1)
                                c_code += f"  {uint_hw} wprev = readFromDevice({addr_expr(w_start)},0) & {lo_mask};\n"
                                hi_mask = hex(~((1 << last_bits) - 1) & ((1 << wordsize) - 1))
                                c_code += f"  {uint_hw} wlast = readFromDevice({addr_expr(w_end)},0) & {hi_mask};\n"
                            else:
                                c_code += f"  {uint_hw} wprev = 0;\n"
                                c_code += f"  {uint_hw} wlast = 0;\n"
                            c_code += f"  for (i = 0; i < {nr_hw_words - 1}; i++) {{\n"
                            inner_bound = f" && i*4+j < {nr_bytes}" if nr_bytes % 4 != 0 else ""
                            c_code += f"    {uint_hw} wcurr = (i == {nr_hw_words-2}) ? wlast : 0;\n"
                            c_code += f"    for (j = 0; j < 4{inner_bound}; j++) {{\n"
                            c_code += f"      v = {fname}[i*4+j];\n"
                            c_code += f"      wprev |= v << ({bit_off} + j*8);\n"
                            c_code += f"      if ({hi_shift} > j*8) wcurr |= v >> ({hi_shift} - j*8);\n"
                            c_code += f"    }}\n"
                            c_code += f"    writeToDevice({addr_expr(w_start)}+i, wprev, 0);\n"
                            c_code += f"    wprev = wcurr;\n"
                            c_code += f"  }}\n"
                            c_code += f"  writeToDevice({addr_expr(w_end)}, wprev, 0);\n"

                        c_code += "  PA_RETURN_OK\n"
                        c_code += "}\n\n"

                    continue

                # ======== SCALAR PATH (fsize <= 64) ========
                mask = hex((1 << fsize) - 1)

                # -------- READ --------
                if 'r' in reg_type:
                    all_params = param_parts + [f"{field_uint} *out"]
                    params_str = ", ".join(all_params)
                    c_code += f"PA_RETURN_TYPE rd_{reg}{fn_infix}_{fname}( {params_str} ) {{\n"
                    c_code += addr_code

                    if w_start == w_end:
                        c_code += f"  {uint_hw} entry;\n"
                        c_code += f"  {backend.c_read(addr_expr(w_start), 0, 'entry')}"
                        shift = f" >> {bit_off}" if bit_off else ""
                        c_code += f"  *out = ({field_uint})((entry{shift}) & {mask});\n"
                    else:
                        for w in range(w_start, w_end + 1):
                            c_code += f"  {uint_hw} entry{w};\n"
                        c_code += f"  {backend.c_read(addr_expr(w_start), 0, f'entry{w_start}')}"
                        for w in range(w_start + 1, w_end + 1):
                            c_code += f"  {backend.c_read(addr_expr(w), 1, f'entry{w}')}"
                        lo_mask = hex((1 << lo_bits) - 1)
                        lo_part = f"(entry{w_start} >> {bit_off})" if bit_off else f"entry{w_start}"
                        c_code += f"  *out = ({field_uint})(({lo_part}) & {lo_mask})"
                        field_bit = lo_bits
                        for w in range(w_start + 1, w_end):
                            c_code += f" | (({field_uint})entry{w} << {field_bit})"
                            field_bit += wordsize
                        hi_mask = hex((1 << last_bits) - 1)
                        c_code += f" | (({field_uint})(entry{w_end} & {hi_mask}) << {field_bit})"
                        c_code += ";\n"

                    c_code += "  PA_RETURN_OK\n"
                    c_code += "}\n\n"

                # -------- WRITE --------
                if 'w' in reg_type:
                    all_params = param_parts + [f"{field_uint} {fname}"]
                    params_str = ", ".join(all_params)
                    c_code += f"PA_RETURN_TYPE wr_{reg}{fn_infix}_{fname}( {params_str} ) {{\n"
                    c_code += addr_code

                    if 'r' in reg_type:
                        if w_start == w_end:
                            c_code += f"  {uint_hw} entry;\n"
                            c_code += f"  {backend.c_read(addr_expr(w_start), 0, 'entry')}"
                            if bit_off:
                                c_code += f"  entry = (entry & ~(({uint_hw}){mask} << {bit_off})) | (({uint_hw})({fname} & {mask}) << {bit_off});\n"
                            else:
                                c_code += f"  entry = (entry & ~({uint_hw}){mask}) | (({uint_hw})({fname} & {mask}));\n"
                            c_code += f"  {backend.c_write(addr_expr(w_start), 0, 'entry')}"
                        else:
                            for w in range(w_start, w_end + 1):
                                c_code += f"  {uint_hw} entry{w};\n"
                            c_code += f"  {backend.c_read(addr_expr(w_start), 0, f'entry{w_start}')}"
                            c_code += f"  {backend.c_read(addr_expr(w_end),   1, f'entry{w_end}')}"
                            lo_mask = hex((1 << lo_bits) - 1)
                            if bit_off:
                                c_code += f"  entry{w_start} = (entry{w_start} & ~(({uint_hw}){lo_mask} << {bit_off})) | (({uint_hw})(({field_uint}){fname} & {lo_mask}) << {bit_off});\n"
                            else:
                                c_code += f"  entry{w_start} = (entry{w_start} & ~({uint_hw}){lo_mask}) | (({uint_hw})(({field_uint}){fname} & {lo_mask}));\n"
                            field_bit = lo_bits
                            for w in range(w_start + 1, w_end):
                                wrd_mask = hex((1 << wordsize) - 1)
                                c_code += f"  entry{w} = ({uint_hw})((({field_uint}){fname} >> {field_bit}) & {wrd_mask});\n"
                                field_bit += wordsize
                            hi_mask = hex((1 << last_bits) - 1)
                            c_code += f"  entry{w_end} = (entry{w_end} & ~({uint_hw}){hi_mask}) | (({uint_hw})((({field_uint}){fname} >> {field_bit}) & {hi_mask}));\n"
                            for w in range(w_start, w_end):
                                c_code += f"  {backend.c_write(addr_expr(w), 1, f'entry{w}')}"
                            c_code += f"  {backend.c_write(addr_expr(w_end), 0, f'entry{w_end}')}"
                    else:
                        for w_idx in range(total_hw_words):
                            c_code += f"  {uint_hw} entry{w_idx} = 0;\n"
                        if w_start == w_end:
                            if bit_off:
                                c_code += f"  entry{w_start} = ({uint_hw})(({field_uint}){fname} & {mask}) << {bit_off};\n"
                            else:
                                c_code += f"  entry{w_start} = ({uint_hw})(({field_uint}){fname} & {mask});\n"
                        else:
                            lo_mask = hex((1 << lo_bits) - 1)
                            hi_mask = hex((1 << last_bits) - 1)
                            if bit_off:
                                c_code += f"  entry{w_start} = ({uint_hw})(({field_uint}){fname} & {lo_mask}) << {bit_off};\n"
                            else:
                                c_code += f"  entry{w_start} = ({uint_hw})(({field_uint}){fname} & {lo_mask});\n"
                            field_bit = lo_bits
                            for w in range(w_start + 1, w_end):
                                wrd_mask = hex((1 << wordsize) - 1)
                                c_code += f"  entry{w} = ({uint_hw})((({field_uint}){fname} >> {field_bit}) & {wrd_mask});\n"
                                field_bit += wordsize
                            c_code += f"  entry{w_end} = ({uint_hw})((({field_uint}){fname} >> {field_bit}) & {hi_mask});\n"
                        for w_idx in range(total_hw_words):
                            wr_mode = 1 if w_idx < total_hw_words - 1 else 0
                            c_code += f"  {backend.c_write(addr_expr(w_idx), wr_mode, f'entry{w_idx}')}"

                    c_code += "  PA_RETURN_OK\n"
                    c_code += "}\n\n"

    c_code += "#endif\n// EOF\n"
    return c_code

# ------------------------------------------------------------ genRandomStruct
def genRandomStruct(reg,
                    isSingle,
                    entries,
                    fields,
                    wordsize,
                    hasPort,
                    hasSlice,
                    hasSet):

    nr_ports = 1
    nr_slice = 1
    nr_sets = 1

    if debug: print("genRandomStruct for "+str(reg))

    if hasPort:
        nr_ports = gRegs[reg]['maxPort']+1

    if hasSet:
        nr_sets = gRegs[reg]['maxSet']+1

    if hasSlice:
        nr_slice = gRegs[reg]['maxSlice']+1

    if isSingle:
        entries = 1

    if hasSlice and type(entries) == list:
        decl_entries = max(entries)
    else:
        decl_entries = entries
    c_global = "  t_{reg} {reg}_data[{entries}][{nr_ports}][{nr_sets}][{nr_slice}];\n".format(
        reg=reg, entries=decl_entries, nr_ports = nr_ports, nr_sets=nr_sets, nr_slice = nr_slice )

    if hasSlice:
        if type(entries) == list:
            nr_entries = entries
        else:
            nr_entries = [entries] * nr_slice
    else:
        nr_entries = [entries]

    if hasSlice and hasPort:
        port_min = [0] * nr_slice
        port_max = [0] * nr_slice
        for slc in range(nr_slice):
            portLst = getPorts(reg,slc)
            port_min[slc] = min(portLst)
            port_max[slc] = max(portLst)
    else:
        port_min = [0] * nr_slice
        port_max = [nr_ports] * nr_slice

    set_random = ''

    for field in fields:
        size = calcSize(field['Start bit'],field['End bit'])

        if size<=wordsize:
            t = Template("""
          random_value = (random()<<32 | random());
          ${reg}_data[i][portId][setId][sliceId].$field_name = random_value & $mask;""")
            set_random += t.substitute( reg=reg,
                                       mask = str(hex((2**size)-1)).rstrip("L"),
                                       field_name = convertToLegal(field['Name']) )
        else:
            t = Template("""
          ${reg}_data[i][portId][setId][sliceId].$field_name[$idx] = random() & $mask;""")
            nr_uint8 = (size+7) // 8
            for j in range(nr_uint8):
                mask = '0xff'
                if j == nr_uint8-1 and size % 8:
                    mask = str(hex((2**( size % 8 ))-1))
                set_random += t.substitute( reg=reg,
                                            idx=j,
                                            mask = mask,
                                            field_name = convertToLegal(field['Name']) )

    t = Template("""
void set_random_${reg}() {
  uint64_t random_value;
  int port_min[] = { $port_min };
  int port_max[] = { $port_max };
  int entries[] = { $entries };
  printf("Creating random numbers for: $reg\\n");
  for (int setId=0; setId < $nr_sets; setId++) {
    for (int sliceId=0; sliceId < $nr_slice; sliceId++) {
      for (int portId=port_min[sliceId]; portId < port_max[sliceId]; portId++) {
        for (int i=0; i < entries[sliceId]; i++){$set_random
        }
      }
    }
  }
}
""")
    c = t.substitute( reg=reg, nr_sets=nr_sets, nr_slice=nr_slice,
                      entries = ','.join( [ str(e) for e in nr_entries ] ),
                      port_min = ','.join( [ str(e) for e in port_min ] ),
                      port_max = ','.join( [ str(e) for e in port_max ] ),
                      set_random = set_random)

    return (c_global,c)

# ------------------------------------------------------------ genRandomWrites
def genRandomWrites(reg,
                    isSingle,
                    entries,
                    hasSlice,
                    hasPort,
                    hasSet,
                    backend):

    if debug: print("genRandomWrites for "+str(reg))

    nr_ports = 1
    nr_slice = 1
    nr_sets = 1
    devPtr = ""
    sliceLine = ""
    portLine = ""
    setLine = ""
    devPtr = ""

    if backend.isMemory():
        devPtr = "device_ptr,"

    if hasPort:
        nr_ports = gRegs[reg]['maxPort']+1

    if hasSet:
        nr_sets = gRegs[reg]['maxSet']+1

    if hasSlice:
        nr_slice = gRegs[reg]['maxSlice']+1

    if isSingle:
        entries = 1

    if backend.isMemory():
        dev_ptr = "uint64_t* device_ptr"
    else:
        dev_ptr = ''

    if hasSlice:
        if type(entries) == list:
            nr_entries = entries
        else:
            nr_entries = [entries] * nr_slice
    else:
        nr_entries = [entries]

    if hasSlice and hasPort:
        port_min = [0] * nr_slice
        port_max = [0] * nr_slice
        for slc in range(nr_slice):
            portLst = getPorts(reg,slc)
            port_min[slc] = min(portLst)
            port_max[slc] = max(portLst)
    else:
        port_min = [0] * nr_slice
        port_max = [nr_ports] * nr_slice


    slice_pfix = ""
    if hasSlice:
        sliceLine =  "sliceId, "
        slice_pfix = "_slice"
    if hasPort:
        portLine = "portId, "
    if hasSet:
        setLine =  "setId, "
    if not isSingle:
        entry= "i, "
    else:
        entry=""

    wr_code = "          wr_"+reg+slice_pfix+"("+devPtr+sliceLine+portLine+setLine+entry+"&"+reg+"_data[i][portId][setId][sliceId]);\n";

    t = Template("""
void write_${reg}( $dev_ptr ) {
  int port_min[] = { $port_min };
  int port_max[] = { $port_max };
  int entries[] = { $entries };
  printf("Writing to $reg\\n");
  for (int setId=0; setId < $nr_sets; setId++) {
    for (int sliceId=0; sliceId < $nr_slice; sliceId++) {
      for (int portId=port_min[sliceId]; portId < port_max[sliceId]; portId++) {
        for (int i=0; i < entries[sliceId]; i++){
$wr_code
        }
      }
    }
  }
}
""")
    c = t.substitute( dev_ptr = dev_ptr, reg=reg, nr_sets=nr_sets, nr_slice=nr_slice,
                      entries = ','.join( [ str(e) for e in nr_entries ] ),
                      port_min = ','.join( [ str(e) for e in port_min ] ),
                      port_max = ','.join( [ str(e) for e in port_max ] ),
                      wr_code = wr_code)
    return c

# ------------------------------------------------------------ genRandomReads
def genRandomReads(reg,
                   isSingle,
                   entries,
                   hasSlice,
                   hasPort,
                   hasSet,
                   backend,
                   definedUintSize):

    if debug: print("genRandomReads for "+str(reg))

    nr_ports = 1
    nr_slice = 1
    nr_sets = 1
    devPtr = ""
    sliceLine = ""
    portLine = ""
    setLine = ""
    devPtr = ""

    if backend.isMemory():
        devPtr = "device_ptr,"

    if hasPort:
        nr_ports = gRegs[reg]['maxPort']+1

    if hasSet:
        nr_sets = gRegs[reg]['maxSet']+1

    if hasSlice:
        nr_slice = gRegs[reg]['maxSlice']+1

    if isSingle:
        entries = 1

    wordsize = definedUintSize;

    if backend.isMemory():
        dev_ptr = "uint64_t* device_ptr"
    else:
        dev_ptr = ''

    if hasSlice:
        if type(entries) == list:
            nr_entries = entries
        else:
            nr_entries = [entries] * nr_slice
    else:
        nr_entries = [entries]

    if hasSlice and hasPort:
        port_min = [0] * nr_slice
        port_max = [0] * nr_slice
        for slc in range(nr_slice):
            portLst = getPorts(reg,slc)
            port_min[slc] = min(portLst)
            port_max[slc] = max(portLst)
    else:
        port_min = [0] * nr_slice
        port_max = [nr_ports] * nr_slice

    slice_pfix = ""
    if hasSlice:
        sliceLine = "sliceId, "
        slice_pfix = "_slice"
    if hasPort:
        portLine = "portId, "
    if hasSet:
        setLine = "setId, "
    if not isSingle:
        entry= "i, "
    else:
        entry= ""

    t = Template("""
          rd_$reg$slice_pfix($devPtr$sliceLine$portLine$setLine$entry&${reg}_tmp);
          """)
    check_code = t.substitute(reg=reg, devPtr=devPtr, sliceLine=sliceLine,
                              setLine=setLine, portLine=portLine, entry=entry, slice_pfix=slice_pfix)
    for field in gRegs[reg]['Fields']:
        size = calcSize(field['Start bit'],field['End bit'])
        fieldName = convertToLegal(field['Name'])
        if size<=wordsize:
            t = Template("""
          if( ${reg}_tmp.$fieldName != ${reg}_data[i][portId][setId][sliceId].$fieldName ) {
            printf("ERROR in $reg field $fieldName Got:%lx Expected %lx \\n",
                   (long unsigned int)${reg}_tmp.$fieldName,
                   (long unsigned int)${reg}_data[i][portId][setId][sliceId].$fieldName);
            error += 1;
            exit(-1);
          } else {
            correct += 1;
          }""")
            check_code += t.substitute( reg=reg, fieldName=fieldName )
        else:
            t = Template("""
          if( ${reg}_tmp.$fieldName[$idx] != ${reg}_data[i][portId][setId][sliceId].$fieldName[$idx] ) {
            printf("ERROR in $reg part [$idx] field $fieldName Got:%lx Expected %lx \\n",
                   (long unsigned int)${reg}_tmp.$fieldName[$idx],
                   (long unsigned int)${reg}_data[i][portId][setId][sliceId].$fieldName[$idx]);
            error += 1;
            exit(-1);
          } else {
            correct += 1;
          }""")

            nr_uint8 = (size+7) // 8
            for j in range(nr_uint8):
                check_code += t.substitute( reg=reg, fieldName=fieldName, idx=j )

    t = Template("""
void read_${reg}( $dev_ptr ) {
  int port_min[] = { $port_min };
  int port_max[] = { $port_max };
  int entries[] = { $entries };
  t_$reg ${reg}_tmp;
  printf("Reading from $reg\\n");
  for (int setId=0; setId < $nr_sets; setId++) {
    for (int sliceId=0; sliceId < $nr_slice; sliceId++) {
      for (int portId=port_min[sliceId]; portId < port_max[sliceId]; portId++) {
        for (int i=0; i < entries[sliceId]; i++){$check_code
        }
      }
    }
  }
}
""")
    c = t.substitute( dev_ptr = dev_ptr, reg=reg, nr_sets=nr_sets, nr_slice=nr_slice,
                      entries = ','.join( [ str(e) for e in nr_entries ] ),
                      port_min = ','.join( [ str(e) for e in port_min ] ),
                      port_max = ','.join( [ str(e) for e in port_max ] ),
                      check_code=check_code)

    return c

# ------------------------------------------------------------ createCApplStub
def createCApplStub(backend,bwFunctions=False,bytesPerSec=False,main=True):
    c_code = """// (C) Packet Architects AB
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <inttypes.h>
#include "flexswitch.h"

#define TRUE 1
#define FALSE 0

"""
    if main==True:
        c_code +="int main (int argc, char **argv) {\n"
        c_code += backend.initDevice(malloced_mem=False)

    return c_code
# ------------------------------------------------------------ genReadWriteTest
def genReadWriteTest(gRegs,backend,definedUintSize):
    c_pre = createCApplStub(backend,main=False)
    c_global = "//\n// Global variables for all structs\n//\n\n"
    c_global += "int error=0;\n"
    c_global += "int correct=0;\n"

    confbus_width = backend.getWordSize();
    # the wr_rd_test can not use the backends real read/write functions
    c_global += """    
    
uint{dwidth}_t readFromDevice(uint64_t *device_ptr,uint64_t address,int mode) {{
  uint{dwidth}_t ret_data;
  uint{dwidth}_t *dev_ptr_{dwidth};
  dev_ptr_{dwidth} = (uint{dwidth}_t*)(device_ptr);
  ret_data = dev_ptr_{dwidth}[address];
  return ret_data;
}}

uint8_t dev_ptr_written[100000000];
    
void writeToDevice(uint64_t *device_ptr,uint64_t address,uint{dwidth}_t data,int mode) {{
  uint{dwidth}_t *dev_ptr_{dwidth};
  dev_ptr_{dwidth} = (uint{dwidth}_t*)(device_ptr);
  dev_ptr_{dwidth}[address] = data;

  // printf("Address %li is written!\\n",address);
    
  // This is a write check.    
  if( dev_ptr_written[address] ==1) {{
    printf("Address %li is aready written!\\n",address);
    printf("ERROR - Terminating\\n");
    exit(-1);
  }}

  dev_ptr_written[address] = 1;
  
}}
""".format( dwidth=confbus_width )

    c = """
int main(int argc,char **argv) {

"""
    # rd/wr test always use a malloced memory area to test the API and
    # being independent of having a actual device connected.
    c += backend.initDevice(malloced_mem=True)
    c += """
    for(int i=0;i<100000000;i++) {
       dev_ptr_written[i] = 0;
    } 
    """
    uint = "uint"+str(definedUintSize)+"_t"
    c += uint+" random_value;\n"
    c += uint+" port;\n"
    c += uint+" slice;\n"
    c += uint+" set;\n"

    c_random = ""
    c_write  = ""
    c_read   = ""
    c_write_func  = ""
    c_read_func   = ""

    for reg in gRegs:
        (c_glb_tmp,c_rnd_tmp) = genRandomStruct(reg=reg,
                                                isSingle=gRegs[reg]['Single'],
                                                entries=gRegs[reg]['Entries'],
                                                fields=gRegs[reg]['Fields'],
                                                wordsize=definedUintSize,
                                                hasSlice=gRegs[reg]['hasSlice'],
                                                hasPort=gRegs[reg]['hasPort'],
                                                hasSet=gRegs[reg]['hasSet'])
        c_global += c_glb_tmp;
        c_random += c_rnd_tmp;
        c += "  set_random_"+reg+"();\n"
        if 'rw' in gRegs[reg]['Type']:
            c_write_func += genRandomWrites(reg=reg,
        	                            isSingle=gRegs[reg]['Single'],
        	                            entries=gRegs[reg]['Entries'],
        	                            hasSlice=gRegs[reg]['hasSlice'],
        	                            hasPort=gRegs[reg]['hasPort'],
        	                            hasSet=gRegs[reg]['hasSet'],
        	                            backend=backend)
            if backend.isMemory():
                c_write+= "  write_"+reg+"(device_ptr);\n"
            else:
                c_write+= "  write_"+reg+"();\n"
            c_read_func += genRandomReads(reg,
        	                          gRegs[reg]['Single'],
        	                          gRegs[reg]['Entries'],
        	                          hasSlice=gRegs[reg]['hasSlice'],
        	                          hasPort=gRegs[reg]['hasPort'],
        	                          hasSet=gRegs[reg]['hasSet'],
        	                          backend=backend,
                                          definedUintSize=definedUintSize)
            if backend.isMemory():
                c_read+= "  read_"+reg+"(device_ptr);\n"
            else:
                c_read+= "  read_"+reg+"();\n"
    c_end =  "  printf(\"Correct values:%d\\n\",correct);\n"
    c_end += "  if(error==0) {\n"
    c_end += "    printf(\"Test passed.\\n\");\n"
    c_end += "  } else {\n"
    c_end += "    printf(\"total ERRORS:%d\\n\",error);\n"
    c_end += "    printf(\"Test FAILED!\\n\");\n"
    c_end += "    exit(2);\n"
    c_end += "  }\n"
    c_end += "}\n//EOF\n"

    return c_pre+c_global+c_random+c_write_func+c_read_func+c+c_write+c_read+c_end

# ------------------------------------------------------------ writeFile
def writeFile(path,fileName,txt,fileList,exe=False):
    if fileList != None:
        fileList.append(fileName[:-2])
    f = open(path+fileName,"w")
    f.write(txt)
    f.close()
    if exe:
        os.chmod(path+fileName, 0o777)

# ------------------------------------------------------------ addSpace
def addSpace(name,newSize):
    txtSize = len(name)
    if txtSize>newSize:
        return name
    else:
        before=False
        for space in range(newSize-txtSize):
            if before:
                before = False
                name = " "+name
            else:
                before = True
                name = name+" "
        return name

# ------------------------------------------------------------ genDump
def genCheckVersion(gregs,backend,malloced_mem):
    cv = gregs['CoreVersion']
    fld = cv['Fields']
    value = fld[0]['Default value']
    c_code = createCApplStub(backend,main=False)
    c_code += """
int main (int argc, char **argv) {
  t_CoreVersion cv;
"""
    cv_addr, cv = getCoreVersion(gRegs)
    c_code += backend.initDevice(malloced_mem=malloced_mem)
    if backend.isMemory():
        c_code += "  rd_CoreVersion(device_ptr,&cv);\n"
    else:
        c_code += "  rd_CoreVersion(&cv);\n"

    c_code += """
  if(cv.version == """+str(hex(value))+""" ) {
    printf("OK - Core Version is correct (%x)\\n",cv.version);
    exit(0); //OK!
  } else {
    printf("\\nERROR - Core Version is _NOT_ correct\\n");
    printf("  Expected: """+str(hex(value))+""" got: %x\\n",cv.version);
    exit(-1); //Not OK
  }
}
"""
    return c_code

# ------------------------------------------------------------ genCheckPyVersion
def genCheckPyVersion(gregs,backend):
    cv = gregs['CoreVersion']    
    fld = cv['Fields']
    value = fld[0]['Default value']
    version = f"0x{value:08x}"
    py_code = """#!/usr/bin/python
# (C) Packet Architects AB
from flexswitch import *
initFlexSwitch()
cv = CoreVersion()
rd_CoreVersion(cv);
if cv.version=="""+version+""":
    print "Correct core version! ("+hex(cv.version)+")"
else:
    print "ERROR: not correct version of core!, got:"+hex(cv.version)+" but expected: """+str(hex(value))+""" "
    exit(-1)
"""
    return py_code

# ------------------------------------------------------------ genCompScript
def genCompScript():
    code ="#!/bin/bash\n"
    code += "# (C) Packet Architects AB\n"
    code += "# - Compile script\n"
    code += "CC=gcc\n\n"
    code += "DATE=$(date)\n"
    code += "echo $DATE\n"
    code += "CFLAGS=\"-I. -DFLEXSW_DEBUG -DFLEXSW_ERROR_ASSERTS\"\n\n"

    return code

def genCleanScript(fileList,backend,pyapi,capi):
    code ="#!/bin/bash\n"
    code += "# (C) Packet Architects AB\n"
    if pyapi:
        code += "rm -f libflexswitch.so\n"
        code += "rm -f *.o\n"
        code += "rm -f *.pyc\n"
        code += "rm -rf __pycache__\n"
    if capi:
        code += "rm -f *.o\n"
        code += backend.cleanCommands()

    for fileName in fileList:
      code += "rm -f " + fileName + "\n"
    return code

def genTestScript(pyapi):
    code ="#!/bin/bash\n"
    code += "# (C) Packet Architects AB\n"
    if pyapi:
        code += """
echo 'Compiling dynamic library containing the C-API'
./comp.sh

echo 'Test loading the library'
python3 -c "from flexswitch import *; import ctypes; flxsw = ctypes.cdll.LoadLibrary('./libflexswitch.so')"
if [[ $? != 0 ]]; then
    echo "failed to load dynamic library libflexswitch.so"
    echo "FAILED."
    exit 1
fi

echo 'Test rdreg/wrreg'
./rdreg.py --ls >/dev/null
if [[ $? != 0 ]]; then
    echo "rdreg.py failed"
    echo "FAILED."
    exit 1
fi
./wrreg.py --ls >/dev/null
if [[ $? != 0 ]]; then
    echo "rdreg.py failed"
    echo "FAILED."
    exit 1
fi

"""
    code += """

echo 'Compiling the C-API and test program'
./comp.sh

echo "Running wr_rd_test"
./wr_rd_test >/dev/null
if [[ $? != 0 ]]; then
  echo "wr_rd_test FAILED."
  exit 1
fi
"""
    return code

# ------------------------------------------------------------ genCompDemoScript
def genCompDemoScript(fileList,backend):
    code ="#!/bin/bash\n"
    code += "# (C) Packet Architects AB\n"
    code += "# - Compile script\n"
    code += "CC=gcc\n\n"
    code += "DATE=$(date)\n"
    code += "echo $DATE\n"
    code += "FLAGS=\"-I.\"\n\n"
    code += "FLAGS_NETLINK=\"-I/usr/include/libnl3 -L/lib/x86_64-linux-gnu/ -L/usr/lib/x86_64-linux-gnu/ -lnl-3 -lnl-genl-3 -lnl-route-3\"\n"
    code += "chmod a+x .\n"
    code += backend.getCompileLine()
    code += "$CC $FLAGS -c flexswitch.c -o flexswitch.o\n"

    for file in fileList:
        if file=='stat':
            code += "$CC $FLAGS_NETLINK -o "+file+" "+file+".c\n"
        else:
            code += "$CC flexswitch.o "+backend.getCompileModule()+" $FLAGS -o "+file+" "+file+".c\n"
        code += "chmod a+x "+file+"\n"

    return code


# ------------------------------------------------------------ genPyRdReg
def genPyReg(name,hasSlice,hasPort,hasSet,opType):
    sliceTxt = ""
    portTxt=""
    setTxt=""
    sliceTxt=""

    if opType:
        op_type = "rd"
        op_txt = "Read"
    else:
        op_type = "wr"
        op_txt = "Write"

    slice_pfix = ''
    if hasSlice:
        sliceTxt = " slice,"
        slice_pfix = '_slice'
    if hasPort:
        portTxt = " port,"
    if hasSet:
        setTxt = " set,"

    py =  "def "+op_type+"_"+name+slice_pfix+"( "+sliceTxt+portTxt+setTxt+" data ):\n"
    py += "    flxsw."+op_type+"_"+name+".restype=func_ret_type\n"
    py += "    ret = flxsw."+op_type+"_"+name+slice_pfix+"( device_ptr, "+sliceTxt+portTxt+setTxt+"ctypes.byref(data))\n"
    py += "    if func_ret_type: assert ret==0, \""+op_txt+" function "+name+" failed.\"\n"
    py += "    \n"

    if hasSlice:
        py +=  "def "+op_type+"_"+name+"( "+portTxt+setTxt+" data ):\n"
        py += "    flxsw."+op_type+"_"+name+".restype=func_ret_type\n"
        py += "    ret = flxsw."+op_type+"_"+name+"( device_ptr, "+portTxt+setTxt+"ctypes.byref(data))\n"
        py += "    if func_ret_type: assert ret==0, \""+op_txt+" function "+name+" failed.\"\n"
        py += "    \n"
    return py

# ------------------------------------------------------------ genPyRdReg
def genPyTab(name,hasSlice,hasPort,hasSet,opType):
    sliceTxt = ""
    portTxt=""
    sliceTxt=""
    setTxt=""

    if opType:
        op_type = "rd"
        op_txt = "Read"
    else:
        op_type = "wr"
        op_txt = "Write"

    slice_pfix = ''
    if hasSlice:
        sliceTxt = " slice,"
        slice_pfix = '_slice'
    if hasPort:
        portTxt = " port,"
    if hasSet:
        setTxt = " set,"

    py =  "def "+op_type+"_"+name+slice_pfix+"( "+sliceTxt+portTxt+setTxt+"idx , data ):\n"
    py += "    flxsw."+op_type+"_"+name+slice_pfix+".restype=func_ret_type\n"
    py += "    ret = flxsw."+op_type+"_"+name+slice_pfix+"( device_ptr, "+sliceTxt+portTxt+setTxt+"idx, ctypes.byref(data)"")\n"
    py += "    if func_ret_type: assert ret==0, \""+op_txt+" function "+name+" failed.\"\n"
    py += "    \n"

    if hasSlice:
        py +=  "def "+op_type+"_"+name+"( "+portTxt+setTxt+"idx , data ):\n"
        py += "    flxsw."+op_type+"_"+name+".restype=func_ret_type\n"
        py += "    ret = flxsw."+op_type+"_"+name+"( device_ptr, "+portTxt+setTxt+"idx, ctypes.byref(data)"")\n"
        py += "    if func_ret_type: assert ret==0, \""+op_txt+" function "+name+" failed.\"\n"
        py += "    \n"
    return py

# ------------------------------------------------------------ genPyApi
def genPyApi(gRegs,backend,definedUintSize):
    py = """#
# (C) Packet Architects AB.
#
# FlexSwitch python API
#
import ctypes
import os

func_ret_type = None # set to ctypes.c_int if C library is compiled with return values

def initFlexSwitch( libso_path=None ):
    \"\"\" Initialize driver and load dynamic library with the C-API functions. \"\"\"
    global flxsw
    global device_ptr
    if not libso_path:
        try:
            flxsw = ctypes.cdll.LoadLibrary('./libflexswitch.so')
        except OSError:
            this_dir = os.path.dirname(os.path.abspath(__file__))
            flxsw = ctypes.cdll.LoadLibrary(
                this_dir + '/libflexswitch.so')
    else:
        flxsw = ctypes.cdll.LoadLibrary(libso_path)
    flxsw.init_mem_driver.restype = ctypes.c_void_p
    device_ptr = ctypes.c_void_p( flxsw.init_mem_driver() )

# -----
# If the C-API has been compiled with debug prints these functions
# turns on/off the prints.
def enable_api_prints():
  print_enable = ctypes.c_bool.in_dll(flxsw, "flexsw_print_enable")
  print_enable.value = 1

def disable_api_prints():
  print_enable = ctypes.c_bool.in_dll(flxsw, "flexsw_print_enable")
  print_enable.value = 0

# -----
# These are only implemented in Verilator models to control
# burst write mode in the device_read_write functions.
def set_burst_mode():
    flxsw.set_burst_mode()

def clr_burst_mode():
    flxsw.clr_burst_mode()
# -----

# --------- definitions of all structs used by the C-API -------
"""
    for reg in gRegs:
        py += genStruct(reg,
                        fields=gRegs[reg]['Fields'],
                        backend=backend,
                        c_code=False,
                        definedUintSize=definedUintSize,
                        use_bitfields=False,
                        field_widths={})
    py += """

# --------- wrappers for all C-API functions -------
"""
    for reg in gRegs:
        if gRegs[reg]['Single']:
            if 'r' in gRegs[reg]['Type']:
                py += genPyReg(reg,
                               hasSlice=gRegs[reg]['hasSlice'],
                               hasPort=gRegs[reg]['hasPort'],
                               hasSet=gRegs[reg]['hasSet'],
                               opType=True)

            if 'w' in gRegs[reg]['Type']:
                py += genPyReg(reg,
                               hasSlice=gRegs[reg]['hasSlice'],
                               hasPort=gRegs[reg]['hasPort'],
                               hasSet=gRegs[reg]['hasSet'],
                               opType=False)
        else:
            if 'r' in gRegs[reg]['Type']:
                py += genPyTab(reg,
                               hasSlice=gRegs[reg]['hasSlice'],
                               hasPort=gRegs[reg]['hasPort'],
                               hasSet=gRegs[reg]['hasSet'],
                               opType=True)

            if 'w' in gRegs[reg]['Type']:
                py += genPyTab(reg,
                               hasSlice=gRegs[reg]['hasSlice'],
                               hasPort=gRegs[reg]['hasPort'],
                               hasSet=gRegs[reg]['hasSet'],
                               opType=False)


    py += """
# -------- database with information for each register ---------
class FlexswitchDB(object):
    registers = {
"""
    for reg in gRegs:
        fields = ''
        for f in  field_db[reg]:
            fields += '                                    "{0}" : {1},\n'.format(f,field_db[reg][f])


        # when a register has both slice and port then we need a list of ports per
        # slice
        if gRegs[reg]['hasSlice'] and gRegs[reg]['hasPort']:
            nr_ports = gRegs[reg]['maxPort']+1
            nr_slices = gRegs[reg]['maxSlice']+1
            ports_per_slice = [0] * nr_slices
            for slc in range(nr_slices):
                portLst = getPorts(reg,slc)
                port_min = min(portLst)
                port_max = max(portLst)
                nr = port_max - port_min + 1
                ports_per_slice[slc] = nr
            nr_ports = ports_per_slice
        else:
            nr_ports = gRegs[reg]['maxPort']+1

        # TODO: add address but need to cleanup createName so that it always
        #       returns integers
        #           'address'    : {address},
        py += """
        '{name}': {{
                   'nr_slices'  : {slices},
                   'nr_ports'   : {ports},
                   'nr_sets'    : {sets},
                   'nr_entries' : {entries},
                   'rd_func'    : {rd_func},
                   'wr_func'    : {wr_func},
                   'type'       : '{type}',
                   'struct'     : {name},
                   'fields'     : {{
{fields}
                                  }}
                   }},
        """.format( slices=gRegs[reg]['maxSlice']+1 if gRegs[reg]['hasSlice'] else 0,
                    ports=nr_ports if gRegs[reg]['hasPort'] else 0,
                    sets=gRegs[reg]['maxSet']+1 if gRegs[reg]['hasSet'] else 0,
                    entries=gRegs[reg]['Entries'] if not gRegs[reg]['Single'] else 0,
                    #address=gRegs[reg]['Address'],
                    rd_func = 'rd_'+reg if 'r' in gRegs[reg]['Type'] else 'None',
                    wr_func = 'wr_'+reg if 'w' in gRegs[reg]['Type'] else 'None',
                    type=gRegs[reg]['Type'],
                    name=reg,
                    fields=fields)
    py += "    }\n\n"
    py += "    statistics = {\n"

    for reg in gRegs:
        if gRegs[reg]['Statistics']:
            py += "        '{}': 1,\n".format(reg)
    py += "    }\n"

    res = '    core_parameters = '
    res += pprint.pformat(params,indent=8)
    py += res

    return py


# +---------------------------------------------------+
# |--------------------- Main ------------------------|
# +---------------------------------------------------+
if __name__ == '__main__':
    # Parse arguments.
    parser = argparse.ArgumentParser(description='Create c or py api.')

    parser.add_argument('--capi', action='store_true',
                        help='create C-API.')

    parser.add_argument('--cappl', action='store_true',
                        help='create minimal C application')

    parser.add_argument('--cappl-old', action='store_true',
                        help='create C Applications for reading,writing tables and registers.')

    parser.add_argument('--pyapi', action='store_true',
                        help='create Py-API.')

    parser.add_argument('--backend', type=str,
                        help='Which backend function to use. Check under /api/genBackend for backends.')

    parser.add_argument('--output', type=str,
                        help='Output directory. Default is ./output/api')

    parser.add_argument('--ow', action='store_true',
                        help='Overwrite output directory.')

    parser.add_argument('--lowercap', action='store_true',
                        help='only use lower cap letters in functions.')

    parser.add_argument('--underscore', action='store_true',
                        help='Use underscore to seperate words.')

    parser.add_argument('--intSize', type=str,
                        help='The size of the APIs unsigned integers. Default it is 64 bits.')

    parser.add_argument('yaml_file', type=str, nargs=1,
                        help='Register database Yaml File to use.')

    parser.add_argument('config_file', type=str,nargs=1,
                        help='config yaml file to use for parameters.')

    args = parser.parse_args()

    if dbg:
        print("nr of arguments:"+str(len(sys.argv)))

    if len(sys.argv)<2:
        parser.print_help()

    if args.intSize:
        intSize = int(args.intSize)
    else:
        intSize = 32

    if args.backend==None:
        print("Please specify a bankend type to use")
        exit(-1)

    backend_name = args.backend
    outputDir = gitpath.root() + "/output/api"

    if args.output!=None:
        print(args.output)
        outputDir = args.output
        print("Output is now:"+outputDir)

    outputDirC = outputDir+"/c/"
    outputDirPy = outputDir+"/py/"

    print("Output is in directory:"+outputDir)

    if os.path.exists(outputDir) and not args.ow:
        #if args.ow:
        #    print "Clearing directory for overwrite."
        #    shutil.rmtree(outputDir)
        #    os.mkdir(outputDir,0o777)
        #    if args.capi:
        #        os.mkdir(outputDirC,0o777)
        #    if args.pyapi:
        #        os.mkdir(outputDirPy,0o777)
        #else:
        print("ERROR: Existing output directory, not overwriting.")
        exit(-1)
    else:
        #os.mkdir(outputDir,0o777)
        #if args.capi:
        #    os.mkdir(outputDirC,0o777)
        #if args.pyapi:
        #    os.mkdir(outputDirPy,0o777)
        if args.capi:
            if not os.path.exists(outputDirC):
                os.makedirs(outputDirC,0o777)
        if args.pyapi:
            if not os.path.exists(outputDirPy):
                os.makedirs(outputDirPy,0o777)

    # create the backend selected
    if backend_name == 'memBackend_16' or backend_name == 'emptyMemBackend_16':
        backend = api.genBackend.emptyMemBackend_16.genReal()
        test_backend = backend
    elif backend_name == 'memBackend_32' or backend_name == 'emptyMemBackend_32':
        backend = api.genBackend.emptyMemBackend_32.genReal()
        test_backend = backend
    elif backend_name == 'apb_32':
        backend = api.genBackend.apb_32.genReal()
        test_backend = backend
    elif backend_name == 'apb_64':
        backend = api.genBackend.apb_64.genReal()
        test_backend = backend
    elif backend_name == 'memBackend_64' or backend_name == 'emptyMemBackend_64':
        backend = api.genBackend.emptyMemBackend_64.genReal()
        test_backend = backend
    elif backend_name == 'nb_32' :
        backend = api.genBackend.nb_32.genReal()
        test_backend = api.genBackend.emptyMemBackend_32.genReal()
    elif backend_name == 'nb_64' :
        backend = api.genBackend.nb_64.genReal()
        test_backend = api.genBackend.emptyMemBackend_64.genReal()
    elif backend_name == 'nb' :
        backend = api.genBackend.nb.genReal()

    wsize = backend.getWordSize()
    uint = "uint"+str(wsize)+"_t"

    # Load YML file.

    print(f"Arguments: {args}")
    print(f"Loading Yml file: {args.yaml_file[0]}")
    c = yaml.load_all( open(args.yaml_file[0], 'r'), Loader=yaml.Loader )
    slices={}

    with open(args.config_file[0], 'r') as F:
        config = yaml.load(F.read(), Loader=yaml.Loader)
        
    params = config['defines']
    hwconf = config['hwconf']

    has_fsm = hwconf['ipp_fsm']
    print(f"Has FSM: {has_fsm} {hwconf['ipp_fsm']}")

    # Python code to generate.
    py_code =  "## (C) Packet Architects AB\n"
    py_code += "## Generated code for python interfaces.\n"

    # -----------------------------------------
    #   Loop to walk through registers/tables
    # -----------------------------------------

    print("Walking through all register / table entries to create slice information")
    for t in c:
        if 'conf bus 0' in t:
            regs = t['conf bus 0']
            for reg in regs:
                sliceReg = reg.split()
                (regName,hasSlice,sliceId,hasPort,portId,hasSet,setId) = createName(reg,args.lowercap,args.underscore)
                if debug:
                    print("Processing: "+reg+"| Internal name:"+regName+" Set:"+str(hasSet)+" Port:"+str(hasPort)+" Port Id:"+str(portId)+" slice Id:"+str(sliceId))

                rw_type = regs[reg]['Type']

                if 'Statistics' not in regs[reg]:
                    statistics = None
                else:
                    statistics = regs[reg]['Statistics']              
                createReg(name=regName,
                          fields=regs[reg]['Fields'],
                          address=regs[reg]['Start address'],
                          parts=regs[reg]['Address step'],
                          width=regs[reg]['Width'],
                          entries=regs[reg]['Number of entries'],
                          rtype=rw_type,
                          statistics = statistics,
                          single=(regs[reg]['Number of entries']==1),
                          hasSlice=hasSlice,
                          sliceId = sliceId,
                          hasPort=hasPort,
                          portId=portId,
                          hasSet=hasSet,
                          setId=setId,
                          backend=backend)

    mergeSliceEntries()
    removeSingleSliceIndexes()

    print("Writing output files")

    
    # ----------- common part, the flexswitch C-code ----------
    if args.capi or args.cappl or args.cappl_old or args.pyapi:
        print("Creating Capi code")
        fileList = []
        print("Writing flexswitch.c,h")
        if args.capi:
            (h_code,c_code) = genCapi(gRegs,backend,intSize,use_bitfields=True,field_db={}, hwconf_yml=config)
            writeFile(outputDirC,"flexswitch.h",h_code,None)
            writeFile(outputDirC,"flexswitch.c",c_code,None)
            print("Writing flexswitch_fields.c,h")
            c_fields = genFieldCapi(gRegs,backend,intSize)
            #writeFile(outputDirC,"flexswitch_fields.h",h_fields,None)
            writeFile(outputDirC,"flexswitch_fields.h",c_fields,None)
        if args.pyapi:
            # we can not use bit fields in the C structs when there is a Python API on top.
            field_db = {} # used in FlexswitchDB
            (h_code,c_code) = genCapi(gRegs,backend,intSize,use_bitfields=False,field_db=field_db, hwconf_yml=config)
            writeFile(outputDirPy,"flexswitch.h",h_code,None)
            writeFile(outputDirPy,"flexswitch.c",c_code,None)
    
    ## ------------ C-API -------------------------------------
    #if args.capi:
    #    print("Copy C and H code files")
    #    backend.copyCFiles(outputDirC)

    #    objs = backend.objFiles()
    #    objs += ' flexswitch.o'

    #    compScript = genCompScript()
    #    compScript += backend.compileCommands()
    #    compScript += "$CC $CFLAGS -c flexswitch.c\n"
    #    compScript += "$CC $CFLAGS -c flexswitch_fields.c\n"
    #    objs += ' flexswitch_fields.o'
    #    # read/write test
    #    writeFile(outputDirC,"wr_rd_test.c",
    #        genReadWriteTest(
    #            gRegs,
    #            backend=test_backend, # can not user real device backend for test
    #            definedUintSize=intSize),fileList)
    #    compScript += "$CC $CFLAGS -o wr_rd_test wr_rd_test.c flexswitch.o\n"

    #    if args.cappl:
    #        print("Creating minimal c-applications",backend_name)
    #        fileList = []

    #        # if backend start with 'mem*' it needs a malloced memory
    #        # area and an init_mem_driver call to be able to compile.
    #        malloced_mem = backend_name.find('memBackend_') == 0

    #        # Then check version code
    #        print(" - Writing check core application", malloced_mem)
    #        writeFile(outputDirC,"chkversion.c",genCheckVersion(gRegs,test_backend,malloced_mem),fileList)

    #        for f in fileList:
    #            compScript += '$CC $CFLAGS {objs} {src} -o {aout}\n'.format(
    #              objs = objs,
    #              src = f + '.c',
    #              aout = f )
    #    # ---------------------------------------------------------

    #    print(" - Creating test script.")
    #    testScript = genTestScript(pyapi=False)
    #    writeFile(outputDirC,"test.sh",testScript,None,exe=True)

    #    print(" - Creating cleanup script.")
    #    fileList += ['wr_rd_test']
    #    cleanScript = genCleanScript(fileList,backend,args.pyapi,args.capi)
    #    writeFile(outputDirC,"clean.sh",cleanScript,None,exe=True)

    #    print(" - Creating compiler script.")
    #    writeFile(outputDirC,"comp.sh",compScript,None,exe=True)
    #    

    ## ------------------- Python API ----------------------------------
    #if args.pyapi:
    #    print("Copy C and H code files")
    #    backend.copyCFiles(outputDirPy)

    #    compScript = genCompScript()
    #    compScript += "$CC $CFLAGS -c flexswitch.c\n"
    #    # read/write test
    #    writeFile(outputDirPy,"wr_rd_test.c",
    #        genReadWriteTest(
    #            gRegs,
    #            backend=test_backend, # can not user real device backend for test
    #            definedUintSize=intSize),fileList)
    #    compScript += "$CC $CFLAGS -o wr_rd_test wr_rd_test.c flexswitch.o\n"

    #    print("Creating Python API")
    #    py_code = genPyApi(gRegs,backend,intSize)
    #    writeFile(outputDirPy,"flexswitch.py",py_code,None)
    #    py_code = genCheckPyVersion(gRegs,backend)
    #    writeFile(outputDirPy,"chkversion.py",py_code,None,exe=True)
    #    py_code = ""
    #    writeFile(outputDirPy,"__init__.py",py_code,None)
    #    shutil.copy2("./api/rdreg.py",outputDirPy)
    #    shutil.copy2("./api/wrreg.py",outputDirPy)

    #    compScript += "$CC $CFLAGS -g -fPIC -shared -o libflexswitch.so {dev_rdwr} flexswitch.c -lc\n".format(
    #                       dev_rdwr = backend.getSourceFile() )

    #    print(" - Creating compiler script.")
    #    writeFile(outputDirPy,"comp.sh",compScript,None,exe=True)

    #    print(" - Creating test script.")
    #    testScript = genTestScript(pyapi=True)
    #    writeFile(outputDirPy,"test.sh",testScript,None,exe=True)

    #    print(" - Creating cleanup script.")
    #    cleanScript = genCleanScript(['wr_rd_test'],backend,args.pyapi,args.capi)
    #    writeFile(outputDirPy,"clean.sh",cleanScript,None,exe=True)

