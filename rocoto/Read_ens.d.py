#!/usr/bin/env python

import os
import numpy as np
import struct


sFile = "nc2020112000_00_ens.d"
sFile = "cm2020112000_00_ens.d"

print(sFile)
#144, 37, 84, 20

ND=144*37*84*20
ND=15
fh = open(sFile, 'rb')



f = struct.unpack('>f', fh.read(4))
print(f)

for k in range(ND): #144*37*84*20):
	f = struct.unpack('>f', fh.read(4))
	print(k, f)

f = struct.unpack('>f', fh.read(4))
print(f)

f = struct.unpack('>f', fh.read(4))
print(f)


#f = struct.unpack('>f', fh.read(4))
#print(f)

#f = struct.unpack('>f', fh.read(4))
#print(f)


fh.close()
