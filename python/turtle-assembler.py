import re

# Test 'Ri' variants before 'd', since the latter implies the first.
label_tk = re.compile('\s*(?P<name>\w+):')
move1_tk = re.compile('\s*move\s+(?P<Ri>r\d)')
move2_tk = re.compile('\s*move\s+(?P<c>-?\w+)')
turn1_tk = re.compile('\s*turn\s+(?P<Ri>r\d)')
turn2_tk = re.compile('\s*turn\s+(?P<d>-?\w]+)')
load_tk = re.compile('\s*load\s+(?P<Ri>r\d)\s+(?P<n>-?\w+)')
add_tk = re.compile('\s*add\s+(?P<Ri>r\d)\s+(?P<Rj>r\d)')
trace1_tk = re.compile('\s*trace\s+(?P<Ri>r\d)')
trace2_tk = re.compile('\s*trace\s+(?P<d>-?\w+)')
beq_tk = re.compile('\s*beq\s+(?P<Ri>r\d)\s+(?P<Rj>r\d)\s+(?P<a>\w+)')
bne_tk = re.compile('\s*bne\s+(?P<Ri>r\d)\s+(?P<Rj>r\d)\s+(?P<a>\w+)')
bge_tk = re.compile('\s*bge\s+(?P<Ri>r\d)\s+(?P<Rj>r\d)\s+(?P<a>\w+)')
bgt_tk = re.compile('\s*bgt\s+(?P<Ri>r\d)\s+(?P<Rj>r\d)\s+(?P<a>\w+)')
halt_tk = re.compile('\s*halt')

def tokenize_line(line):
    # Drop comments
    line = re.sub(r';.*', '', line)

    # Ignore lines with only whitespace
    if re.match('^\s*$', line):
        return

    # Tokenize
    m = label_tk.match(line)
    if m:
        return dict(type= 'label', name= m.group('name'))

    m = move1_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'move_Ri',
                    Ri= m.group('Ri'))

    m = move2_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'move_c',
                    c= m.group('c'))

    m = turn1_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'turn_Ri',
                    Ri= m.group('Ri'))

    m = turn2_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'turn_d',
                    d= m.group('d'))

    m = load_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'load',
                    Ri= m.group('Ri'), n= m.group('n'))

    m = add_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'add',
                    Ri= m.group('Ri'), Rj= m.group('Rj'))

    m = trace1_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'trace_Ri',
                    Ri= m.group('Ri'))

    m = trace2_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'trace_d',
                    d= m.group('d'))

    m = beq_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'beq',
                    Ri= m.group('Ri'), Rj= m.group('Rj'),
                    a= m.group('a'))

    m = beq_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'beq',
                    Ri= m.group('Ri'), Rj= m.group('Rj'),
                    a= m.group('a'))

    m = bne_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'bne',
                    Ri= m.group('Ri'), Rj= m.group('Rj'),
                    a= m.group('a'))

    m = bge_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'bge',
                    Ri= m.group('Ri'), Rj= m.group('Rj'),
                    a= m.group('a'))

    m = bgt_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'bgt',
                    Ri= m.group('Ri'), Rj= m.group('Rj'),
                    a= m.group('a'))

    m = halt_tk.match(line)
    if m:
        return dict(type= 'instr', opcode= 'halt')

    raise SyntaxError('Syntax error on line %s' % line)


def tokenize_file(filename):
    with open(filename, "r") as f:
        r = []
        for line in f:
            l = tokenize_line(line)
            if l:
                r.append(l)
    return r

def num(s, bits=6):
    n = int(s, base=0)
    b = 2**bits
    if n < b:
        return n
    else:
        raise ValueError('%s does not fit in %s bits' % (n, bits))

def reg(s):
  r = int(s[1], base=10)
  if 0 <= r <= 7:
      return r
  else:
      raise ValueError('Unknown register %s' % s)

def genlabels(lines):
    labels = dict()
    pc = 0
    instrs = []
    for l in lines:
        if l['type'] == 'label':
            labels[l['name']] = pc
        else:
            instrs.append(l)
            pc += 1
    return (instrs, labels)

def gencode(instrs, labels):
    codes = []
    for (i, pc) in zip(instrs, range(len(instrs))):
        if i['opcode'] == 'move_c':
            codes.append((0x0, 0, 0, num(i['c'], 6)))

        elif i['opcode'] == 'move_Ri':
            codes.append((0x1, reg(i['Ri']), 0, 0))

        elif i['opcode'] == 'turn_d':
            codes.append((0x2, 0, 0, num(i['d'], 2)))

        elif i['opcode'] == 'turn_Ri':
            codes.append((0x3, reg(i['Ri']), 0, 0))

        elif i['opcode'] == 'load':
            codes.append((0x4, reg(i['Ri']), 0, num(i['n'], 6)))

        elif i['opcode'] == 'add':
            codes.append((0x5, reg(i['Ri']), reg(i['Rj']), 0))

        elif i['opcode'] == 'trace_d':
            codes.append((0x6, 0, 0, num(i['d'], 1)))

        elif i['opcode'] == 'trace_Ri':
            codes.append((0x7, 0, 0, reg(i['Ri'])))

        elif i['opcode'] == 'beq':
            codes.append((0x8, reg(i['Ri']), reg(i['Rj']),
                          labels[i['a']] - pc - 1))

        elif i['opcode'] == 'bne':
            codes.append((0x9, reg(i['Ri']), reg(i['Rj']),
                          labels[i['a']] - pc - 1))

        elif i['opcode'] == 'bge':
            codes.append((0xA, reg(i['Ri']), reg(i['Rj']),
                          labels[i['a']] - pc - 1))

        elif i['opcode'] == 'bgt':
            codes.append((0xB, reg(i['Ri']), reg(i['Rj']),
                          labels[i['a']] - pc - 1))

        elif i['opcode'] == 'halt':
            codes.append((0xF, 0, 0, 0))

    return codes

# Pack lines into 16-bit words
def pack(codes):
    r = ""
    for c in codes:
        r += "%X" % ((c[0] & 0b1111) << 12
                    |(c[1] & 0b111)  << 9
                    |(c[2] & 0b111)  << 6
                    |(c[3] & 0b111111))
    return r

def run():
    import sys
    f = sys.argv[1]
    print(pack(gencode(*genlabels(tokenize_file(f)))))

if __name__ == '__main__': run()

# Debug
# print(tokenize_file("snake"))
# print(gencode(*genlabels(tokenize_file("snake"))))
# print(pack(gencode(*genlabels(tokenize_file("snake")))))
