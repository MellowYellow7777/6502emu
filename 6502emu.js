/*

vvvvvvv the correct implementation
of adc with decimal mode.. finally

  var aa = a[i];
  var bb = b[i];
  var as = a[i] >> 7 & 1;
  var bs = b[i] >> 7 & 1;
  var zr = (aa + bb + cin) & 0xff === 0
  var al = aa & 0x0f;
  var bl = bb & 0x0f;
  var rl = al + bl + cin;
  var cl = rl > 0x09;
  if (cl) rl = (rl + 0x06) & 0x0f;
  var ah = aa >> 4 & 0x0f;
  var bh = bb >> 4 & 0x0f;
  var rh = ah + bh + cl;
  var ch = rh > 0x09;
  var ng = rh >> 3 & 1;
  var ov = ~(as ^ bs) & (as ^ ng) & 1;
  if (ch) rh = (rh + 0x06) & 0x0f;
  var r = rh << 4 | rl;
  var cout = ch;


function ADC(v) { // D = 1
  var ci = PS & 1;
  var as = A >> 7 & 1;
  var bs = v >> 7 & 1;
  var ZF = (A + v + ci) & 0xff === 0;
  var al = A & 0x0f;
  var bl = v & 0x0f;
  var rl = al + bl + ci;
  var cl = rl > 0x09;
  if (cl) rl = (rl + 0x06) & 0x0f;
  var ah = A >> 4 & 0x0f;
  var bh = v >> 4 & 0x0f;
  var rh = ah + bh + cl;
  var CF = rh > 0x09;
  var NF = rh >> 3 & 1;
  var VF = ~(as ^ bs) & (as ^ NF) & 1;
  if (CF) rh = (rh + 0x06) & 0x0f;
  var r = rh << 4 | rl;
  A = r;
  PS = PS & 0b00111100
     | NF << 7
     | VF << 6
     | ZF << 1
     | CF;
}






// verbose

// r8 holds the 8 bit registers
// A, X, Y, SP, PS
// r16 holds the 16 bit register(s)
// PC
// mem holds the 64kb ram

// typed arrays are convenient (and fast)
// because they automatically honor bit-widths

var r8 = new Uint8Array(8);
var r16 = new Uint16Array(8);
var RAM = new Uint8Array(0x10000);
// note: RAM is used here,
//       mem is used in the actual code

// pseudo properties
// these are replaced in the actual code
// ie. A -> r8[0]
//     X -> r8[1]
//     etc.
Object.defineProperties(this, {
  A: {
    get() { return r8[0]; },
    set(v) { r8[0] = v; },
  },
  X: {
    get() { return r8[1]; },
    set(v) { r8[1] = v; },
  },
  Y: {
    get() { return r8[2]; },
    set(v) { r8[2] = v; },
  },
  SP: {
    get() { return r8[3]; },
    set(v) { r8[3] = v; },
  },
  PS: {
    get() { return r8[4]; },
    set(v) { r8[4] = v; },
  },
  PC: {
    get() { return r16[0]; },
    set(v) { r16[0] = v; },
  },
});

// note: all functions are flattened in code
//       up to the 'instruction' lut
//       ie. nextByte() -> RAM[PC++]

// fetch opcode or single byte argument
function nextByte() {
  return RAM[PC++];
}

// fetch 2 byte argument
function next2Bytes() {
  return RAM[PC++] | RAM[PC++] << 8;
}

// fetch 2 bytes at address
function read2Bytes(address) {
  return RAM[address] | RAM[address + 1] << 8;
}

// fetch 2 bytes at zero page address
function read2Byteszpg(address) {
  return RAM[address & 0xff] | RAM[address + 1 & 0xff] << 8;
}

// fetch immediate value (next byte)
var readRAMimm = nextByte;

// fetch zero page value
function readRAMzpg() {
  return RAM[nextByte()];
}

// fetch zero page + x value
function readRAMzpx() {
  return RAM[nextByte() + X & 0xff];
}

// fetch zero page + y value
function readRAMzpy() {
  return RAM[nextByte() + Y & 0xff];
}

// fetch absolute value
function readRAMabs() {
  return RAM[next2Bytes()];
}

// fetch absolute + x value
function readRAMabx() {
  return RAM[next2Bytes() + X]
}

// fetch absolute + y value
function readRAMaby() {
  return RAM[next2Bytes() + Y]
}

// indirect x is retrieved via
// zp = next byte
// lo = mem[zp + x]
// hi = mem[zp + x + 1] (note: wraps in zero page)
// pt = lo | hi << 8
// return mem[pt]
function readRAMinx() {
  return RAM[read2Byteszpg(nextByte() + X)];
}

// indirect y is retrieved via
// zp = next byte
// lo = mem[zp]
// hi = mem[zp + 1] (note: wraps in zero page)
// pt = (lo | hi << 8) + y
// return mem[pt]
function readRAMiny() {
  return RAM[read2Byteszpg(nextByte()) + Y];
}

// fetch next byte as signed 8 bit value
// note: used only for branch instructions
function readRAMrel() {
  return (nextByte() << 24) >> 24;
}

// getIDX = readRAM without reading the value
// ie. readRAMzp returns RAM[nextByte()]
//      getIDXxp returns     nextByte()
var getIDXzpg = nextByte;

// fetch zero page + x pointer
function getIDXzpx() {
  return nextByte() + X & 0xff
}

// fetch zero page + y pointer
function getIDXzpy() {
  return nextByte() + Y & 0xff
}

// fetch absolute pointer
var getIDXabs = next2Bytes;

// fetch absolute + x pointer
function getIDXabx() {
  return next2Bytes() + X
}

// fetch absolute + y pointer
function getIDXaby() {
  return next2Bytes() + Y
}

// fetch indirect pointer
// note: used only for jmp ($xxxx) instructions
//       the bug is reproduced via some bw ops
function getIDXind() {
  var p = next2Bytes();
  return RAM[p] | RAM[(p & 0xff00) | ((p + 1) & 0xff)] << 8;
}

// fetch indirect x pointer
// note: see readRAMinx for alg
//       "pt" is returned here
//       instead of "mem[pt]"
function getIDXinx() {
  return read2Byteszpg(nextByte() + X);
}

// fetch indirect y pointer
// note: see readRAMiny for alg
//       "pt" is returned here
//       instead of "mem[pt]"
function getIDXiny() {
  return read2Byteszpg(nextByte()) + Y;
}

// push a value to stack
// note: SP-- returns SP pre decrement
function pushStack(value) {
  RAM[0x100 | SP--] = value;
}

// pull a value from stack
// note: ++SP returns SP post increment
//       although ++SP returns SP + 1 (unwrapped),
//       in case of SP = 255, ++SP would return 256
//       but we are already doing an or with 0x100 (256)
//       so that can just be ignored here
function pullStack() {
   return RAM[0x100 | ++SP];
}

// bit masks for flag positions in PS
// note: all flags and ops involving flags
//       are reduced fully in code
//       ie. PS & ~(FLAG_N | FLAG_Z)
//        -> PS & 125
var FLAG_C = 0b00000001
var FLAG_Z = 0b00000010
var FLAG_I = 0b00000100
var FLAG_D = 0b00001000
var FLAG_B = 0b00010000
var FLAG_U = 0b00100000
var FLAG_V = 0b01000000
var FLAG_N = 0b10000000

// unused helper
function setFlag(flag, value) {
  PS = (PS & ~flag) | (-value & flag);
}

// very common to set N and Z flags
// note: v - 1 >> 7 results in
//       0b11111111 for zero
//       0b00000000 for all other 8 bit values
function setNZflags(v) {
  PS = PS & ~(FLAG_N | FLAG_Z)
       | FLAG_N & v
       | FLAG_Z & v - 1 >> 7;
}

// abstraction to support memory mapped output
// note: this is also flattened in code,
//       but likely will be re-abstracted in future
//       to support more general memory mapping
//       currently $0200-$05ff maps to 32x32 display
function writeRAM(address,value) {
  if (0x0200 <= address && address < 0x600) {
    drawPixel(address,value);
  }
  return RAM[address] = value;
}

// general instruction functions
// note: parameter name
//       v is used for "readRAM" values
//       p is used for "getIDX" pointers
// note: many of these (as well as flag upds)
//       are replaced in code with luts which
//       hold precalculated values.

// load a,x,y with value
function LDA(v) { setNZflags(A = v); }
function LDX(v) { setNZflags(X = v); }
function LDY(v) { setNZflags(Y = v); }

// store a,x,y at pointer
function STA(p) { writeRAM(p,A); }
function STX(p) { writeRAM(p,X); }
function STY(p) { writeRAM(p,Y); }

// transfers
function TAX() { setNZflags(X = A); }
function TAY() { setNZflags(Y = A); }
function TXA() { setNZflags(A = X); }
function TYA() { setNZflags(A = Y); }
function TSX() { setNZflags(X = SP);}
// txs is the only transfer that doesnt affect flags
function TXS() { SP = X; }

// push a to stack
function PHA() { pushStack(A); }

// push ps to stack
// note: the value pushed always has B and U bits = 1
function PHP() {
  pushStack(PS | FLAG_B | FLAG_U);
}

// pull a from stack, set flags
function PLA() { setNZflags(A = pullStack()); }

// pull ps from stack
// note: when pushing PS to the stack, B and U bits are always
//       handled explicitly. so although you can technically
//       pull values with modified B and/or U flags,
//       it ultimately has no effect in software.
//  see: PHP, BRK, IRQ, NMI, RTI
function PLP() {
  PS = pullStack();
}

// inc/dec x/y, set flags
// note: ++x does x = x + 1, which returns x + 1
//       --x does x = x - 1, which returns x - 1
//       meaning x wraps because it is a uint8 array value
//       but the returned value does not, so & 0xff is needed
function INX() { setNZflags(++X & 0xff); }
function INY() { setNZflags(++Y & 0xff); }
function DEX() { setNZflags(--X & 0xff); }
function DEY() { setNZflags(--Y & 0xff); }

// inc/dec value at pointer, set flags
function INC(p) { setNZflags(writeRAM(p, RAM[p] + 1)); }
function DEC(p) { setNZflags(writeRAM(p, RAM[p] - 1)); }

// unfortunately the most basic operation
// is the most complicated...
// add value to A (with carry)
function ADC(v) {
  var c = PS & FLAG_C, r;

  if (PS & FLAG_D) {   // decimal mode
    r = (A & 0x0f) + (v & 0x0f) + c; // add lower 4 bits with carry
    if (r > 0x09) { 
      r = 0x10 | ((r + 6) & 0x0f); // adjust for lower decimal carry
    }
    r += (A & 0xf0) + (v & 0xf0); // add upper 4 bits
    // set final decimal carry via bitwise only solution
    c = r >> 8 | (r >> 7 & (r >> 6 | r >> 5)) & FLAG_C;
    // adjust for higher decimal carry
    r += 0x60 * c;
  } else {
    r = A + v + c;
    c = r >> 8 & FLAG_C; // result is not unwrapped (bit 8 is carry)
  }
  // set flags
  // note: v flag uses bitwise result regardless
  PS = PS & ~(FLAG_C | FLAG_V | FLAG_N | FLAG_Z)
       | FLAG_N & r
       | (~(A ^ v) & (A ^ r) & FLAG_N) >> 1
       | FLAG_Z & r - 1 >> 7
       | c;
  A = r; // r = bitwise result or decimal result if d flag set
}

// subtract from A (with borrow)
function SBC(v) {
  ADC(~v & 0xff);
}

// compare A to value
function CMP(v) {
  var r = A - v;
  PS = PS & ~(FLAG_C | FLAG_N | FLAG_Z)
       | FLAG_N & r
       | FLAG_Z & (r & 0xff) - 1 >> 7
       | FLAG_C & ~r >> 8;
}

// compare X to value
function CPX(v) {
  var r = X - v;
  PS = PS & ~(FLAG_C | FLAG_N | FLAG_Z)
       | FLAG_N & r
       | FLAG_Z & (r & 0xff) - 1 >> 7
       | FLAG_C & ~r >> 8;
}

// compare Y to value
function CPY(v) {
  var r = Y - v;
  PS = PS & ~(FLAG_C | FLAG_N | FLAG_Z)
       | FLAG_N & r
       | FLAG_Z & (r & 0xff) - 1 >> 7
       | FLAG_C & ~r >> 8;
}

// and, or, xor A with value, set flags
function AND(v) { setNZflags(A &= v); }
function ORA(v) { setNZflags(A |= v); }
function EOR(v) { setNZflags(A ^= v); }

// bit instruction works like this:
//   N and V bits (2 msb) are copied from value
//   Z bit is set from result of A & value
function BIT(v) {
  PS = PS & ~(FLAG_N | FLAG_V | FLAG_Z)
       | FLAG_N & v
       | FLAG_V & v
       | FLAG_Z & (A & v) - 1 >> 7
}

// arithmetic shift left (accumulator)
function ASL_A() {
  // grab bit 7 before it gets shifted out
  var c = (A & FLAG_N) >> 7;
  A <<= 1;
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
       | FLAG_N & A
       | FLAG_Z & A - 1 >> 7
       | c;
}

// logical shift right (accumulator)
function LSR_A() {
  // grab bit 1 before it gets shifted out
  var c = A & FLAG_C;
  A >>>= 1;
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
//     | FLAG_N & A commented out bc 0 is shifted into N bit
       | FLAG_Z & A - 1 >> 7
       | c;
}

// rotate left through carry (accumulator)
function ROL_A() {
  // grab bit 7 before it gets shifted out
  var c = (A & FLAG_N) >> 7;
  // A = 0b76543210 , carry = C
  //       ////////
  //  -> 0b6543210C , carry = old bit 7
  A = A << 1 | PS & FLAG_C;
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
       | FLAG_N & A
       | FLAG_Z & A - 1 >> 7
       | c;
}

// rotate right through carry (accumulator)
function ROR_A() {
  // grab bit 0 before it gets shifted out
  var c = A & FLAG_C;
  // A = 0b76543210 , carry = C
  //       \\\\\\\\
  //  -> 0bC7654321 , carry = old bit 0
  A = A >>> 1 | (PS & FLAG_C) << 7;
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
       | FLAG_N & A
       | FLAG_Z & A - 1 >> 7
       | c;
}

// arithmetic shift left (at pointer)
// see: ASL_A
function ASL_M(p) {
  var M = RAM[p];
  var c = (M & FLAG_N) >> 7;
  writeRAM(p, M <<= 1);
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
       | FLAG_N & M
       | FLAG_Z & M - 1 >> 7
       | c;
}

// logical shift right (at pointer)
// see: LSR_A
function LSR_M(p) {
  var M = RAM[p];
  var c = M & FLAG_C;
  writeRAM(p, M >>>= 1);
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
//     | FLAG_N & M
       | FLAG_Z & M - 1 >> 7
       | c;
}

// rotate left through carry (at pointer)
// see: ROL_A
function ROL_M(p) {
  var M = RAM[p];
  var c = (M & FLAG_N) >> 7;
  writeRAM(p, M = M << 1 | PS & FLAG_C);
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
       | FLAG_N & M
       | FLAG_Z & M - 1 >> 7
       | c;
}

// rotate right through carry (at pointer)
// see: ROR_A
function ROR_M(p) {
  var M = RAM[p];
  var c = M & FLAG_C;
  writeRAM(p, M = M >>> 1 | (PS & FLAG_C) << 7);
  PS = PS & ~(FLAG_N | FLAG_Z | FLAG_C)
       | FLAG_N & M
       | FLAG_Z & M - 1 >> 7
       | c;
}

// set/clear flag bits
function SEC() { PS |= FLAG_C; }
function SEI() { PS |= FLAG_I; }
function SED() { PS |= FLAG_D; }
function CLC() { PS &= ~FLAG_C; }
function CLI() { PS &= ~FLAG_I; }
function CLD() { PS &= ~FLAG_D; }
function CLV() { PS &= ~FLAG_V; }

// conditional branches
function BCS(v) { if (PS & FLAG_C) PC += v; }
function BEQ(v) { if (PS & FLAG_Z) PC += v; }
function BVS(v) { if (PS & FLAG_V) PC += v; }
function BMI(v) { if (PS & FLAG_N) PC += v; }
function BCC(v) { if (~PS & FLAG_C) PC += v; }
function BNE(v) { if (~PS & FLAG_Z) PC += v; }
function BVC(v) { if (~PS & FLAG_V) PC += v; }
function BPL(v) { if (~PS & FLAG_N) PC += v; }

// unconditional jump (to pointer)
function JMP(p) { PC = p; }

// jump to subroutine
//     push PC-1 hi byte to stack
//     push PC-1 lo byte to stack
//     jump to pointer
function JSR(p) {
  PC--;
  pushStack(PC >> 8);
  pushStack(PC);
  PC = p;
}

// return from subroutine
//     pull PC-1 from stack (lo byte, then hi byte)
//     and un-subract 1 from it
function RTS() {
  PC = pullStack() + (pullStack() << 8) + 1;
}

// "break" aka software interrupt
// acts like a non-maskable IRQ
//     push PC+1 hi byte to stack
//     push PC+1 lo byte to stack
//     push PS to stack, with U and B flags set (see PLP)
//     set I flag
//     jump to IRQ vector
function BRK() {
  PC++;
  pushStack(PC >>> 8);
  pushStack(PC);
  pushStack(PS | FLAG_U | FLAG_B);
  PS |= FLAG_I;
  PC = read2Bytes(0xfffe);
}

// return from interrupt
//     pull PS from stack
//     pull PC from stack (lo byte, then hi byte)
function RTI() {
  PS = pullStack();
  PC = pullStack() + (pullStack() << 8);
}

// no-op, do nothing
function NOP() {}

// all instructions are defined to the 'instruction' array in code.
// this serves as an lut, given the op-code.
// these all have a comment with the instruction and addressing mode.
// these comments are in the form:
//     // INST mode
// the equivalent function call + argument would be:
//     INST() for implied instructions (which have take no argument)
//     INST(readRAMmode()) for read instructions (INST has a "v" parameter)
//     INST(getIDXmode()) for write instructions (INST has a "p" parameter)
// ie. // BRK imp -> BRK()
//     // LDA zpg -> LDA(readRAMzpg()) (LDA has a "v" parameter)
//     // STA zpg -> STA(getIDXzpg()) (STA has a "p" parameter)
// in code these are all either flattened out, or more likely replaced entirely
// with luts which hold all precalculated values. the following substitutions
// are still made:
//     A -> r8[A]
//     X -> r8[X]
//     Y -> r8[Y]
//    SP -> r8[SP]
//    PS -> r8[PS]
//    PC -> r16[PC]
//   RAM -> mem
// note: A, X, Y, etc. are assigned their own index (0, 1, 2, etc.) within the
//       closure where instructions are defined. this doesnt apply outside of
//       that block.
// all flag masks are substituted and reduced as well,
// ie. (~(FLAG_N | FLAG_Z)) & 0xff -> 125
*/

var r8 = new Uint8Array(8);
var r16 = new Uint16Array(8);
var mem = new Uint8Array(0x10000);
var dbgarr = new Array(0x10000).fill('');
var A = 0;
var X = 1;
var Y = 2;
var SP = 3;
var PS = 4;
var PC = 0;

function step() {
  mem[0xfe] = Math.floor(256*Math.random());
switch (mem[r16[0]++]) {
case 0x00: // BRK imp
  r16[PC]++;
  mem[0x100 | r8[SP]--] = r16[PC] >>> 8;
  mem[0x100 | r8[SP]--] = r16[PC];
  mem[0x100 | r8[SP]--] = r8[PS] | 16;
  r8[PS] |= 4;
  r16[PC] = mem[0xfffe] | mem[0xffff] << 8;
  break;
case 0x01: // ORA inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0x02: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x03: // SLO inx
  var a = mem[r16[PC]++] + r8[X];
  var p = mem[a & 0xff] | mem[a + 1 & 0xff] << 8;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x04: // NOP zpg
  r16[PC]++;
  break;
case 0x05: // ORA zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[mem[r16[PC]++]]];
  break;
case 0x06: // ASL zpg
  var p = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  mem[p] = asl_lut[mem[p]];
  break;
case 0x07: // SLO zpg
  var p = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x08: // PHP imp
  mem[0x100 | r8[SP]--] = r8[PS] | 48;
  break;
case 0x09: // ORA imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[r16[PC]++]];
  break;
case 0x0A: // ASL acc
  r8[PS] = r8[PS] & 124 | ls_lut[r8[A]];
  r8[A] = asl_lut[r8[A]];
  break;
case 0x0B: // ANC imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[r16[PC]++]];
  r8[PS] = r8[PS] & 254 | r8[PS] >> 7;
  break;
case 0x0C: // NOP abs
  r16[PC] += 2;
  break;
case 0x0D: // ORA abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0x0E: // ASL abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  mem[p] = asl_lut[mem[p]];
  break;
case 0x0F: // SLO abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x10: // BPL rel
  var v = mem[r16[PC]++];
  if (~r8[PS] & 128) r16[PC] += (v << 24) >> 24;
  break;
case 0x11: // ORA iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0x12: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x13: // SLO iny
  var a = mem[r16[PC]++];
  var p = (mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y];
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x14: // NOP zpx
  r16[PC]++;
  break;
case 0x15: // ORA zpx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0x16: // ASL zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  mem[p] = asl_lut[mem[p]];
  break;
case 0x17: // SLO zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x18: // CLC imp
  r8[PS] &= 254;
  break;
case 0x19: // ORA aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0x1A: // NOP imp
  break;
case 0x1B: // SLO aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x1C: // NOP abx
  r16[PC] += 2;
  break;
case 0x1D: // ORA abx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0x1E: // ASL abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  mem[p] = asl_lut[mem[p]];
  break;
case 0x1F: // SLO abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] |= mem[p] = asl_lut[mem[p]]];
  break;
case 0x20: // JSR abs
  var v = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r16[PC]--;
  mem[0x100 | r8[SP]--] = r16[PC] >> 8;
  mem[0x100 | r8[SP]--] = r16[PC];
  r16[PC] = v;
  break;
case 0x21: // AND inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0x22: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x23: // RLA inx
  var a = mem[r16[PC]++] + r8[X];
  var p = mem[a & 0xff] | mem[a + 1 & 0xff] << 8;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x24: // BIT zpg
  var v = mem[mem[r16[PC]++]];
  r8[PS] = r8[PS] & 61 | bit_lut[r8[A] << 8 | v];
  break;
case 0x25: // AND zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[mem[r16[PC]++]]];
  break;
case 0x26: // ROL zpg
  var p = mem[r16[PC]++];
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rls_lut[i];
  mem[p] = rol_lut[i];
  break;
case 0x27: // RLA zpg
  var p = mem[r16[PC]++];
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x28: // PLP imp
  r8[PS] = mem[0x100 | ++r8[SP]] & 239 | 32;
  break;
case 0x29: // AND imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[r16[PC]++]];
  break;
case 0x2A: // ROL acc
  var i = (r8[PS] & 1) << 8 | r8[A];
  r8[PS] = r8[PS] & 124 | rls_lut[i];
  r8[A] = rol_lut[i];
  break;
case 0x2B: // ANC imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[r16[PC]++]];
  r8[PS] = r8[PS] & 254 | r8[PS] >> 7;
  break;
case 0x2C: // BIT abs
  var v = mem[mem[r16[PC]++] | mem[r16[PC]++] << 8];
  r8[PS] = r8[PS] & 61 | bit_lut[r8[A] << 8 | v];
  break;
case 0x2D: // AND abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0x2E: // ROL abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rls_lut[i];
  mem[p] = rol_lut[i];
  break;
case 0x2F: // RLA abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x30: // BMI rel
  var v = mem[r16[PC]++];
  if (r8[PS] & 128) r16[PC] += (v << 24) >> 24;
  break;
case 0x31: // AND iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0x32: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x33: // RLA iny
  var a = mem[r16[PC]++];
  var c = r8[PS] & 1;
  var p = (mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y];
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x34: // NOP zpx
  r16[PS]++;
  break;
case 0x35: // AND zpx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0x36: // ROL zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rls_lut[i];
  mem[p] = rol_lut[i];
  break;
case 0x37: // RLA zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x38: // SEC imp
  r8[PS] |= 1;
  break;
case 0x39: // AND aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0x3A: // NOP imp
  break;
case 0x3B: // RLA aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x3C: // NOP abx
  r16[PC] += 2;
  break;
case 0x3D: // AND abx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0x3E: // ROL abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rls_lut[i];
  mem[p] = rol_lut[i];
  break;
case 0x3F: // RLA abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | ls_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] &= mem[p] = rol_lut[c << 8 | mem[p]]];
  break;
case 0x40: // RTI imp
  r8[PS] = mem[0x100 | ++r8[SP]];
  r16[PC] = mem[0x100 | ++r8[SP]] + (mem[0x100 | ++r8[SP]] << 8);
  break;
case 0x41: // EOR inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0x42: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x43: // SRE inx
  var a = mem[r16[PC]++] + r8[X];
  var p = mem[a & 0xff] | mem[a + 1 & 0xff] << 8;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x44: // NOP zpg
  r16[PC]++;
  break;
case 0x45: // EOR zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[mem[r16[PC]++]]];
  break;
case 0x46: // LSR zpg
  var p = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  mem[p] = lsr_lut[mem[p]];
  break;
case 0x47: // SRE zpg
  var p = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x48: // PHA imp
  mem[0x100 | r8[SP]--] = r8[A];
  break;
case 0x49: // EOR imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[r16[PC]++]];
  break;
case 0x4A: // LSR acc
  r8[PS] = r8[PS] & 124 | rs_lut[r8[A]];
  r8[A] = lsr_lut[r8[A]];
  break;
case 0x4B: // ALR imm
  r8[PS] = r8[PS] & 124 | rs_lut[r8[A] &= mem[r16[PC]++]];
  r8[A] = lsr_lut[r8[A]];
  break;
case 0x4C: // JMP abs
  r16[PC] = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  break;
case 0x4D: // EOR abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0x4E: // LSR abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  mem[p] = lsr_lut[mem[p]];
  break;
case 0x4F: // SRE abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x50: // BVC rel
  var v = mem[r16[PC]++];
  if (~r8[PS] & 64) r16[PC] += (v << 24) >> 24;
  break;
case 0x51: // EOR iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0x52: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x53: // SRE iny
  var a = mem[r16[PC]++];
  var p = (mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y];
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x54: // NOP zpx
  r16[PC]++;
  break;
case 0x55: // EOR zpx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0x56: // LSR zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  mem[p] = lsr_lut[mem[p]];
  break;
case 0x57: // SRE zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x58: // CLI imp
  r8[PS] &= 251;
  break;
case 0x59: // EOR aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0x5A: // NOP imp
  break;
case 0x5B: // SRE aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x5C: // NOP abx
  r16[PC] += 2;
  break;
case 0x5D: // EOR abx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0x5E: // LSR abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  mem[p] = lsr_lut[mem[p]];
  break;
case 0x5F: // SRE abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] ^= mem[p] = lsr_lut[mem[p]]];
  break;
case 0x60: // RTS imp
  r16[PC] = mem[0x100 | ++r8[SP]] + (mem[0x100 | ++r8[SP]] << 8) + 1;
  break;
case 0x61: // ADC inx
  var a = mem[r16[PC]++] + r8[X];
  var v = mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x62: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x63: // RRA inx
  var a = mem[r16[PC]++] + r8[X];
  var p = mem[a & 0xff] | mem[a + 1 & 0xff] << 8;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x64: // NOP zpg
  r16[PC]++;
  break;
case 0x65: // ADC zpg
  var v = mem[mem[r16[PC]++]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x66: // ROR zpg
  var p = mem[r16[PC]++];
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rrs_lut[i];
  mem[p] = ror_lut[i];
  break;
case 0x67: // RRA zpg
  var p = mem[r16[PC]++];
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x68: // PLA imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[0x100 | ++r8[SP]]];
  break;
case 0x69: // ADC imm
  var v = mem[r16[PC]++];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x6A: // ROR acc
  var i = (r8[PS] & 1) << 8 | r8[A];
  r8[PS] = r8[PS] & 124 | rrs_lut[i];
  r8[A] = ror_lut[i];
  break;
case 0x6B: // ARR imm
  var a = r8[A] = ror_lut[(r8[PS] & 1) << 8 | (r8[A] & mem[r16[PC]++])];
  r8[PS] = r8[PS] & 60
         | 128 & a
         | 64 & (a ^ (a << 1))
         | 2 & a - 1 >> 7
         | 1 & a >> 5;
  break;
case 0x6C: // JMP ind
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r16[PC] = mem[p] | mem[(p & 0xff00) | ((p + 1) & 0xff)] << 8;
  break;
case 0x6D: // ADC abs
  var v = mem[mem[r16[PC]++] | mem[r16[PC]++] << 8];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x6E: // ROR abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rrs_lut[i];
  mem[p] = ror_lut[i];
  break;
case 0x6F: // RRA abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x70: // BVS rel
  var v = mem[r16[PC]++];
  if (r8[PS] & 64) r16[PC] += (v << 24) >> 24;
  break;
case 0x71: // ADC iny
  var a = mem[r16[PC]++];
  var v = mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x72: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x73: // RRA iny
  var a = mem[r16[PC]++];
  var p = (mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y];
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x74: // NOP zpx
  r16[PC]++;
  break;
case 0x75: // ADC zpx
  var v = mem[mem[r16[PC]++] + r8[X] & 0xff];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x76: // ROR zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rrs_lut[i];
  mem[p] = ror_lut[i];
  break;
case 0x77: // RRA zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x78: // SEI imp
  r8[PS] |= 4;
  break;
case 0x79: // ADC aby
  var v = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x7A: // NOP imp
  break;
case 0x7B: // RRA aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x7C: // NOP abx
  r16[PC] += 2;
  break;
case 0x7D: // ADC abx
  var v = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x7E: // ROR abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  var i = (r8[PS] & 1) << 8 | mem[p];
  r8[PS] = r8[PS] & 124 | rrs_lut[i];
  mem[p] = ror_lut[i];
  break;
case 0x7F: // RRA abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  var c = r8[PS] & 1;
  r8[PS] = r8[PS] & 124 | rs_lut[mem[p]];
  var v = mem[p] = ror_lut[c << 8 | mem[p]];
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0x80: // NOP imm
  r16[PC]++;
  break;
case 0x81: // STA inx
  var a = mem[r16[PC]++] + r8[X];
  var p = mem[a & 0xff] | mem[a + 1 & 0xff] << 8;
  mem[p] = r8[A];
  break;
case 0x82: // NOP imm
  r16[PC]++;
  break;
case 0x83: // SAX inx
  var a = mem[r16[PC]++] + r8[X];
  var p = mem[a & 0xff] | mem[a + 1 & 0xff] << 8;
  mem[p] = r8[A] & r8[X];
  break;
case 0x84: // STY zpg
  var p = mem[r16[PC]++];
  mem[p] = r8[Y];
  break;
case 0x85: // STA zpg
  var p = mem[r16[PC]++];
  mem[p] = r8[A];
  break;
case 0x86: // STX zpg
  var p = mem[r16[PC]++];
  mem[p] = r8[X];
  break;
case 0x87: // SAX zpg
  var p = mem[r16[PC]++];
  mem[p] = r8[A] & r8[X];
  break;
case 0x88: // DEY imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = dec_lut[r8[Y]]];
  break;
case 0x89: // NOP imm
  r16[PC]++;
  break;
case 0x8A: // TXA imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X]];
  break;
case 0x8B: // XAA imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[r16[PC]++] & r8[X]];
  break;
case 0x8C: // STY abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  mem[p] = r8[Y];
  break;
case 0x8D: // STA abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  mem[p] = r8[A];
  break;
case 0x8E: // STX abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  mem[p] = r8[X];
  break;
case 0x8F: // SAX abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  mem[p] = r8[A] & r8[X];
  break;
case 0x90: // BCC rel
  var v = mem[r16[PC]++];
  if (~r8[PS] & 1) r16[PC] += (v << 24) >> 24;
  break;
case 0x91: // STA iny
  var a = mem[r16[PC]++];
  var p = (mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y];
  mem[p] = r8[A];
  break;
case 0x92: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0x93: // AHX iny
  var a = mem[r16[PC]++];
  var p = (mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y];
  mem[p] = r8[A] & r8[X] & ((p >> 8) + 1);
  break;
case 0x94: // STY zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  mem[p] = r8[Y];
  break;
case 0x95: // STA zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  mem[p] = r8[A];
  break;
case 0x96: // STX zpy
  var p = mem[r16[PC]++] + r8[Y] & 0xff;
  mem[p] = r8[X];
  break;
case 0x97: // SAX zpy
  var p = mem[r16[PC]++] + r8[Y] & 0xff;
  mem[p] = r8[A] & r8[X];
  break;
case 0x98: // TYA imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[Y]];
  break;
case 0x99: // STA aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  mem[p] = r8[A];
  break;
case 0x9A: // TXS imp
  r8[SP] = r8[X];
  break;
case 0x9B: // TAS  aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  r8[SP] = mem[p] = r8[A] & r8[X] & ((p >> 8) + 1);
  break;
case 0x9C: // SHY abx
  var b = (mem[r16[PC]++] | mem[r16[PC]++] << 8);
  mem[b + r8[Y] & 0xffff] = r8[Y] & ((b >> 8) + 1);
  break;
case 0x9D: // STA abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  mem[p] = r8[A];
  break;
case 0x9E: // SHX aby
  var b = (mem[r16[PC]++] | mem[r16[PC]++] << 8);
  mem[b + r8[Y] & 0xffff] = r8[X] & ((b >> 8) + 1);
  break;
case 0x9F: // AHX aby
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff;
  mem[p] = r8[A] & r8[X] & ((p >> 8) + 1);
  break;
case 0xA0: // LDY imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = mem[r16[PC]++]];
  break;
case 0xA1: // LDA inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0xA2: // LDX imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = mem[r16[PC]++]];
  break;
case 0xA3: // LAX inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0xA4: // LDY zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = mem[mem[r16[PC]++]]];
  break;
case 0xA5: // LDA zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[mem[r16[PC]++]]];
  break;
case 0xA6: // LDX zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = mem[mem[r16[PC]++]]];
  break;
case 0xA7: // LAX zpg
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[mem[r16[PC]++]]];
  break;
case 0xA8: // TAY imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = r8[A]];
  break;
case 0xA9: // LDA imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[r16[PC]++]];
  break;
case 0xAA: // TAX imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = r8[A]];
  break;
case 0xAB: // LAX imm
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[r16[PC]++]];
  break;
case 0xAC: // LDY abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xAD: // LDA abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xAE: // LDX abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xAF: // LAX abs
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xB0: // BCS rel
  var v = mem[r16[PC]++];
  if (r8[PS] & 1) r16[PC] += (v << 24) >> 24;
  break;
case 0xB1: // LDA iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0xB2: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0xB3: // LAX iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0xB4: // LDY zpx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0xB5: // LDA zpx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0xB6: // LDX zpy
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = mem[mem[r16[PC]++] + r8[Y] & 0xff]];
  break;
case 0xB7: // LAX zpy
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[mem[r16[PC]++] + r8[Y] & 0xff]];
  break;
case 0xB8: // CLV imp
  r8[PS] &= 191;
  break;
case 0xB9: // LDA aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0xBA: // TSX imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = r8[SP]];
  break;
case 0xBB: // LAS aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = r8[SP] &= mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0xBC: // LDY abx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0xBD: // LDA abx
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0xBE: // LDX aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0xBF: // LAX aby
  r8[PS] = r8[PS] & 125 | nz_lut[r8[A] = r8[X] = mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0xC0: // CPY imm
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[Y] << 8 | mem[r16[PC]++]];
  break;
case 0xC1: // CMP inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0xC2: // NOP imm
  r16[PC]++;
  break;
case 0xC3: // DCP inx
  var a = mem[r16[PC]++] + r8[X];
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]];
  break;
case 0xC4: // CPY zpg
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[Y] << 8 | mem[mem[r16[PC]++]]];
  break;
case 0xC5: // CMP zpg
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[mem[r16[PC]++]]];
  break;
case 0xC6: // DEC zpg
  var p = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = dec_lut[mem[p]]];
  break;
case 0xC7: // DCP zpg
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[mem[r16[PC]++]]];
  break;
case 0xC8: // INY imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[Y] = inc_lut[r8[Y]]];
  break;
case 0xC9: // CMP imm
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[r16[PC]++]];
  break;
case 0xCA: // DEX imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = dec_lut[r8[X]]];
  break;
case 0xCB: // AXS imm
  var v = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | cmp_lut[(r8[X] &= r8[A]) << 8 | v];
  r8[X] -= v;
  break;
case 0xCC: // CPY abs
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[Y] << 8 | mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xCD: // CMP abs
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xCE: // DEC abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = dec_lut[mem[p]]];
  break;
case 0xCF: // DCP abs
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xD0: // BNE rel
  var v = mem[r16[PC]++];
  if (~r8[PS] & 2) r16[PC] += (v << 24) >> 24;
  break;
case 0xD1: // CMP iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0xD2: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0xD3: // DCP iny
  var a = mem[r16[PC]++];
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]];
  break;
case 0xD4: // NOP zpx
  r16[PC]++;
  break;
case 0xD5: // CMP zpx
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0xD6: // DEC zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = dec_lut[mem[p]]];
  break;
case 0xD7: // DCP zpx
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[mem[r16[PC]++] + r8[X] & 0xff]];
  break;
case 0xD8: // CLD imp
  r8[PS] &= 247;
  break;
case 0xD9: // CMP aby
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0xDA: // NOP imp
  break;
case 0xDB: // DCP aby
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]];
  break;
case 0xDC: // NOP abx
  r16[PC] += 2;
  break;
case 0xDD: // CMP abx
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0xDE: // DEC abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = dec_lut[mem[p]]];
  break;
case 0xDF: // DCP abx
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[A] << 8 | 0xff & --mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]];
  break;
case 0xE0: // CPX imm
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[X] << 8 | mem[r16[PC]++]];
  break;
case 0xE1: // SBC inx
  var a = mem[r16[PC]++] + r8[X];
  var v = ~(mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xE2: // NOP imm
  r16[PC]++;
  break;
case 0xE3: // ISC inx
  var a = mem[r16[PC]++] + r8[X];
  var v = ~(++mem[mem[a & 0xff] | mem[a + 1 & 0xff] << 8]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xE4: // CPX zpg
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[X] << 8 | mem[mem[r16[PC]++]]];
  break;
case 0xE5: // SBC zpg
  var v = ~(mem[mem[r16[PC]++]]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xE6: // INC zpg
  var p = mem[r16[PC]++];
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = inc_lut[mem[p]]];
  break;
case 0xE7: // ISC zpg
  var v = ~(++mem[mem[r16[PC]++]]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xE8: // INX imp
  r8[PS] = r8[PS] & 125 | nz_lut[r8[X] = inc_lut[r8[X]]];
  break;
case 0xE9: // SBC imm
  var v = ~(mem[r16[PC]++]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xEA: // NOP imp
  break;
case 0xEB: // SBC imm
  var v = ~(mem[r16[PC]++]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xEC: // CPX abs
  r8[PS] = r8[PS] & 124 | cmp_lut[r8[X] << 8 | mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]];
  break;
case 0xED: // SBC abs
  var v = ~(mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xEE: // INC abs
  var p = mem[r16[PC]++] | mem[r16[PC]++] << 8;
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = inc_lut[mem[p]]];
  break;
case 0xEF: // ISC abs
  var v = ~(++mem[mem[r16[PC]++] | mem[r16[PC]++] << 8]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xF0: // BEQ rel
  var v = mem[r16[PC]++];
  if (r8[PS] & 2) r16[PC] += (v << 24) >> 24;
  break;
case 0xF1: // SBC iny
  var a = mem[r16[PC]++];
  var v = ~(mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xF2: // HLT imm
  halt(mem[r16[PC]++]);
  break;
case 0xF3: // ISC iny
  var a = mem[r16[PC]++];
  var v = ~(++mem[(mem[a & 0xff] | mem[a + 1 & 0xff] << 8) + r8[Y]]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xF4: // NOP zpx
  r16[PC]++;
  break;
case 0xF5: // SBC zpx
  var v = ~(mem[mem[r16[PC]++] + r8[X] & 0xff]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xF6: // INC zpx
  var p = mem[r16[PC]++] + r8[X] & 0xff;
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = inc_lut[mem[p]]];
  break;
case 0xF7: // ISC zpx
  var v = ~(++mem[mem[r16[PC]++] + r8[X] & 0xff]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xF8: // SED imp
  r8[PS] |= 8;
  break;
case 0xF9: // SBC aby
  var v = ~(mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xFA: // NOP imp
  break;
case 0xFB: // ISC aby
  var v = ~(++mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[Y] & 0xffff]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xFC: // DBG imp (custom op code)
  debugger;
  break;
case 0xFD: // SBC abx
  var v = ~(mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
case 0xFE: // INC abx
  var p = (mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff;
  r8[PS] = r8[PS] & 125 | nz_lut[mem[p] = inc_lut[mem[p]]];
  break;
case 0xFF: // ISC abx
  var v = ~(++mem[(mem[r16[PC]++] | mem[r16[PC]++] << 8) + r8[X] & 0xffff]) & 0xff;
  var i = r8[A] 
        | v << 8
        | (r8[PS] & 1) << 16
        | (r8[PS] & 8) << 14;
  r8[A] = adc_lut[i];
  r8[PS] = r8[PS] & 60 | adc_ps_lut[i];
  break;
}
}

function NMI() {
  mem[0x100 | r8[3]--] = r16[0] >>> 8;
  mem[0x100 | r8[3]--] = r16[0];
  mem[0x100 | r8[3]--] = r8[4] & 239 | 32;
  r8[4] |= 4;
  r16[0] = mem[0xfffa] | mem[0xfffb] << 8;
}

function RES() {
  r8[4] = r8[4] & 247 | 36;
  r8[3] = 0xfd;
  r16[0] = mem[0xfffc] | mem[0xfffd] << 8;
}

function IRQ() {
  if (r8[4] & 4) return;
  mem[0x100 | r8[3]--] = r16[0] >>> 8;
  mem[0x100 | r8[3]--] = r16[0];
  mem[0x100 | r8[3]--] = r8[4] & 239 | 32;
  r8[4] |= 4;
  r16[0] = mem[0xfffe] | mem[0xffff] << 8;
}

function sendReset() {
  var _step = step;
  step = function() {
    RES();
    step = _step;
  }
}

function sendIRQ() {
  var _step = step;
  step = function() {
    IRQ();
    step = _step;
  }
}

function sendNMI() {
  var _step = step;
  step = function() {
    NMI();
    step = _step;
  }
}

var palettes = [
  ["#000000", "#ffffff", "#880000", "#aaffee", "#cc44cc", "#00cc55", "#0000aa", "#eeee77", "#dd8855", "#664400", "#ff7777", "#333333", "#777777", "#aaff66", "#0088ff", "#bbbbbb"],
  ['#000000','#000024','#000049','#00006d','#000092','#0000b6','#0000db','#0000ff','#005500','#005524','#005549','#00556d','#005592','#0055b6','#0055db','#0055ff','#00aa00','#00aa24','#00aa49','#00aa6d','#00aa92','#00aab6','#00aadb','#00aaff','#00ff00','#00ff24','#00ff49','#00ff6d','#00ff92','#00ffb6','#00ffdb','#00ffff','#240000','#240024','#240049','#24006d','#240092','#2400b6','#2400db','#2400ff','#245500','#245524','#245549','#24556d','#245592','#2455b6','#2455db','#2455ff','#24aa00','#24aa24','#24aa49','#24aa6d','#24aa92','#24aab6','#24aadb','#24aaff','#24ff00','#24ff24','#24ff49','#24ff6d','#24ff92','#24ffb6','#24ffdb','#24ffff','#490000','#490024','#490049','#49006d','#490092','#4900b6','#4900db','#4900ff','#495500','#495524','#495549','#49556d','#495592','#4955b6','#4955db','#4955ff','#49aa00','#49aa24','#49aa49','#49aa6d','#49aa92','#49aab6','#49aadb','#49aaff','#49ff00','#49ff24','#49ff49','#49ff6d','#49ff92','#49ffb6','#49ffdb','#49ffff','#6d0000','#6d0024','#6d0049','#6d006d','#6d0092','#6d00b6','#6d00db','#6d00ff','#6d5500','#6d5524','#6d5549','#6d556d','#6d5592','#6d55b6','#6d55db','#6d55ff','#6daa00','#6daa24','#6daa49','#6daa6d','#6daa92','#6daab6','#6daadb','#6daaff','#6dff00','#6dff24','#6dff49','#6dff6d','#6dff92','#6dffb6','#6dffdb','#6dffff','#920000','#920024','#920049','#92006d','#920092','#9200b6','#9200db','#9200ff','#925500','#925524','#925549','#92556d','#925592','#9255b6','#9255db','#9255ff','#92aa00','#92aa24','#92aa49','#92aa6d','#92aa92','#92aab6','#92aadb','#92aaff','#92ff00','#92ff24','#92ff49','#92ff6d','#92ff92','#92ffb6','#92ffdb','#92ffff','#b60000','#b60024','#b60049','#b6006d','#b60092','#b600b6','#b600db','#b600ff','#b65500','#b65524','#b65549','#b6556d','#b65592','#b655b6','#b655db','#b655ff','#b6aa00','#b6aa24','#b6aa49','#b6aa6d','#b6aa92','#b6aab6','#b6aadb','#b6aaff','#b6ff00','#b6ff24','#b6ff49','#b6ff6d','#b6ff92','#b6ffb6','#b6ffdb','#b6ffff','#db0000','#db0024','#db0049','#db006d','#db0092','#db00b6','#db00db','#db00ff','#db5500','#db5524','#db5549','#db556d','#db5592','#db55b6','#db55db','#db55ff','#dbaa00','#dbaa24','#dbaa49','#dbaa6d','#dbaa92','#dbaab6','#dbaadb','#dbaaff','#dbff00','#dbff24','#dbff49','#dbff6d','#dbff92','#dbffb6','#dbffdb','#dbffff','#ff0000','#ff0024','#ff0049','#ff006d','#ff0092','#ff00b6','#ff00db','#ff00ff','#ff5500','#ff5524','#ff5549','#ff556d','#ff5592','#ff55b6','#ff55db','#ff55ff','#ffaa00','#ffaa24','#ffaa49','#ffaa6d','#ffaa92','#ffaab6','#ffaadb','#ffaaff','#ffff00','#ffff24','#ffff49','#ffff6d','#ffff92','#ffffb6','#ffffdb','#ffffff'],
  ['#000000','#400000','#800000','#c00000','#ff0000','#004000','#404000','#804000','#c04000','#ff4000','#008000','#408000','#808000','#c08000','#ff8000','#00c000','#40c000','#80c000','#c0c000','#ffc000','#00ff00','#40ff00','#80ff00','#c0ff00','#ffff00','#000040','#400040','#800040','#c00040','#ff0040','#004040','#404040','#804040','#c04040','#ff4040','#008040','#408040','#808040','#c08040','#ff8040','#00c040','#40c040','#80c040','#c0c040','#ffc040','#00ff40','#40ff40','#80ff40','#c0ff40','#ffff40','#000080','#400080','#800080','#c00080','#ff0080','#004080','#404080','#804080','#c04080','#ff4080','#008080','#408080','#808080','#c08080','#ff8080','#00c080','#40c080','#80c080','#c0c080','#ffc080','#00ff80','#40ff80','#80ff80','#c0ff80','#ffff80','#0000c0','#4000c0','#8000c0','#c000c0','#ff00c0','#0040c0','#4040c0','#8040c0','#c040c0','#ff40c0','#0080c0','#4080c0','#8080c0','#c080c0','#ff80c0','#00c0c0','#40c0c0','#80c0c0','#c0c0c0','#ffc0c0','#00ffc0','#40ffc0','#80ffc0','#c0ffc0','#ffffc0','#0000ff','#4000ff','#8000ff','#c000ff','#ff00ff','#0040ff','#4040ff','#8040ff','#c040ff','#ff40ff','#0080ff','#4080ff','#8080ff','#c080ff','#ff80ff','#00c0ff','#40c0ff','#80c0ff','#c0c0ff','#ffc0ff','#00ffff','#40ffff','#80ffff','#c0ffff','#ffffff','#000000','#000000','#000000'],
  ['#000000','#FF0B00','#FF1700','#FF2200','#FF2E00','#FF3900','#FF4400','#FF5000','#FF5B00','#FF6700','#FF7200','#FF7D00','#FF8900','#FF9400','#FFA000','#FFAB00','#FFB600','#FFC200','#FFCD00','#FFD900','#FFE400','#FFEF00','#FFFB00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FFFF00','#FBFF00','#EFFF00','#E4FF00','#D9FF00','#CDFF00','#C2FF00','#B6FF00','#ABFF00','#A0FF00','#94FF00','#89FF00','#7DFF00','#72FF00','#67FF00','#5BFF00','#50FF00','#44FF00','#39FF00','#2EFF00','#22FF00','#17FF00','#0BFF00','#00FF00','#00FF0B','#00FF17','#00FF22','#00FF2E','#00FF39','#00FF44','#00FF50','#00FF5B','#00FF67','#00FF72','#00FF7D','#00FF89','#00FF94','#00FFA0','#00FFAB','#00FFB6','#00FFC2','#00FFCD','#00FFD9','#00FFE4','#00FFEF','#00FFFB','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FFFF','#00FBFF','#00EFFF','#00E4FF','#00D9FF','#00CDFF','#00C2FF','#00B6FF','#00ABFF','#00A0FF','#0094FF','#0089FF','#007DFF','#0072FF','#0067FF','#005BFF','#0050FF','#0044FF','#0039FF','#002EFF','#0022FF','#0017FF','#000BFF','#0000FF','#0B00FF','#1700FF','#2200FF','#2E00FF','#3900FF','#4400FF','#5000FF','#5B00FF','#6700FF','#7200FF','#7D00FF','#8900FF','#9400FF','#A000FF','#AB00FF','#B600FF','#C200FF','#CD00FF','#D900FF','#E400FF','#EF00FF','#FB00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FF','#FF00FB','#FF00EF','#FF00E4','#FF00D9','#FF00CD','#FF00C2','#FF00B6','#FF00AB','#FF00A0','#FF0094','#FF0089','#FF007D','#FF0072','#FF0067','#FF005B','#FF0050','#FF0044','#FF0039','#FF002E','#FF0022','#FF0017','#FF000B'],
];

var colors = palettes[1];


function drawPixel(address,value) {
  var index = address - 0x200;
  var x = index % 0x20;
  var y = Math.floor(index / 0x20);
  ctx.fillStyle = colors[value % colors.length];
  ctx.fillRect(x*pxsize,y*pxsize,pxsize,pxsize);
}

function draw() {
  for(var i = 0x200; i < 0x600; i++) {
    drawPixel(i,mem[i]);
  }
}

var registerHash;

function formatHex(n,b) {
  return (+n).toString(16).padStart(b,'0');
}

function formatBin(n,b) {
  return (+n).toString(2).padStart(b,'0');
}

function updDisplay() {
  requestAnimationFrame(() => {
    var r = {
      A: r8[0],
      X: r8[1],
      Y: r8[2],
      SP: r8[3],
      PS: r8[4],
      PC: r16[0],
    };
    var hash = JSON.stringify(r);
    if (hash !== registerHash) {
      var A = '$' + formatHex(r.A,2);
      var X = '$' + formatHex(r.X,2);
      var Y = '$' + formatHex(r.Y,2);
      var SP = '$' + formatHex(r.SP,2);
      var PS = '%' + formatBin(r.PS,8);
      var PC = '$' + formatHex(r.PC,4);

      var text = `A: ${A} X: ${X} Y: ${Y} SP: ${SP} PC: ${PC} PS: ${PS}`;
      registerDiv.textContent = text;
      registerHash = hash;
      var ramtext = '       00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f';
      for (var i = 0; i < 16; i++) {
        ramtext += '\n' + '00' + formatHex(i,1) + '0 : ' + Array.from(mem.slice(i*16, (i+1)*16)).map(n => formatHex(n,2)).join(' ');
      }
      ramDiv.textContent = ramtext;

    }
  });
}

var running = false;

function run() {
  running = true;
  cycles = 0;
  lt = performance.now();
  dbg ? dbgloop() : loop();
}

function halt(dbg=0) {
  running = false;
  runstopbtn.innerText = 'Run';
  if (dbg) debugger;
}

dbg = false;
var cps = 100;
var cycles;
var lt;

function loop(t) {
  if (!running) return;

  var nt = t ?? lt;
  var dt = (nt - lt)/1000;
  lt = nt;

  cycles += dt*cps;

  while (cycles >= 1 && running) {
    step();
    cycles--;
  }
  draw();
  updDisplay();
  requestAnimationFrame(loop);
}

function dbgloop(t) {
  if (!running) return;

  var nt = t ?? lt;
  var dt = (nt - lt)/1000;
  lt = nt;

  cycles += dt*cps;

  while (cycles >= 1 && running) {
    var pc = r16[0];
    var line = dbgarr[pc];
    var opcode = mem[pc];
    console.log('pc: ' + formatHex(pc,4) + ' opcode: ' + formatHex(opcode,2) + ' line: ' + line);
    step();
    cycles--;
  }
  draw();
  updDisplay();
  requestAnimationFrame(dbgloop);
}

function runBenchmark() {
  running = true;
  simInstrCount = cycles = 0;
  simSecStartTime = lt = performance.now();
  bloop();
}

var bench_cycles = 0;
var bench_lt = performance.now();

let simInstrCount = 0;
let simSecStartTime = performance.now();

function bloop(t) {
  if (!running) return;
  
  var nt = t ?? performance.now();
  var dt = (nt - lt) / 1000;
  lt = nt;
  
  cycles += dt * cps;
  
  while (cycles >= 1 && running) {
    step();
    cycles--;
    simInstrCount++;

    if (simInstrCount >= cps) {
  var t = performance.now();
  var elapsedMs = t - simSecStartTime;
  var actualIPS = simInstrCount / (elapsedMs / 1000);
  console.log("Actual instructions per second: " + actualIPS.toFixed(2));
  simInstrCount = 0;
  simSecStartTime = t;
    }
  }
  draw();
  updDisplay();
  requestAnimationFrame(bloop);
}

window.onkeypress = function(event) {
  mem[0xff] = event.which;
}

class InputDevice {
  static all = [];
  type = null;
  constructor(memory, address, element = window) {
    if (this.type && !InputDevice.all.find(d => d.type === this.type)) {
      InputDevice.all.push(this);
    } else {
      return;
    }
    this.memory = memory;
    this.element = element;
    this.address = address;
    this.init();
  }

  init() {}

  remove() {
    var i = InputDevice.all.indexOf(this);
    if (i > -1) InputDevice.all.splice(i,1);
  }

  update() {
    this.memory.set(this.internalState, this.address);
  }
}

class KeyboardInputDevice extends InputDevice {
  type = 'keyboard';
  constructor(memory, address, element = window) {
    super(memory,address,element);
  }

  init() {
    this.internalState = new Uint8Array(1);
    this.onKeyPress = this.onKeyPress.bind(this);
    this.element.addEventListener('keypress', this.onKeyPress);
  }

  remove() {
    this.element.removeEventListener('keypress', this.onKeyPress);
    super.remove();
  }

  onKeyPress(event) {
    this.internalState[0] = event.which;
  }
}

class RNGInputDevice extends InputDevice {
  static index = 0;
  type = 'rng' + RNGInputDevice.index++;
  constructor(memory, address, element = window) {
    super(memory,address,element);
  }

  init() {
    this.internalState = new Uint8Array(1);
  }

  update() {
    this.internalState[0] = Math.random() * 256;
    super.update();
  }
}
