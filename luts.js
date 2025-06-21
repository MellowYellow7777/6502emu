function u8a(bw) {
  return new Uint8Array(2**bw);
}



z_lut = u8a(8).map((_,i) => (i === 0) << 1);
n_lut = u8a(8).map((_,i) => i & 128);

nz_lut = n_lut.map((m,i) => m | z_lut[i]);

ls_lut = u8a(8).map((_,i) => nz_lut[i << 1 & 0xff] | i >> 7);
rs_lut = u8a(8).map((_,i) => nz_lut[i >> 1] | i & 1);
rls_lut = u8a(9).map((_,i) => nz_lut[i << 1 & 0xff | i >> 8] | (i & 0xff) >> 7);
rrs_lut = u8a(9).map((_,i) => nz_lut[i >> 1 & 0xff] | i & 1);

bit_lut = u8a(16).map((_,i) => i & 192 | ((i >> 8 & i) === 0) << 1);



inc_lut = u8a(8).map((_,i) => i + 1);
dec_lut = u8a(8).map((_,i) => i - 1);




cmp_lut = u8a(16).map((_,i) => {
  var b = i & 255;
  var a = i >> 8;
  return (a < b) << 7
       | (a === b) << 1
       | (a >= b) << 0;
});








asl_lut = u8a(8).map((_,i) => i << 1);
lsr_lut = u8a(8).map((_,i) => i >> 1);
rol_lut = u8a(9).map((_,i) => i << 1 | i >> 8);
ror_lut = u8a(9).map((_,i) => i >> 1);









//  PS = PS & 125
//     | nz_lut[A = and_lut[A | v << 8]];
//  PS = PS & 125
//     | nz_lut[A = ora_lut[A | v << 8]];
//  PS = PS & 125
//     | nz_lut[A = eor_lut[A | v << 8]];

and_lut = u8a(16).map((_,i) => i & i >> 8);
ora_lut = u8a(16).map((_,i) => i | i >> 8);
eor_lut = u8a(16).map((_,i) => i ^ i >> 8);



//  var i = A 
//        | v << 8
//        | PS & 1 << 16
//        | PS & 8 << 14;
//  A = adc_lut[i];
//  PS = PS & 60 | adc_ps_lut[i];

adc_lut = u8a(18).map((_,i) => {
  var A = i >> 0 & 255;
  var B = i >> 8 & 255;
  var C = i >> 16 & 1;
  var D = i >> 17 & 1;
  if (!D) return A + B + C;
  var rl = (A & 15) + (B & 15) + C;
  var cl = rl > 9;
  rl += cl * 0x06;
  var rh = (A & 240) + (B & 240) + (cl << 4);
  var ch = rh > 0x90;
  rh += ch * 0x60;
  return rh | rl & 15;
});

adc_ps_lut = u8a(18).map((_,i) => {
  var A = i >> 0 & 255;
  var B = i >> 8 & 255;
  var C = i >> 16 & 1;
  var D = i >> 17 & 1;
  var br = A + B + C;
  var r = br & 255;
  var CF = br >> 8;
  var ZF = (r === 0) << 1;
  if (D) {
    var rl = (A & 15) + (B & 15) + C;
    var cl = (rl > 9) << 4; // (rl + 22) >> 1 & 240;
    var rh = (A & 240) + (B & 240) + cl;
    r = rh | rl;
    CF = rh > 0x90 & 1;
  }
  var NF = r & 128;
  var VF = (~(A ^ B) & (A ^ r)) >> 1 & 64;
  return NF | VF | ZF | CF;
});
