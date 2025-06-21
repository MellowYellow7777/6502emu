var assemble = (function () {

var fixedPoint = '0';

class Define {
  static all = [];
  static keys = [];
  static lut = {};
  constructor(key,value) {
    this.key = key;
    this.value = value;
    Define.add(this);
  }
  static add(define) {
    var {key,value} = define;
    if (this.has(key)) return;
    this.all.push(define);
    this.keys.push(key);
    this.lut[key] = define;
  }
  static has(key) {
    return this.keys.includes(key);
  }
}

class Label {
  static all = [];
  static keys = [];
  static lut = {};
  constructor(key,value) {
    this.key = key;
    this.value = value;
    Label.add(this);
  }
  static add(label) {
    var {key,value} = label;
    if (this.has(key)) return;
    this.all.push(label);
    this.keys.push(key);
    this.lut[key] = label;
  }
  static has(key) {
    return this.keys.includes(key);
  }
}

class Token {
  constructor(type,value) {
    this.type = type;
    this.value = value;
  }
}

function resolved(value) {
  return {type: 'value', value, resolved: true};
}

function unresolved(value) {
  return {type: 'value', value, resolved: false};
}

class ResolvableValue extends Token {
  constructor(value) {
    super('value',value);
    this.init(value);
  }
  init() {

  }
  resolve() {
    var value = this.value;
    var jstype = typeof value;
    if (jstype === 'number') {
      return resolved(value);
    } else if (jstype === 'string') {
      if (Define.has(value)) {
        var def = Define.lut[value];
        return def.value.resolve();
      } else if (Label.has(value)) {
        var lab = Label.lut[value];
        return lab.value.resolve();
      }
    }
    return unresolved(value);
  }
}

class Expression extends ResolvableValue {
  static fromString(string) {
    var tokens = this.tokenize(string);
    return new Expression(tokens);
  }
  static tokenize(string) {
    var tokens = [];
    var rgx = />>>|>>|<<|\*\*|[~+\-*\/%<>&|^()]|f\.|\$[0-9A-Fa-f]+|\d+\.?\d*|[a-zA-Z_][a-zA-Z0-9_]*/g;
    var match;
    while ((match = rgx.exec(string)) !== null) {
      tokens.push(match[0]);
    }
    var operators = ['~','+','-','<','>','**','*','/','%','<<','>>','>>>','&','^','|','f.'];
    var parenthesis = ['(',')'];
    var digits = '0123456789.';
    var alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_';
    var hexdec = '0123456789abcdefABCDEF';
    tokens = tokens.map((value,i) => {
      if (operators.includes(value)) return new Token('operator',value);
      if (parenthesis.includes(value)) return new Token('parenthesis',value);
      if (value[0] === '$') return new ResolvableValue(parseInt(value.slice(1),16),true);
      if (digits.includes(value[0])) return new ResolvableValue(+value,true);
      if (alpha.includes(value[0])) return new ResolvableValue(value);
      return new Token('unknown',value);
    });
    return tokens;
  }
  constructor(tokens) {
    super(tokens);
  }

  init(tokens) {
    this.type = 'expression';
    this.value = this.group(tokens);
  }

  group(tokens) {
    var arr = [];
    var inner = [];
    var depth = 0;
    tokens.forEach(token => {
      if (depth > 0) {
        if (token.value === '(') {
          depth++;
          inner.push(token);
        } else if (token.value === ')') {
          depth--
          if (depth === 0) {
            arr.push(new Expression(inner));
            inner = [];
          } else {
            inner.push(token);
          }
        } else {
          inner.push(token);
        }
      } else if (token.value === '(') {
        depth++;
      } else {
        arr.push(token);
      }
    });
    return arr;
  }

  resolve() {
    var fail = 0;
    var tokens = this.value.map(value => {
      if (!value.resolve) return value;
      value = value.resolve();
      if (!value.resolved) fail = true;
      return value;
    });
    if (fail) return unresolved(this.value);

    var ops = ['%','~','+','-','<','>','f.'];
    function maybeUnary(token) {
      return token.type === 'operator' && ops.includes(token.value);
    }
    for (var i = 1; i < tokens.length; i++) {
      var token = tokens[i];
      if (token.type === 'value') {
        while (i > 0 && maybeUnary(tokens[i-1]) && (i === 1 || tokens[i-2].type === 'operator')) {
          var op = tokens[i-1].value;
          if (op === '%') token.value = +('0b' + token.value.toString());
          if (op === '~') token.value ^= -1;
          if (op === '-') token.value *= -1;
          if (op === '<') token.value &= 0xff;
          if (op === '>') {
            token.value &= 0xff00;
            token.value >>= 8;
          }
          if (op === 'f.') {
            if (fixedPoint.toString() !== '0') {
              var mode = fixedPoint.toString();
              var [int,frc] = mode.split('.').map(n => +n);
              var scale = 1 << frc;
              var mask = (1 << (int + frc)) - 1;
              token.value = token.value * scale & mask;
            }
          }
          tokens.splice(i-1,1);
          i--;
        }
      }
    }
    function doOp(left,op,right) {
      switch (op) {
        case '**':
          return left ** right;
        case '*':
          return left * right;
        case '/':
          return left / right;
        case '%':
          return left % right;
        case '+':
          return left + right;
        case '-':
          return left - right;
        case '>>>':
          return left >>> right;
        case '>>':
          return left >> right;
        case '<<':
          return left << right;
        case '&':
          return left & right;
        case '|':
          return left | right;
        case '^':
          return left ^ right;
      }
    }
    function nextIndex() {
      return tokens.findIndex(token => 
        token.type === 'operator' &&
        ops.includes(token.value)
      );
    }
    function lastIndex() {
      return tokens.findLastIndex(token => 
        token.type === 'operator' &&
        ops.includes(token.value)
      );
    }
    function doAll(_ops,getIndex) {
      ops = _ops;
      var index = getIndex();
      while (index > -1) {
        var res = doOp(...tokens.slice(index-1, index +2).map(t => t.value));
        tokens.splice(index-1, 3, resolved(res));
        index = getIndex();
      }
    }
    doAll(['**'], lastIndex);
    doAll(['*','/','%'], nextIndex);
    doAll(['+','-'], nextIndex);
    doAll(['<<','>>','>>>'], nextIndex);
    doAll(['&'], nextIndex);
    doAll(['^'], nextIndex);
    doAll(['|'], nextIndex);
    if (tokens.length === 1) return tokens[0];
    return unresolved(this.value);
  }
}



var programCode;

function assemble(asm) {
  if (arguments.length === 0) {
    asm = programCode;
  } else {
    programCode = asm;
  }
  Define.all = [];
  Define.keys = [];
  Define.lut = {};
  Label.all = [];
  Label.keys = [];
  Label.lut = {};
  var tokens = [];

  asm = asm.replaceAll(/ *;.*$/gm,'');


asm = asm.replace(
  /^\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(.*)$|^\s*\.fp\s+(.*)$/gm,
  (_, defineKey, defineExpr, fpString) => {

    if (defineKey) {
      new Define(defineKey, Expression.fromString(defineExpr));
      return '';
    }

    if (fpString) {
      fixedPoint = fpString.trim();
      return '';
    }

    return '';
  }
);

  asm = asm.replaceAll(/^\s+/gm,'');
  asm = asm.replaceAll(/\s+$/gm,'');
  asm = asm.replaceAll(/\s{2,}/gm,' ');
  asm = asm.replace(/^(\s*[a-zA-Z_][a-zA-Z0-9_]*):\s*(?=\S)/gm, "$1:\n");

  var instructions = ['adc','and','asl','bcc','bcs','beq','bit','bmi','bne','bpl','brk','bvc','bvs','clc','cld','cli','clv','cmp','cpx','cpy','dec','dex','dey','eor','inc','inx','iny','jmp','jsr','lda','ldx','ldy','lsr','nop','ora','pha','php','pla','plp','rol','ror','rti','rts','sbc','sec','sed','sei','sta','stx','sty','tax','tay','tsx','txa','txs','tya',
// undoc
'hlt','slo','rla','sre','rra','sax','lax','dcp','isc','anc','alr','arr','xaa','tas','las','axs','dop','top','dbg'
];
  var branch_ins = ['bcc','bcs','beq','bmi','bne','bpl','bvc','bvs'];
  var acc_ins = ['asl','lsr','rol','ror'];
  var imp_ins = ['brk','clc','cld','cli','clv','dex','dey','inx','iny','nop','pha','php','pla','plp','rti','rts','sec','sed','sei','tax','tay','tsx','txa','txs','tya'];

  var directives = ['org','db','dw','byte','word','cps','fp','palette'];

  var ins_lens = {
    'implied': 1,
    'accumulator': 1,
    'immediate': 2,
    'zero page': 2,
    'zero page x': 2,
    'zero page y': 2,
    'absolute': 3,
    'absolute x': 3,
    'absolute y': 3,
    'indirect': 3,
    'indirect x': 2,
    'indirect y': 2,
    'relative': 2,
    'memory': 3,
    'indexed x': 3,
    'indexed y': 3,
  };

  asm.split('\n').forEach(line => {
    if (line[0] === '.') {
      var type = directives.find(d => line.toLowerCase().startsWith('.' + d));
      if (type) {
        var args = line.slice(line.toLowerCase().indexOf(type) + type.length).trim();
        if (args.length) {
          args = args.split(',').map(a => a.trim());
        } else {
          args = [];
        }
        return tokens.push({type: 'directive', value: type, arguments: args, line});
      }
    }
    if (line[line.length-1] === ':') {
      return tokens.push({type: 'label', value: line.slice(0,-1), arguments: [], line});
    }
    var instruction = instructions.find(ins => line.toLowerCase().startsWith(ins));
    if (instruction) {
      var _line = line;
      line = line.slice(3).trim();

      var args, mode, ambig = false;

      if (instruction === 'hlt' && !line.length) {
        mode = 'immediate';
        args = ['$00'];
      } else if (instruction === 'dbg') {
        instruction = 'hlt';
        mode = 'immediate';
        args = ['$ff'];
      } else if (instruction === 'dop') {
        instruction = 'nop';
        mode = 'zero page';
        args = ['$00'];
      } else if (instruction === 'top') {
        instruction = 'nop';
        mode = 'absolute';
        args = ['$0000'];
      } else if (branch_ins.includes(instruction)) {
        mode = 'relative';
        args = [line];
      } else if (imp_ins.includes(instruction) && !line.length) {
        mode = 'implied';
        args = [];
      } else if (acc_ins.includes(instruction) && (
          line.toLowerCase() === 'a' || !line.length)) {
        mode = 'accumulator';
        args = [];
      } else if ((args = line.matchAll(/\((.*)\)\s?,\s?[yY]/gm).toArray()).length) {
        mode = 'indirect y';
        args = [args[0][1].trim()];
      } else if ((args = line.matchAll(/\((.*),\s?[xX]\s?\)/gm).toArray()).length) {
        mode = 'indirect x';
        args = [args[0][1].trim()];
      } else if ((args = line.matchAll(/\((.*)\)/gm).toArray()).length) {
        mode = 'indirect';
        args = [args[0][1].trim()];
      } else if ((args = line.matchAll(/(.*),\s?[yY]/gm).toArray()).length) {
        mode = 'indexed y';
        ambig = true;
        args = [args[0][1].trim()];
      } else if ((args = line.matchAll(/(.*),\s?[xX]/gm).toArray()).length) {
        mode = 'indexed x';
        ambig = true;
        args = [args[0][1].trim()];
      } else if ((args = line.matchAll(/#(.*)/gm).toArray()).length) {
        mode = 'immediate';
        args = [args[0][1].trim()];
      } else {
        mode = 'memory';
        ambig = true;
        args = [line];
      }

      if (mode === 'memory') {
        if (['jmp','jsr'].includes(instruction)) {
          mode = 'absolute';
          ambig = false;
        }
      } else if (mode === 'indexed x') {
        if (['sty'].includes(instruction)) {
          mode = 'zero page x';
          ambig = false;
        }
      } else if (mode === 'indexed y') {
        if (['lda','sta','ora','and','eor','adc','sbc','cmp'].includes(instruction)) {
          mode = 'absolute y';
          ambig = false;
        } else if (['stx'].includes(instruction)) {
          mode = 'zero page y';
          ambig = false;
        }
      }

      tokens.push({type: 'instruction', value: instruction, mode, arguments: args, ambig, line: _line});
    }
  });

  tokens.forEach(token => {
    token.arguments = token.arguments.map(arg => Expression.fromString(arg));
    if (token.type === 'label') {
      token.value = new Label(token.value, new ResolvableValue(0));
    }
  });

  var dirty = true;
  var i = 0;
  var pc = 0;

  while (dirty && i++ < 100) {
    dirty = false;
    pc = 0;

    for (var j = 0; j < tokens.length; j++) {
      var token = tokens[j];
      if (token.type === 'label') {
        if (token.value.value.resolve().value !== pc) {
          token.value.value = new ResolvableValue(pc);
          dirty = true;
        }
      } else if (token.type === 'instruction') {
        var arg = token.arguments.length ? token.arguments[0].resolve() : false;
        if (arg && arg.resolved && token.ambig) {
          var size = arg.value < 0x100 ? 1 : 2;
          var old = token.mode;
          if (['memory','zero page','absolute'].includes(token.mode)) {
            token.mode = size === 1 ? 'zero page' : 'absolute';
          } else if (['indexed x','zero page x','absolute x'].includes(token.mode)) {
            token.mode = size === 1 ? 'zero page x' : 'absolute x';
          } else if (['indexed y','zero page y','absolute y'].includes(token.mode)) {
            token.mode = size === 1 ? 'zero page y' : 'absolute y';
          }
          if (old !== token.mode) {
            dirty = true;
          }
        }

        pc += ins_lens[token.mode] || 1;
      } else if (token.type === 'directive') {
        token.arguments.forEach((arg,i) => arg.resolve());
        var directive = token.value;
        if (directive === 'org') {
          var arg = token.arguments[0].resolve();
          if (arg.resolved) pc = arg.value;
        } else if (directive === 'cps') {
          var arg = token.arguments[0].resolve();
          if (arg.resolved) cpsinp.value = cps = arg.value;
        } else if (directive === 'palette') {
          var arg = token.arguments[0].resolve();
          if (arg.resolved) colors = palettes[arg.value];
        } else if (directive === 'db' || directive === 'byte') {
          pc += token.arguments.length;
        } else if (directive === 'dw' || directive === 'word') {
          pc += token.arguments.length * 2;
        }
      }
    }
  }
  console.log(i);
  mem.fill(0);
  dbgarr.fill('');

  opcodelut = [
    [0x00,'brk','implied'],
    [0x01,'ora','indirect x'],
    [0x02,'hlt','immediate'],
    [0x03,'slo','indirect x'],
    [0x04,'nop','zero page'],
    [0x05,'ora','zero page'],
    [0x06,'asl','zero page'],
    [0x07,'slo','zero page'],
    [0x08,'php','implied'],
    [0x09,'ora','immediate'],
    [0x0a,'asl','accumulator'],
    [0x0b,'anc','immediate'],
    [0x0c,'nop','absolute'],
    [0x0d,'ora','absolute'],
    [0x0e,'asl','absolute'],
    [0x0f,'slo','absolute'],
    [0x10,'bpl','relative'],
    [0x11,'ora','indirect y'],
    [0x12,'hlt','immediate'],
    [0x13,'slo','indirect y'],
    [0x14,'nop','zero page x'],
    [0x15,'ora','zero page x'],
    [0x16,'asl','zero page x'],
    [0x17,'slo','zero page x'],
    [0x18,'clc','implied'],
    [0x19,'ora','absolute y'],
    [0x1a,'nop','implied'],
    [0x1b,'slo','absolute y'],
    [0x1c,'nop','absolute x'],
    [0x1d,'ora','absolute x'],
    [0x1e,'asl','absolute x'],
    [0x1f,'slo','absolute x'],
    [0x20,'jsr','absolute'],
    [0x21,'and','indirect x'],
    [0x22,'hlt','immediate'],
    [0x23,'rla','indirect x'],
    [0x24,'bit','zero page'],
    [0x25,'and','zero page'],
    [0x26,'rol','zero page'],
    [0x27,'rla','zero page'],
    [0x28,'plp','implied'],
    [0x29,'and','immediate'],
    [0x2a,'rol','accumulator'],
    [0x2b,'anc','immediate'],
    [0x2c,'bit','absolute'],
    [0x2d,'and','absolute'],
    [0x2e,'rol','absolute'],
    [0x2f,'rla','absolute'],
    [0x30,'bmi','relative'],
    [0x31,'and','indirect y'],
    [0x32,'hlt','immediate'],
    [0x33,'rla','indirect y'],
    [0x34,'nop','zero page x'],
    [0x35,'and','zero page x'],
    [0x36,'rol','zero page x'],
    [0x37,'rla','zero page x'],
    [0x38,'sec','implied'],
    [0x39,'and','absolute y'],
    [0x3a,'nop','implied'],
    [0x3b,'rla','absolute y'],
    [0x3c,'nop','absolute x'],
    [0x3d,'and','absolute x'],
    [0x3e,'rol','absolute x'],
    [0x3f,'rla','absolute x'],
    [0x40,'rti','implied'],
    [0x41,'eor','indirect x'],
    [0x42,'hlt','immediate'],
    [0x43,'sre','indirect x'],
    [0x44,'nop','zero page'],
    [0x45,'eor','zero page'],
    [0x46,'lsr','zero page'],
    [0x47,'sre','zero page'],
    [0x48,'pha','implied'],
    [0x49,'eor','immediate'],
    [0x4a,'lsr','accumulator'],
    [0x4b,'alr','immediate'],
    [0x4c,'jmp','absolute'],
    [0x4d,'eor','absolute'],
    [0x4e,'lsr','absolute'],
    [0x4f,'sre','absolute'],
    [0x50,'bvc','relative'],
    [0x51,'eor','indirect y'],
    [0x52,'hlt','immediate'],
    [0x53,'sre','indirect y'],
    [0x54,'nop','zero page x'],
    [0x55,'eor','zero page x'],
    [0x56,'lsr','zero page x'],
    [0x57,'sre','zero page x'],
    [0x58,'cli','implied'],
    [0x59,'eor','absolute y'],
    [0x5a,'nop','implied'],
    [0x5b,'sre','absolute y'],
    [0x5c,'nop','absolute x'],
    [0x5d,'eor','absolute x'],
    [0x5e,'lsr','absolute x'],
    [0x5f,'sre','absolute x'],
    [0x60,'rts','implied'],
    [0x61,'adc','indirect x'],
    [0x62,'hlt','immediate'],
    [0x63,'rra','indirect x'],
    [0x64,'nop','zero page'],
    [0x65,'adc','zero page'],
    [0x66,'ror','zero page'],
    [0x67,'rra','zero page'],
    [0x68,'pla','implied'],
    [0x69,'adc','immediate'],
    [0x6a,'ror','accumulator'],
    [0x6b,'arr','immediate'],
    [0x6c,'jmp','indirect'],
    [0x6d,'adc','absolute'],
    [0x6e,'ror','absolute'],
    [0x6f,'rra','absolute'],
    [0x70,'bvs','relative'],
    [0x71,'adc','indirect y'],
    [0x72,'hlt','immediate'],
    [0x73,'rra','indirect y'],
    [0x74,'nop','zero page x'],
    [0x75,'adc','zero page x'],
    [0x76,'ror','zero page x'],
    [0x77,'rra','zero page x'],
    [0x78,'sei','implied'],
    [0x79,'adc','absolute y'],
    [0x7a,'nop','implied'],
    [0x7b,'rra','absolute y'],
    [0x7c,'nop','absolute x'],
    [0x7d,'adc','absolute x'],
    [0x7e,'ror','absolute x'],
    [0x7f,'rra','absolute x'],
    [0x80,'nop','immediate'],
    [0x81,'sta','indirect x'],
    [0x82,'nop','immediate'],
    [0x83,'sax','indirect x'],
    [0x84,'sty','zero page'],
    [0x85,'sta','zero page'],
    [0x86,'stx','zero page'],
    [0x87,'sax','zero page'],
    [0x88,'dey','implied'],
    [0x89,'nop','immediate'],
    [0x8a,'txa','implied'],
    [0x8b,'xaa','immediate'],
    [0x8c,'sty','absolute'],
    [0x8d,'sta','absolute'],
    [0x8e,'stx','absolute'],
    [0x8f,'sax','absolute'],
    [0x90,'bcc','relative'],
    [0x91,'sta','indirect y'],
    [0x92,'hlt','immediate'],
    [0x93,'ahx','indirect y'],
    [0x94,'sty','zero page x'],
    [0x95,'sta','zero page x'],
    [0x96,'stx','zero page y'],
    [0x97,'sax','zero page y'],
    [0x98,'tya','implied'],
    [0x99,'sta','absolute y'],
    [0x9a,'txs','implied'],
    [0x9b,'tas','absolute y'],
    [0x9c,'shy','absolute x'],
    [0x9d,'sta','absolute x'],
    [0x9e,'shx','absolute y'],
    [0x9f,'ahx','absolute y'],
    [0xa0,'ldy','immediate'],
    [0xa1,'lda','indirect x'],
    [0xa2,'ldx','immediate'],
    [0xa3,'lax','indirect x'],
    [0xa4,'ldy','zero page'],
    [0xa5,'lda','zero page'],
    [0xa6,'ldx','zero page'],
    [0xa7,'lax','zero page'],
    [0xa8,'tay','implied'],
    [0xa9,'lda','immediate'],
    [0xaa,'tax','implied'],
    [0xab,'lax','immediate'],
    [0xac,'ldy','absolute'],
    [0xad,'lda','absolute'],
    [0xae,'ldx','absolute'],
    [0xaf,'lax','absolute'],
    [0xb0,'bcs','relative'],
    [0xb1,'lda','indirect y'],
    [0xb2,'hlt','immediate'],
    [0xb3,'lax','indirect y'],
    [0xb4,'ldy','zero page x'],
    [0xb5,'lda','zero page x'],
    [0xb6,'ldx','zero page y'],
    [0xb7,'lax','zero page y'],
    [0xb8,'clv','implied'],
    [0xb9,'lda','absolute y'],
    [0xba,'tsx','implied'],
    [0xbb,'las','absolute y'],
    [0xbc,'ldy','absolute x'],
    [0xbd,'lda','absolute x'],
    [0xbe,'ldx','absolute y'],
    [0xbf,'lax','absolute y'],
    [0xc0,'cpy','immediate'],
    [0xc1,'cmp','indirect x'],
    [0xc2,'nop','immediate'],
    [0xc3,'dcp','indirect x'],
    [0xc4,'cpy','zero page'],
    [0xc5,'cmp','zero page'],
    [0xc6,'dec','zero page'],
    [0xc7,'dcp','zero page'],
    [0xc8,'iny','implied'],
    [0xc9,'cmp','immediate'],
    [0xca,'dex','implied'],
    [0xcb,'axs','immediate'],
    [0xcc,'cpy','absolute'],
    [0xcd,'cmp','absolute'],
    [0xce,'dec','absolute'],
    [0xcf,'dcp','absolute'],
    [0xd0,'bne','relative'],
    [0xd1,'cmp','indirect y'],
    [0xd2,'hlt','immediate'],
    [0xd3,'dcp','indirect y'],
    [0xd4,'nop','zero page x'],
    [0xd5,'cmp','zero page x'],
    [0xd6,'dec','zero page x'],
    [0xd7,'dcp','zero page x'],
    [0xd8,'cld','implied'],
    [0xd9,'cmp','absolute y'],
    [0xda,'nop','implied'],
    [0xdb,'dcp','absolute y'],
    [0xdc,'nop','absolute x'],
    [0xdd,'cmp','absolute x'],
    [0xde,'dec','absolute x'],
    [0xdf,'dcp','absolute x'],
    [0xe0,'cpx','immediate'],
    [0xe1,'sbc','indirect x'],
    [0xe2,'nop','immediate'],
    [0xe3,'isc','indirect x'],
    [0xe4,'cpx','zero page'],
    [0xe5,'sbc','zero page'],
    [0xe6,'inc','zero page'],
    [0xe7,'isc','zero page'],
    [0xe8,'inx','implied'],
//  [0xe9,'sbc','immediate'],
//  [0xea,'nop','implied'],
    [0xeb,'sbc','immediate'],
    [0xec,'cpx','absolute'],
    [0xed,'sbc','absolute'],
    [0xee,'inc','absolute'],
    [0xef,'isc','absolute'],
    [0xf0,'beq','relative'],
    [0xf1,'sbc','indirect y'],
    [0xf2,'hlt','immediate'],
    [0xf3,'isc','indirect y'],
    [0xf4,'nop','zero page x'],
    [0xf5,'sbc','zero page x'],
    [0xf6,'inc','zero page x'],
    [0xf7,'isc','zero page x'],
    [0xf8,'sed','implied'],
    [0xf9,'sbc','absolute y'],
    [0xfa,'nop','implied'],
    [0xfb,'isc','absolute y'],
    [0xfc,'nop','absolute x'],
    [0xfd,'sbc','absolute x'],
    [0xfe,'inc','absolute x'],
    [0xff,'isc','absolute x'],
// these are at the end so the assembler uses the right op-code
    [0xe9,'sbc','immediate'],
    [0xea,'nop','implied'],
  ].reduce((a,v) => {
    var [code,inst,mode] = v;
    if (!a[inst]) a[inst] = {};
    a[inst][mode] = code;
    return a;
  }, {});
  pc = 0;

  for (var i = 0; i < tokens.length; i++) {
    var token = tokens[i];
    var line = token.line ?? '';
    if (token.type === 'instruction') {
      var inst = token.value;
      var mode = token.mode;
      var opcode = opcodelut[inst][mode];
      var size = ins_lens[mode];
      dbgarr[pc] = line;
      mem[pc++] = opcode;
      if (size === 2) {
        var arg = token.arguments[0].resolve().value;
        if (mode === 'relative') {
          dbgarr[pc] = line;
          mem[pc++] = arg - pc;
        } else {
          dbgarr[pc] = line;
          mem[pc++] = arg;
        }
      } else if (size === 3) {
        var arg = token.arguments[0].resolve().value;
        dbgarr[pc] = line;
        mem[pc++] = arg;
        dbgarr[pc] = line;
        mem[pc++] = arg >> 8;
      }
    } else if (token.type === 'directive') {
      var dir = token.value;
      var args = token.arguments.map(arg => arg.resolve());
      var arg = args[0];
      if (dir === 'org' && arg) {
        pc = arg.value;
      } else if (dir === 'db' || dir === 'byte') {
        args.forEach(arg => {
          mem[pc++] = arg.value;
        });
      } else if (dir === 'dw' || dir === 'word') {
        args.forEach(arg => {
          dbgarr[pc] = line;
          mem[pc++] = arg.value;
          dbgarr[pc] = line;
          mem[pc++] = arg.value >> 8;
        });
      }
    }
  }

  return mem;
}

return assemble;

})();
