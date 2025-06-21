<head>
  <meta charset="UTF-8">
  <title>6502</title>
  <style>
    html, body {
      margin: 0;
      padding: 0;
      width: 100vw;
      height: 100vh;
      display: flex;
      overflow: hidden;
    }
    #code-wrapper {
      position: absolute;
      width: 50vw;
      top: 0px;
      left: 0px;
      bottom: 0px;
    }
    #disp-wrapper {
      position: absolute;
      width: 50vw;
      height: 50vw;
      top: 0px;
      right: 0px;
    }
    #info-wrapper {
      position: absolute;
      width: 50vw;
      height: 50vh;
      bottom: 0px;
      right: 0px;
      display: flex;
      flex-direction: column;
      align-items: center;
    }
    #lr-handle {
      position: absolute;
      width: 8px;
      inset: 0px 0px 0px calc(-4px + 50vw);
      background-color: gray;
    }
    #ud-handle {
      position: absolute;
      width: 50vw;
      height: 8px;
      right: 0px;
      top: calc(-4px + 50vh);
      background-color: gray;
    }
    textarea {
      position: absolute;
      top: 30px;
      left: 5px;
      bottom: 5px;
      width: calc(100% - 14px);
      resize: none;
      outline: none;
      font: 12px "Kurinto Mono";
    }
    .canvas-container {
      position: absolute;
      top: 5px;
      right: 5px;
      display: flex;
      align-items: flex-end;
    }
    #resizer {
      width: 10px;
      height: 10px;
      background: gray;
      cursor: nesw-resize;
      position: absolute;
      bottom: -5px;
      left: -5px;
    }
    #button-container {
      display: flex;
      flex-direction: row;
      height: 20px;
      position: absolute;
      top: 5px;
      left: 5px;
      gap: 5px;
    }
    #reg-div {
      font: 18px "Kurinto Mono";
      white-space: pre;
      margin: 16 0 10 0;
    }
    #ram-div {
      font: 18px "Kurinto Mono";
      white-space: pre;
    }
  </style>
</head>
<body>
  <div id="code-wrapper">
    <div id="button-container">
      <button id="assemble-button">Assemble</button>
      <button id="runstop-button">Run</button>
      <button id="reset-button">Reset</button>
      <button id="step-button">Step</button>
      <div id="cps-wrapper">
        <span>CPS: </span>
        <input type=number id="cps-input" value=100 style=width:75px;/>
      </div>
      <button id="irq-button">IRQ</button>
      <button id="nmi-button">NMI</button>
    </div>
    <textarea id=area spellcheck=false></textarea>
  </div>
  <div id="disp-wrapper">
    <div class="canvas-container">
      <canvas id=canvas></canvas>
      <div id=resizer></div>
    </div>
  </div>
  <div id="info-wrapper">
    <div id="reg-div"></div>
    <div id="ram-div"></div>
  </div>
  <div id="lr-handle"></div>
  <div id="ud-handle"></div>
  <script src=examples.js></script>
  <script src=luts.js></script>
  <script src=6502emu.js></script>
  <script src=assembler.js></script>
  <script>
    var codewrapper = document.getElementById('code-wrapper');
    var dispwrapper = document.getElementById('disp-wrapper');
    var infowrapper = document.getElementById('info-wrapper');
    var lrhandle = document.getElementById('lr-handle');
    var udhandle = document.getElementById('ud-handle');

    lrhandle.onpointerdown = function(event) {
      this.setPointerCapture(event.pointerId);
      var mx = window.innerWidth;
      this.onpointermove = function(event) {
        var x = event.clientX;
        var t = x / mx;
        t = Math.min(.8,Math.max(.2,t));
        var l = Math.round(t * mx);
        var r = mx - l;
        this.style.left = l - 4 + 'px';
        codewrapper.style.width = l + 'px';
        dispwrapper.style.width = r + 'px';
        infowrapper.style.width = r + 'px';
        udhandle.style.width = r + 'px';
      }
      this.onpointerup = function(event) {
        this.releasePointerCapture(event.pointerId);
        this.onpointermove = this.onpointerup = null;
      }
    }

    udhandle.onpointerdown = function(event) {
      this.setPointerCapture(event.pointerId);
      var my = window.innerHeight;
      this.onpointermove = function(event) {
        var y = event.clientY;
        var t = y / my;
        t = Math.min(.8,Math.max(.2,t));
        var u = Math.round(t * my);
        var d = my - u;
        this.style.top = u - 4 + 'px';
        dispwrapper.style.height = u + 'px';
        infowrapper.style.height = d + 'px';
      }
      this.onpointerup = function(event) {
        this.releasePointerCapture(event.pointerId);
        this.onpointermove = this.onpointerup = null;
      }
    }


    var canvas = document.getElementById('canvas');
    var area = document.getElementById('area');
    var ctx = canvas.getContext('2d');
    var resizer = document.getElementById('resizer');
    var assemblebtn = document.getElementById('assemble-button');
    var runstopbtn = document.getElementById('runstop-button');
    var resetbtn = document.getElementById('reset-button');
    var stepbtn = document.getElementById('step-button');
    var cpsinp = document.getElementById('cps-input');
    var irqbtn = document.getElementById('irq-button');
    var nmibtn = document.getElementById('nmi-button');
    var registerDiv = document.getElementById('reg-div');
    var ramDiv = document.getElementById('ram-div');

    var assembled = false;

    assemblebtn.onclick = function(event) {
      var asm = area.value;
      assemble(asm);
      sendReset();
      draw();
    }

    runstopbtn.onclick = function(event) {
      if (running) {
        halt();
        runstopbtn.innerText = 'Run';
      } else {
        run();
        runstopbtn.innerText = 'Stop';
      }
    }

    resetbtn.onclick = sendReset;

    irqbtn.onclick = IRQ;

    nmibtn.onclick = NMI;

    stepbtn.onclick = function(event) {
      step();
      updDisplay();
    }

    cpsinp.oninput = function(event) {
      cps = +event.target.value;
    }

    function draw() {
      for (var a=0x200; a<0x600; a++) {
        drawPixel(a,mem[a]);
      }
    }

    resizer.onmousedown = function(event) {
      event.preventDefault();
      document.onmousemove = resizeCanvas;
      document.onmouseup = stopResize;
    };

    function resizeCanvas(event) {
      var width = window.innerWidth - event.clientX - 5;
      var height = event.clientY - 5;
      width = Math.round(width / 32) * 32;
      height = Math.round(height / 32) * 32;
      canvas.width = canvas.height = Math.max(32, Math.min(width, height));
      draw();
    }

    function stopResize() {
      document.onmousemove = document.onmouseup = null;
    }
    
    canvas.width = canvas.height = 320;
    var pxsize = 10;

    var params = new URLSearchParams(location.search);

    function loadAsmRaw(txt) {
      area.value = txt;
      assemble(txt);
    }

    if (params.has('code')) {
      loadAsmRaw(params.get('code'));
    }
    if (params.has('example')) {
      var example = params.get('example');
      if (example in examples) {
        loadAsmRaw(examples[example]);
      }
    }

    draw();
    updDisplay();
  </script>
</body>
</html>
