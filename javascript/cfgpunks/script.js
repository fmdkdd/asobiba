document.addEventListener('DOMContentLoaded', init);

// CodeMirror instance
let cm

// Setup CodeMirror editor and run analysis
function init() {
  const source = document.querySelector('#source')
  cm = CodeMirror.fromTextArea(source, {
    lineNumbers: true,
  })

  // Save to text area on change lets the browser save the content of the area
  // without explicitly using local storage.
  cm.on('change', _ => cm.save())

  cm.on('change', debounce(analyze, 20))
  analyze() // trigger analysis on load
}

function analyze() {
  const code = parse(cm.getValue())
  // TODO: validate code (ensure labels are defined and unique)
  const cfg = build_cfg(code)
  const dead = find_dead_code(code, cfg)

  highlight_dead_code(dead, code)
}

// Highlight dead code lines in the editor.
//
// DEAD is the array of dead lines indices in CODE.
function highlight_dead_code(dead, code) {
  // Convert to editor lines
  const dead_lines = dead.map(l => code[l].line)

  cm.eachLine(l => {
    if (dead_lines.includes(cm.getLineNumber(l))) {
      cm.addLineClass(l, 'text', 'dead')
    } else {
       cm.removeLineClass(l, 'text', 'dead')
    }
  })
}

// Return a control-flow graph (CFG) of CODE, starting at line START.
//
// The CFG is an array of successors, as indices into the CODE array.
function build_cfg(code, start = 0) {
  // Indices of the lines where the execution can continue after executing line
  // N.
  function successors(n) {
    const instr = code[n]

    switch (instr.keyword) {
    case 'JUMP':
      return [label_line(instr.args[0])]

    default:
      return n + 1 < code.length ? [n + 1] : []
    }
  }

  const labels_cache = {}

  // The line index where LABEL is defined.
  function label_line(label) {
    if (labels_cache[label] == null) {
      // Find the corresponding MARK instruction
      labels_cache[label] = code
        .findIndex(c => c.keyword == 'MARK' && c.args[0] == label)
    }

    return labels_cache[label]
  }


  // The control-flow graph
  const cfg = []
  // Indices of nodes yet to visit
  const queue = [start]
  // Indices of visited nodes
  const seen = []

  while (queue.length > 0) {
    const n = queue.shift()
    seen.push(n)
    cfg[n] = successors(n)
    const unseen = cfg[n]
          .filter(s => !seen.includes(s) &&!queue.includes(s))
    queue.push(...unseen)
  }

  return cfg
}

// Return the indices of lines in CODE that will never be executed, given
// control-flow graph CFG.
function find_dead_code(code, cfg) {
  // It's just the complementary set of the lines that are present in the CFG.
  const cand = new Set(Object.keys(code))
  Object.keys(cfg)
    .forEach(l => cand.delete(l))

  return Array.from(cand).map(l => parseInt(l))
}

// Return an array of decoded instructions from raw text SRC.
//
// An instruction is an object
// {keyword: string, args: [string], line: number}.
function parse(src) {
  const lines = src.split('\n')
  return lines
    .map(l => l.trim())
    .map(number_line)
    .filter(l => l.text.length > 0)
    .map(parse_line)

  function number_line(l, i) {
    return {text: l, line: i}
  }

  function parse_line(l) {
    // KEYWORD [ARG1 [ARG2 [ARG3]]]
    const [keyword, ...args] = l.text.split(/ +/)
    return {keyword, args, line: l.line}
  }
}


// Return a function (R) that calls F *once* after DELAY.  Further calls to R
// before DELAY are ignored, and will result in a single call to F after DELAY.
function debounce(f, delay = 80) {
  let timer
  return function() {
    if (timer) {
      clearTimeout(timer)
    }
    timer = setTimeout(_ => f.apply(this, arguments), delay)
  }
}
