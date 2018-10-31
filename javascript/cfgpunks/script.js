/**
 * Prototype for an EXAPUNKS editor doing dead code analysis.
 *
 * Idea from
 * https://old.reddit.com/r/exapunks/comments/9s0oxn/the_game_should_use_unreachable_code_detection/
 *
 * We use CodeMirror for the editor.  Whenever the code changes, we run the dead
 * code analysis and highlight the dead code in light grey text.
 *
 * If the user clicks a REPL keyword, we show the dead code analysis from the
 * point of view of the spawned EXA (i.e., starting from the REPL target label).
 */

// TODO: currently we need to click on the gutter to change the starting line,
// but that is buggy if the document changes, as that will invalidate the line.
// The less surprising UX is to always analyse from the marker, as long as the
// marker is present, and remove the marker if we remove the line.

document.addEventListener('DOMContentLoaded', init);

// CodeMirror instance
let cm

// Setup CodeMirror editor and run analysis
function init() {
  const source = document.querySelector('#source')
  cm = CodeMirror.fromTextArea(source, {
    lineNumbers: true,
  })

  // Turn text to uppercase automatically
  cm.on('beforeChange', (_, obj) => {
    obj.update(null, null,
               obj.text.map(l => l.toUpperCase()))
  })

  // Save to text area on change lets the browser save the content of the area
  // without explicitly using local storage.
  cm.on('change', _ => cm.save())

  // Trigger analysis on input change
  cm.on('change', debounce(analyze, 20))

  // Whenever we click on the gutter, analyze code starting from this line from
  // now on.
  cm.on('gutterClick', (_, line) => {
    highlight_lines([line], 'start-line', 'gutter')
    analyze(line)
  })

  // Trigger analysis on load
  analyze()
}

// Analyze code to find and highlight dead code starting from editor line START.
function analyze(start = find_start_line()) {
  const code = parse(cm.getValue())
  // TODO: validate code (ensure labels are defined and unique)
  const cfg = build_cfg(code, code_line(code, start))
  const dead = find_dead_code(code, cfg)

  highlight_dead_code(dead, code)
}

// Return the editor line of code the user clicked on, or the default starting
// line of the program (0).
function find_start_line() {
  // TODO:
  return 0
}

// Highlight dead code lines in the editor.
//
// DEAD is the array of dead lines indices in CODE.
function highlight_dead_code(dead, code) {
  // Convert to editor lines
  const dead_lines = dead.map(l => code[l].line)

  highlight_lines(dead_lines, 'dead')
}

// Add class CSS to lines LINES, and remove it on all the other lines in the
// document.
//
// WHERE has the same meaning as in cm.addLineClass.
function highlight_lines(lines, css, where = 'text') {
  cm.eachLine(l => {
    if (lines.includes(cm.getLineNumber(l))) {
      cm.addLineClass(l, where, css)
    } else {
      cm.removeLineClass(l, where, css)
    }
  })
}

// Return the index of instruction in CODE corresponding to editor LINE.
function code_line(code, line) {
  return code.findIndex(l => l.line === line)
}

// Return a control-flow graph (CFG) of CODE, starting at index START.
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

    case 'FJMP':
    case 'TJMP':
      const targets = [label_line(instr.args[0])]
      if (n + 1 < code.length) {
        targets.push(n + 1)
      }
      return targets

    case 'HALT':
      return []

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
