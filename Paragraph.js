const _ = require('./lodash')

const formatsEqual = (f1, f2) =>
  _.isEqual(f1.slice().sort(), f2.slice().sort())

class Selection {
  get single() {
    return (
      this.start.pid === this.end.pid &&
      this.start.offset === this.end.offset
    )
  }

  // Shortcut for this.start.offset for single selections
  get caret() {
    // TODO: Test me
    if (!this.single) {
      throw new Error("Cannot call caret on range selection")
    }
    return this.start.offset
  }

  constructor(start, end, backwards = false) {
    this.start = start
    this.end = end || start
    this.backwards = backwards

    if (this.start.offset < 0) {
      throw new Error("Cannot create Selection with illegal start offset " + this.start.offset)
    }
    if (this.end.offset < 0) {
      throw new Error("Cannot create Selection with illegal end offset + " + this.end.offset)
    }

    if (this.start === this.end && this.start.offset > this.end.offset) {
      throw new Error("Error initializing Selection: end offset cannot precede start offset.")
    }
  }

  // Returns new single selection at the start of this selection.
  // If this selection is a multiple selection, returns itself.
  collapse() {
    // TODO: should we allow this?
    if (this.single) {
      throw new Error("Cannot call collapse() on single selection")
    }
    return this.single ? this : new Selection(this.start)
  }

  // For a single selection, returns new
  // Selection advanced by `n` characters
  shiftSingle(n) {
    if (!this.single) {
      throw new Error("Cannot call shiftSingle() on range selection")
    }
    return new Selection({ ...this.start, offset: this.start.offset + n })
  }
}

class Run {
  // Returns an empty run (no text or formatting)
  static empty() {
    return new Run('', [])
  }

  constructor(text, formats = []) {
    // TODO: remove id
    this.text = text
    this.formats = formats
  }

  get length() {
    // TODO: memoize
    return this.text.length
  }

  // Splits run into two separate runs at offset
  split(offset) {
    // If we perform a split on either edge of the run, return the umodified run
    // for one half, and an empty run for the other. This prevents using null semanitcally
    // (which I'm convinced is the devil), and turns out to play quite nicely with optimizeRuns.
    if (offset === 0) {
      return [Run.empty(), this]
    }
    else if (offset === this.length) {
      return [this, Run.empty()]
    }

    const textBefore = this.text.slice(0, offset)
    const textAfter = this.text.slice(offset, this.length)
    return [
      new Run(textBefore, this.formats),
      new Run(textAfter, this.formats)
    ]
  }

  /**
   * Returns resulting [new run, new selection] pair
   * from inserting the `text` at `selection`.
   *
   * @param selection Selection object
   * @param text String to insert
   *
   * @return resulting run
   */
  insert(text, start, end) {
    [start, (end || 0)].forEach(offset => {
      if (offset < 0 || offset > this.length) {
        throw new Error("Illegal offset Run " + offset);
      }
    })

    // Single selection
    if (end === undefined) {
      const before = this.text.slice(0, start)
      const after = this.text.slice(start)
      const modifiedText = before + text + after

      return new Run(modifiedText, this.formats)
    }
    else {
      const before = this.text.slice(0, start)
      const after = this.text.slice(end)
      const modifiedText = before + text + after
      return new Run(modifiedText, this.formats)
    }
  }

  // Alias for inserting at start of run
  insertStart(text) {
    return this.insert(text, 0)
  }

  // Alias for inserting at end of run
  insertEnd(text) {
    return this.insert(text, this.length)
  }

  /**
   * Returns the [new run, new selection] pair from removing at `selection`.
   *
   * @param selection Selection object
   *
   * @return [new run, new selection]
   */
  remove(start, end) {
    [start, (end || 0)].forEach(offset => {
      if (offset < 0 || offset > this.length) {
        throw new Error("Illegal offset into Run " + offset);
      }
    })

    // Single-selection
    if (end === undefined || end === null) {
      if (start === 0) {
        throw new Error('Cannot call remove() on a single selection at position 0.')
      }

      const before = this.text.slice(0, start - 1)
      const after = this.text.slice(start)
      const modifiedText = before + after

      return new Run(modifiedText, this.formats)
    }
    else {
      const before = this.text.slice(0, start)
      const after = this.text.slice(end)
      const modifiedText = before + after

      return new Run(modifiedText, this.formats)
    }
  }

  // Returns a new Run with `formats` added
  applyFormats(formats) {
    const intersection = new Set([...formats, ...this.formats])
    return new Run(this.text, [...intersection])
  }

  // Returns a new Run with `formats` removed
  removeFormats(formats) {
    return new Run(this.text, this.formats.filter(f => !formats.includes(f)))
  }

  render() {
    // TODO: this should maybe be changed to use CSS or something
    const stringifyFormats = (stringifier) => this.formats.map(f => stringifier(f)).join('')
    return stringifyFormats(f => `<${f}>`) + this.text + stringifyFormats(f => `</${f}>`)
  }
}

class Paragraph {
  static fromTemplate(obj) {
    return [
      new Selection(
        obj.selection.start,
        obj.selection.end,
      ),
      new Paragraph(
        obj.runs.map(r => new Run(r.text, r.formats))
      ),
    ]
  }

  // Given a list of runs, returns a list with adjacent
  // runs of equal formatting merged, and empty runs removed.
  static optimizeRuns(runs) {
    let optimized = [runs[0]]

    for (let i = 1; i < runs.length; i++) {
      if (formatsEqual(optimized[optimized.length - 1].formats, runs[i].formats)) {
        // Combine adjacent runs with equal formatting
        optimized[optimized.length - 1] = optimized[optimized.length - 1].insertEnd(runs[i].text)
      }
      else if (!runs[i].text) {
        // Remove empty runs
        continue
      }
      else {
        // No optimiziation to do here, move along
        optimized.push(runs[i])
      }
    }

    return optimized
  }

  constructor(runs) {
    this.runs = runs
    this.id = -1 // TODO
  }

  // Returns the sum of the lengths of the paragraph's runs
  get length() {
    // TODO: memoize
    return this.runs.reduce((acc, run) => acc + run.length, 0)
  }

  // Like atOffset, but returns pair for run immediately before text caret
  // TODO: better documentation
  beforeOffset(offset) {
    if (offset < 0 || offset > this.length) {
      throw new Error("Illegal offset into Paragraph " + offset)
    }

    if (offset === 0) {
      return [0, 0]
    }

    let runIdx = -1;
    let offsetIntoRun = 0;
    let sumOfPreviousOffsets = 0;

    while (sumOfPreviousOffsets < offset) {
      runIdx += 1
      offsetIntoRun = offset - sumOfPreviousOffsets
      sumOfPreviousOffsets += this.runs[runIdx].length
    }

    return [runIdx, offsetIntoRun]
  }

  // Returns [run index, run offset] at `offset` into paragraph
  atOffset(offset) {
    if (offset < 0 || offset > this.length) {
      throw new Error("Illegal offset into Paragraph " + offset)
    }

    if (offset === this.length) {
      const lastRunIdx = this.runs.length - 1
      return [lastRunIdx, this.runs[lastRunIdx].length]
    }

    let runIdx = -1;
    let offsetIntoRun = 0;
    let sumOfPreviousOffsets = 0;

    while (sumOfPreviousOffsets <= offset) {
      runIdx += 1
      offsetIntoRun = offset - sumOfPreviousOffsets
      sumOfPreviousOffsets += this.runs[runIdx].length
    }

    return [runIdx, offsetIntoRun]
  }

  // runs: array of Runs
  // selection: Selection
  insert(runs, selection) {
    if (selection.single) {
      // Get run under text caret
      const [targetRunIdx, targetRunOffset] = this.atOffset(selection.caret)
      const targetRun = this.runs[targetRunIdx]

      const runsBeforeTarget = this.runs.slice(0, targetRunIdx)
      const runsAfterTarget = this.runs.slice(targetRunIdx + 1, this.runs.length)

      if (targetRunOffset === 0) {
        // Caret is at the beginning of a run
        var newRuns = [
          ...runsBeforeTarget,
          ...runs,
          targetRun,
          ...runsAfterTarget
        ]
      }
      else if (targetRunOffset === targetRun.length) {
        // Caret is at the end of a run. (This will
        // only happen at the last item run the list of runs)
        var newRuns = [
          ...runsBeforeTarget,
          targetRun,
          ...runs,
          ...runsAfterTarget
        ]
      }
      else {
        // Caret is in the middle of a run.
        const [t1, t2] = targetRun.split(targetRunOffset)
        var newRuns = [
          ...runsBeforeTarget,
          t1, ...runs, t2,
          ...runsAfterTarget
        ]
      }

      return [
        new Paragraph(Paragraph.optimizeRuns(newRuns)),
        selection.shiftSingle(runs.reduce((lens, r) => lens + r.length, 0))
      ]
    }
    else {
      // When a range is selected, remove the range and insert normally
      const [paragraphRemoved, selectionRemoved] = this.remove(selection)
      return paragraphRemoved.insert(runs, selectionRemoved)
    }
  }

  remove(selection) {
    [selection.start.offset, selection.end.offset || 0].forEach(offset => {
      if (offset < 0 || offset > this.length) {
        throw new Error("Illegal offset into paragraph: " + offset)
      }
    })

    if (selection.single) {
      if (selection.caret === 0) {
        throw new Error("Cannot perform single-selection remove (backspace) on Paragraph at start of Paragraph.")
      }

      const [targetRunIdx, targetRunOffset] = this.beforeOffset(selection.caret)
      const targetRun = this.runs[targetRunIdx]

      const newRuns = [...this.runs]
      newRuns[targetRunIdx] = targetRun.remove(targetRunOffset)

      return [
        new Paragraph(Paragraph.optimizeRuns(newRuns)),
        selection.shiftSingle(-1)
      ]
    }
    else {
      const [startRunIdx, startOffset] = this.atOffset(selection.start.offset)
      const [endRunIdx, endOffset] = this.atOffset(selection.end.offset)
      const startRun = this.runs[startRunIdx]
      const endRun = this.runs[endRunIdx]

      if (startRun === endRun) {
        var newRuns = [...this.runs]
        newRuns[startRunIdx] = startRun.remove(startOffset, endOffset)
      }
      else {
        const [startRunTrimmed, startRunDiscarded] = startRun.split(startOffset)
        const [endRunDiscarded, endRunTrimmed] = endRun.split(endOffset)

        const runsBeforeStart = this.runs.slice(0, startRunIdx)
        const runsAfterEnd = this.runs.slice(endRunIdx + 1, this.runs.length)

        var newRuns = [
          ...runsBeforeStart,
          startRunTrimmed,
          endRunTrimmed,
          ...runsAfterEnd
        ]
      }

      return [
        new Paragraph(Paragraph.optimizeRuns(newRuns)),
        selection.collapse()
      ]
    }
  }

  render() {
    return this.runs.map(r => r.render()).join('')
  }
}

const run1 = new Run('Foobar 1.', ["bold"])
const run2 = new Run(' Foobar 2.')
const run3 = new Run(' Foobar 3.', ["italic"])
const paragraph = new Paragraph([run1, run2, run3])

const myParagraph = new Paragraph([
  new Run('Hello.', ['italic']),
  new Run('A', []),
  new Run('Goodbye.', ['bold'])
])

const [p, s] = myParagraph.insert(
  [new Run('hello!', ['bold'])],
  new Selection({ pid: 1, offset: 3 }, { pid: 1, offset: 8 })
)

const testTemplate = {
  selection: {
    range: false,
    start: {
      pid: 1,
      offset: 4 // between l and o
    }
  },
  runs: [
    {
      id: 1,
      text: "Hello world!",
      formats: [],
    },
    {
      id: 2,
      text: "This is bold.",
      formats: ['bold'],
    }
  ]
}

const testState = Paragraph.fromTemplate(testTemplate)

// console.log(testState)
// console.log(testState[0].end)

module.exports =  { Selection, Run, Paragraph }
