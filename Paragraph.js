function arraysEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (a.length != b.length) return false;

  for (var i = 0; i < a.length; ++i) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

const formatsEqual = (f1, f2) =>
  arraysEqual(f1.slice().sort(), f2.slice().sort())

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

  // For a single selection, returns new Selection moved by `n` characters
  shiftSingle(n) {
    if (!this.single) {
      throw new Error("Cannot call shiftSingle() on range selection")
    }
    return new Selection({ ...this.start, offset: this.start.offset + n })
  }

  // For a single selection, returns new Selection with caret set at `offset` in same
  // paragraph. It is the responsibility of the caller to ensure the offset is legal.
  // TODO: test
  setSingle(offset) {
    if (!this.single) {
      throw new Error("Cannot call setSingle() on range selection")
    }
    return new Selection({ ...this.start, offset: offset })
  }
}

class Run {
  // Returns an empty run (no text or formatting)
  static empty() {
    return new Run('', [])
  }

  constructor(text, formats = []) {
    this.text = text
    this.formats = formats
  }

  get length() {
    // TODO: memoize
    return this.text.length
  }

  get empty() {
    return this.text === null || this.text === undefined || this.text === ''
  }

  // Splits run into two separate runs at offset
  split(offset) {
    // If we perform a split on either edge of the run, return the umodified run
    // for one half, and an empty run for the other. This prevents using null semanitcally
    // (which I'm convinced is the devil), and turns out to play quite nicely with optimizeRuns.

    // TODO: are these two checks necessary? won't string.slice() automatically cover me here...?
    // Might just have to check if text === '' at end of block, if anything formats will have something,
    // which we don't want to leak either.
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

  // TODO: can applyFormats and removeFormats be removed?

  // Returns a new Run with `formats` added
  applyFormats(formats) {
    const intersection = new Set([...formats, ...this.formats])
    return new Run(this.text, [...intersection])
  }

  // Returns a new Run with `formats` removed
  removeFormats(formats) {
    return new Run(this.text, this.formats.filter(f => !formats.includes(f)))
  }

  // Returns a new Run with `f` toggled. E.g. if `f` is 'italic' and the run
  // currently has italics on, it will be removed; if not, it will be added.
  toggleFormat(format) {
    const newFormats = [...this.formats]
    const fIdx = newFormats.indexOf(format)

    if (fIdx !== -1) {
      // Remove format from newFormats if present
      newFormats.splice(fIdx, 1)
    }
    else {
      // Or add it if not
      newFormats.push(format)
    }

    return new Run(this.text, newFormats)
  }

  // TODO: we shouldn't even have render inside the data model at all
  render() {
    // TODO: this should maybe be changed to use CSS or something
    // (Also there is a bug here)
    const stringifyFormats = (stringifier) => this.formats.map(f => stringifier(f)).join('')
    return stringifyFormats(f => `<${f}>`) + this.text + stringifyFormats(f => `</${f}>`)
  }
}

class Paragraph {
  static fromTemplate(obj) {
    // TODO: this is wrong
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
    const firstWithText = runs.findIndex(r => !r.empty)
    if (firstWithText === -1) {
      return [runs[0]]
    }

    let optimized = [runs[firstWithText]]

    for (let i = firstWithText + 1; i < runs.length; i++) {
      if (formatsEqual(optimized[optimized.length - 1].formats, runs[i].formats)) {
        // Combine adjacent runs with equal formatting
        optimized[optimized.length - 1] = optimized[optimized.length - 1].insertEnd(runs[i].text)
      }
      else if (runs[i].empty) {
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

  constructor(runs = [new Run('', [])]) {
    this.runs = runs
    this.id = -1 // TODO
  }

  // Returns the sum of the lengths of the paragraph's runs
  get length() {
    // TODO: memoize
    return this.runs.reduce((len, run) => len + run.length, 0)
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

  // TODO: description
  // runs: array of Runs
  // selection: Selection
  insert(runs, selection) {
    // =========================================================================================
    // TODO: this function needs to be overloaded with two other options for the first argument:
    // (1) A single run, which will be equivalent to an array of 1 run, and (2) a string, which
    // will be equivalent to a single run with no formatting.
    // =========================================================================================

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
        // Range-selection where start and end are within a single run
        var newRuns = [...this.runs]
        newRuns[startRunIdx] = startRun.remove(startOffset, endOffset)
      }
      else {
        // Range-selection where start and end are within different runs
        const [startRunKept, startRunDiscarded] = startRun.split(startOffset)
        const [endRunDiscarded, endRunKept] = endRun.split(endOffset)

        const runsBeforeStart = this.runs.slice(0, startRunIdx)
        const runsAfterEnd = this.runs.slice(endRunIdx + 1, this.runs.length)

        var newRuns = [
          ...runsBeforeStart,
          startRunKept,
          endRunKept,
          ...runsAfterEnd
        ]
      }

      return [
        new Paragraph(Paragraph.optimizeRuns(newRuns)),
        selection.collapse()
      ]
    }
  }

  // Returns new Paragraph with runs inside selection replaced by the result
  // of calling `f` on each one. Will split runs if necessary so only selected.
  // text is changed. Returned list of runs will be optimized.
  mapSelection(f, selection) {
    const [startRunIdx, startOffset] = this.atOffset(selection.start.offset)
    const [endRunIdx, endOffset] = this.atOffset(selection.end.offset)
    const runsBeforeStart = this.runs.slice(0, startRunIdx)
    const runsAfterEnd = this.runs.slice(endRunIdx + 1, this.runs.length)
    const runsBetween = this.runs.slice(startRunIdx + 1, endRunIdx)

    // Split start and end runs into *just* the text that is within the
    // selection -- the text outside of the selection will be transformed
    const [startOutside, startInside] = this.runs[startRunIdx].split(startOffset)
    const [endInside, endOutside]   = this.runs[endRunIdx].split(endOffset)

    const selectedArea = [startInside, ...runsBetween, endInside]
    const transformed = selectedArea.map(r => f(r))

    const newRuns = Paragraph.optimizeRuns([
      ...runsBeforeStart,
      startOutside,
      ...transformed,
      endOutside,
      ...runsAfterEnd
    ])

    return new Paragraph(newRuns)
  }

  // Returns a list of all formats shared by the whole of `selection.` Example:
  // calling getFormat on the selection "<i><b>x</b></i><i>y</i>" will return
  // ["italic"], as that is the only format shared by the whole of the selection.
  getFormats(selection) {
    const [startRunIdx] = this.atOffset(selection.start.offset)
    const [endRunIdx] = this.beforeOffset(selection.end.offset)

    let commonFormats = this.runs[startRunIdx].formats

    for (let i = startRunIdx; i <= endRunIdx; i++) {
      const formats = this.runs[i].formats
      commonFormats = commonFormats.filter(f => formats.includes(f))
    }

    return commonFormats
  }

  applyFormats(formats, selection) {
    return this.mapSelection((r) => r.applyFormats(formats), selection)
  }

  removeFormats(formats, selection) {
    return this.mapSelection((r) => r.removeFormats(formats), selection)
  }

  // Toggles the given format on the selection. If all text in the selection
  // is formatted with `format`, then it will turn that format off for the selection.
  // If not, it will turn it on.
  toggleFormat(format, selection) {
    if (selection.single) {
      throw new Error("Cannot toggleFormat on a single selection.")
    }

    const [startRunIdx] = this.atOffset(selection.start.offset)
    const [endRunIdx] = this.atOffset(selection.end.offset)

    // Does the current selection already have the format we're toggling?
    const formatActivated = this.getFormats(selection).includes(format)

    // If so, remove that format from selection.
    if (formatActivated) {
      return this.removeFormats([format], selection)
    }
    else {
      // If not, apply that format to the selection.
      return this.applyFormats([format], selection)
    }
  }

  render() {
    return this.runs.map(r => r.render()).join('')
  }
}

// TODO: Document class

const run1 = new Run('Foobar 1.', ["bold"])
const run2 = new Run(' Foobar 2.')
const run3 = new Run(' Foobar 3.', ["italic"])
const paragraph = new Paragraph([run1, run2, run3])

const myParagraph = new Paragraph([
  new Run('Hello.', ['italic']),
  new Run('A', []),
  new Run('Goodbye.', ['bold'])
])

// TODO: do a full test inside a browser with just one paragraph, but basic editing features and rendering working.

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

const testState = Paragraph.fromTemplate(testTemplate);

// module.exports =  { Selection, Run, Paragraph }

export { Selection, Run, Paragraph }
