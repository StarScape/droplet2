const _ = require('./lodash')

const formatsEqual = (f1, f2) =>
  _.isEqual(f1.slice().sort(), f2.slice().sort())

class Selection {
  get single() {
    return (
      this.start.elem === this.end.elem &&
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

    if (this.start === this.end && this.start.offset > this.end.offset) {
      throw new Error("Error initializing Selection: end offset cannot precede start offset.")
    }
  }

  // Returns new single selection at the start of this selection.
  // If this selection is a multiple selection, returns itself.
  collapse() {
    if (this.single) {
      throw new Error("Cannot call collapse() on single selection")
    }
    return this.single ? this : new Selection(this.start)
  }

  // For a single selection, returns new
  // Selection advanced by `n` characters
  incrementSingle(n) {
    if (!this.single) {
      throw new Error("Cannot call incrementSingle() on range selection")
    }
    return new Selection({ ...this.start, offset: this.start.offset + n })
  }
}

class Run {
  get length() {
    // TODO: memoize
    return this.text.length
  }

  constructor(id, text, formats = []) {
    // TODO: remove id
    this.id = id
    this.text = text
    this.formats = formats
  }

  // TODO: we can do away with Selections at a Run level (oops...).
  // Selection start and end is defined as a paragraph id and an offset into
  // that paragraph anyway, so there's really no need to go through the rigamaroll
  // of get paragraph level selection -> call modification on Run -> receive new Selection
  // relative to Run -> translate back up to paragraph-level selection. We can just strip out
  // the two intermediate steps. Just be careful and commit before you do it, you baboon.

  /**
   * Returns resulting [new run, new selection] pair
   * from inserting the `text` at `selection`.
   *
   * @param selection Selection object
   * @param text String to insert
   *
   * @return [resulting run, resulting selection]
   */
  insert(selection, text) {
    [selection.start.offset, selection.end.offset].forEach(offset => {
      if (offset < 0 || offset > this.length) {
        throw new Error("Illegal offset Run " + offset);
      }
    })

    if (selection.single) {
      const before = this.text.slice(0, selection.caret)
      const after = this.text.slice(selection.caret)
      const modifiedText = before + text + after

      return [
        new Run(this.id, modifiedText, this.formats),
        selection.incrementSingle(text.length)
      ]
    }
    else {
      const before = this.text.slice(0, selection.start.offset)
      const after = this.text.slice(selection.end.offset)
      const modifiedText = before + text + after

      return [
        new Run(this.id, modifiedText, this.formats),
        selection.collapse().incrementSingle(text.length)
      ]
    }
  }

  /**
   * Returns the [new run, new selection] pair from removing at `selection`.
   *
   * @param selection Selection object
   *
   * @return [new run, new selection]
   */
  remove(selection) {
    [selection.start.offset, selection.end.offset].forEach(offset => {
      if (offset < 0 || offset > this.length) {
        throw new Error("Illegal offset into Run " + offset);
      }
    })

    if (selection.single) {
      if (selection.caret === 0) {
        throw new Error('Cannot call remove() on a single selection at position 0')
      }

      const before = this.text.slice(0, selection.caret - 1)
      const after = this.text.slice(selection.caret)
      const modifiedText = before + after

      return [
        new Run(this.id, modifiedText, this.formats),
        selection.incrementSingle(-1)
      ]
    }
    else {
      const before = this.text.slice(0, selection.start.offset)
      const after = this.text.slice(selection.end.offset)
      const modifiedText = before + after

      return [
        new Run(this.id, modifiedText, this.formats),
        selection.collapse()
      ]
    }
  }

  // Returns a new Run with `formats` added
  applyFormats(formats) {
    const intersection = new Set([...formats, ...this.formats])
    return new Run(this.id, this.text, [...intersection])
  }

  // Returns a new Run with `formats` removed
  removeFormats(formats) {
    return new Run(this.id, this.text, this.formats.filter(f => !formats.includes(f)))
  }

  render() {
    // TODO: this should maybe be changed to use CSS or something
    const stringifyFormats = (stringifier) => this.formats.map(f => stringifier(f)).join('')
    return stringifyFormats(f => `<${f}>`) + this.text + stringifyFormats(f => `</${f}>`)
  }

  // TODO: helper methods before(offset) and after(offset)
}

class Paragraph {
  static fromTemplate(obj) {
    return [
      new Selection(

        obj.selection.start,
        obj.selection.end,
      ),
      new Paragraph(
        obj.runs.map(r => new Run(r.id, r.text, r.formats))
      ),
    ]
  }

  // Returns the sum of the lengths of the paragraph's runs
  get length() {
    // TODO: memoize
    return this.runs.reduce((r1, r2) => r1.length + r2.length, 0)
  }

  constructor(runs) {
    this.runs = runs
  }

  // Returns the run at `offset`, and `offset` translated to an offset into that run.
  // Returned as a pair, [run, runOffset].
  runAtOffset(offset) {
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

  // TODO: description
  insert(selection, content) {
    if (selection.single) {
      return this.insertSingle(selection, content)
    }
    else {
      // TODO: multiple selection
      return this.insertRange(selection, content)
    }
  }

  insertRange(selection, content) {
    if (content instanceof Run) {
      
    }
    else if (content instanceof Array && content[0] instanceof Paragraph) {

    }
    else {
      throw new Error("Unrecognized form of content passed to insertRange " + content)
    }
  }

  // Insert for single selection
  insertSingle(selection, content) {
    // Get runs on either side of text caret
    // TODO: change to something better than run1/run2
    const [run1Idx, run1Offset] = this.runAtOffset(selection.caret)
    const [run2Idx, run2Offset] = this.runAtOffset(selection.caret + 1)
    const run1 = this.runs[run1Idx]
    const run2 = this.runs[run2Idx]

    if (formatsEqual(run1.formats, content.formats)) {
      const [newRun, newSelection] = run1.insert(
        new Selection({ elem: selection.elem, offset: run1Offset }),
        content.text
      )

      const newRuns = Object.assign([], this.runs, { [run1Idx]: newRun });
      return [new Paragraph(newRuns), selection.incrementSingle(content.length)]
    }
    else if (formatsEqual(run2.formats, content.formats)) {
      const [newRun, newSelection] = run2.insert(
        new Selection({ elem: selection.elem, offset: 0 }),
        content.text
      )

      const newRuns = Object.assign([], this.runs, { [run2Idx]: newRun });
      return [new Paragraph(newRuns), selection.incrementSingle(content.length)]
    }
    else {
      const [runAfterCaretIdx, runAfterCaretOffset] = this.runAtOffset(selection.caret + 1)

      const newRuns = this.runs.slice();
      newRuns.splice(run1Idx + 1, 0, content)
      return [new Paragraph(newRuns), selection.incrementSingle(content.length)]
    }
  }

  // TODO: remove
  // TODO: applyFormats
  // TODO: removeFormats

  render() {
    return this.runs.map(r => r.render()).join('')
  }
}

const run1 = new Run(1, 'Foobar 1.', ["bold"])
const run2 = new Run(2, ' Foobar 2.')
const run3 = new Run(3, ' Foobar 3.', ["italic"])
const paragraph = new Paragraph([run1, run2, run3])

// [b:"Foobar 1.|"][" Foobar 2."][i:" Foobar 3."]
//   .insert(9, " Foobar 1.5.", ["b"])
//     -> paragraph: [b:"Foobar 1."][i:" Foobar x."][" Foobar 2."][i:" Foobar 3."]
//     -> selection: 18

const [paragraph2, selection] = paragraph.insert(
  new Selection({ elem: 1, offset: 9 }),
  new Run(4, " Foobar x.", ['bold']),
)

console.log(paragraph2.render());
console.log(paragraph2.runs.length);

// [b:"Foobar 1.|"][" Foobar 2."][i:" Foobar 3."]
//   .insert(9, " Foobar 1.5.", [])
//     -> paragraph: [b:"Foobar 1."][i:" Foobar x."][" Foobar 2."][i:" Foobar 3."]
//     -> selection: 18

const [p, s] = paragraph.insert(
  new Selection({ elem: 1, offset: 9 }),
  new Run(4, " Foobar x.", [])
)

console.log(p.render())
console.log(p.runs.length)
console.log(p.runs[1].render())

// console.log(paragraph.runAtOffset(0)) // 1
// console.log(paragraph.runAtOffset(8)) // 1
// console.log(paragraph.runAtOffset(9)) // 2
// console.log(paragraph.runAtOffset(11)) // 2
// console.log(paragraph.runAtOffset(18)) // 2
// console.log(paragraph.runAtOffset(25)) // 3
// console.log(paragraph.runAtOffset(28)) // 3


// [b:"Foobar 1."][" Foobar 2."][i:" Foobar 3."].insert(8, " Foobar 1.5.", "i")
//   -> paragraph: [b:"Foobar 1."][i:" Foobar 1.5."][" Foobar 2."][i:" Foobar 3."]
//   -> selection: 20

const testTemplate = {
  selection: {
    range: false,
    start: {
      elem: 1,
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
