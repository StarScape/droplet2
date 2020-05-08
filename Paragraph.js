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

      return new Run(this.id, modifiedText, this.formats)
    }
    else {
      const before = this.text.slice(0, start)
      const after = this.text.slice(end)
      const modifiedText = before + text + after
      return new Run(this.id, modifiedText, this.formats)
    }
  }

  // Alias for inserting at start of run
  insertStart(text) {
    this.insert(text, 0)
  }

  // Alias for inserting at end of run
  insertEnd(text) {
    this.insert(text, this.length)
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
    if (end === undefined) {
      if (start === 0) {
        throw new Error('Cannot call remove() on a single selection at position 0.')
      }

      const before = this.text.slice(0, start - 1)
      const after = this.text.slice(start)
      const modifiedText = before + after

      return new Run(this.id, modifiedText, this.formats)
    }
    else {
      const before = this.text.slice(0, start)
      const after = this.text.slice(end)
      const modifiedText = before + after

      return new Run(this.id, modifiedText, this.formats)
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
    return this.runs.reduce((acc, run) => acc + run.length, 0)
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
      const [targetRunIdx, targetRunOffset] = this.runAtOffset(selection.caret)
      const targetRun = this.runs[targetRunIdx]

      if (formatsEqual(beforeCaret.formats, content[0].formats)) {
        content[0] = beforeCaret.insertStart(content[0].text)
      }
      else {
        content.splice(0, 0, beforeCaret)
      }

      const lastRun = content[content.length-1]
      if (formatsEqual(afterCaret.formats, lastRun.formats)) {
        content[content.length-1] = lastRun.insertEnd(afterCaret.text)
      }
      else {
        content.push(afterCaret)
      }
    }
    else {
      // TODO: multiple selection
      // return this.remove(selection).insertRange(selection, content)
    }
  }

  insertRange(content, selection) {
    if (content instanceof Run) {
      
    }
    else {
      throw new Error("Unrecognized form of content passed to insertRange " + content)
    }
  }

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

// const [paragraph2, selection] = paragraph.insert(
//   new Selection({ elem: 1, offset: 9 }),
//   new Run(4, " Foobar x.", ['bold']),
// )

// console.log(paragraph2.render());
// console.log(paragraph2.runs.length);

// [b:"Foobar 1.|"][" Foobar 2."][i:" Foobar 3."]
//   .insert(9, " Foobar 1.5.", [])
//     -> paragraph: [b:"Foobar 1."][i:" Foobar x."][" Foobar 2."][i:" Foobar 3."]
//     -> selection: 18

// const [p, s] = paragraph.insert(
//   new Selection({ elem: 1, offset: 9 }),
//   new Run(4, " Foobar x.", [])
// )

// console.log(p.render())
// console.log(p.runs.length)
// console.log(p.runs[1].render())

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
