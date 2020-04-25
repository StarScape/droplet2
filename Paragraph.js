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

  // Returns the run at `offset` into the paragraph
  runAtOffset(offset) {
    if (offset < 0 || offset > this.length) {
      throw new Error("Illegal offset into Paragraph " + offset)
    }

    // length: 9, 10, 10
    let runIdx = -1;
    let sumOfPreviousOffsets = 0; //this.runs[runIdx].length; // 9

    while (sumOfPreviousOffsets <= offset) {
      runIdx += 1
      sumOfPreviousOffsets += this.runs[runIdx].length
    }

    return this.runs[runIdx]
  }

  insert(selection, text, formats) {
    if (selection.single) {
      const run = this.runAtOffset(selection.caret)
    }
    else {
      // TODO
    }
  }

  // TODO: remove
  // TODO: applyFormats
  // TODO: removeFormats
}

const run1 = new Run(1, 'Foobar 1.', ["bold"])
const run2 = new Run(2, ' Foobar 2.')
const run3 = new Run(3, ' Foobar 3.', ["italic"])
const paragraph = new Paragraph([run1, run2, run3])

// [b:"Foobar 1."][" Foobar 2."][i:" Foobar 3."]
//   .insert(8, " Foobar 1.5.", "i")
//     -> paragraph: [b:"Foobar 1."][i:" Foobar x."][" Foobar 2."][i:" Foobar 3."]
//     -> selection: 18
const paragraph2 = paragraph.insert(new Selection({ elem: 1, offset: 8 }), new Run(4, " Foobar x.", ["italic"]))

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
