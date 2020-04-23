class Selection {
  get single() {
    return (
      this.start.elem === this.end.elem &&
      this.start.offset === this.end.offset
    )
  }

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
    return this.text.length
  }

  constructor(id, text, formats = []) {
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
        throw new Error("Illegal offset " + offset);
      }
    })

    if (selection.single) {
      const { offset } = selection.start

      const before = this.text.slice(0, offset)
      const after = this.text.slice(offset)
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
    if (selection.single) {
      const before = this.text.slice(0, selection.start.offset - 1)
      const after = this.text.slice(selection.end.offset)
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

    // TODO: edge cases
  }

  // TODO: applyFormats
  // TODO: removeFormats
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

  constructor(runs) {
    this.runs = runs
  }

  insert(selection, text) {
    if (!selection.range) {

    }
  }
}

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
