class Selection {
  get single() {
    return (
      this.start.elem === this.end.elem &&
      this.start.offset === this.end.offset
    )
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
  }

  // TODO: applyFormats
  // TODO: removeFormats
}

const run1 = new Run(1, "Foobar")
const run2 = new Run(1, "Foo")

// "Foobar".insert(1, 3, 'a') -> "Fabar", sel(1)
console.log(run1.insert(new Selection({ offset: 1, elem: 1 }, { offset: 3, elem: 1 }), 'a'))

// "Foobar".insert(0, 6, 'Fizzbuz') -> "Fizzbuzz", sel(8)
console.log(run1.insert(new Selection({ offset: 0, elem: 1 }, { offset: 6, elem: 1 }), 'Fizzbuzz'))

// "Foo".insert(1, 'h') -> "Fhoo", sel(2)
console.log(run2.insert(new Selection({ offset: 1, elem: 1 }), 'h'))

// "Foo".insert(0, 'h') -> "hFoo", sel(1)
console.log(run2.insert(new Selection({ offset: 0, elem: 1 }), 'h'))

// "Foobar".insert(6, 'a') -> "Foobara", sel(7)
console.log(run1.insert(new Selection({ offset: 6, elem: 1 }), 'a'))

// "Foobar".insert(7, 'a') -> Error
// console.log(run1.insert(new Selection({ offset: 7, elem: 1 }), 'a'))

// "Foobar".insert(-1, 7, 'whatever') -> Error
// console.log(run1.insert(new Selection({ offset: -1, elem: 1 }, { offset: 7, elem: 1 }), 'whatever'))

// "Foobar".remove(0, 1) -> "oobar", sel(0)
console.log(run1.remove(new Selection({ offset: 0, elem: 1 }, { offset: 1, elem: 1 })))

// "Foobar".remove(1, 3) -> "Fbar", sel(1)
console.log(run1.remove(new Selection({ offset: 1, elem: 1 }, { offset: 3, elem: 1 })))

// "Foobar".remove(0, 6) -> "", sel(0)
console.log(run1.remove(new Selection({ offset: 0, elem: 1 }, { offset: 6, elem: 1 })))

// "Foobar".remove(5) -> Foobr, sel(4)
console.log(run1.remove(new Selection({ offset: 5, elem: 1 })))

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
