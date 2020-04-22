class Selection {
  get single() {
    return (this.start.elem === this.end.elem && this.start.offset === this.end.offset)
  }

  constructor(start, end) {
    this.start = start
    this.end = end || start
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

  insert(selection, text) {
    [selection.start.offset, selection.end.offset].forEach(offset => {
      if (offset < 0 || offset > this.length) {
        throw new Error("Illegal offset " + offset);
      }
    })

    if (selection.single) {
      const { offset } = selection.start;

      const before = this.text.slice(0, offset);
      const after = this.text.slice(offset);
      console.log('offset: ' + offset);
      const modifiedText = before + text + after;

      return new Run(this.id, modifiedText, this.formats)
    }
    else {
      const before = this.text.slice(0, selection.start.offset);
      const after = this.text.slice(selection.end.offset);
      const modifiedText = before + text + after;

      return new Run(this.id, modifiedText, this.formats)
    }

    // TODO: check edge cases (beginning, end, etc)
  }

  remove(selection, text) {
    // TODO
  }
}

const run1 = new Run(1, "Foobar")
const run2 = new Run(1, "Foo")

// "Foobar".insert(1, 3, 'a') -> "Fabar"
console.log(run1.insert(new Selection({ offset: 1, elem: 1 }, { offset: 3, elem: 1 }), 'a'))

// "Foobar".insert(0, 6, 'Fizzbuz') -> "Fizzbuzz"
console.log(run1.insert(new Selection({ offset: 0 }, { offset: 6 }), 'Fizzbuzz'))

// "Foo".insert(1, 'h') -> "Fhoo"
console.log(run2.insert(new Selection({ offset: 1, elem: 1 }), 'h'))

// "Foo".insert(0, 'h') -> "hFoo"
console.log(run2.insert(new Selection({ offset: 0, elem: 1 }), 'h'))

// "Foobar".insert(6, 'a') -> "Foobara"
console.log(run1.insert(new Selection({ offset: 6, elem: 1 }), 'a'))

// "Foobar".insert(7, 'a') -> Error
// console.log(run1.insert(new Selection({ offset: 7, elem: 1 }), 'a'))

// "Foobar".insert(-1, 7, 'whatever') -> Error
// console.log(run1.insert(new Selection({ offset: -1, elem: 1 }, { offset: 7, elem: 1 }), 'whatever'))

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
