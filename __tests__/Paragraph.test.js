const { Run, Selection, Paragraph } = require('../Paragraph.js')

let run1
let run2
let run3
let paragraph

beforeAll(() => {
  run1 = new Run(1, 'Foobar 1.', ["bold"])
  run2 = new Run(2, ' Foobar 2.')
  run3 = new Run(3, ' Foobar 3.', ["italic"])

  paragraph = new Paragraph([run1, run2, run3])
})

// test('runAtOffset', () => {
//   const match = (p, offset, expectedText, expectedOffset) => {
//     const [receivedRunIdx, receivedRunOffset] = p.runAtOffset(offset)
//     expect(p.runs[receivedRunIdx].text).toBe(expectedText)
//     expect(receivedRunOffset).toBe(expectedOffset)
//   }

//   // 'Foobar 1. Foobar 2. Foobar 3.'
//   match(paragraph, 0, 'Foobar 1.', 0)
//   match(paragraph, 5, 'Foobar 1.', 5)
//   match(paragraph, 8, 'Foobar 1.', 8)
//   match(paragraph, 9, 'Foobar 1.', 9)
//   match(paragraph, 10, ' Foobar 2.', 1)
//   match(paragraph, 11, ' Foobar 2.', 2)
//   match(paragraph, 18, ' Foobar 2.', 9)
//   match(paragraph, 19, ' Foobar 2.', 10)
//   match(paragraph, 20, ' Foobar 3.', 1)
//   match(paragraph, 25, ' Foobar 3.', 6)
//   match(paragraph, 28, ' Foobar 3.', 9)

//   expect(() => paragraph.runAtOffset(35)[0]).toThrow()
//   expect(() => paragraph.runAtOffset(-1)[0]).toThrow()
// })

// TODO: change this to test both beforeOffset and atOffset at same time
test('atOffset', () => {
  // 'Foobar 1. Foobar 2. Foobar 3.'
  expect(paragraph.atOffset(0)).toEqual([0, 0])
  expect(paragraph.atOffset(5)).toEqual([0, 5])
  expect(paragraph.atOffset(8)).toEqual([0, 8])
  expect(paragraph.atOffset(9)).toEqual([1, 0])
  
  expect(paragraph.atOffset(10)).toEqual([1, 1])
  expect(paragraph.atOffset(11)).toEqual([1, 2])
  expect(paragraph.atOffset(18)).toEqual([1, 9])

  expect(paragraph.atOffset(19)).toEqual([2, 0])
  expect(paragraph.atOffset(20)).toEqual([2, 1])
  expect(paragraph.atOffset(25)).toEqual([2, 6])
  expect(paragraph.atOffset(28)).toEqual([2, 9])
  expect(paragraph.atOffset(29)).toEqual([2, 10])

  expect(() => paragraph.atOffset(35)[0]).toThrow()
  expect(() => paragraph.atOffset(-1)[0]).toThrow()
})

test('optimizeRuns', () => {
  const runs = [
    new Run(1, 'a', ['italic']),
    new Run(1, 'b', ['italic']),
    new Run(1, 'c', []),
    new Run(1, '', []),         // make sure empty run is removed
    new Run(1, 'd', ['bold']),
    new Run(1, 'e', ['bold']),
    new Run(1, 'f', ['bold']),
    new Run(1, 'g', []),
    new Run(1, 'h', []),
    new Run(1, 'i', ['bold']),
    new Run(1, 'j', [])
  ]

  const optimized = Paragraph.optimizeRuns(runs)

  // Check that each set of adjacent runs w/ identical formatting
  // are compressed into each other, and have the same formatting.
  expect(optimized[0].text).toBe('ab')
  expect(optimized[0].formats).toEqual(['italic'])

  expect(optimized[1].text).toBe('c')
  expect(optimized[1].formats).toEqual([])

  expect(optimized[2].text).toBe('def')
  expect(optimized[2].formats).toEqual(['bold'])

  expect(optimized[3].text).toBe('gh')
  expect(optimized[3].formats).toEqual([])

  expect(optimized[4].text).toBe('i')
  expect(optimized[4].formats).toEqual(['bold'])

  expect(optimized[5].text).toBe('j')
  expect(optimized[5].formats).toEqual([])

  expect(optimized.length).toBe(6)

})

// Provide a simple way to render a paragraph as a string, thereby allowing us
// to check output of insert() in the test below without manually checking each
// item in the array every time, and all the glorious headache that would provide.
//
// As of writing, this is identical to the render() methods on Run and Paragraph, but
// it is doubtful that it will always be. This can stay here as a simple solution for
// the tests.
const stringMapFormats = (r, func) => r.map(f => func(f)).join('')
const naiveRunRender = (r) => {
  return stringMapFormats(r.formats, f => `<${f}>`) + r.text + stringMapFormats(r.formats, f => `</${f}>`)
}
const naiveParagraphRender = (p) => p.runs.map(r => naiveRunRender(r)).join('')

describe('insert (single selection)', () => {
  // In HTML:
  // <b>Foobar 1.</b> Foobar 2.<i> Foobar 3.</i>
  const paragraph = new Paragraph([
    new Run(1, 'Foobar 1.', ["bold"]),
    new Run(2, ' Foobar 2.'),
    new Run(3, ' Foobar 3.', ["italic"])
  ])

  test('at end of run with same formatting', () => {
    const [p, s] = paragraph.insert(
      [ new Run(4, " Foobar 1.5.", ['bold']) ],
      new Selection({ pid: 1, offset: 9 })
    )

    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Foobar 1. Foobar 1.5.</bold> Foobar 2.<italic> Foobar 3.</italic>'
    )

    expect(s.start.offset).toEqual(21)
  })

  // TODO: write these other tests to work like the one above.

  test('at end of run with different formatting', () => {
    const [p, s] = paragraph.insert(
      [new Run(4, " Foobar 1.5.", [])],
      new Selection({ pid: 1, offset: 9 })
    )

    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Foobar 1.</bold> Foobar 1.5. Foobar 2.<italic> Foobar 3.</italic>'
    )

    expect(s.start.offset).toEqual(21)
  })

  test('middle of run with different formatting', () => {
    const [p, s] = paragraph.insert(
      [new Run(4, "bizzbuzz", ['italic'])],
      new Selection({ pid: 1, offset: 16 })
    )

    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Foobar 1.</bold> Foobar<italic>bizzbuzz</italic> 2.<italic> Foobar 3.</italic>'
    )

    expect(s.start.offset).toEqual(24)
  })

  test('start of paragraph', () => {
    const [p, s] = paragraph.insert(
      [new Run(4, "Pre. ", ['underline'])],
      new Selection({ pid: 1, offset: 0 })
    )

    expect(naiveParagraphRender(p)).toEqual(
      '<underline>Pre. </underline><bold>Foobar 1.</bold> Foobar 2.<italic> Foobar 3.</italic>'
    )

    expect(s.start.offset).toEqual(5)
  })

  test('end of paragraph', () => {
    const [p, s] = paragraph.insert(
      [new Run(4, ' Post.', [])],
      new Selection({ pid: 1, offset: 29 })
    )

    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Foobar 1.</bold> Foobar 2.<italic> Foobar 3.</italic> Post.'
    )

    expect(s.start.offset).toEqual(35)
  })
})

describe('remove (single selection)', () => {
  const paragraph = new Paragraph([
    new Run(1, 'Foobar 1.', ["bold"]),
    new Run(2, ' Foobar 2.'),
    new Run(3, ' Foobar 3.', ["italic"])
  ])

  test('at beginning of paragraph', () => {
    expect(() => paragraph.remove(new Selection({ pid: 1, offset: 0 }))).toThrow()
  })

  test('middle of paragraph', () => {
    const [p, s] = paragraph.remove(new Selection({ pid: 1, offset: 6 }))
    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Fooba 1.</bold> Foobar 2.<italic> Foobar 3.</italic>'
    )
    expect(s.caret).toEqual(5)
  })

  test('end of run', () => {
    const [p, s] = paragraph.remove(new Selection({ pid: 1, offset: 9 }))
    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Foobar 1</bold> Foobar 2.<italic> Foobar 3.</italic>'
    )
    expect(s.caret).toEqual(8)
  })

  test('end of paragraph', () => {
    const [p, s] = paragraph.remove(new Selection({ pid: 1, offset: 29 }))
    expect(naiveParagraphRender(p)).toEqual(
      '<bold>Foobar 1.</bold> Foobar 2.<italic> Foobar 3</italic>' // no period at end
    )
    expect(s.caret).toEqual(28)
  })

  test('single-character run', () => {
    const myParagraph = new Paragraph([
      new Run(1, 'Hello.', ['italic']),
      new Run(2, 'A', []),
      new Run(3, 'Goodbye.', ['bold'])
    ])
    const [p, s] = myParagraph.remove(new Selection({ pid: 1, offset: 7 }))
    expect(naiveParagraphRender(p)).toEqual(
      '<italic>Hello.</italic><bold>Goodbye.</bold>'
    )
    expect(p.runs.length).toEqual(2)
  })
})
