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

test('runAtOffset', () => {
  const match = (p, offset, expectedText, expectedOffset) => {
    const [receivedRunIdx, receivedRunOffset] = p.runAtOffset(offset)
    expect(p.runs[receivedRunIdx].text).toBe(expectedText)
    expect(receivedRunOffset).toBe(expectedOffset)
  }

  // 'Foobar 1. Foobar 2. Foobar 3.'
  match(paragraph, 0, 'Foobar 1.', 0)
  match(paragraph, 5, 'Foobar 1.', 5)
  match(paragraph, 8, 'Foobar 1.', 8)
  match(paragraph, 9, 'Foobar 1.', 9)
  match(paragraph, 10, ' Foobar 2.', 1)
  match(paragraph, 11, ' Foobar 2.', 2)
  match(paragraph, 18, ' Foobar 2.', 9)
  match(paragraph, 19, ' Foobar 2.', 10)
  match(paragraph, 20, ' Foobar 3.', 1)
  match(paragraph, 25, ' Foobar 3.', 6)
  match(paragraph, 28, ' Foobar 3.', 9)

  expect(() => paragraph.runAtOffset(35)[0]).toThrow()
  expect(() => paragraph.runAtOffset(-1)[0]).toThrow()
})

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
