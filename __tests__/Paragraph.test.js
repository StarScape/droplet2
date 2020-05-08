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

describe('insert', () => {
  test('single-selection insert at end of run with different formats', () => {

  })
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

describe('insert', () => {
  test('single-selection insert at end of run with different formats', () => {

  })
})
