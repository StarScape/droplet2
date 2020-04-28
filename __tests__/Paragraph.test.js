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
    const [receivedRun, receivedRunOffset] = p.runAtOffset(offset)
    expect(receivedRun.text).toBe(expectedText)
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
