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
  expect(paragraph.runAtOffset(0).text).toBe('Foobar 1.')   // beginning of 1
  expect(paragraph.runAtOffset(5).text).toBe('Foobar 1.')   // middle of 1
  expect(paragraph.runAtOffset(8).text).toBe('Foobar 1.')   // end of 1

  expect(paragraph.runAtOffset(9).text).toBe(' Foobar 2.')  // beginning of 2
  expect(paragraph.runAtOffset(11).text).toBe(' Foobar 2.') // middle of 2
  expect(paragraph.runAtOffset(18).text).toBe(' Foobar 2.') // end of 2

  expect(paragraph.runAtOffset(19).text).toBe(' Foobar 3.') // beginning of 3
  expect(paragraph.runAtOffset(25).text).toBe(' Foobar 3.') // middle of 3
  expect(paragraph.runAtOffset(28).text).toBe(' Foobar 3.') // end of 3
})
