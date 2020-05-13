const { Selection } = require('../Paragraph.js')

test('initialization', () => {
  const sel = new Selection({ pid: 1, offset: 1 }, { pid: 1, offset: 12 })

  expect(sel.start.offset).toBe(1)
  expect(sel.end.offset).toBe(12)

  expect(() =>
    new Selection({ pid: 1, offset: -1 })
  ).toThrow()

  expect(() => 
    new Selection({ pid: 1, offset: 0 }, { pid: 1, offset: -100 })
  ).toThrow()
})

test('single', () => {
  const s1 = new Selection({ pid: 1, offset: 1 })
  const s2 = new Selection({ pid: 1, offset: 1 }, { pid: 1, offset: 2 })
  const s3 = new Selection({ pid: 1, offset: 1 }, { pid: 2, offset: 1 })

  expect(s1.single).toBe(true)
  expect(s2.single).toBe(false)
  expect(s3.single).toBe(false)
})

test('caret', () => {
  const single = new Selection({ pid: 1, offset: 144 })
  expect(single.caret).toBe(144)

  const range = new Selection({ pid: 1, offset: 0 }, { pid: 1, offset: 33 })
  expect(() => range.caret).toThrow()
})

test('shiftSingle', () => {
  const s = new Selection({ pid: 1, offset: 10 })
  expect(s.shiftSingle(5).caret).toBe(15)
  expect(() => s.shiftSingle(-100)).toThrow()
})

test('collapse', () => {
  const s = new Selection({ pid: 1, offset: 12 }, { pid: 1, offset: 33 })
  const collapsed = s.collapse()
  expect(collapsed.caret).toBe(12)
  expect(collapsed.single).toBe(true)
})
